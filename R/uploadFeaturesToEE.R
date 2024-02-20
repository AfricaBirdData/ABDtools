#' Upload features to Google Earth Engine server
#'
#' @param feats An \href{https://github.com/r-spatial/sf}{sf} object with the
#' spatial points or polygons to upload to GEE.
#' @param asset_id A character string with the name we want our features be
#' saved on the server with.
#' @param load If TRUE (default), the GEE asset is loaded into the R session.
#' @param max_feats Maximum number of features the function will try to upload
#' without splitting into pieces. The default is a sensible choice but you can
#' try modify it to serve your purposes.
#' @param max_try Maximum number of tries the function checks for the creation
#' of the asset on GEE.
#' @param monitor Logical. If TRUE (default) monitoring printed messages produced
#' by `rgee` will displayed. If FALSE, only high-level messages will be displayed.
#' @param transfer A list with parameters used to communicate to and from GEE. It
#' must have to elements named `via` and `bucket`. See \code{\link[rgee]{sf_as_ee}}
#'
#' @return Features are uploaded to GEE. In addition the resulting GEE feature
#' collection can be loaded into the R environment.
#' @details At this stage this features must be either polygons representing
#' pentads from the ABAP project or points representing count locations from the
#' CWAC project. Both must be in an \href{https://github.com/r-spatial/sf}{sf}
#' format, and are uploaded to GEE servers via "getInfo_to_asset" (see \link[rgee]{sf_as_ee}).
#' If there are more than `max_feats` features, the function will upload two objects and merge them
#' in the server under the name provided in `asset_id`. Keep in mind that
#' two intermediate objects will also be stored in the GEE assets directory under
#' the names "p1" and "p2". We recommend visiting your account and cleaning
#' unnecesary objects regularly.
#' The creation of the asset might take some time after all the computations are
#' finished. The function will check every minute during `max_try` minutes, and if
#' it is not successful at finding the asset, it will return an error.
#' @export
#'
#' @examples
#' \dontrun{
#' # Load CWAC counts
#' counts <- CWAC::getCwacSiteCounts(23472927)
#'
#' # Set an ID for your remote asset (data in GEE)
#' assetId <- file.path(ee_get_assethome(), 'counts')
#'
#' # Upload to GEE (if not done already - do this only once per asset)
#' uploadFeaturesToEE(feats = counts,
#'                    asset_id = assetId,
#'                    load = FALSE)
#'
#' # Load the remote asset to you local computer to work with it
#' ee_counts <- ee$FeatureCollection(assetId)
#'
#' # Alternatively we can upload to GEE and load the asset in one call
#' ee_counts <- uploadFeaturesToEE(feats = counts,
#'                                 asset_id = assetId,
#'                                 load = TRUE)
#' }
uploadFeaturesToEE <- function(feats, asset_id, load = TRUE, max_feats = 16250,
                             max_try = 10, monitor = TRUE,
                             transfer = list(via = "getInfo_to_asset", bucket = NULL)){

  total_feats <- nrow(feats)

  message(paste(Sys.time(), "Uploading features to", asset_id))

  # Upload features
  if(!monitor) sink(nullfile())

  if(total_feats > max_feats){                                   # For large objects

    print("Object larger than max_feats, so splitting data into pieces")

    parts <- total_feats %/% max_feats
    extra <- (total_feats %% max_feats) > 0

    parts <- parts + extra

    part_feats <- ceiling(total_feats / parts)

    ps <- vector("list", length = parts)

    for(i in seq_len(parts)){

      ini <- part_feats * (i - 1) + 1
      end <- min(total_feats, part_feats * i)

      ps[[i]] <- feats[ini:end, ]

    }

    names(ps) <- paste0("p", seq_len(parts))

    eenames <- file.path(rgee::ee_get_assethome(), names(ps))

    # Upload the different feature collections to GEE
    message("Uploading features:")
    for(i in seq_along(ps)){
      message(paste("Part", i))
      rgee::sf_as_ee(ps[[i]], assetId = eenames[i],
                     via = transfer$via, bucket = transfer$bucket)
      message("Done")
    }

    # Create an array to store feature collections
    feat_col_array <- vector("list", length = parts)

    # Loop to create feature collections and add them to the array
    for (i in seq_len(parts)) {
      feat_col = rgee::ee$FeatureCollection(eenames[i])
      feat_col_array[[i]] <- feat_col
    }

    merged_col <- feat_col_array$reduce(

      # Function to merge two feature collections
      rgee::ee_utils_pyfunc(
        function(collection1, collection2){
          return(collection1$merge(collection2))
        })

    )

    # Here we might need ee_table_to_drive or ee_table_to_gcs
    task <- rgee::ee_table_to_asset(collection = merged_col,
                                    description = "merged feats",
                                    assetId = asset_id,
                                    overwrite = TRUE)
    task$start()

  } else {                              # For small objects

    rgee::sf_as_ee(feats,
                   assetId = asset_id,
                   via = transfer$via,
                   bucket = transfer$bucket)
  }

  # check that the asset has been produced and wait longer otherwise
  assets <- rgee::ee_manage_assetlist(rgee::ee_get_assethome())

  try = 1
  while(!asset_id %in% assets$ID && try <= max_try){
    message(paste("Checking GEE asset status", try, "of", max_try))
    Sys.sleep(60)
    assets <- rgee::ee_manage_assetlist(rgee::ee_get_assethome())
    try = try + 1
  }

  if(!asset_id %in% assets$ID){
    error(paste("Asset", asset_id, "was not created after", max_try, "minutes."))
  } else {
    message(paste("Asset", asset_id, "created"))
  }

  if(!monitor) sink()

  # Load if required
  if(load){
    out <- rgee::ee$FeatureCollection(asset_id)
    return(out)
  }

}
