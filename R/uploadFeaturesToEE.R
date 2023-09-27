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
                             max_try = 10, monitor = TRUE){

  nfeats <- nrow(feats)

  message(paste(Sys.time(), "Uploading features to", asset_id))

  # Upload features
  if(!monitor) sink(nullfile())

  if(nfeats > max_feats){                                   # For large objects

    print("Object larger than max_feats, so splitting in half")

    halfeats <- nfeats %/% 2

    ps <- list(p1 = feats %>%
                 dplyr::slice(1:halfeats),
               p2 = feats %>%
                 dplyr::slice((halfeats + 1):nfeats))

    eenames <- sprintf("%s/%s", rgee::ee_get_assethome(), c("p1", "p2"))

    lapply(seq_along(ps), function(i)
      rgee::sf_as_ee(ps[[i]], assetId = eenames[i], via = "getInfo_to_asset"))

    eep1 <- rgee::ee$FeatureCollection(eenames[1])
    eep2 <- rgee::ee$FeatureCollection(eenames[2])

    out <- eep1$merge(eep2)

    task <- rgee::ee_table_to_asset(collection = out,
                                    description = "CWAC merged feats",
                                    assetId = asset_id,
                                    overwrite = TRUE)
    task$start()

  } else {                              # For small objects

    rgee::sf_as_ee(feats,
                   assetId = asset_id,
                   via = "getInfo_to_asset")
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
