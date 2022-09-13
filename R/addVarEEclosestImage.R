#' Add variable from closest image in a Google Earth Engine Collection
#'
#' @description Each feature in a feature collection (i.e. row in asimple feature
#' collection) is matched with the image in an image collection that is closest
#' to it in time.
#' @param ee_feats A feature collection we want to annotate with GEE data.
#' We need to upload an sf object with the features to GEE. This object must
#' also have character column with the dates that need to be matched against the
#' image collection dates. The format must be "yyyy-mm-dd" and the column must
#' be named "Date".
#' @param collection Either a character string with the name of the collection
#' we want to use or a GEE collection produced with \code{ee$ImageCollection()}.
#' See \href{https://developers.google.com/earth-engine/datasets/catalog}{GEE catalog}.
#' @param reducer A character string specifying the function apply when
#' extracting values for each feature. It is common to use "mean", "sum" or
#' "count". But there are many other, see 'ee.Reducer' under Client Libraries at
#' \url{https://developers.google.com/earth-engine/apidocs}.
#' @param maxdiff Maximum difference in days allowed for an image to be matched with
#' data.
#' @param bands Select specific bands from the image. Only one band at a time is
#' allowed for now.
#' @param unmask GEE masks missing values, which means they are not used for
#' computing means, counts, etc. Sometimes we might want to avoid this behaviour
#' and use 0 instead of NA. If so, set unmask to TRUE.
#' @param monitor Logical. If TRUE (default) monitoring messages produced
#' by `rgee` will displayed. If FALSE, only high-level messages will be displayed.
#'
#' @return A dataframe similar to \code{ee_feats} with variables added from the
#' \code{bands} selected from \code{collection}. Note that following \href{https://github.com/r-spatial/rgee}{rgee}
#' the name of the new variables will be the selected band (\code{bands} or else
#' all bands from \code{collection} followed by the spatial reducer \code{reducer}.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Load the remote data asset
#' ee_points <- ee$FeatureCollection(pointAssetId)  # assetId must correspond to an asset in your GEE account
#'
#'# Annotate points with TerraClimate dataset
#' points_tmmn <- addVarEEclosestImage(ee_feats = ee_points,
#'                                    collection = "IDAHO_EPSCOR/TERRACLIMATE",
#'                                    maxdiff = 15,
#'                                    bands = "tmmn")
#'
#'# Annotate polygons with TerraClimate dataset
#' ee_polgs <- ee$FeatureCollection(polgsAssetId)  # assetId must correspond to an asset in your GEE account
#'
#' features_tmmn <- addVarEEclosestImage(ee_feats = ee_polgs,
#'                                    collection = "IDAHO_EPSCOR/TERRACLIMATE",
#'                                    maxdiff = 15,
#'                                    reducer = "mean",
#'                                    bands = "tmmn")
#' }
addVarEEclosestImage <- function(ee_feats, collection, reducer = NULL, maxdiff,
                                 bands = NULL, unmask = FALSE, monitor = TRUE){

  # Get image
  if(is.character(collection)){
    ee_layer <- rgee::ee$ImageCollection(collection)
  } else if("ee.imagecollection.ImageCollection" %in% class(collection)){
    ee_layer <- collection
  } else {
    stop("collection must be either a character string or a GEE image collection")
  }

  # Get nominal scale for the layer (native resolution) and projection
  scale <- ee_layer$first()$projection()$nominalScale()$getInfo()

  # Subset bands
  if(!is.null(bands)){
    ee_layer <- ee_layer$select(bands)
  }

  # Remove missing values (this will depend on the layer)
  if(unmask){
    ee_layer <- ee_layer$unmask()
  }

  # Function to add date in milliseconds
  addTime <- function(feature) {
    datemillis <- rgee::ee$Date(feature$get('Date'))$millis()
    return(feature$set(list('date_millis' = datemillis)))
  }

  # Add date in milliseconds
  ee_feats <- ee_feats$map(addTime)

  # Set filter to select images within max time difference
  maxDiffFilter = rgee::ee$Filter$maxDifference(
    difference = maxdiff*24*60*60*1000,        # days * hr * min * sec * milliseconds
    leftField = "date_millis",                 # Timestamp of the visit
    rightField = "system:time_start"           # Image date
  )

  # Set a saveBest join that finds the image closest in time
  saveBestJoin <- rgee::ee$Join$saveBest(
    matchKey = "bestImage",
    measureKey = "timeDiff"
  )

  # Apply the join
  best_matches <- saveBestJoin$apply(ee_feats, ee_layer, maxDiffFilter)

  # A spatial reducer is necessary but for points we can use 'mean' and still
  # obtain the value corresponding to the point location
  if(is.null(reducer)){
    eff_reducer <- "mean"
  } else {
    eff_reducer <- reducer
  }

  # Function to add value from the matched image
  eeReducer <- paste0("rgee::ee$Reducer$", eff_reducer, "()")
  add_value <- function(feature){

    # Get the image selected by the join
    img <- rgee::ee$Image(feature$get("bestImage"))$select(bands)

    # Reduce values within feature
    site_val <- img$reduceRegion(eval(parse(text = eeReducer)),
                                   feature$geometry(),
                                   scale)

    # Return the data containing value and image date.
    return(feature$set('val', site_val$get(bands),
                       'DateTimeImage', img$get('system:index')))

  }

  if(!monitor) sink(nullfile())

  # Add values to the data and download
  out <- best_matches$map(add_value) %>%
    rgee::ee_as_sf(via = 'drive')

  if(!monitor) sink()

  # Fix names and variables
  if(is.null(reducer)){
    layer_name <- bands
  } else {
    layer_name <- paste0(bands, "_", eff_reducer)
  }

  out <- out %>%
    dplyr::rename_with(~gsub("val", layer_name, .x), .cols = dplyr::starts_with("val")) %>%
    dplyr::select(-c(id, date_millis, bestImage)) %>%
    dplyr::select(dplyr::everything(), DateTimeImage)

  return(out)

}
