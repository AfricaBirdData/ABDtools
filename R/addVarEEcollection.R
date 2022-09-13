#' Add variable from Google Earth Engine Collection
#'
#' @param ee_feats A feature collection with the sites we want to annotate.
#' We should have uploaded an sf object with the sites to GEE, previously.
#' See \code{\link{uploadFeaturesToEE}}
#' @param collection Either a character string with the name of the collection
#' we want to use or a GEE collection produced with \code{ee$ImageCollection()}.
#' See \href{https://developers.google.com/earth-engine/datasets/catalog}{GEE catalog}.
#' @param dates A character vector with two elements c(start, end). Format must be
#' "yyyy-mm-dd".
#' @param temp_reducer A character string specifying the function to summarize
#' collection across time. It is common to use "mean", "sum" or "count", but
#' there are many others, see 'ee.Reducer' under Client Libraries at
#' \url{https://developers.google.com/earth-engine/apidocs}.
#' @param spt_reducer A character string specifying the function to apply when
#' extracting values for each feature. It is common to use "mean", "sum" or
#' "count", but there are many others, see 'ee.Reducer' under Client Libraries at
#' \url{https://developers.google.com/earth-engine/apidocs}.
#' @param bands Select specific bands from the image. If not specified, features
#' will be annotated with all bands (one column for each band).
#' @param unmask GEE masks missing values, which means they are not used for
#' computing means, counts, etc. Sometimes we might want to avoid this behaviour
#' and use 0 instead of NA. If so, set unmask to TRUE.
#' @param monitor Logical. If TRUE (default) monitoring messages produced
#' by `rgee` will displayed. If FALSE, only high-level messages will be displayed.
#'
#' @return A dataframe similar to \code{ee_feats} with variables added from the
#' \code{bands} selected from \code{collection}. Note that following \href{https://github.com/r-spatial/rgee}{rgee}
#' the name of the new variables will be the selected band (\code{bands} or else
#' all bands from \code{collection} followed by the spatial reducer \code{spt_reducer}.
#' The temporal reducer \code{temp_reducer} does not appear in the
#' name, and therefore, it is up to the user to keep track of how the temporal
#' reducer summarized the collection.
#' @export
#'
#' @examples
#' \dontrun{
#' # Load the remote data asset
#' ee_points <- ee$FeatureCollection(pointAssetId)  # assetId must correspond to an asset in your GEE account
#'
#'# Annotate points with TerraClimate dataset
#' points_tmmn <- addVarEEcollection(ee_feats = ee_points,
#'                                    collection = "IDAHO_EPSCOR/TERRACLIMATE",
#'                                    dates = c("2010-01-01", "2011-01-01"),
#'                                    temp_reducer = "mean",
#'                                    bands = "tmmn")
#'
#'# Annotate polygons with TerraClimate dataset
#' ee_polgs <- ee$FeatureCollection(polgsAssetId)  # assetId must correspond to an asset in your GEE account
#'
#' features_tmmn <- addVarEEcollection(ee_feats = ee_polgs,
#'                                    collection = "IDAHO_EPSCOR/TERRACLIMATE",
#'                                    dates = c("2010-01-01", "2011-01-01"),
#'                                    temp_reducer = "mean",
#'                                    spt_reducer = "mean",
#'                                    bands = "tmmn")
#' }
addVarEEcollection <- function(ee_feats, collection, dates,
                               temp_reducer = NULL, spt_reducer = NULL,
                               bands = NULL, unmask = FALSE, monitor = TRUE){

  # Get image
  if(is.character(collection)){
    ee_layer <- rgee::ee$ImageCollection(collection)$
      filterDate(dates[1], dates[2])
  } else if("ee.imagecollection.ImageCollection" %in% class(collection)){
    ee_layer <- collection$
      filterDate(dates[1], dates[2])
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

  # Reduce to image
  ee_temp_reducer <- paste0("rgee::ee$Reducer$", temp_reducer, "()")
  ee_layer <- ee_layer$reduce(eval(parse(text = ee_temp_reducer)))

  if(!monitor) sink(nullfile())

  # A spatial reducer is necessary but for points we can use 'mean' and still
  # obtain the value corresponding to the point location
  if(is.null(spt_reducer)){
    eff_reducer <- "mean"
  } else {
    eff_reducer <- spt_reducer
  }

  # Extract layer values
  ee_spt_reducer <- paste0("rgee::ee$Reducer$", eff_reducer, "()")
  sites_layer <- ee_layer %>%
    rgee::ee$Image$reduceRegions(ee_feats,
                                 eval(parse(text = ee_spt_reducer)),
                                 scale = scale) %>%
    rgee::ee_as_sf(via = "drive")

  # Fix layer name
  if(!is.null(bands) & length(bands) == 1){

    if(is.null(spt_reducer)){
      layer_name <- bands
    } else {
      layer_name <- paste0(bands, "_", eff_reducer)
    }

    sites_layer <- sites_layer %>%
      dplyr::rename("{layer_name}" := eff_reducer)

  }

  if(!monitor) sink()


  return(sites_layer)


}
