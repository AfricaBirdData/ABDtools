#' Add variable from Google Earth Engine Image
#'
#' @param ee_feats A feature collection with the sites we want to annotate.
#' We should have uploaded an sf object with the sites to GEE, previously.
#' See \code{\link{uploadFeaturesToEE}}
#' @param image Either a character string with the name of the image we want to
#' use or an GEE image produced with \code{ee$Image()}. See
#' \href{https://developers.google.com/earth-engine/datasets/catalog}{GEE catalog}.
#' @param reducer Optional. A character string specifying the function apply when
#' extracting values for each sites. This is unnecessary when annotating points, but
#' you might have uploaded polygons instead. This reducer will compute a spatial
#' summary of the environmental layer within the polygon. It is common to use "mean", "sum" or
#' "count". But there are many other, see 'ee.Reducer' under Client Libraries at
#' \url{https://developers.google.com/earth-engine/apidocs}.
#' @param bands Select specific bands from the image. If not specified, sites
#' will be annotated with all bands (one column for each band).
#' @param unmask GEE masks missing values, which means they are not used for
#' computing means, counts, etc. Sometimes we might want to avoid this behaviour
#' and use 0 instead of NA. If so, set unmask to TRUE.
#' @param monitor Logical. If TRUE (default) monitoring messages produced
#' by `rgee` will displayed. If FALSE, only high-level messages will be displayed.
#'
#' @return A dataframe similar to \code{ee_sites} with variables added from the
#' \code{bands} selected from \code{collection}. Note that following \href{https://github.com/r-spatial/rgee}{rgee}
#' the name of the new variables will be: the selected band (\code{bands} or else
#' all bands from \code{collection} followed by the spatial reducer \code{reducer}, if present.
#' @export
#'
#' @examples
#' \dontrun{
#' # Load the remote data asset
#' ee_points <- ee$FeatureCollection(pointAssetId)  # assetId must correspond to an asset in your GEE account
#'
#'# Annotate sites with the surface water occurrence image
#' sites_tmmn <- addVarEEcollection(ee_feats = ee_points,
#'                                  image = "JRC/GSW1_3/GlobalSurfaceWater",
#'                                  bands = "occurrence")
#'
#'# We can also annotate polygons using the mean surface water occurrence within
#'# the polygon
#' ee_polgs <- ee$FeatureCollection(polgsAssetId)  # assetId must correspond to an asset in your GEE account
#'
#' sites_tmmn <- addVarEEcollection(ee_feats = ee_polgs,
#'                                  image = "JRC/GSW1_3/GlobalSurfaceWater",
#'                                  reducer = "mean",
#'                                  bands = "occurrence")
#' }
addVarEEimage <- function(ee_feats, image, reducer = NULL,
                          bands = NULL, unmask = FALSE, monitor = TRUE){

  # Get image
  if(is.character(image)){
    ee_layer <- rgee::ee$Image(image)
  } else if("ee.image.Image" %in% class(image)){
    ee_layer <- image
  } else {
    stop("image must be either a character string or a GEE image")
  }

  # Subset bands
  if(!is.null(bands)){
    ee_layer <- ee_layer$select(bands)
  }

  # Remove missing values (this will depend on the layer)
  if(unmask){
    ee_layer <- ee_layer$unmask()
  }

  message(paste(Sys.time(), "Annotating", deparse(substitute(ee_feats)), "with", image))

  # Get nominal scale for the layer (native resolution)
  scale <- ee_layer$projection()$nominalScale()$getInfo()

  if(!monitor) sink(nullfile())

  # A spatial reducer is necessary but for points we can use 'mean' and still
  # obtain the value corresponding to the point location
  if(is.null(reducer)){
    eff_reducer <- "mean"
  } else {
    eff_reducer <- reducer
  }

  # Extract layer values
  eeReducer <- paste0("rgee::ee$Reducer$", eff_reducer, "()")
  sites_layer <- ee_layer %>%
    rgee::ee$Image$reduceRegions(ee_feats,
                                 eval(parse(text = eeReducer)),
                                 scale = scale) %>%
    rgee::ee_as_sf(via = "drive")

  # Fix layer name
  if(!is.null(bands) & length(bands) == 1){

    if(is.null(reducer)){
      layer_name <- bands
    } else {
      layer_name <- paste0(bands, "_", eff_reducer)
    }

      sites_layer <- sites_layer %>%
          dplyr::rename("{layer_name}" := eff_reducer)

  }

  if(!monitor) sink()

  # This should do something similar but get problems with maxFeatures
  # sites_layer <- ee_extract(x = ee_layer,
  #                                   y = ee_sites,
  #                                   fun = eval(parse(text = reducer)),
  #                                   scale = scale,
  #                                   sf = TRUE,
  #                                   via = "drive")

  return(sites_layer)


}
