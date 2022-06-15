motus_prop_viewshed_visible <- function(coords = c(), ht = 10, zoom = 10, halfangle = 10) { # 38.897659, -77.036564
# RTH 35.6837, -118.3053
  if (!requireNamespace("elevatr", quietly = TRUE)) install.packages("elevatr", quiet = TRUE)
  if (!requireNamespace("sp", quietly = TRUE)) install.packages("sp", quiet = TRUE)
  if (!requireNamespace("rgeos", quietly = TRUE)) install.packages("rgeos", quiet = TRUE)
  if (!requireNamespace("raster", quietly = TRUE)) install.packages("raster", quiet = TRUE)
  if (!requireNamespace("mapview", quietly = TRUE)) install.packages("mapview", quiet = TRUE)
  if (!requireNamespace("rgdal", quietly = TRUE)) install.packages("rgdal", quiet = TRUE)
  
  # Set up mapview display options
  mt <- c("CartoDB.DarkMatter", "Esri.WorldImagery", "Esri.WorldTopoMap", "OpenStreetMap")
  mapview::mapviewOptions(basemaps = mt, na.color = "#FFFFFF00")
  
  # Calculate range rings
  lon <- coords[2]; lat <- coords[1]
  coords <- cbind(lon, lat)
  prj <- sprintf("+proj=laea +x_0=0 +y_0=0 +lon_0=%f +lat_0=%f", lon, lat)
  
  pt_ll <- sp::SpatialPoints(coords, sp::CRS("+init=epsg:4326"))
  pt <- sp::spTransform(pt_ll, sp::CRS(prj))
  poly20 <- rgeos::gBuffer(pt, width = 20000, quadsegs = 12)
  poly15 <- rgeos::gBuffer(pt, width = 15000, quadsegs = 12)
  poly10 <- rgeos::gBuffer(pt, width = 10000, quadsegs = 12)
  poly5 <- rgeos::gBuffer(pt, width = 5000, quadsegs = 12)
  
  # Get DEM
  suppressWarnings(
    suppressMessages(
      elev <- elevatr::get_elev_raster(poly20, z = zoom, clip = "locations", verbose = FALSE)
    )
  )

  ## Antenna height above sea level
  pt_elev <- raster::extract(elev, pt) + ht
  
  # Create elevation deficit raster
  out <- elev - pt_elev
  
  # Calculate distance from antenna to each pixel
  suppressWarnings(
    suppressMessages(
      dist <- raster::distanceFromPoints(elev, coords)
    )
  )
  
  # Calculate potential viewshed at every pixel
  deg2rad <- function(deg) {(deg * pi) / (180)}
  tandr = tan(deg2rad(halfangle))
  suppressWarnings(
    suppressMessages(
      pot_vs <- 2*tandr*dist
    )
  )

  # Create fraction of potential viewshed raster (minus sign adds negative deficit)
  out2 = (pot_vs/2 - out)/pot_vs
  
  # If pixel is higher than top of viewshed, it obscures the whole viewshed
  out2[out > pot_vs/2] = NA
  # close to antenna, when viewshed is tiny, you are capturing all of viewshed
  out2[out2 >1] = 1
  
  # set up range rings
  suppressWarnings(
    rings <- mapview::mapview(pt, layer.name = "Proposed station", 
                          alpha.regions = 0, cex = 4, color = "orange",
                          legend = FALSE, label = NULL) +
      mapview::mapview(poly15, layer.name = "15 km range", 
                       alpha.regions = 0, color = "white",
                       legend = FALSE, label = NULL) +
      mapview::mapview(poly10, layer.name = "10 km range", 
                       alpha.regions = 0, color = "white",
                       legend = FALSE, label = NULL) +
      mapview::mapview(poly5, layer.name = "5 km range", 
                       alpha.regions = 0, color = "white",
                       legend = FALSE, label = NULL))
  
  if (all(pt_elev > max(raster::values(elev), na.rm = TRUE))) {
    message("No elevation deficits found in viewshed. Displaying only 5, 10, and 15 km range rings.")
    suppressWarnings(
      m <- mapview::mapview(out2, layer.name = "Prop. viewshed visible") +
        rings)
  } else {
    out[out < 0] <- NA
    suppressWarnings(
      m <- 
        mapview::mapview(out2, layer.name = "Prop. viewshed visible") +
        rings)
  }

  m <- leaflet::setView(m@map, sp::coordinates(pt_ll)[1], 
                        sp::coordinates(pt_ll)[2],
                        zoom = 11)
  return(m)
}