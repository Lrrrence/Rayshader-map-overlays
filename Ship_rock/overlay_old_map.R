library(rstudioapi) # get working dir
library(terra) #working with rasters
library(tidyterra) #plotting terra objects
library(tidyverse) #utilities
library(sf) #vectors
library(elevatr) #elevation data
library(rayshader)
library(viridis) # colours

# set working directory
current_file <- rstudioapi::getActiveDocumentContext()$path
current_dir <- dirname(current_file)
setwd(current_dir)
cat("Current working directory set to:", getwd(), "\n")

# Load the GeoTIFF file
topo_map <- rast("C:/Users/yulel/Documents/R Scripts/Map plots/Rayshader-Contour/Ship rock/NM_Ship Rock_192373_1934_62500_geo.tif")
cat(crs(topo_map))

# get original crs
og_crs <- crs(topo_map)

# plot
p <- ggplot()+
      geom_spatraster_rgb(data = topo_map)
p
#ggsave(filename = "original topo.png", plot = p, width = 12, height = 8, dpi = 300)

# reproject to WGS84 (lat/long)
topo_map_latlong <- terra::project(topo_map, "EPSG:4326")
# get extent
e <- ext(topo_map_latlong)
# make polygon from extent
p <- as.polygons(e)
# get coords
c <- crds(p)
# convert to df
c_df <- data.frame(lon = c[, 1], lat = c[, 2])
# create bbox sf object
bbox_sf <- st_as_sf(c_df, coords = c("lon", "lat"), crs = 4326)

# use elevatr to get elevation data
elev <- get_elev_raster(locations = bbox_sf,
                     z = 12, prj = "EPSG:4326", clip = "locations")
elev <- rast(elev) # to terra

#plot
p <-  ggplot() +
  geom_spatraster_contour_filled(
    data = elev,
    breaks = seq(minmax(elev)[1],minmax(elev)[2],50),
    alpha = 1
  ) +
  geom_spatraster_contour(
    data = elev,
    breaks = seq(minmax(elev)[1],minmax(elev)[2],50),
    color = "grey30",
    linewidth = 0.1
  ) +
  scale_fill_hypso_d()+
  theme(panel.background = element_rect(fill = "white", color = NA),
        plot.background = element_rect(fill = "white", color = NA),)
p
#ggsave(filename = "elev_contours.png", plot = p, width = 12, height = 8, dpi = 300)

#####################################
# leaftlet plot to get neatline coords

library(leaflet)
library(leafem)

topo_map_agg <- terra::aggregate(topo_map, fact=4, fun=mean) # reduce size

# Create the leaflet map with mouse coordinates
m <- leaflet() %>%
  addProviderTiles(providers$OpenStreetMap) %>%
  addRasterImage(topo_map_agg, opacity = 0.8, project = TRUE) %>%
  addMouseCoordinates()
m
# ctrl + Lclick to copy coordinates to clipboard

library(htmlwidgets)
saveWidget(m, file="m.html")

# " lon: -109.04536 | lat: 36.50004 | zoom: 19 "
# " lon: -109.04607 | lat: 36.75007 | zoom: 19 "
# " lon: -108.74999 | lat: 36.75008 | zoom: 19 "
# " lon: -108.75043 | lat: 36.49986 | zoom: 19 "

#####################################
# crop to neatlines

elevation_range <- minmax(elev)
min_elevation <- elevation_range[1, 1]

base_raster <- elev * 0 + (min_elevation-150)

# use coords from leaflet map
x <- c(-109.04536, -109.04607, -108.75043, -108.74999)
y <- c(36.50004, 36.75007, 36.49986, 36.75008)
coordinates <- data.frame(x = x, y = y)

# Convert the data frame to an sf object with the initial CRS (WGS84)
sf_points <- st_as_sf(coordinates, coords = c("x", "y"), crs = 4326)

#polygon for plot
sf_polygon <- sf_points %>%
  st_union() %>%
  st_cast("POLYGON")

# plot polygon overlay
p <- ggplot()+
  geom_spatraster_rgb(data = topo_map)+
  geom_sf(data = sf_polygon, fill = "blue", alpha = 0.5)
p
#ggsave(filename = "polygon_overlay.png", plot = p, width = 12, height = 8, dpi = 300)

# crop elevation to extent of points
interior_elevation <- terra::crop(elev, extent(sf_points))
elev_crop <- merge(interior_elevation, base_raster)

p <- ggplot()+
  geom_spatraster(data = elev_crop) +
  scale_fill_viridis() # Use Viridis color palette
p
#ggsave(filename = "elevation_crop.png", plot = p, width = 12, height = 8, dpi = 300)

#####################################
# reproject

elev_crop <- terra::project(elev_crop, og_crs)
cat(crs(elev_crop))

######################################
# create rayshader plot

# reduce res
topo_map <- aggregate(topo_map, fact=2, fun=mean)
dim(topo_map) # y*x

# breakout RGB layers
names(topo_map) <- c("r", "g", "b")
topo_r <- rayshader::raster_to_matrix(topo_map$r)
topo_g <- rayshader::raster_to_matrix(topo_map$g)
topo_b <- rayshader::raster_to_matrix(topo_map$b)
topo_rgb_array <- array(0, dim = c(nrow(topo_r), ncol(topo_r), 3))
topo_rgb_array[,,1] <- topo_r/255
topo_rgb_array[,,2] <- topo_g/255
topo_rgb_array[,,3] <- topo_b/255
topo_rgb_array <- aperm(topo_rgb_array, c(2,1,3))

dims <- dim(topo_rgb_array)
width = dims[2]
height = dims[1]

#set scale of z
zscale = 8

# rayshade
elev_mat <- raster_to_matrix(elev_crop)
elev_mat = resize_matrix(elev_mat, scale=1)
#ray_shadow <- ray_shade(elev_mat, sunaltitude = 30, zscale = zscale, multicore = TRUE)
#ambient_shadow <- ambient_shade(elev_mat, zscale = zscale)

elev_mat %>%
  sphere_shade() %>%
  add_overlay(topo_rgb_array, rescale_original = TRUE) %>%
  #add_shadow(ray_shadow, max_darken = 0.5) %>%
  #add_shadow(ambient_shadow, 0.5) %>%
  plot_3d(heightmap = elev_mat, 
          zscale = zscale,
          solid = FALSE,
          water = FALSE, 
          fov = 0, 
          theta = 0, 
          zoom = 0.75, 
          phi = 90,
          background = "white")

render_camera(theta=0,phi=89,fov=60,zoom=0.6)

# render_snapshot("shiprock_snapshot_v2.png",
#                 software_render = TRUE,
#                 width = width,
#                 height = height)

###############

environment_light <- "C:/Users/yulel/Documents/R Scripts/env/sunflowers_puresky_4k.hdr"

render_highquality(
  "shiprock_highquality.png", 
  parallel = TRUE, 
  sample_method = "sobol",
  samples = 250,
  light = FALSE, 
  interactive = FALSE,
  environment_light = environment_light,
  intensity_env = 0.7,
  rotate_env = 90,
  width = width, 
  height = height
)
