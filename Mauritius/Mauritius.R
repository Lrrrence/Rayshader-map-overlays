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

topo_map <- rast("mauritius_georef_crop.tif")
cat(crs(topo_map))

# get original crs
og_crs <- crs(topo_map)

# plot
p <- ggplot()+
      geom_spatraster_rgb(data = topo_map)
p
#ggsave(filename = "original topo.png", plot = p, width = 12, height = 8, dpi = 300)

# get extent
e <- ext(topo_map)
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
                     z = 11, prj = "EPSG:4326", clip = "locations")
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
        plot.background = element_rect(fill = "white", color = NA),
        legend.position = "none")
p
#ggsave(filename = "elev_contours.png", plot = p, width = 12, height = 8, dpi = 300)

####################################
# get island sf and crop/mask elev
# alternatives: giscoR::gisco_get_countries() or rnaturalearth::ne_countries()

library(giscoR)
mauritius_sf <- gisco_get_countries(country = "Mauritius", resolution = 01 )
plot(mauritius_sf)

# library(geodata)
# # Download GADM data for Mauritius
# mauritius <- gadm(country = "Mauritius", level = 0, path = tempdir())
# # Convert to sf object
# mauritius_sf <- st_as_sf(mauritius)

# Crop the sf object to the bounding box
mauritius_cropped <- st_crop(mauritius_sf, bbox_sf)
plot(st_geometry(mauritius_cropped))
# crop/mask elevation to extent
elev_crop <- terra::crop(elev,mauritius_cropped)
elev_masked <- mask(elev_crop, mauritius_cropped)
# create base raster of map extent
base_raster <- elev * 0
# merge base with mask
elev <- merge(elev_masked,base_raster)

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
        plot.background = element_rect(fill = "white", color = NA),
        legend.position = "none")
p

#####################################
# leaftlet plot to get neatline coords

library(leaflet)
library(leafem)

topo_map_agg <- terra::aggregate(topo_map, fact=8, fun=mean) # reduce size

# Create the leaflet map with mouse coordinates
m <- leaflet() %>%
  addProviderTiles(providers$OpenStreetMap) %>%
  addRasterImage(topo_map_agg, opacity = 0.8, project = TRUE) %>%
  addMouseCoordinates()
m
# ctrl + Lclick to copy coordinates to clipboard

library(htmlwidgets)
saveWidget(m, file="leaflet.html")

#TL-TR-BR-BL
# " lon: 57.29202 | lat: -19.92266 | zoom: 19 "
# " lon: 57.83451 | lat: -19.92211 | zoom: 19 "
# " lon: 57.83283 | lat: -20.54365 | zoom: 17 "
# " lon: 57.29144 | lat: -20.54527 | zoom: 18 "

#####################################
# crop to neatlines

elevation_range <- minmax(elev)
min_elevation <- elevation_range[1, 1]

base_raster <- elev * 0 + (min_elevation-50)

# use coords from leaflet map
x <- c(57.29202, 57.83451, 57.83283, 57.29144)
y <- c(-19.92266, -19.92211, -20.54365, -20.54527)
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
interior_elevation <- terra::crop(elev, ext(sf_points))
elev_crop <- merge(interior_elevation, base_raster)

p <- ggplot()+
  geom_spatraster(data = elev_crop) +
  scale_fill_viridis() # Use Viridis color palette
p
#ggsave(filename = "elevation_crop.png", plot = p, width = 12, height = 8, dpi = 300)

######################################
# create rayshader plot

# reduce res
topo_map_final <- aggregate(topo_map, fact=3, fun=mean)
dim(topo_map_final) # y*x

# breakout RGB layers
names(topo_map_final) <- c("r", "g", "b")
topo_r <- rayshader::raster_to_matrix(topo_map_final$r)
topo_g <- rayshader::raster_to_matrix(topo_map_final$g)
topo_b <- rayshader::raster_to_matrix(topo_map_final$b)
topo_rgb_array <- array(0, dim = c(nrow(topo_r), ncol(topo_r), 3))
topo_rgb_array[,,1] <- topo_r/255
topo_rgb_array[,,2] <- topo_g/255
topo_rgb_array[,,3] <- topo_b/255
topo_rgb_array <- aperm(topo_rgb_array, c(2,1,3))

dims <- dim(topo_rgb_array)
width = dims[2]
height = dims[1]

#set scale of z
zscale = 12

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
          windowsize = 1200,
          background = "white")

render_camera(theta=0,phi=89,fov=60,zoom=0.6)

# render_snapshot("shiprock_snapshot_v2.png",
#                 software_render = TRUE,
#                 width = width,
#                 height = height)

###############

environment_light <- "C:/Users/yulel/Documents/R Scripts/env/cape_hill_4k.hdr"

render_highquality(
  "mauritius_highquality.png", 
  parallel = TRUE, 
  sample_method = "sobol",
  samples = 250,
  light = FALSE, 
  interactive = FALSE,
  environment_light = environment_light,
  intensity_env = 1.2,
  rotate_env = 90,
  width = width, 
  height = height
)
