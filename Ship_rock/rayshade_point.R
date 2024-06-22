library(tidyverse)
library(elevatr) # get elev raster
library(terra) # work with rasters
library(sf) # work with vectors
library(rnaturalearth) # get world/country sf
library(rnaturalearthdata) # get world/country sf
library(ambient) # for generating clouds
library(glue)
library(rstudioapi) # get working dir
library(rayshader)

### get colour palettes ###
library(RColorBrewer) 
library(colorspace)
library(MetBrewer)
library(NatParksPalettes)
library(scico)
#########################

# the ‘sf’ package is a successor of ‘sp’, creation of `terra` as a successor of `raster`

# set working directory
current_file <- rstudioapi::getActiveDocumentContext()$path
current_dir <- dirname(current_file)
setwd(current_dir)
cat("Current working directory set to:", getwd(), "\n")

#set a map name for filename later
map <- "madeira"

# points (y,x) from gmaps
# 13.25, 123.68 mt mayon, phillipines
# 35.36, 138.73 fuji
# 5.97, -62.53 angel falls, venezuela
# 32.74, -17.00 madeira
# 36.6875, -108.836389 shiprock, New Mexico
# 36.1433, -109.3434 navajo canyon

##########################################################

# get sf from rnaturalearth
world <- ne_countries(type = "countries", scale = "large")

# Sample XY point
point <- cbind(x = -109.3434 , y = 36.1433)

# Create an sf POINT object
point_sf <- st_point(point)%>%
  st_sfc(crs = st_crs(4326))

# Define the buffer size for the bounding box
side_length <- 0.8

# Create a bounding box around the point
circular_bbox_sf <- st_buffer(st_point(point), dist = side_length / 2, endCapStyle = "ROUND")%>%
  st_sfc(crs = st_crs(4326))
square_bbox_sf <- st_buffer(st_point(point), dist = side_length / 2, endCapStyle = "SQUARE")%>%
  st_sfc(crs = st_crs(4326))

# Convert sf objects to data frames for ggplot
circular_bbox_df <- st_sf(geometry = st_geometry(circular_bbox_sf))
square_bbox_df <- st_sf(geometry = st_geometry(square_bbox_sf))
point_df <- st_sf(geometry = st_geometry(point_sf))

# Clip the shapefile
world <- st_make_valid(world)
clipped_world <- st_crop(world, square_bbox_sf)

ggplot() +
  geom_sf(data = world) +
  geom_sf(data = point_df, color = "red", size = 3, shape = 16) +
  theme_minimal()

# Plot the bounding boxes and point using ggplot
ggplot() +
  geom_sf(data = clipped_world) +
  geom_sf(data = square_bbox_df, fill = NA, color = "black") +
  geom_sf(data = circular_bbox_df, fill = NA, color = "blue") +
  geom_sf(data = point_df, color = "red", size = 3, shape = 16) +
  theme_minimal()

############### get elevation raster #############

# use elevatr to get elevation data
r <- get_elev_raster(locations = square_bbox_df,
                       z = 12, prj = "EPSG:4326", clip = "locations")

# to re-project raster
raster::crs(r) <- "EPSG:4326" 
crs(r)

# use terra to create contour lines
r <- rast(r)
x <- as.contour(r)
class(x)
plot(r)
plot(x, add=TRUE)

# to df for rayshader
r_df <- as.data.frame(r,xy=TRUE)
names(r_df)[3] <- "z"

#################### get colours ################### ---- 

colors <- natparks.pals("Acadia")
#colors <- met.brewer("OKeeffe1")
colors <- rev(met.brewer("Demuth"))
colors <- c(colors)

# glacier style
c1 <- natparks.pals("Glacier", 5)
c2 <- scico(palette = "lajolla", n = 5)
colors <- c("white",
            c1[5:3], 
            # lighten(c1[10], .5), 
            # lighten(c1[10], .75),
            c2[4:1],
            "white")
#   height_shade(texture = grDevices::colorRampPalette(colors, bias = .5)(256)) %>%

# white base orange top
c1 <- scico(palette = "grayC", n = 5)
c2 <- scico(palette = "lajolla", n = 5)
colors <- c(c1[2:4], rev(c2[1:4]))
#   height_shade(texture = grDevices::colorRampPalette(c("white", "grey90", colors), bias = .5)(256)) %>%

colors <- grDevices::colorRampPalette(colors)(256)
swatchplot(colors)

################### build plot ################### ----

elev_matrix = raster_to_matrix(r)
elev_matrix = resize_matrix(elev_matrix, scale=0.2)

zscale = 5

# calculate rayshader layers
ambmat <- ambient_shade(elev_matrix, zscale = zscale)
raymat <- ray_shade(elev_matrix, zscale = zscale, lambert = TRUE)
#textmat <- texture_shade(elev_matrix)
#watermap <- detect_water(elev_matrix)

elev_matrix %>%
  #sphere_shade(texture = "imhof4") %>%
  height_shade(texture = colors) %>%
  add_shadow(cloud_shade(elev_matrix, zscale=zscale, seed = 4, sun_altitude=20, attenuation_coef = 1.5), 0) %>%
  add_shadow(raymat, max_darken = 0.2) %>%
  add_shadow(ambmat, max_darken = 0.2) %>%
  #add_shadow(textmat, max_darken = 0.5) %>%
  #add_water(watermap, color = "imhof1") %>%
  plot_3d(heightmap = elev_matrix, 
          zscale = zscale,
          solid = FALSE,
          #shadowdepth = shadow_depth,
          water = FALSE, 
          #watercolor="imhof1", 
          #waterlinecolor="white", 
          #waterlinealpha=0.5,
          fov = 0, 
          theta = 0, 
          zoom = 0.75, 
          phi = 90,
          background = "white",
          windowsize = c(1000, 1000),
          baseshape="circle")
Sys.sleep(0.2)
render_camera(theta=-45,phi=89,fov=60,zoom=0.6)

# clouds
render_clouds(elev_matrix, 
              zscale=zscale, 
              seed = 4, 
              cloud_cover = 0.4, 
              fractal_levels = 20, 
              clear_clouds = T, 
              attenuation_coef = 0.5, 
              baseshape = "circle")    

# save to file
render_snapshot("rayshade_madeira.png", 
                clear = FALSE, 
                software_render = TRUE,
                width = 3000,
                height = 3000)


########### render high quality #############

start_time <- Sys.time()
cat(glue("Start Time: {start_time}"), "\n")

render_highquality(
  "madeira_highres_v8.png", 
  parallel = TRUE, 
  sample_method = "sobol",
  samples = 400,
  light = FALSE, 
  interactive = FALSE,
  environment_light = "C:/Users/yulel/Documents/R/env/sunflowers_puresky_4k.hdr",
  intensity_env = 1,
  rotate_env = 0,
  width = width, 
  height = height
)

end_time <- Sys.time()
total_time <- end_time - start_time
rounded_total_time <- round(as.numeric(total_time), 2)

cat(glue("Total time: {rounded_total_time} mins"))
