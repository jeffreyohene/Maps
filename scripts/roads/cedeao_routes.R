# Load required libraries
library(tidyverse)
library(sf)
library(giscoR)
library(ggfx)
library(showtext)

# Set the directory where shapefiles are located
dirr <- "dir/data"

# Function to unzip shapefiles
unzipSHP <- function(filenames) {
  filenames <- list.files(dirr, full.names = TRUE)
  lapply(filenames, unzip)
}

# Unzip shapefiles
unzipSHP(filenames)

# Function to load shapefiles
loadSHP <- function(SHPs, roads) {
  SHPs <- list.files(pattern = "_roads.*\\.shp$")
  
  # Use lapply to import all shapefiles in the list
  roads <- lapply(SHPs, function(road_shp) {
    road <- st_read(road_shp) |> 
      st_transform(crs = 4326)  # Read shapefiles and assign WSG84 projection
    return(road)
  }) %>% 
    bind_rows()  # Merge road lines into a single data.frame
  
  return(roads)
}

# Load shapefiles
roads <- loadSHP()

# Define ECOWAS country codes
ecowas <- c("BEN", "BFA", "CIV", "CPV", "GHA", "GIN", "GMB",
            "GNB", "LBR", "MLI", "NER", "NGA", "SEN", "SLE", "TGO")

# Get country shapefiles for ECOWAS countries
eco_shp <- gisco_get_countries(
  year = "2016",
  epsg = "4326",
  resolution = "10",
  country = ecowas
)

# Define coordinate reference systems
crsLAEA <- "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +datum=WGS84 +units=m +no_defs"
crsLONGLAT <- "+proj=longlat +datum=WGS84 +no_defs"

# Calculate the bounding box of eco_shp
bbox <- st_sfc(
  st_polygon(list(cbind(
    c(-25.35984, 15.97706, 15.97706, -25.35984, -25.35984), 
    c(4.27713, 4.27713, 24.99759, 24.99759, 4.27713)
  ))),
  crs = crsLONGLAT
)

# Transform the bounding box to the desired coordinate reference system
new_prj <- st_transform(bbox, crs = crsLAEA)
bb <- st_bbox(new_prj)

# Filter roads based on RTT_DESCRI values
roads <- roads |> filter(RTT_DESCRI %in% c("Primary Route", "Secondary Route"))

# Create the plot
ggplot() +
  with_outer_glow(
    geom_sf(data = roads, color = "#98FF98", size = 0.05, fill = NA),
    colour = "#98FF98", sigma = 12
  ) +
  geom_sf(data = eco_shp, color = "grey80", size = 0.01, fill = NA) +
  coord_sf(
    crs = crsLAEA,
    xlim = c(bb["xmin"], bb["xmax"]),
    ylim = c(bb["ymin"], bb["ymax"])
  ) +
  theme_minimal() +
  theme(
    axis.line = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    axis.title.x = element_text(size = 13, color = "grey80", hjust = 0.25, vjust = 3, family = "mono"),
    axis.title.y = element_blank(),
    legend.position = "none",
    panel.grid.major = element_line(color = "black", size = 0.2),
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold", size = 24, color = "#98FF98", 
                              hjust = .5, family = "Marhey Medium"),
    plot.margin = unit(c(t = 1, r = -2, b = -1, l = -2), "lines"),
    plot.background = element_rect(fill = "black", color = NA),
    panel.background = element_rect(fill = "black", color = NA),
    legend.background = element_rect(fill = "black", color = NA),
    panel.border = element_blank()
  ) +
  labs(
    x = "©2023 Jeffrey Ohene (https://jeffreyohene.github.io) | Data: DIVA-GIS",
    y = NULL,
    title = "Les routes de la CEDEAO",
    subtitle = "",
    caption = ""
  )

# Save the plot as an image
ggsave(filename = "routes.png", width = 8.5, height = 7, dpi = 600, device = 'png')
