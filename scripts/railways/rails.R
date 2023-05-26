library(pacman)

p_load(tidyverse, sf, giscoR, ggfx, showtext)

sss <- st_read('afr_g2014_2013_0.shp')

# Country code vectors
countries <- c("AGO", "BDI", "BEN", "BFA", "BWA", "CAF", "CIV", "CMR", "COD", 
               "COG", "COM", "CPV", "DJI", "DZA", "EGY", "ERI", "ESH", "ETH", 
               "GAB", "GHA", "GMB", "GNB", "GNQ", "KEN", "LBR", "LBY", "LSO", 
               "MAR", "MDG", "MLI", "MOZ", "MRT", "MUS", "MWI", "NAM", "NER", 
               "NGA", "RWA", "SDN", "SEN", "SHN", "SLE", "SOM", "STP", "SWZ", 
               "SYC", "TCD", "TGO", "TUN", "TZA", "UGA", "ZAF", "ZMB", "ZWE")

# Reading shapefiles for each country into a list
shapefiles <- lapply(countries, function(country) {
  st_read(paste0(country, "_rails.shp")) |>
    st_transform(crs = 4326)
})

# Combining all shapefiles into one
afr <- do.call(rbind, shapefiles)



afr <- st_transform(afr, crs = 4326)

get_africa_map <- function() {
  africa_map <- giscoR::gisco_get_countries(
    year = "2016",
    epsg = "4326",
    resolution = "3",
    region = "Africa"
  )
  
  return(africa_map)
}

africa_map <- get_africa_map()



crsLAEA <- "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +datum=WGS84 +units=m +no_defs"


get_bounding_box <- function(crsLONGLAT, bbox, new_prj, bb) {
  
  crsLONGLAT <- "+proj=longlat +datum=WGS84 +no_defs"
  bbox <- # Define the coordinates
    coords <- st_sfc(
      st_polygon(list(cbind(
        c(-17.0, -17.0, 57.0, 57.0, -17.0),   # x-coordinates in degrees for Lambert projection
        c(-35.0, 40.0, 40.0, -35.0, -35.0)    # y-coordinates in degrees for Lambert projection
      ))),
      crs = crsLONGLAT)
  
  new_prj <- st_transform(bbox, crs = crsLAEA)
  bb <- st_bbox(new_prj)
  
  return(bb)
}

# plot font
font_add("roboto","Roboto-Regular.ttf")

showtext_auto()



# plot
get_railway_map <- function(p, bb) {
  
  bb <- get_bounding_box()
  
  p <- ggplot() +
      geom_sf(data=afr, color="#FFA500", size = 2, fill=NA) +
    geom_sf(data=africa_map, color="grey80", size=0.1, fill=NA) +
    theme_minimal() +
    theme(
      axis.line = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks = element_blank(),
      axis.title.x = element_text(size=50, color="black", hjust=0.25, vjust=3),
      axis.title.y = element_blank(),
      legend.position = "none",
      panel.grid.major = element_line(color = "white", size = 0.2),
      panel.grid.minor = element_blank(),
      plot.title = element_text(face="bold", size=130, color="#FFA500", hjust=.5, 
                                family = "roboto"),
      plot.margin = unit(c(t=1, r=-2, b=-1, l=-2),"lines"),
      plot.background = element_rect(fill = "white", color = NA), 
      panel.background = element_rect(fill = "white", color = NA), 
      legend.background = element_rect(fill = "white", color = NA),
      panel.border = element_blank()) +
    labs(x = "©2023 Jeffrey Ohene (https://jeffreyohene.net) | Data: DIVA-GIS", 
         y = NULL, 
         title = "Les chemins de fer Africains | African Railways", 
         subtitle = "", 
         caption = "")
  
  return(p)
}

p <- get_railway_map()

p

ggsave(filename="afr_railways.png", width= 8.5, height= 7, dpi = 600, 
       device='png', p)

