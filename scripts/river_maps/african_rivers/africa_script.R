library(ggplot2)
library(dplyr)
library(sf)
library(showtext)


dir()

af <- st_read('HydroRIVERS_v10_af.shp')
sss <- st_read('afr_g2014_2013_0.shp')

#afr_riv$geometry

#afr_riv <- af|>
#  filter(ORD_FLOW > 9)
df <- af


#df$geometry <- st_cast(df$geometry,"MULTILINESTRING")

#df$geometry <-st_cast(df$geometry, "POINT")
#df$geometry <-st_cast(df$geometry, "LINESTRING")
#df$geometry <-st_cast(df$geometry[[2]],"MULTILINESTRING")


df <- df |>
  st_cast("MULTILINESTRING")


dff <- af|>
  filter(ORD_FLOW<6)

afr <- dff |>
  mutate(
    width = as.numeric(ORD_FLOW),
    width = case_when(
      width == 1 ~ 1,
      width == 2 ~ 0.7,
      width == 3 ~ 0.5,
      width == 4 ~ 0.4,
      width == 5 ~ 0.4,
      TRUE ~ 0
    )
  ) |>
 st_as_sf()




crsLONGLAT <- "+proj=longlat +datum=WGS84 +no_defs"

get_bounding_box <- function(bbox, new_prj, bb) {
  bbox <- st_sfc(
    st_polygon(list(cbind(
      c(-18.000, 54.4, 54.4, -18.000, -18.000),
      c(-34.8, -34.8, 37.3, 37.3, -34.8)
    ))),
    crs = crsLONGLAT
  )
  
  new_prj <- sf::st_transform(bbox, crs = 4087)
  bb <- sf::st_bbox(new_prj)
  
  return(bb)
}

bbox <- get_bounding_box()


#bbox <- unlist(bbox)
#c("blue","#08306b", "#08519c", "#2171b5"
 #  "#4292c6", "#6baed6", "#9ecae1","white"))
#"#08306b", "#08519c", "#2171b5",
#"#4292c6", "#6baed6", "#9ecae1",
#"#c6dbef", "#deebf7"

#plot(afr)
#land_color = 'white'
#afr$ORD_FLOW <- as.factor(afr$ORD_FLOW)
#afr1 <- afr |>
#  select(ORD_FLOW,geometry,width)
afrr <- afr
afrr <- afr|>
  filter((ORD_CLAS == 1 & width == 1))


#mean(afrr$DIS_AV_CMS)


#afrr$ORD_FLOW <- as.numeric(afrr$ORD_FLOW)
#afrr$ORD_CLAS <- as.numeric(afrr$ORD_CLAS)

afrr <- afr|>
  mutate(
    lange = case_when(
      UPLAND_SKM > 119500 & DIS_AV_CMS > 88 & ORD_CLAS <3 ~ 1,
     # UPLAND_SKM < 100000 & ORD_CLAS > 3 ~ 1,
     # UPLAND_SKM > 100000 & ORD_CLAS >3 ~ 3,
      TRUE ~ 0
    ),
    menge= case_when(
      ORD_FLOW <3 ~ 1,
      ORD_FLOW > 3 ~ 2,
      TRUE ~ 0
    )
  )
  

#(ORD_CLAS <= 3 & ORD_STRA <=2) ~ 1,
#DIS_AV_CMS
#summary(afrr)

#dff <- ifelse((ORD_CLAS == 1 & width == 1) & )

font_add("marhey","Marhey-VariableFont_wght.ttf")

showtext_auto()

#distinct(as.integer(afrr$MAIN_RIV))

afrr <- afrr |>
  st_cast("MULTILINESTRING")


ddd <- afrr |>
  group_by(MAIN_RIV)




river_map <- function() {
  p <- ggplot() +
    geom_sf(data=sss, color="#D3D3D3", size=0.0001, fill='white') +
    geom_sf(data = afrr,aes(color= factor(MAIN_RIV), alpha=factor(lange)
                           )) +
    coord_sf(
      crs = 4087,
      xlim = c(bbox["xmin"], bbox["xmax"]),
      ylim = c(bbox["ymin"], bbox["ymax"])
    ) +
    labs(
      x = "",
      y = "",
      title = "River Basin Map of Africa  |  Carte du Bassin Fluvial de l'Afrique",
      caption = "©2023 Jeffrey Ohene (https://github.com/jeffreyohene) | Data: hydrosheds.org",
    ) +
   # scale_color_manual(
#     name = "",
 #     values = c(
#        "#05014a","#08519c", "#2171b5","#4292c6", "#6baed6","#9ecae1","#9ecae2"
 #     )
#    ) +
    #scale_size(range = c(0.1,0.8)) +
    scale_alpha_manual(values = c("0"=.35,"1"=1))+
    #scale_fill_gradient()+
   # scale_colour_viridis_d(option = "plasma")+ #scale_colour_v
   #  "2"=1,"3" = 1, "4" = .2, "5" = .1
    #)) +
    scale_linewidth_ordinal(range=c(0.1,0.6))+
    theme_minimal() +
    theme(
    #  panel.background = element_blank(),
      legend.background = element_blank(),
      legend.position = "none",
      panel.border = element_blank(),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_blank(),
      plot.title = element_text(
        size = 130, color = "blue", hjust = 0.5, vjust = 0,family = "marhey"
      ),
      plot.subtitle = element_text(
        size = 14, color = "#ac63a0", hjust = 0.5, vjust = 0
      ),
      plot.caption = element_text(
        size = 50, color = "black", hjust = 1, vjust = 10,family = "marhey"
      ),
      axis.title.x = element_text(
        size = 10, color = "grey20", hjust = 0.5, vjust = -6
      ),
      legend.text = element_text(
        size = 9, color = "grey20"
      ),
      legend.title = element_text(size = 10, color = "grey20"),
      strip.text = element_text(size = 12),
      plot.margin = unit(c(t = 1, r = -2, b = -1, l = -2), "lines"),
      axis.title.y = element_blank(),
      axis.ticks = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_blank()
    )
  
  return(p)
}


p1 <- river_map()
 
ggsave(
  filename = "african_rivers.png",
  width = 10, height = 10, dpi = 600,
  device = "png", bg = "white", p1
)
