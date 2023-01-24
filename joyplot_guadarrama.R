# Joy plot Sierra de Guadarrama

library(randomcoloR)
library(reshape2)
library(tibble)
library(scales)
library(readxl)
library(rgeos)
library(cartography)
library(sf)
library(sp)
library(rgdal)
library(raster)
library(geometry)
library(ggtext)
library(mapmisc)
library(RColorBrewer)
library(ggplot2)
library(dplyr)
library(grDevices)
library(NatParksPalettes)
library(colorspace)
library(ggthemes)
library(showtext)
library(sysfonts)
library(ggspatial)
library(ggridges)
library(rnaturalearth)

# Raster from the IGN https://centrodedescargas.cnig.es/CentroDescargas/index.jsp
# Inspired by Joy Division’s 1979 album cover UNKNOWN PLEASURES
# Inspired by https://www.helenmakesmaps.com/post/how-to-joy-plot 
# Plot code from https://rpubs.com/KingaHill/989379 

setwd("/Users/juliag.dealedo/Google Drive/Cap1_JGA-MP/data/MapData/SGUADARRAMA/guadarrama_high/mdts_guadarrama")

elev1 <- raster("PNOA_MDT25_ETRS89_HU30_0507_LID.asc")
elev2 <- raster("PNOA_MDT25_ETRS89_HU30_0431_LID.asc")
elev3 <- raster("PNOA_MDT25_ETRS89_HU30_0432_LID.asc")
elev4 <- raster("PNOA_MDT25_ETRS89_HU30_0457_LID.asc")
elev5 <- raster("PNOA_MDT25_ETRS89_HU30_0458_LID (1).asc")
elev6 <- raster("PNOA_MDT25_ETRS89_HU30_0459_LID (1).asc")
elev7 <- raster("PNOA_MDT25_ETRS89_HU30_0482_LID.asc")
elev8 <- raster("PNOA_MDT25_ETRS89_HU30_0483_LID.asc")
elev9 <- raster("PNOA_MDT25_ETRS89_HU30_0484_LID (1).asc")
elev10 <- raster("PNOA_MDT25_ETRS89_HU30_0506_LID.asc")
elev11 <- raster("PNOA_MDT25_ETRS89_HU30_0508_LID.asc")
elev12 <- raster("PNOA_MDT25_ETRS89_HU30_0509_LID.asc")
elev13 <- raster("PNOA_MDT25_ETRS89_HU30_0531_LID.asc")
elev14 <- raster("PNOA_MDT25_ETRS89_HU30_0532_LID.asc")
elev15 <- raster("PNOA_MDT25_ETRS89_HU30_0533_LID.asc")
elev16 <- raster("PNOA_MDT25_ETRS89_HU30_0534_LID.asc")
elev17 <- raster("PNOA_MDT25_ETRS89_HU30_0556_LID (1).asc")
elev18 <- raster("PNOA_MDT25_ETRS89_HU30_0556_LID.asc")
elev19 <- raster("PNOA_MDT25_ETRS89_HU30_0557_LID.asc")
elevhigh <- merge (elev1, elev2, elev3, elev4, elev5, elev6, elev7, 
                   elev8, elev9, elev10, elev11, elev12, elev13, elev14, 
                   elev15, elev16, elev17, elev18, elev19)
extent1 <- extent(400000, 450000, 4500000, 4550000)
elev_crop <-crop(elevhigh, extent1)
plot(elev_crop)
elev_aggr20 <- aggregate(elev_crop, fact=20)
dem_df <- as.data.frame(elev_aggr20, xy = TRUE, na.rm = FALSE)

joy <- ggplot() +
  ggridges::geom_density_ridges(
    data = dem_df, aes(
      x = x, y = y, group = y, height = layer),
    rel_min_height = 0.002,
    stat = "identity",
    scale = 50,
    fill = "black",
    color = "white",
    size = 0.6) +
  theme_void() +
  theme(panel.background = element_rect(fill = "black", colour = NA),  
        plot.background = element_rect(fill = "black", colour = NA))

joy
ggsave("joy.png")

# Add Spain contour of location
countries <- readOGR("TM_WORLD_BORDERS-0.3.shp", stringsAsFactors = TRUE) 
Spain <- subset(countries, name == "Spain")
e2 <- c(400000, 450000, 4500000, 4550000)
Spain_2 <- crop(Spain, e2)
Sp <-  ggplot() +
  geom_polygon(data = Spain_3, aes(x = long,  y = lat, group=group),
               fill = "black", colour = "white",  size=0.5, linewidth=4) + theme_void() + 
  theme(panel.background = element_rect(fill = "black", colour = NA),  
        plot.background = element_rect(fill = "black", colour = NA))+
  geom_rect(aes(xmin = 400000, xmax = 450000, ymin = 4500000, ymax = 4550000), fill="white")
ggsave("Spain.png")
