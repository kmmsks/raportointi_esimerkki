library (sf)
library(here)
library(ggplot2)
library(patchwork)
library(dplyr)
library(paletteer)
library(colorBlindness)
library(ggthemes)
library(data.table)
library(stringi)
library(magrittr)
#library(ggrepel)


# gis data

#https://geo.stat.fi/geoserver/web/wicket/bookmarkable/org.geoserver.web.demo.MapPreviewPage?0&filter=false

gis <- list()
gis$mkunta <- st_read(here('data', 'mkunta_vaki2017'), options = "ENCODING=WINDOWS-1252")

# yhdista maakunnanan numero ja nimo
gis$mkunta$mkunta <-  factor(gis$mkunta$maakunta, levels = sort(unique(gis$mkunta$maakunta)), 
                             labels = paste(unique(gis$mkunta$maakunta[order(gis$mkunta$maakunta)]) %>% as.numeric(), unique(gis$mkunta$name[order(gis$mkunta$maakunta)])))


gis$shp_in <- st_read(here('data', 'shpt_ja_erva_alueet_2018_kuntajaolla'), options = "ENCODING=WINDOWS-1252")


# vain shp:t
gis$shp <- 
  gis$shp_in %>% 
  group_by(srnhtpr) %>%
  summarise(geometry = sf::st_union(geometry)) %>%
  ungroup()

# helsinki
gis$hki <- 
  gis$shp_in %>% 
  subset(NAMEFIN=='Helsinki') %>% 
  group_by(srnhtpr) %>%
  summarise(geometry = sf::st_union(geometry)) %>%
  ungroup()

gis$hki$srnhtpr <- "Helsinki"

# shp:t ja helsinki
gis$shphki <- rbind(
  st_difference(gis$shp, gis$hki)[1],
  gis$hki
)


# harmonisoidaan koordinaatot
gis$shphki %<>% st_transform(crs = st_crs(gis$mkunta))

# pienet saaret pois
gis$shphki <- st_intersection(gis$shphki, st_combine(gis$mkunta))

gis$shphki %<>% 
  mutate(srnhtpr = replace( srnhtpr, srnhtpr == 'HUS', "HUS ei Helsinki"))

gis$shphki$shp <- gis$shphki$srnhtpr
