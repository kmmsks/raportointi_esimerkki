# library (sf)
# library(here)
# library(ggplot2)
# library(patchwork)
# library(dplyr)
# library(paletteer)
# library(colorBlindness)
# library(ggthemes)
# library(data.table)
# library(stringi)
# library(magrittr)
# library(ggrepel)

# vareja
#palettes$c20a <- c(RColorBrewer::brewer.pal(n = 12, name='Set3'), RColorBrewer::brewer.pal(n = 8, name='Set2'))



# kuvat ------------------------------------------------------------------------

palettes$hj2a <- c('#ef6f6a', 'white', '#8cc2ca')  
palettes$hl_jakso <- c("white", 'ivory', "honeydew1")

#clrs <- c('white', '#c46487')
draw_prevalence_alue_total <- function(round_in = 2, font_family_in = 'sans', font_size_in=3, aluerajat_color = 'gray', clrs = c('white', '#f06719')){
  
  # yhdistetaan karttapohja ja prevalenssidata
    d <- merge(
      gis$shphki,
      dat_figs$prevalence_alue,
      by = 'shp' 
    )
    # rajat scale fill gradientiin
    rate_max <- max(d$adj.rate)
    rate_min <- min(d$adj.rate)
    
  # helsingin sijainti tarvitaan, jotta saadaan helsingin tiedot kohdalleen
  pos_hki <- d %>% subset(shp=="Helsinki") %>% st_bbox()
  
  ggplot(d)+
    geom_sf(aes(fill=adj.rate), color = aluerajat_color)+
    #varitys:
    scale_fill_gradient(low = clrs[1], high = clrs[2], limits = c(rate_min, rate_max))+ 
    # arvo tekstina (pl. Helsinki):
    geom_sf_text(data = d %>% subset(shp != 'Helsinki'), 
                 aes(label =adj.rate %>% round(round_in)),size=font_size_in,family=font_family_in,
    )+
    # Helsingin tieodot:
    annotate("text", 
             x= pos_hki$xmax+70000, 
             y=(pos_hki$ymin -10000), 
             label = paste("Helsinki", d %>% subset(shp == 'Helsinki') %>% .$adj.rate %>% round(round_in)), 
             size=font_size_in,family=font_family_in
    )+
    coord_sf()+
    theme_void()+
    ggtitle('Yhteensä')+
    labs(fill = paste0("Prevalenssi % \n", "(mediaani ", median(d$adj.rate) %>% round(round_in), ")"))

}


# tassa sama mutta diagnooseittain
draw_prevalence_alue_dg <- function(dg_in = "F20", total_prevence = FALSE, round_in = 2, font_family_in = 'sans', font_size_in=3){

   d <- merge(
      gis$shphki,
      dat_figs$prevalence_alue_diagnoosi$asr[dg == dg_in],
      by = 'shp' 
    )
    
    rate_max <- dat_figs$prevalence_alue_diagnoosi$asr[, max(adj.rate)]
    rate_min <- dat_figs$prevalence_alue_diagnoosi$asr[, min(adj.rate)]
    
    rate_range <- d$adj.rate %>% range()
    clrs <- c('#dbdb8d', 'white', '#729ece')
  
  pos_hki <- d %>% subset(shp=="Helsinki") %>% st_bbox()
  
  ggplot(d)+
    geom_sf(aes(fill=adj.rate), color = 'gray')+
    #scale_fill_gradient(low = clrs[1], high = clrs[2], limits = c(rate_min, rate_max))+
    scale_fill_gradientn(
      limits  = rate_range,
      colours = clrs,
      values  = c(0, scales::rescale(median(d$adj.rate), from = rate_range), 1),
    )+
    geom_sf_text(data = d %>% subset(shp != 'Helsinki'), 
                 aes(label =adj.rate %>% round(round_in)),size=font_size_in,family=font_family_in,
    )+
    annotate("text", 
             x= pos_hki$xmax+70000, 
             y=(pos_hki$ymin -10000), 
             label = paste("Helsinki", d %>% subset(shp == 'Helsinki') %>% .$adj.rate %>% round(round_in)), 
             size=font_size_in,family=font_family_in
    )+
    coord_sf()+
    theme_void()+
    ggtitle(dgs2[dg_in])+
    labs(fill = paste0("Prosenttia \n", "(mediaani ", median(d$adj.rate) %>% round(round_in), ")"))

}

# Tassa diagnoosien prosenttiosuus alueittain
draw_pros_alue_dg <- function(dg_in = "F20", total_prevence = FALSE, round_in = 1, font_family_in = 'sans', font_size_in=3){
  
  d <- merge(
    gis$shphki,
    dat_figs$diagnoosien_jakauma_alueittain$pros[dg == dg_in],
    by = 'shp' 
  )
  
  rate_range <- d$pros %>% range()
  clrs <- c('#dbdb8d', 'white', '#729ece')
  
  pos_hki <- d %>% subset(shp=="Helsinki") %>% st_bbox()
  
  ggplot(d)+
    geom_sf(aes(fill=pros), color = 'gray')+
    #scale_fill_gradient(low = clrs[1], high = clrs[2], limits = c(rate_min, rate_max))+
    scale_fill_gradientn(
      limits  = rate_range,
      colours = clrs,
      values  = c(0, scales::rescale(median(d$pros), from = rate_range), 1),
    )+
    geom_sf_text(data = d %>% subset(shp != 'Helsinki'), 
                 aes(label = pros %>% round(round_in)),size=font_size_in,family=font_family_in,
    )+
    annotate("text", 
             x= pos_hki$xmax+70000, 
             y=(pos_hki$ymin -10000), 
             label = paste("Helsinki", d %>% subset(shp == 'Helsinki') %>% .$pros %>% round(round_in)), 
             size=font_size_in,family=font_family_in
    )+
    coord_sf()+
    theme_void()+
    ggtitle(dgs2[dg_in], 
            #subtitle = paste(jakso_in, 'sairaalasta kotiutumisesta. \n', 'Alueiden mediaani', median(d$pros) %>% round(round_in), '%.'  )
            #subtitle = paste(jakso_in, 'sairaalasta kotiutumisesta.'  )
    )+
    labs(fill = paste0("Prosenttia \n", "(mediaani ", median(d$pros) %>% round(round_in), ")"))
  #theme(panel.background = element_rect(fill = hl_jakso_color, color = NA))
  
}
