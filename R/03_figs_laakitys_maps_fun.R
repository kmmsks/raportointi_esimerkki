


draw_laakitys <- function(laake_in = 'laake', dg_in = 'kaikki', pvm_in = "31.1", tarkastelu_in = '1 vuosi', vuosi_in = latest_y$laakitys, 
                          round_in = 1, font_family_in = 'sans', font_size_in=3, clrs = c('#dbdb8d', 'white', '#729ece')){
  
  d <- merge(
    gis$shphki,
    dat$laakitys[diagnoosi %like% dg_in & vuosi == vuosi_in & pvm == pvm_in & tarkastelujakso == tarkastelu_in & laake_type == laake_in],
    by = 'shp' 
  )
  
  rate_range <- d$pros %>% range()
  
  
  pos_hki <- d %>% subset(shp=="Helsinki") %>% st_bbox()
  
  ggplot(d)+
    geom_sf(aes(fill=pros), color = 'gray')+
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
     ggtitle(d$diagnoosi %>% unique() %>% as.character()
     )+
    labs(fill = paste0("Prosenttia \n", "(mediaani ", median(d$pros) %>% round(round_in), ")"))+
    theme(plot.title = element_textbox_simple())

}

#draw_laakitys()
