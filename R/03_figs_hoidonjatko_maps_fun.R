

# taustavarit 1, 2 ja 4 viikon kuviin. Ideana, että erottaa selvasti etta kuvissa on eri data. Ei varmaan tarvi lopulliseen.
palettes$hl_jakso <- c("white", 'ivory', "honeydew1")


draw_hoidonjatko <- function(hoitomuoto_in = "Yhteensä (käynnit)", jakso_in = "1 viikko", round_in = 0, font_family_in = 'sans', font_size_in=3, lang = 'fi',
                             aluerajat_color = 'gray', vuosi_in = latest_y$hoidonjatko){
  # yhdistetaan kartta ja hoidonjatko
  d <- merge(
    gis$shphki,
    dat$hoidonjatko[HOITOMUOTO == hoitomuoto_in &  JAKSO == jakso_in],
    by = 'srnhtpr' 
  )
  
  # vain haluttu vuosi
  d %<>% subset(VUOSI == vuosi_in)
  
  # varitys kaanteinen osassa hoitomuotoja
  ifelse(hoitomuoto_in == "Rehospitalisaatio" | hoitomuoto_in == 'Sosiaalihuolto' | hoitomuoto_in == 'Päivystyskäynti', 
         clrs <- rev(palettes$hj2a), clrs <- palettes$hj2a)
  

  ifelse(jakso_in == "1 viikko", hl_jakso_color <- palettes$hl_jakso[1], ifelse(jakso_in == "4 viikkoa", hl_jakso_color <- palettes$hl_jakso[2], hl_jakso_color <- palettes$hl_jakso[3]))
  
  ifelse(lang=='en', lgnd <- "Percent \n (median ", lgnd <- "Prosenttia \n (mediaani ")

  pos_hki <- d %>% subset(srnhtpr=="Helsinki") %>% st_bbox()
  
  
  ggplot(d)+
    geom_sf(aes(fill=pros), color = aluerajat_color)+
    # scale_fill_gradient(low = 'red', high = 'ivory', limits = c(20, 90))+
    scale_fill_gradientn(
      limits  = range(d$pros),
      colours = clrs,
      values  = c(0, scales::rescale(median(d$pros), from = range(d$pros)), 1),
    )+
    geom_sf_text(data = d %>% subset(srnhtpr != 'Helsinki'), 
                 aes(label =pros %>% round(round_in)),size=font_size_in,family=font_family_in,
    )+
    annotate("text", 
             x= pos_hki$xmax+70000, 
             y=(pos_hki$ymin -10000), 
             label = paste("Helsinki", d %>% subset(srnhtpr == 'Helsinki') %>% .$pros %>% round(round_in)), 
             size=font_size_in,family=font_family_in
    )+
    coord_sf()+
    theme_void()+
    ggtitle(paste(hoitomuoto_in))+
    labs(fill = paste0(lgnd, median(d$pros) %>% round(round_in), ")"))+
    theme(panel.background = element_rect(fill = hl_jakso_color, color = NA))
  
}

