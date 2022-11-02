
# hoidonjatko-datan valmistelu esityksiä varten.

################################################################################
latest_y$hoidonjatko <- dat$hoidonjatko[, max(VUOSI)]


d0 <- dat$hoidonjatko

# prosennttiosuus henkiloista  
d0[, pros := KAYNTEJA / JAKSOJA * 100]
d0 <- d0[!(SHP %in% no_vaesto) & !is.na(SHP)]

d0[SHP == 'Helsingin ja Uudenmaan SHP', SHP := 'HUS ei Helsinki']



# nama modaliteetit 1. riville
summatut_hoitomuodot <- c("Yhteensä", "Yhteensä (käynnit)", "PTH + ESH avohoito (käynnit)")




# long to wide scatter-kuvia varten

# muutetaan space -> _
d0[, hoitomuoto := gsub(' ', '_', HOITOMUOTO)]
d0[, jakso := gsub(' ', '_', JAKSO)]

dat$hoidonjatko_wide <- dcast(d0, SHP ~ jakso + hoitomuoto, value.var = 'pros')

# Muokataat 1. kuvaa varten
d0[, jakso:= factor(JAKSO, levels =  c('1 viikko', '2 viikkoa', '4 viikkoa'), labels = c('1', '2', '4'))]

d0[, shp := factor(SHP, levels = labs_lst$SHP, labels = labs_lst$shp_aluenimi)]

dat$hoidonjatko <- d0


## liitetaan rekisteritieto
dat$hoidonjatko[, srnhtpr := shp] 

rm(d0)
