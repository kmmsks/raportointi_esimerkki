
# Ikävakioitu prevalenssi alueittain -------------------------------------------

# tama jatkoon:
dat_figs <- list()

# tama on apulista ja lopuksi poistetaan:
figdat <- list()

# vaesto sairaanhoitopiireittain
figdat$vaesto <- dat$latest_y[DIAGNOOSI=='F20 skitsofrenia', .(vaesto=sum(VAESTO, na.rm = T)), by=.(shp, IKARYHMA)]

# tapausten maara ikaryhmittain
figdat$n_age <- dat$latest_y[,.(N=sum(N, na.rm = T)), by=.(shp, IKARYHMA)]

# yhdistetaan datat
figdat$age <- merge(
  figdat$n_age,
  figdat$vaesto, 
  by=c('shp','IKARYHMA')
)[,rate:=rate_per*N/vaesto][]

#liitetaan standardivaesto
figdat$age <- merge(
  figdat$age, 
  dat$stpop_latest_y, 
  by='IKARYHMA')

# ikavakiointi
figdat$asr <- figdat$age[, as.list(rate_per*ageadjust.direct(N, vaesto, stdpop=stp)), by=shp]


# Tassa data:
dat_figs$prevalence_alue <- figdat$asr

rm(figdat)

# Diagnoosien jakauma (hierarkinen) --------------------------------------------

# data
dat_figs$diagnoosien_jakauma$dg_1_krt <- dat$latest_y[!is.na(dg), .(n = sum(N, na.rm = T)),dg][,pros:=100*n/sum(n, na.rm = T)]

dat_figs$diagnoosien_jakauma$dg_2_krt <- dat$latest_y_dg_2_krt[!is.na(DIAGNOOSI), .(n = sum(N, na.rm = T)),dg][,pros:=100*n/sum(n, na.rm = T)]

#lukumaarat
n_1_krt <- dat_figs$diagnoosien_jakauma$dg_1_krt[,sum(n)]
n_2_krt <- dat_figs$diagnoosien_jakauma$dg_2_krt[,sum(n)]


# Diagnsoosien jakauma aleuittain ----------------------------------------------


dat_figs$diagnoosien_jakauma_alueittain$pros <- dat$latest_y[!is.na(dg) & dg!='Muut' ,sum(N, na.rm = T),.(dg, shp)][,pros:=100*V1/sum(V1, na.rm = T),shp]


dat_figs$diagnoosien_jakauma_alueittain$median <- dat_figs$diagnoosien_jakauma_alueittain$pros[,.(median=median(pros)),dg]


# # Diagnoosikohtainen prevalenssi alueittain ----------------------------------

figdat <- list()

figdat$vaesto <- dat$latest_y[dg=='F20',.(vaesto=sum(VAESTO, na.rm = T)), by=.(shp, IKARYHMA)]

figdat$n_age <- dat$latest_y[!is.na(dg) & dg!='Muut',.(N=sum(N, na.rm = T)), by=.(dg, shp, IKARYHMA)]

figdat$age <- merge(
  figdat$n_age, 
  figdat$vaesto, 
  by=c('IKARYHMA', 'shp'))

figdat$age <- merge(
  figdat$age, 
  dat$stpop_latest_y, 
  by='IKARYHMA')

# ikavakiointi
dat_figs$prevalence_alue_diagnoosi$asr <- figdat$age[, as.list(rate_per*ageadjust.direct(N, vaesto, stdpop=stp)), by=.(dg, shp)]

# shp nimet
#dat_figs$prevalence_alue_diagnoosi$asr[, shp := factor(SHP, levels = labs_lst$SHP, labels = labs_lst$shp_aluenimi)]

# mediaani rate
dat_figs$prevalence_alue_diagnoosi$median_rate <- dat_figs$prevalence_alue_diagnoosi$asr[,.(median=median(adj.rate)),dg]



rm(figdat)
