


# Valmistelaan laakitys niin, etta sen voi lititaa gis-dataan.

# ei ole harmoniassa psyk-datan kanssa.

# viimeisin vuosi 
latest_y$laakitys <- dat$laakitys_in[, max(vuosi)]


dat$laakitys_in[shp == 'Helsingin ja Uudenmaan SHP', shp := 'HUS ei Helsinki']


dat$laakitys_in[, shp := factor(shp, levels = labs_lst$SHP, labels = labs_lst$shp_aluenimi)]


# diagnoosit

f2_kaikki <-  dat$laakitys_in[, .( diagnoosi = 0, pop = sum(pop, na.rm = TRUE), laake = sum(laake, na.rm = T),klotsapiini = sum(klotsapiini, na.rm = T), depot = sum(depot, na.rm = T)), 
                keyby = .(shp, vuosi, pvm, tarkastelujakso)]

dat$laakitys_all <- 
  rbindlist(list(dat$laakitys_in, f2_kaikki), use.names = TRUE)

dat$laakitys <- dat$laakitys_all %>% melt(id.vars= c('shp', 'diagnoosi', 'vuosi', 'pvm', 'tarkastelujakso', 'pop'), value.vars = c('laake', 'klotsapiini', 'depot'), 
                                      variable.name = 'laake_type')

dat$laakitys[, diagnoosi := factor(diagnoosi, levels = seq(0,9), labels = dgs_laakitys)]


dat$laakitys[, pros := value/pop*100]


dat$laakitys_in <- NULL
dat$laakitys_all <- NULL
rm(f2_kaikki)
