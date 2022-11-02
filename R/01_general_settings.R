

# general settings kaikkiin esitelmiin
# psyk datan valmistelut, kuvien asetukset, labelit

################################################################################


# rate:t montako henkiloa kohden
rate_per <- 100


# data Prepare --------------------------------------------------------------

# alueet, jotka eivat kuulu tarkasteluun
no_vaesto <- c('Ulkomaat', 'Tuntematon', 'Tieto puuttuu')

# Diagnoosit ja niiden lyhenteet
dgs <- c('F20 skitsofrenia',
         'F25 skitsoaffektiiviset häiriöt',
         'F22,24 harhaluuloisuushäiriöt',
         'F23 Akuutit ja ohimenevät psykoottiset häiriöt',
         'F21 skitsotyyppinen häiriö',
         'F28 Muu ei-elimellinen psykoottinen häiriö',
         'F29 Määrittämätön ei-elimellinen psykoottinen häiriö',
         'Muut')

dgs1 <- c('F20', 'F25', 'F22,24', 'F23', 'F21', 'F28', 'F29', 'Muut')

diagnoosit_cap <- "F20.0-20.9 Skitsofrenia; F25.0-25.9 Skitsoaffektiiviset häiriöt; F22.0-22.9, 24 Harhaluuloisuushäiriöt; F23.0-23.9 Akuutit ja ohimenevät psykoottiset häiriöt; F21 Skitsotyyppinen häiriö; F28 Muu ei-elimellinen psykoottinen häiriö; F29 Määrittämätön ei-elimellinen psykoottinen häiriö
"

dgs2 <- c("F20" = "F20 Skitsofrenia", "F25" = " F25 Skitsoaffektiiviset häiriöt", "F22,24" = "F22,24 Harhaluuloisuushäiriöt" ,
          "F23" = "F23 Akuutit ja ohimenevät psykoottiset häiriöt", "F21" = "F21 Skitsotyyppinen häiriö", "F28" = "F28 Muu ei-elimellinen psykoottinen häiriö",
          "F29" = "F29 Määrittämätön ei-elimellinen psykoottinen häiriö")

dgs_laakitys <- c(
    "F2 kaikki",
    'F20 skitsofrenia',
    'F25 skitsoaffektiiviset häiriöt',
    'F22,24 harhaluuloisuushäiriöt',
    'F23 Akuutit ja ohimenevät psykoottiset häiriöt',
    'F21 skitsotyyppinen häiriö',
    'F28 Muu PH',
    'F29 Määrittämätön PH',
    'ICPC-2',
    'Muut')


prepare_data <- function(dat_in){
  dat_in[is.na(N), N := 0]
  dat_in[is.na(VAESTO), VAESTO := 0]
  
  # HUS nimetaan uudestaan
  dat_in[SHP == 'Helsingin ja Uudenmaan SHP', SHP := 'HUS ei Helsinki']
  
  # dg:t factoriksi
  dat_in[, DIAGNOOSI := factor(DIAGNOOSI, levels = seq(1,8), labels = dgs)]
  
  dat_in[, dg := factor(DIAGNOOSI, labels = dgs1)]
  
}

prepare_data(dat$psyk)
prepare_data(dat$dg_2_krt)

# poimi arvot ---------------------------------------------------------------

# viimeisin vuosi datassa
latest_y <- list()
latest_y$psyk <- dat$psyk[, max(VUOSI, na.rm = TRUE)]

# viimeisimman vuoden standardivaesto
dat$stpop_latest_y <- dat$psyk[VUOSI == latest_y$psyk & DIAGNOOSI == 'F20 skitsofrenia', .(stp = sum(VAESTO, na.rm = T)), by = IKARYHMA]

## viimeisimman vuoden data
dat$latest_y <- dat$psyk[VUOSI == latest_y$psyk & !(SHP %in% no_vaesto) & !is.na(SHP)]

# SHP puuttuu
dat$latest_y_shp_missing <- dat$psyk[VUOSI == latest_y$psyk & (SHP %in% no_vaesto | is.na(SHP))]

# kokonais-rate viimeisimpana vuonna
rate_latest_y <- rate_per*dat$latest_y[, sum(N, na.rm = T)] / dat$stpop_latest_y[, sum(stp, na.rm = T)]

dat$latest_y_dg_2_krt <- dat$dg_2_krt[VUOSI == latest_y$psyk & !(SHP %in% no_vaesto) & !is.na(SHP)]


#techSettings -------------------------------------------------------


theme_set(theme_bw()+ 
            theme(strip.background =element_rect(fill="white")
            )
)
text_size <- 16

# tahan listataan tarvittavat varit
palettes <- list()
palettes$hj2a <- c('#ef6f6a', 'white', '#8cc2ca')  


palettes$color_thl <- c("#2f62ad", "#be3f72", "#519b2f", "#29a0c1", "#cc77ac", "#faa61a", "#7bc143", "#606060", 
               "#c3c2c6", "#dcdfe2", "#2f62ad", "#be3f72", "#519b2f", "#29a0c1", "#cc77ac", "#faa61a", 
               "#7bc143", "#606060", "#c3c2c6", "#dcdfe2","#2f62ad", "#be3f72")

palettes$color_thl2 <- c("#2f62ad", "#be3f72", "#519b2f", "#29a0c1", "#cc77ac", "#faa61a", "red", "#606060", 
                "#c3c2c6", "deeppink", "#2f62ad", "#be3f72", "#519b2f", "#29a0c1", "#cc77ac", "#faa61a", 
                "#7bc143", "#606060", "#c3c2c6", "deeppink","#2f62ad", "#be3f72")


n_decim <- function(x, n_decim=2, big_mark = ' '){
  #format(as.numeric(sprintf(paste0("%.", n_decim, "f"), x)), big.mark = big_mark)
  formatC(x, big.mark = big_mark, digits = n_decim, decimal.mark=',', format = 'f')
}

#r figtext --------------------------------------------------------

# labels, kuvatekstit ym.
labs_lst <- list()
labs_lst$y_ikav <- paste('Ikävakioitu esiintyvyys per', rate_per, 'henkilöä', sep = ' ')
labs_lst$x_vuodet <- seq(2010,latest_y$psyk + 1,2)
labs_lst$tilanne <-paste0("Tilanne 31.12.", latest_y, ".")
labs_lst$ci <- "Errorbar: 95 % luottamusväli."
labs_lst$alueet <- "Alue: Sairaanhoitopiirit, Helsingin kaupunki ja muu HUS erikseen."
labs_lst$suomi <- "Koko Suomi."
labs_lst$mediaani <- "Katkoviiva kuvaa alueiden mediaania."


# Aluiden nimet ----------------------------------------------------------------


labs_lst$SHP  <- c("Ahvenanmaa", "Etelä-Karjalan SHP", "Etelä-Pohjanmaan SHP", 
                   "Etelä-Savon SHP", "HUS ei Helsinki", "Helsinki", "Itä-Savon SHP", 
                   "Kainuun SHP", "Kanta-Hämeen SHP", "Keski-Pohjanmaan SHP", "Keski-Suomen SHP", 
                   "Kymenlaakson SHP", "Lapin SHP", "Länsi-Pohjan SHP", "Pirkanmaan SHP", 
                   "Pohjois-Karjalan SHP", "Pohjois-Pohjanmaan SHP", "Pohjois-Savon SHP", 
                   "Päijät-Hämeen SHP", "Satakunnan SHP", "Vaasan SHP", "Varsinais-Suomen SHP"
)

labs_lst$shp_aluenimi <- c("Ahvenanmaa", "Etelä-Karjala", "Etelä-Pohjanmaa", 
                    "Etelä-Savo", "HUS ei Helsinki", "Helsinki", "Itä-Savo", 
                    "Kainuu", "Kanta-Häme", "Keski-Pohjanmaa", "Keski-Suomi", 
                    "Kymenlaakso", "Lappi", "Länsi-Pohja", "Pirkanmaa", 
                    "Pohjois-Karjala", "Pohjois-Pohjanmaa", "Pohjois-Savo", 
                    "Päijät-Häme", "Satakunta", "Vaasa", "Varsinais-Suomi"
)

# shp nimet
dat$latest_y[, shp := factor(SHP, levels = labs_lst$SHP, labels = labs_lst$shp_aluenimi)]

