# ===============================================================
# Project ziekteverzuim rijksoverheid
# 
# algemene functies
# 
# 
# 
# ===============================================================

# selectie van de artiesten in de -------------------------
#    het jaar-slot en posities-slot
f_artiesten_selecteren <- function(x_waarde_min = 1999,
                                   x_waarde_max = laatste_jaar_top_2000,
                                   y_waarde_min = 1, 
                                   y_waarde_max = 500) {
  
  f_geg <- lijst %>%
    filter(jaar_lijst >= x_waarde_min,
           jaar_lijst <= x_waarde_max,
           positie >= y_waarde_min,
           positie <= y_waarde_max)
  
  f_geg <- distinct(f_geg, artiest)
  f_geg <- f_geg %>% arrange(artiest)
  f_geg <- as.character(f_geg$artiest)  
  return(f_geg)
}



# selectie van jaren --------------------------------------
f_jaren <- function(vanaf_jaar = 1999) {
  f_geg <- lijst %>% filter(jaar_lijst >= vanaf_jaar)
  f_geg <- distinct(f_geg, jaar_lijst)
  f_geg <- as.double(f_geg$jaar_lijst)
  return(f_geg)
}


# selectie van posities -----------------------------------
f_posities <- function() {
  f_geg <- 1:2000
  return(f_geg)
}


# wijzig een NA in een nul waarde -------------------------
f_NA_naar_nul <- function (x) {
  x[is.na(x)] <- "0"
  return(x)
}



# logische operator toevoegen:
`%notin%` <- Negate(`%in%`)



