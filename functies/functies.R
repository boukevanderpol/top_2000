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
f_artiesten_selecteren <- function(jaar1 = 1999) {
  
  #f_geg <- lijst %>%
  #  filter(jaar_lijst >= jaar1)
  f_geg <- distinct(lijst, met_the)
  f_geg <- f_geg %>% arrange(met_the)
  f_geg <- as.character(f_geg$met_the)  
  #f_geg <- append(" ", f_geg)
  return(f_geg)
}



f_artiestsong_selecteren <- function(jaar1 = 1999,
                                     artiest1 = "Falco") {
  f_geg <- lijst %>%
    filter(#jaar_lijst >= jaar1,
           artiest == artiest1)
  f_geg <- distinct(f_geg, titel)
  f_geg <- f_geg %>% arrange(titel)
  f_geg <- as.character(f_geg$titel)
  f_geg <- append("(leeg)", f_geg)
  return(f_geg)
}



# selectie van jaren --------------------------------------
f_jaren <- function(vanaf_jaar = 1999) {
  f_geg <- lijst %>% filter(jaar_lijst >= vanaf_jaar)
  f_geg <- distinct(f_geg, jaar_lijst)
  f_geg <- as.double(f_geg$jaar_lijst)
  return(f_geg)
}



# wijzig een NA in een nul waarde -------------------------
f_NA_naar_nul <- function (x) {
  x[is.na(x)] <- "0"
  return(x)
}



# logische operator toevoegen:
`%notin%` <- Negate(`%in%`)



