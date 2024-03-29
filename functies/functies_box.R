# ===============================================================
# Project top 2000
# 
# functies waaarmee waarden voor de valueboxen worden gevuld
# 
# 
# 
# ===============================================================


vb_hoogste_positie <- function(artiest1 = "Queen", # "Nick Cave & The Bad Seeds", #"Falco", #
                               song1 = "Bohemian Rhapsody"){#  "(leeg)") {
  
  if (song1 == "(leeg)") {

    box_geg <- lijst %>%
      filter(met_the == artiest1)

  } else {

    box_geg <- lijst %>%
      filter(met_the == artiest1,
             titel == song1)
  }
  
  hoogste_positie <- min(box_geg$positie)
  song2 <- box_geg %>%
    filter(positie == hoogste_positie)
  
  if (nrow(song2) == 1) {
    naam_song = song2$titel[[1]]
    jaar_song = song2$jaar[[1]]
  } else if (nrow(song2) == 2) {
    naam_song = song2$titel[[1]]
    jaar_song1 = song2$jaar[[1]]
    jaar_song2 = song2$jaar[[2]]
    jaar_song = paste0(jaar_song1, " & ", jaar_song2)
  } else {
    naam_song = song2$titel[[1]]
    jaar_song1 = song2$jaar[[1]]
    jaar_song2 = song2$jaar[[2]]
    jaar_song = paste0(jaar_song1, ", ",
                       jaar_song2, ", ", "etc.")
    
  }
  
  tekst <- paste0(hoogste_positie," - ",
                  naam_song, " - ",
                  jaar_song)
  
  return(tekst)

}



vb_zv_kleur <- function(afk = "AZ") {
  
  geg_tabel_lj <- verzuim %>%
    filter(onderwerp %in% c("zv%"),
           org_afk %in% afk,
           jaar %in% c("2022"))
  geg_tabel_lj$maand_nr <- as.integer(geg_tabel_lj$maand_nr)
  zv_perc <- geg_tabel_lj$aantal[max(geg_tabel_lj$maand_nr)]
  kleur <- ifelse(gem_verzuim_rijk > zv_perc, "green", "maroon") 
  
  return(kleur)
  
}



vb_zv_icoon <- function(afk = "AZ") {
  
  geg_tabel_lj <- verzuim %>%
    filter(onderwerp %in% c("zv%"),
           org_afk %in% afk,
           jaar %in% c("2022"))
  geg_tabel_lj$maand_nr <- as.integer(geg_tabel_lj$maand_nr)
  zv_perc <- geg_tabel_lj$aantal[max(geg_tabel_lj$maand_nr)]
  icoon <- ifelse(gem_verzuim_rijk > zv_perc, "arrow-up", "arrow-down") 
  
  return(icoon)
  
}



vb_aantal_zieken <- function(afk = "AZ") {
  
  geg_tabel_lj <- zieken_onderdelen %>%
    filter(org_afk %in% afk,
           onderwerp %in% "Aantal zieken",
           jaar_wknr %in% c("2022"),
           week_nr < 35) %>%           # LELIJK !!!!!
    group_by(jaar_wknr, week_nr) %>%
    summarise(aantal = mean(aantal),
              .groups = "drop") %>%
    mutate(jaar = jaar_wknr,
           periode = week_nr) %>%
    select(jaar, periode, aantal)
  geg_tabel_lj$periode <- as.integer(geg_tabel_lj$periode)
  aantal_z <- geg_tabel_lj$aantal[max(geg_tabel_lj$periode)]
  aantal_z <- round(aantal_z, 0)
  
  return(aantal_z)
  
}



vb_aantal_ziekmeldingen <- function(afk = "BZK") {
  
  geg_tabel_lj <- ziekmeldingen_onderdelen %>%
    filter(org_afk %in% afk,
           onderwerp %in% "Aantal ziekmeldingen",
           jaar_wknr %in% c("2022"),
           week_nr < 35) %>%           # LELIJK !!!!!
    group_by(jaar_wknr, week_nr) %>%
    summarise(aantal = sum(aantal),
              .groups = "drop") %>%
    mutate(jaar = jaar_wknr,
           periode = week_nr) %>%
    select(jaar, periode, aantal)
  geg_tabel_lj$periode <- as.integer(geg_tabel_lj$periode)
  aantal_zm <- geg_tabel_lj$aantal[max(geg_tabel_lj$periode)]

  return(aantal_zm)
  
}



