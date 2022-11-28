# ===============================================================
# Project top 2000
# 
# Functies waaarmee grafieken worden gemaakt
# 
# 
# 
# ===============================================================


g_lijnen1 <- function() {
  geg <- mtcars %>% filter(wt > 3)
  ggplot(data = geg, aes(x = mpg, y = disp)) + 
    geom_point()
}


g_lijnen <- function(x_waarde_min = 1999,
                     artiest1 = "U2", # "Nick Cave & The Bad Seeds", #"Falco", #
                     song1 = "(leeg)"
                     ) {
  
  # gegevens samenstellen
  if (song1 == "(leeg)") {
    g_geg <- lijst %>%
      filter(jaar_lijst >= x_waarde_min,
             artiest == artiest1)
    temp1 <- distinct(g_geg, titel)
    rij_mediaan <- tibble()
  } else {
    g_geg <- lijst %>%
      filter(jaar_lijst >= x_waarde_min,
             artiest == artiest1,
             titel == song1)
    temp1 <- distinct(g_geg, titel)
    rij_mediaan <- tibble()
  }

  for (i in seq_along(temp1$titel)) {
  temp2 <- g_geg %>% 
    filter(titel == temp1$titel[[i]]) %>%
    arrange(jaar_lijst)
  
    if((length(temp2$jaar_lijst) %% 2) == 0) {
      # even aantal observaties
      rij_mediaan_temp <- temp2 %>%
        filter(jaar_lijst == median(temp2$jaar_lijst) + 0.5)
    } else {
      # oneven aantal observaties
      rij_mediaan_temp <- temp2 %>%
        filter(jaar_lijst == median(temp2$jaar_lijst))
    }
  rij_mediaan <- bind_rows(rij_mediaan, rij_mediaan_temp)
  }
  
  # grafiek samenstellen
  grafiek <- 
    ggplot() + 
    geom_line(data = g_geg, 
              aes(x = jaar, 
                  y = positie, 
                  colour = artiest_song, 
                  group = artiest_song),
              linewidth = 2) +
    geom_label_repel(data = rij_mediaan,
                     aes(x = jaar, 
                         y = positie + ((max(g_geg$positie) - min(g_geg$positie)) * 0.01),
                         label = titel),
                     size = 4) + 
    #geom_text(data = rij_mediaan,
    #          aes(x = jaar, y = positie + ((max(g_geg$positie) - min(g_geg$positie)) * 0.01),
    #              label = titel), 
    #          size = 4,
    #          vjust = 0,
    #          colour = "black",
    #          position = position_nudge(x = 1, y = (max(g_geg$positie) - min(g_geg$positie)) * 0.01)) + 
    scale_y_reverse() +
    theme_minimal() + 
    theme(legend.position = "none", 
          axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))

  return(grafiek)
  
  
}

