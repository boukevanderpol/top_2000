# ===============================================================
# Project top 2000
# 
# Functies waaarmee grafieken worden gemaakt
# 
# 
# 
# ===============================================================


g_lijnen1 <- function() {
  ggplot(data = mtcars, aes(x = mpg, y = cyl)) + 
    geom_point()
}


g_lijnen <- function(x_waarde_min = 1999,
                     x_waarde_max = laatste_jaar_top_2000,
                     y_waarde_min = 1, 
                     y_waarde_max = 500,
                     artiest1 = "U2", # "Nick Cave & The Bad Seeds", #"Falco", #
                     song = NULL) {
  
  # gegevens samenstellen
  g_geg <- lijst %>%
    filter(jaar_lijst >= x_waarde_min,
           jaar_lijst <= x_waarde_max,
           positie >= y_waarde_min,
           positie <= y_waarde_max)
  g_geg_artiest <- lijst %>%
    filter(jaar_lijst >= x_waarde_min,
           jaar_lijst <= x_waarde_max,
           positie >= y_waarde_min,
           positie <= y_waarde_max,
           artiest == artiest1)
  if (!is_empty(song)) {
    g_geg_artiest <- g_geg_artiest %>%
      filter(titel == song)
  }
  if (!is_empty(g_geg_artiest)) { 
    temp1 <- distinct(g_geg_artiest, titel)
    g_geg_artiest_label <- tibble()
    for (i in 1:nrow(temp1)) {
      temp2 <- g_geg_artiest %>%
        filter(titel == temp1$titel[[i]])
      temp3 <- temp2 %>% 
        filter(jaar_lijst == temp2$jaar_lijst[[ceiling(nrow(temp2)/2)]])
      g_geg_artiest_label <- bind_rows(g_geg_artiest_label, temp3)
    }
  }
  
  # grafiek samenstellen
  grafiek <- 
    ggplot() + 
    geom_line(data = g_geg, 
              aes(x = jaar, 
                  y = positie, 
                  #colour = artiest_song, 
                  group = artiest_song),
              size = 1,
              colour = "gray75") + # rel(1.2)) +
    geom_line(data = g_geg_artiest, 
              aes(x = jaar, 
                  y = positie, 
                  colour = artiest_song, 
                  group = artiest_song),
              size = 2) +
    geom_label_repel(data = g_geg_artiest_label,
                     aes(x = jaar, 
                         y = positie + ((max(g_geg$positie) - min(g_geg$positie)) * 0.01),
                         label = titel),
                     size = 4) + 
    geom_text(data = g_geg_artiest_label,
              aes(x = jaar, y = positie + ((max(g_geg$positie) - min(g_geg$positie)) * 0.01),
                  label = titel), 
              size = 4,
              vjust = 0,
              colour = "black",
              position = position_nudge(x = 1, y = (max(g_geg$positie) - min(g_geg$positie)) * 0.01)) + 
    theme_minimal() + 
    theme(legend.position = "none")
  
  
  
}


g_lijne2 <- function(x_waarde_min = 1999,
                     x_waarde_max = laatste_jaar_top_2000,
                     #y_waarde_min = 1, 
                     #y_waarde_max = 500,
                     artiest1 = "U2", # "Nick Cave & The Bad Seeds", #"Falco", #
                     song = NULL) {
  
  # gegevens samenstellen
  g_geg <- lijst %>%
    filter(jaar_lijst >= x_waarde_min,
           jaar_lijst <= x_waarde_max,
           #positie >= y_waarde_min,
           #positie <= y_waarde_max,
           artiest == artiest1)
  #g_geg_artiest <- lijst %>%
  #  filter(jaar_lijst >= x_waarde_min,
  #         jaar_lijst <= x_waarde_max,
  #         positie >= y_waarde_min,
  #         positie <= y_waarde_max,
  #         artiest == artiest1)
  #if (!is_empty(song)) {
  #  g_geg_artiest <- g_geg_artiest %>%
  #    filter(titel == song)
  #}
  #if (!is_empty(g_geg_artiest)) { 
  #  temp1 <- distinct(g_geg_artiest, titel)
  #  g_geg_artiest_label <- tibble()
  #  for (i in 1:nrow(temp1)) {
  #    temp2 <- g_geg_artiest %>%
  #      filter(titel == temp1$titel[[i]])
  #    temp3 <- temp2 %>% 
  #      filter(jaar_lijst == temp2$jaar_lijst[[ceiling(nrow(temp2)/2)]])
  #    g_geg_artiest_label <- bind_rows(g_geg_artiest_label, temp3)
  #  }
  #}
  
  # grafiek samenstellen
  grafiek <- 
    ggplot() + 
    geom_line(data = g_geg, 
              aes(x = jaar, 
                  y = positie, 
                  colour = artiest_song, 
                  group = artiest_song),
              size = 2,
              ) +
    #geom_line(data = g_geg_artiest, 
    #          aes(x = jaar, 
    #              y = positie, 
    #              colour = artiest_song, 
    #              group = artiest_song),
    #          size = 2) +
    #geom_label_repel(data = g_geg_artiest_label,
    #                 aes(x = jaar, 
    #                     y = positie + ((max(g_geg$positie) - min(g_geg$positie)) * 0.01),
    #                     label = titel),
    #                 size = 4) + 
#    geom_text(data = g_geg_artiest_label,
#              aes(x = jaar, y = positie + ((max(g_geg$positie) - min(g_geg$positie)) * 0.01),
#                  label = titel), 
#              size = 4,
#              vjust = 0,
#              colour = "black",
#              position = position_nudge(x = 1, y = (max(g_geg$positie) - min(g_geg$positie)) * 0.01)) + 
    theme_minimal() + 
    theme(legend.position = "none")


  
}
  



