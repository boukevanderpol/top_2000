# ===============================================================
# Project top 2000
# 
# Functies om gegevens te laden en klaar te maken voor gebruik
# 
# 
# 
# ===============================================================

# library(tidyverse)

# verzuimpercentage -----------------------------------
f_laden_lijst <- function(x) { 
  
  temp <- readxl::read_xlsx(
    path = x,
    col_names = TRUE,
    col_types = c("numeric", "numeric", "numeric", 
                  "text", "text", "text", "numeric", 
                  "text", "text", "text", "text"),
    .name_repair = "unique"
  )
  return(temp)
}

lijst <- f_laden_lijst("./gegevens/top2000_tm2021.xlsx")

lijst <- lijst %>% select(-c("zonder_formule", "met_formule", "nr"))

lijst$met_the <- stringr::str_to_lower(lijst$met_the)
lijst$met_the <- stringr::str_to_title(lijst$met_the)
lijst$titel <- stringr::str_to_lower(lijst$titel)
lijst$titel <- stringr::str_to_title(lijst$titel)

lijst$jaar <- factor(lijst$jaar_lijst, 
                     levels = c(1999:2021))
lijst$positie_factor <- factor(lijst$positie, 
                               levels = c(1:2000))
lijst$jaar_song <- factor(lijst$jaar_song, 
                           levels = c(1924:2030))
lijst$artiest_song <- paste(lijst$met_the, "-", lijst$titel)

# doorsnedes ------------------------------------------
#artiest_lijst <- distinct(lijst, artiest)
