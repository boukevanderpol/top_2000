# ===================================================
# Top 2000
#
# 
# ===================================================

library(tidyverse)
library(readxl)

jaren <- c("2012", "2013", "2014", "2015", "2016", 
           "2017", "2018", "2019", "2020", "2021")
lijst <- tibble()
for (i in seq_along(jaren)) {
  aaa <- readxl::read_xlsx("lijst_1.xlsx",
                           sheet = i)
  lijst <- bind_rows(lijst, aaa)
}
rm(aaa)
readr::write_csv2(lijst, 
                  file = "./top2000-shiny/lijst.csv")



