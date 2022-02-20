library(tidyverse)
library(readxl)
library(installr)

pad <- "D:/Users/polde/Documents/R/top_2000"
setwd(pad)

aaa <- readxl::read_xlsx(
  path = "lijst1999-2021.xlsx",
  sheet = "alles",
  col_names = T
)




