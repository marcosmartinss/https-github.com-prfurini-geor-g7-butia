#' ---
#' titulo:  
#' autores: 
#' data: 2021-11-03
#' ---

# packages ----------------------------------------------------------------
library(tidyverse)
library(sp)
library(sf)

# Lendo os dados de occorrencia das spp ------------------------------------------------------------------
atlantic.birds <- read.csv("ATLANTIC_BIRDS_quantitative.csv")
head(atlantic.birds)
dim(atlantic.birds)
str(atlantic.birds)


# filtrar linhas por valores de uma coluna
atlantic.birds <- atlantic.birds %>% 
  dplyr::filter(Municipality == "GÁLIA")
head(atlantic.birds)
dim(atlantic.birds)

sort(unique(atlantic.birds$Municipality))

