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
atlantic.birds <- read.csv("ATLANTIC_BIRDS_quantitative.csv", encoding = "ASCII")
head(atlantic.birds)

sort(unique(atlantic.birds$Municipality))

dim(atlantic.birds[atlantic.birds$Municipality == "GAR+çA", ])
