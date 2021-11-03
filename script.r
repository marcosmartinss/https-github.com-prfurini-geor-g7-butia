#' ---
<<<<<<< HEAD
#' title: tabalho_fina_disciplina - estrutura e manipulacao de dados na linguagem R
#' author: Fulbert Gnonlonfoun
#' date: 2021-11-03
#' ---
atlantic.birds <- read.csv("ATLANTIC_BIRDS_quantitative.csv")
=======
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
>>>>>>> d7cec99ccc436593500780c50126dbe247e9c847
head(atlantic.birds)

sort(unique(atlantic.birds$Municipality))

dim(atlantic.birds[atlantic.birds$Municipality == "GAR+çA", ])
