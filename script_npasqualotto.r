
# packages ----------------------------------------------------------------
library(tidyverse)
library(sp)
library(sf)

# Lendo os dados de occorrencia das spp ------------------------------------------------------------------
# atlantic.birds <- readr::read_csv(
#   here::here("ATLANTIC_BIRDS_quantitative.csv"))
atlantic.birds <- read.csv(
  here::here("ATLANTIC_BIRDS_quantitative.csv"))

head(atlantic.birds)
dim(atlantic.birds)


# filtrar dados para os municipios de Gallia por valores de uma coluna
sort(unique(atlantic.birds$Municipality))

<<<<<<< HEAD
<<<<<<< HEAD
atlantic.birds.filtro <- atlantic.birds %>% 
  dplyr::filter(Municipality %in% c("GÁLIA", "GAR+çA", "GARCA"))
head(atlantic.birds.filtro)
dim(atlantic.birds.filtro)

# criando objeto espacial com o pacote sf 
atlantic.birds.vct <- atlantic.birds.filtro %>% 
  sf::st_as_sf(coords = c("Longitude_x", "Latitude_y"), crs = 4326)
atlantic.birds.vct

# plot
plot(atlantic.birds.vct$geometry, pch = 20, main = NA, axes = TRUE, graticule = TRUE)
=======
>>>>>>> 620521f25a43c0ada1e188a6356d03a635125c89
=======
>>>>>>> 620521f25a43c0ada1e188a6356d03a635125c89
