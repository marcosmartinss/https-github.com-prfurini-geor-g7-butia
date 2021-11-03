
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

# criando objeto espacial com o pacote sf 
atlantic.birds.points <- atlantic.birds %>% 
  sf::st_as_sf(coords = c("Longitude_x", "Latitude_y"), crs = 4326)
atlantic.birds.points

# baixar shape Brasil 2020
br_2020 <- geobr::read_country(year = 2020)

# filtrar apenas o estado de Sao Paulo
sp <- br_2020 %>% 
  dplyr::filter(abbrev_state == "SP")
head(sp)

# filtrar dados para o estado de SP
atlantic.birds.sp <- atlantic.birds.points %>% 
  dplyr::filter(State == "SP")
head(atlantic.birds.sp)
dim(atlantic.birds.sp)

# numero de pontos de ocorrencia diferentes
length(unique(atlantic.birds.sp$geometry)) # Sao 349!

# plot dados de aves em Sao Paulo
plot(sp$geom, pch = 20, main = NA, axes = TRUE, graticule = TRUE)
plot(atlantic.birds.sp$geometry, pch = 20, main = NA, axes = TRUE, graticule = TRUE, add= TRUE)



