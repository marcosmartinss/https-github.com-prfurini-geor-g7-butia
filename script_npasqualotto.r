
# packages ----------------------------------------------------------------
library(tidyverse)
library(sp)
library(sf)
library(spatialEco)

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

# converter sistema de coordenadas dos dados das especies para SIRGAS 2000
atlantic.birds.points <- sf::st_transform(atlantic.birds.points, crs = 5880)
atlantic.birds.points
st_crs(atlantic.birds.points)

# filtrar dados para o estado de SP
atlantic.birds.sp <- atlantic.birds.points %>% 
  dplyr::filter(State == "SP")
head(atlantic.birds.sp)
dim(atlantic.birds.sp)

# baixar shape Brasil 2020
br_2020 <- geobr::read_country(year = 2020)

# converter sistema de coordenadas para SIRGAS 2000
br_2020 <- sf::st_transform(br_2020, crs = 5880)
br_2020
st_crs(br_2020)

# filtrar apenas o estado de Sao Paulo
sp <- br_2020 %>% 
  dplyr::filter(abbrev_state == "SP")
sp

# numero de pontos de ocorrencia diferentes
length(unique(atlantic.birds.sp$geometry)) # Sao 349!

# plot dados de aves em Sao Paulo
plot(sp$geom, pch = 20, main = NA, axes = TRUE, graticule = TRUE)
plot(atlantic.birds.sp$geometry, pch = 20, main = NA, axes = TRUE, graticule = TRUE, add= TRUE)


# selecionando aleatoriamente 100 pontos distantes entre si por pelo menos 3 km
atlantic.birds.sp.sample <- subsample.distance(as_Spatial(atlantic.birds.sp), size = 100, d = 3000, replacement = FALSE, echo=T)
atlantic.birds.sp.sample

# convertendo o objeto sp para sf
atlantic.birds.sp.sample <- st_as_sf(atlantic.birds.sp.sample)
head(atlantic.birds.sp.sample)

# plot amostra aleatória dos pontos
plot(subsamp$geometry, pch = 20, main = NA, axes = TRUE, graticule = TRUE, add= TRUE, col= "red")
