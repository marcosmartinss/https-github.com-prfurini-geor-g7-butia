
# Pacotes ----------------------------------------------------------------
library(tidyverse)
library(sp)
library(sf)
library(spatialEco)
library(plotly)

# Dados de ocorrencia das spp de ave --------------------------------------

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
dim(atlantic.birds.points)

# converter sistema de coordenadas dos dados das especies para SIRGAS 2000
atlantic.birds.points <- sf::st_transform(atlantic.birds.points, crs = 5880)
atlantic.birds.points

# filtrar dados para o estado de SP
atlantic.birds.sp <- atlantic.birds.points %>% 
  dplyr::filter(State == "SP")
head(atlantic.birds.sp)
dim(atlantic.birds.sp)

# numero de pontos de amostragem diferentes
length(unique(atlantic.birds.sp$geometry)) # Sao 349!

# Limites area de estudo (estado SP) -------------------------------------

# baixar shape Brasil 2020
br_2020 <- geobr::read_country(year = 2020)

# converter sistema de coordenadas para SIRGAS 2000
br_2020 <- sf::st_transform(br_2020, crs = 5880)
br_2020

# filtrar apenas o estado de Sao Paulo
sp <- br_2020 %>% 
  dplyr::filter(abbrev_state == "SP")
sp

# Amostra aleatoria com limite de distancia ------------------------------

# selecionando aleatoriamente 150 pontos, distantes entre si por pelo menos 5 km
set.seed(5)
atlantic.birds.sp.sample <- subsample.distance(as_Spatial(atlantic.birds.sp), size = 150, d = 5000, replacement = FALSE, echo=T)
atlantic.birds.sp.sample

# convertendo o objeto sp para sf
atlantic.birds.sp.sample <- st_as_sf(atlantic.birds.sp.sample)
head(atlantic.birds.sp.sample)

# Plot area de estudo, dados de ocorrencia ------------------------------

plot(sp$geom, pch = 20, main = NA, axes = TRUE, graticule = TRUE) # area de estudo
plot(atlantic.birds.sp$geometry, pch = 20, main = NA, axes = TRUE, graticule = TRUE, add= TRUE) # pontos com ocorrencia das spp

# plotando a amostra aleatoria dos pontos
plot(atlantic.birds.sp.sample$geometry, pch = 20, main = NA, axes = TRUE, graticule = TRUE, add= TRUE, col= "red")

# plot interativo (zoom)
map_rc_2020_plotly_int <- ggplotly(
  ggplot() +
    geom_sf(data = sp) +
    geom_sf(data = atlantic.birds.sp) +
    geom_sf(data = atlantic.birds.sp.sample, col = "red") +
    theme_bw(base_size = 16))
map_rc_2020_plotly_int

# Riqueza de spp de aves pontos da amostra aleatoria ---------------------


# % florestal (IF_2020) em cada ponto da amostra aleatoria  --------------

# Dist pontos amostra aleatoria a UC mais proxima ------------------------

# Input GLM --------------------------------------------------------------