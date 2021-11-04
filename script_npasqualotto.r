
# Pacotes ----------------------------------------------------------------------
library(tidyverse)
library(here)
library(sp)
library(sf)
library(spatialEco)
library(plotly)

# Carregando de ocorrencia das spp de aves -------------------------------------
atlantic.birds <- read.csv("ATLANTIC_BIRDS_quantitative.csv")

head(atlantic.birds)
dim(atlantic.birds)

# Riqueza de spp de aves em cada coordenada ------------------------------------
# Agrupar a riqueza das spp de ave por ponto de amostragem
atlantic.birds_group_latlong <- atlantic.birds %>% 
  dplyr::group_by(Latitude_y, Longitude_x) %>% 
  dplyr::summarise(riqueza = n())
print(atlantic.birds_group_latlong, n= 10)

# juncao de tabela de atributos
atlantic.birds.riqueza <- dplyr::left_join(atlantic.birds, atlantic.birds_group_latlong, by = "Latitude_y","Longitude_x")
atlantic.birds.riqueza
dim(atlantic.birds.riqueza)

# Objeto espacial, reprojetacao e filtro para area de estudo (SP) --------------

# criando objeto espacial com o pacote sf 
colnames(atlantic.birds.riqueza)[5] <- "Longitude_x"
atlantic.birds.points <- atlantic.birds.riqueza %>% 
  sf::st_as_sf(coords = c("Longitude_x", "Latitude_y"), crs = 4326)
atlantic.birds.points

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

# Amostra aleatoria com limite de distancia ------------------------------------

# selecionando aleatoriamente 150 pontos, distantes entre si por pelo menos 5 km
set.seed(5)
atlantic.birds.sp.sample <- subsample.distance(as_Spatial(atlantic.birds.sp), size = 150, d = 5000, replacement = FALSE, echo=T)
atlantic.birds.sp.sample

# convertendo o objeto sp para sf
atlantic.birds.sp.sample <- st_as_sf(atlantic.birds.sp.sample)
head(atlantic.birds.sp.sample)

# df com dados de riqueza para cada uma das 150 amostras -----------------------
df.riq <- as.data.frame(atlantic.birds.sp.sample)
df.riq <- df.riq[, c(1, 25)]
head(df.riq, 20)

# Plot area de estudo, dados de ocorrencia -------------------------------------

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

# Dist pontos amostra aleatoria a UC mais proxima ------------------------------


# % florestal (IF_2020) em cada ponto da amostra aleatoria  --------------------


# Input GLM --------------------------------------------------------------------