# Pacotes ----------------------------------------------------------------
library(tidyverse)
library(here)
library(sp)
library(sf)
library(spatialEco)
library(plotly)
#------------------------------------------------------------------------------------------------------------------------

# Criando um diretorio
dir.create(here::here("vetor"))

# Mapa Brasil
br_2020 <- geobr::read_country(year = 2020, showProgress = FALSE)
br_2020

plot(br_2020$geom, col = "gray", main = NA, axes = TRUE, graticule = TRUE)

# filtrar apenas o estado de Sao Paulo
sp <- br_2020 %>% 
  dplyr::filter(abbrev_state == "SP")
head(sp)

plot(sp$geom, col = "gray", main = NA, axes = TRUE, graticule = TRUE)

# importando dados
options(timeout = 600)

#importando polígonos #????deu errado
for(i in c(".dbf", ".prj", ".shp", ".shx")){
  download.file(url = paste0("https://geo.fbds.org.br/SP/AGUAI/USO/", i),
                destfile = here::here("C:/Users/profm/Documents/Doutorado Geografia PUC Minas/Disciplinas 2021_02/Geospacial com R/GitHub/projeto_final/https-github.com-prfurini-geor-g7-butia", paste0("SP_3500303_USO", i)), mode = "wb")
}

# acessar a tabela de atributos
atlantic.birds.sp.sample <- sf::st_drop_geometry(atlantic.birds.sp.sample)
atlantic.birds.sp.sample

# classe
class(atlantic.birds.sp.sample)

# acessar a tabela de atributos
atlantic.birds_group_lat

# juncao de tabela de atributos
atlantic.birds.riqueza <- dplyr::left_join(atlantic.birds, atlantic.birds_group_lat, by = "Latitude_y","Longitude_x")
atlantic.birds.riqueza

head(atlantic.birds.riqueza)
dim(atlantic.birds.riqueza)

# criando objeto espacial com o pacote sf 
atlantic.bird_proj <- atlantic.birds.riqueza %>% 
  sf::st_as_sf(coords = c("Latitude_y","Longitude_x.x"), crs = 4326)

atlantic.bird_proj
dim(atlantic.bird_proj)

# converter sistema de coordenadas dos dados das especies para SIRGAS 2000
atlantic.bird_proj <- sf::st_transform(atlantic.bird_proj, crs = 5880)
atlantic.bird_proj

# filtrar dados para o estado de SP
atlantic.birds.sp_proj <- atlantic.bird_proj %>% 
  dplyr::filter(State == "SP")
head(atlantic.birds.sp_proj)
dim(atlantic.birds.sp_proj)

length(unique(atlantic.birds.sp_proj$geometry))

# Amostra aleatoria com limite de distancia ------------------------------

# selecionando aleatoriamente 150 pontos, distantes entre si por pelo menos 5 km
set.seed(5)
atlantic.birds.sample_proj <- subsample.distance(as_Spatial(atlantic.birds.sp_proj), size = 150, d = 5000, replacement = FALSE, echo=T)
atlantic.birds.sp.sample

# convertendo o objeto sp para sf
atlantic.birds.sample_proj <- st_as_sf(atlantic.birds.sample_proj)
head(atlantic.birds.sample_proj)

# Camada de UCs
uc <- sf::st_read(here::here("UCs.shp"), quiet = TRUE)
uc

plot(uc [10], col = c("blue", "orange", "gray30", "forestgreen", "green", "yellow", "pink", "red", "violet", "orange"), main = NA, axes = TRUE, graticule = TRUE)

sf::st_crs(uc)

# converter sistema de coordenadas dos dados das ucs para SIRGAS 2000
uc <- sf::st_transform(uc, crs = 5880)
uc

# calcular distancia das UCS
st_distance(.sample, UCs)

ab_sample_dist_uc <- atlantic.birds.sp_proj %>% 
  dplyr::mutate(dist_uc = sf::st_distance(atlantic.birds.sp_proj, uc))
ab_sample_dist_uc

