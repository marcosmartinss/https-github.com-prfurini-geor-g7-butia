#' ---
#' title: Operacoes vetoriais UCs
#' author: Felipe Bufalo
#' date: 2021-11-03
#' ---

# packages ----------------------------------------------------------------
library(tidyverse)
library(sf)

# importar  poligonos para UCs no Brasil
uc <- sf::st_read(here::here("ucsei.shp"), quiet = TRUE)
uc

# importar limites da Mata Atlantica
ma <- sf::st_read(here::here("mata_atlantica", 
                             "inventario_florestal_if_mar2010.shp"), 
                  quiet = TRUE)
ma

# baixar shape Brasil 2020
br_2020 <- geobr::read_country(year = 2020)

# filtrar apenas o estado de Sao Paulo
sp <- br_2020 %>% 
  dplyr::filter(abbrev_state == "SP")
head(sp)

# Check CRS:
st_crs(ma)==st_crs(sp) # diferentes

# Convertendo todo mundo para WGS84
sp <- sf::st_transform(sp, crs = 4989)
ma <- sf::st_transform(ma, crs = 4989)

# Check CRS:
st_crs(ma)==st_crs(sp) # OK

# tentativa de filtro espacial, ERRO
ma_sp <- sf::st_intersects(x = ma, y = sp)

