#' ---
#' title: Preparando shapefiles cobertura florestal
#' author: Felipe Bufalo
#' date: 2021-11-03
#' ---

# packages ----------------------------------------------------------------
library(tidyverse)
library(sf)

# baixar shape Brasil 2020
br_2020 <- geobr::read_country(year = 2020)

# filtrar apenas o estado de Sao Paulo
sp <- br_2020 %>% 
  dplyr::filter(abbrev_state == "SP")
head(sp)

# importar limites da Mata Atlantica
#setwd("/Volumes/GoogleDrive/My Drive/Unesp/Doutorado/disciplinas | cursos/Introdução ao uso de dados geoespaciais no R/shapefiles")
ma <- sf::st_read("inventario_florestal_if_mar2010.shp")
ma
#setwd("/Users/febufalo/Documents/Github/Grupo_butia/https-github.com-prfurini-geor-g7-butia")

# Check CRS:
st_crs(ma)==st_crs(sp) # diferentes

# Convertendo todo mundo para SIRGAS2000
sp <- sf::st_transform(sp, crs = 4989)
ma <- sf::st_transform(ma, crs = 4989)

# Check CRS:
st_crs(ma)==st_crs(sp) # OK

# Importar poligonos de UCs para o Brasil
uc <- sf::st_read(here::here("ucsei.shp"), quiet = TRUE)
st_crs(uc) = 4989
uc

# Check CRS:
st_crs(uc)==st_crs(sp) # OK

# tentativa de filtro espacial ucs e sp. Tentei colocar os limites de sp, 
# mas erro.
uc_sp <- st_crop(uc, c(xmin=-53.10986, xmax=-4.16137, 
                       ymin=-25.35794, ymax=-19.77989))

# tentativa de filtro espacial ma e sp, erro tambem
ma_sp <- sf::st_intersects(x = ma, y = sp)

# plot
pdf() # assim eu consigo plotar no mac
plot(uc [10], col = "darkgreen", main = NA, axes = TRUE, graticule = TRUE)
dev.off()
