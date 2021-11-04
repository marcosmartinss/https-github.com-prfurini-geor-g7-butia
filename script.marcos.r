library(tidyverse)
library(sp)
library(sf)
#------------------------------------------------------------------------------------------------------------------------
  
# Camada de UCs Estaduais de Protecao Integral
uc <- sf::st_read(here::here("ucsei.shp"), quiet = TRUE)
uc

plot(uc [10], col = c("blue", "orange", "gray30", "forestgreen", "green"), main = NA, axes = TRUE, graticule = TRUE)

sf::st_crs(uc)

# Criando um diretorio
dir.create(here::here("vetor"))

# Mapa de Galia_SP
galia_uso <- sf::st_read(here::here("Galia_USO", "USO", "SP_3516606_USO.shp"), quiet = TRUE)
galia_uso

plot(galia_uso [5], col = c("blue", "orange", "gray30", "forestgreen", "green"), main = NA, axes = TRUE, graticule = TRUE)

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

for(i in c(".dbf", ".prj", ".shp", ".shx")){
  download.file(url = paste0("https://geo.fbds.org.br/SP/AGUAI/USO/", i),
                destfile = here::here("C:/Users/profm/Documents/Doutorado Geografia PUC Minas/Disciplinas 2021_02/Geospacial com R/GitHub/projeto_final/https-github.com-prfurini-geor-g7-butia", paste0("SP_3500303_USO", i)), mode = "wb")
}
