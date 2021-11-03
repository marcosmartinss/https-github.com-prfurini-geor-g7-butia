library(sf)

uc <- sf::st_read(here::here("ucsei.shp"), quiet = TRUE)
uc

plot(uc [10], col = c("blue", "orange", "gray30", "forestgreen", "green"), main = NA, axes = TRUE, graticule = TRUE)

sf::st_crs(uc)

dir.create(here::here("vetor"))

for(i in c(".dbf", ".prj", ".shp", ".shx")){
  download.file(url = paste0("http://datageo.ambiente.sp.gov.br/geoserver/ows?service=WMS&version=1.1.0&request=GetMap&layers=datageowms:UCS_ESTADUAIS_SP_POL&styles=&bbox=-51.81640316514246,-24.43011526988209,-44.32236139998661,-20.980665679200712&width=287&height=245&tiled=true&srs=EPSG:4326&format=image/png&transparent=true", i),
                destfile = here::here("vetor", paste0("ms_tmp_ucsfi", i), mode = "wb")
  
galia_uso <- sf::st_read(here::here("Galia_USO", "USO", "SP_3516606_USO.shp"), quiet = TRUE)
galia_uso

plot(galia_uso [5], col = c("blue", "orange", "gray30", "forestgreen", "green"), main = NA, axes = TRUE, graticule = TRUE)

br_2020 <- geobr::read_country(year = 2020, showProgress = FALSE)
br_2020

plot(br_2020$geom, col = "gray", main = NA, axes = TRUE, graticule = TRUE)

git add -Av

UCS_FED_kml <- sf::st_read(here::here("C:/Users/profm/Documents/Doutorado Geografia PUC Minas/Disciplinas 2021_02/Geospacial com R/GitHub/projeto_final/https-github.com-prfurini-geor-g7-butia", "datageowms-UC_Federais_MMA.kml"), quiet = TRUE)
UCS_FED_kml
                
