
# Pacotes ----------------------------------------------------------------
library(tidyverse)
library(here)
library(sp)
library(sf)
library(spatialEco)
library(plotly)

# Dados de ocorrencia das spp de ave --------------------------------------

# atlantic.birds <- readr::read_csv(
#   here::here("ATLANTIC_BIRDS_quantitative.csv"))
atlantic.birds <- read.csv("ATLANTIC_BIRDS_quantitative.csv")

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


# Explorando a distribuicao dos pontos aleatorios ----
# criando objeto da classe "owin"
library(spatstat)
sp_win <- as.owin(sp)
plot(sp_win)

# construindo objeto da classe "ppp"
atlantic.birds.sp.sample.ppp <- ppp(
  atlantic.birds.sp.sample@coords[,1], 
  atlantic.birds.sp.sample@coords[,2], 
  window = sp_win)

class(atlantic.birds.sp.sample.ppp) # ppp

# Plot
plot(atlantic.birds.sp.sample.ppp)

# plotando kernel inicial, com raio de busca arbitrario
kernel <- density.ppp(atlantic.birds.sp.sample.ppp, 
                      sigma = 15000, 
                      edge = FALSE) # sigma arbitrario (subjetivo)
plot(kernel)

# criando kernel utilizando abordagens estatisticas
# usei bandwidth de diggle (1989) por nao suavisar demais a densidade
raio_diggle <- bw.diggle(atlantic.birds.sp.sample.ppp)

kernel_diggle <- density.ppp(atlantic.birds.sp.sample.ppp, 
                             sigma = raio_diggle)
#plot
plot(kernel_diggle, main = "raio baseado em Diggle 1989")

# convertendo kernel para raster 
library(raster)
kernel_diggle <- raster(kernel_diggle)
class(kernel_diggle)

# atribuindo o mesmo crs dos nossos pontos para o raster
crs(kernel_diggle) <- crs(atlantic.birds.sp.sample)

# Plot mais interessante
library(tmap)
tmap_mode("plot")
tm_shape(kernel_diggle) +
  tm_raster(title = '', style = "fisher") +
  tm_shape(sp) +
  tm_borders() +
  tm_legend(legend.outside = T) +
  tm_graticules(lwd = 0) + 
  tm_layout(main.title = "Pontos de amostragem Atlantic Birds SP")
#tmap_save(filename = "./img/kernel.png")

# Teste de padroes de distribuicao ----
# qual o padrao de distribuicao dos pontos de amostragem em sp?
# ja que aparentemente nossos pontos tem distribuicao nao homogenea:
# L-function for IPP (distribuicao nao-homogenea dos pontos)
# ?Linhom
L_inhom <- envelope(atlantic.birds.sp.sample.ppp, 
                    Linhom, 
                    nsim = 50) # quanto maior mais demora +- 40 seg com 50
                     
L_inhom # com esse tamanho de ppp, 50 simulacoes dao significancia de .04

# plot: 
# valores positivos indicam padrao nao homogeneo (agrupacao)
# valores negativos indicam padrao homogeneo de distribuicao
# sombra cinza indica intervalo de confianca (.04)
plot(L_inhom, . -r ~ r, legend = FALSE) 

## entre ~10 km e  ~70 km de distancia os pontos apresentam distribuicao 
## nao homogenea (agregada)

## se avaliamos distancias maiores que ~100 km,  os pontos tem distribuicao
## homogenea

# plot para visualizarmos os pontos -> + pontos em algumas areas
plot(atlantic.birds.sp.sample.ppp)

# Plot area de estudo, dados de ocorrencia ------------------------------
# convertendo o objeto sp para sf
atlantic.birds.sp.sample <- st_as_sf(atlantic.birds.sp.sample)
head(atlantic.birds.sp.sample)

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


mapview::mapview(atlantic.birds.sp.sample)

# Riqueza de spp de aves pontos da amostra aleatoria ---------------------
atlantic.birds

atlantic.birds_group_lat <- atlantic.birds %>% 
  dplyr::group_by(Latitude_y, Longitude_x) %>% 
  dplyr::summarise(riqueza = n())
print(atlantic.birds_group_lat, n= 230)

# fazer o join


# % florestal (IF_2020) em cada ponto da amostra aleatoria  --------------

# Dist pontos amostra aleatoria a UC mais proxima ------------------------

# Input GLM --------------------------------------------------------------