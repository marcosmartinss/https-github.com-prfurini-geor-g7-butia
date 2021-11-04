
# Pacotes ----------------------------------------------------------------------
library(tidyverse)
library(here)
library(sp)
library(sf)
library(spatialEco)
library(plotly)
library(raster)
library(spatstat)

# Carregando de ocorrencia das spp de aves -------------------------------------
atlantic.birds <- read.csv(here::here("ATLANTIC_BIRDS_quantitative.csv"))

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

# Explorando a distribuicao dos pontos aleatorios ------------------------------
# criando objeto da classe "owin"
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

# df com dados de riqueza para cada uma das 150 amostras -----------------------

# convertendo o objeto sp para sf
atlantic.birds.sp.sample <- st_as_sf(atlantic.birds.sp.sample)
head(atlantic.birds.sp.sample)

# criando df com dados de interesse
df.riq <- as.data.frame(atlantic.birds.sp.sample)
df.riq <- df.riq[, c(1, 25)]
head(df.riq, 20)

# Dist pontos amostra aleatoria a UC mais proxima ------------------------------
# Camada de UCs
uc <- sf::st_read(here::here("UCs.shp"), quiet = TRUE)
uc

# converter sistema de coordenadas dos dados das ucs para SIRGAS 2000
uc <- sf::st_transform(uc, crs = 5880)
uc

# calcular distancia das UCS
uc_cast <- uc %>% 
  dplyr::st_cast("POLYGON")
uc_cast

ab_sample_dist_uc <- atlantic.birds.sp.sample %>% 
  dplyr::mutate(dist_uc = sf::st_distance(atlantic.birds.sp.sample, uc_cast))
dim(ab_sample_dist_uc$dist_uc)

# % florestal (IF_2020) em cada ponto da amostra aleatoria  --------------------

# Carregando shapefile como sf
IFlor <- sf::st_read(here::here("InventarioFlorestal2020.shp"), quiet = TRUE, options = "ENCODING=WINDOWS-1252")
IFlor

# Removendo classes de cobertura não florestais
unique(IFlor$FITOFISION)

IFlor_filt <- IFlor %>% 
  dplyr::filter(FITOFISION %in% c("Floresta Ombrófila Densa", "Floresta Estacional Semidecidual",
                                  "Floresta Ombrófila Mista", "Savana Florestada", 
                                  "Floresta Ombrófila Densa das Terras Baixas", "Floresta Estacional Decidual"))
IFlor_filt

# Reprojetando 
IFlor_filt <- sf::st_transform(IFlor_filt, crs = 5880)

# Add id para floresta
IFlor_filt <- IFlor_filt %>% 
  dplyr::mutate(id_floresta = rep(1, length(IFlor_filt$FITOFISION)))
IFlor_filt

# Rasterizar os poligonos 
IFlor_rast <- fasterize::fasterize(sf = IFlor_filt, raster = , field = "1")
IFlor_rast

# plot
# plot(IFlor_rast, col = viridis::viridis(10))
# plot(atlantic.birds.sp.sample$geom, add = TRUE)

# Quantificar % floresta em buffers de 3 km ao redor das amostras


# Plot area de estudo, dados de ocorrencia -------------------------------------

plot(sp$geom, pch = 20, main = NA, axes = TRUE, graticule = TRUE) # area de estudo
plot(uc$geometry, col = "green", main = NA, axes = TRUE, graticule = TRUE, add= TRUE)# UCs
plot(atlantic.birds.sp$geometry, pch = 20, main = NA, axes = TRUE, graticule = TRUE, add= TRUE) # pontos com ocorrencia das spp
plot(atlantic.birds.sp.sample$geometry, pch = 20, main = NA, axes = TRUE, graticule = TRUE, add= TRUE, col= "red") # amostra aleatoria

# plot interativo (zoom)
map_rc_2020_plotly_int <- ggplotly(
  ggplot() +
    geom_sf(data = sp) +
    geom_sf(data = atlantic.birds.sp) +
    geom_sf(data = atlantic.birds.sp.sample, col = "red") +
    theme_bw(base_size = 16))
map_rc_2020_plotly_int

# Input GLM --------------------------------------------------------------------

# df final 
df.riq

# GLM
(mod_null <- glm(riqueza ~ 1, family = "poisson", data = df.riq))

(mod_dist_ucs <- glm(riqueza ~ Dist_UCs, family = "poisson", data = df.riq))

(mod_dist_fl <- glm(riqueza ~ Porc_flor, family = "poisson", data = df.riq))

(mod_dist_int <- glm(riqueza ~ Dist_UCs*Porc_flor, family = "poisson", data = df.riq))
