library(here); library(readr)
library(tidyverse); library(readxl)
library(writexl); library(lubridate)
library(dplyr); library(tmap)
library(ggplot2); library(writexl)
library(sp); library(sf)
library(geobr); library(rnaturalearth)
library(tmap); library(wesanderson)

#Abrir limtes
af <-  sf::st_read("03_dados_espaciais","ma_limite_consensual_muylaert_et_al_2018_wgs84")

#db = banco de dados dos Mamiferos de medio e grande porte
#db2 = banco de dados Mammals Alien

#---------
#Mammal Med Large
#Abir planilha
bd <- readr::read_csv("02_Dados/ATLANTIC_MAMMAL_MID_LARGE _assemblages_and_sites.csv")
bd 

#Filtro data
bd <- dplyr::filter(bd, Year_finish %in% as.character(2000:2018)) 
bd

#Tirar NA
bd <- bd %>% 
  tidyr::drop_na(Latitude) %>% 
  tidyr::drop_na(Longitude)
bd

# Vector
bd_ve <- bd %>% 
  dplyr::mutate(lon = Longitude, lat = Latitude,  .before = 1) %>% 
  sf::st_as_sf(coords = c("lon", "lat"), crs = 4326)
bd_ve

plot(bd_ve$geometry, pch = 20)


#Apenas pontos dentro da AF -Selecionar feições
bd_I <- bd_ve %>% 
  dplyr::filter(sf::st_intersects(x = af, y = ., sparse = FALSE))
bd_I

#Grid = bd
af_grid <- af %>% 
  sf::st_make_grid(cellsize = 1, square = FALSE, crs = 4326) %>%
  sf::st_as_sf() %>% 
  dplyr::mutate(ID = 1:nrow(.))
af_grid

plot(af$geometry, col = "gray", main = NA, axes = TRUE, graticule = TRUE)
plot(af_grid, col = adjustcolor("red", .0), add = TRUE)

af_grid_in <-af_grid[af, ] #Quadriculas so em AF

plot(af$geometry, col = "gray", main = NA, axes = TRUE, graticule = TRUE)
plot(af_grid_in, col = adjustcolor("red", .1), add = TRUE)

af_grid_count  <- af_grid_in %>% 
  dplyr::mutate(n = sf::st_intersects(x = ., bd_I) %>% lengths())
af_grid_count

plot(af_grid_count["n"], main = NA, axes = TRUE, graticule = TRUE) #Hexagono colorido af e bd

#Visualizar ID e bd
af_bd_hex_cont <- af_grid %>% 
  dplyr::mutate(n = sf::st_intersects(x = ., bd_I) %>% lengths())
plot(af_bd_hex_cont)

#Associar as especies com Hexagonos
af_join_bd <- bd_I[,"Actual_species_Name"] %>% 
  sf::st_join(x=af_grid_in, y = ., join = st_intersects) 
plot(af_join_bd)

af_join_bd %>%   sf::st_drop_geometry() %>% 
  dplyr::distinct(ID, Actual_species_Name, .keep_all =TRUE)  %>% 
  dplyr::group_by(ID)
plot(af_join_bd)

af_join_bd_sp <- af_join_bd %>% 
  tidyr::drop_na(Actual_species_Name) %>% 
  dplyr::distinct(ID, Actual_species_Name, .keep_all = TRUE) %>% 
  dplyr::group_by(ID) %>% 
  dplyr::summarise(nsp = n())

#af_join_bd_sp
plot(af_join_bd_sp[, "nsp"])



#---------
# Alien Mammals
#Abir planilha
bd2 <- readr::read_delim("02_Dados/NEOTROPICAL_ALIEN_MAMMALS_OCCURENCE_v1_0.csv", ";")
bd2

#Filtro data
bd2 <- dplyr::filter(bd2, RECORD_YEAR %in% as.character(2000:2018)) 
bd2

#Tirar NA
bd2 <- bd2 %>% 
  tidyr::drop_na(LAT_Y) %>% 
  tidyr::drop_na(LONG_X)
bd2

# Vector
bd2_ve <- bd2 %>% 
  dplyr::mutate(lon = LONG_X, lat = LAT_Y,  .before = 1) %>% 
  sf::st_as_sf(coords = c("lon", "lat"), crs = 4326)
bd2_ve

plot(bd2_ve$geometry, pch = 20)


#Apenas pontos dentro da AF -Selecionar feições
bd2_I <- bd2_ve %>% 
  dplyr::filter(sf::st_intersects(x = af, y = ., sparse = FALSE))
bd2_I

#Grid = bd2
af_grid <- af %>% 
  sf::st_make_grid(cellsize = 1, square = FALSE, crs = 4326) %>%
  sf::st_as_sf() %>% 
  dplyr::mutate(ID = 1:nrow(.))
af_grid

plot(af$geometry, col = "gray", main = NA, axes = TRUE, graticule = TRUE)
plot(af_grid, col = adjustcolor("red", .0), add = TRUE)

af_grid_in <-af_grid[af, ] #Quadriculas so em AF

plot(af$geometry, col = "gray", main = NA, axes = TRUE, graticule = TRUE)
plot(af_grid_in, col = adjustcolor("red", .1), add = TRUE)

af_grid_count  <- af_grid_in %>% 
  dplyr::mutate(n = sf::st_intersects(x = ., bd2_I) %>% lengths())
af_grid_count

plot(af_grid_count["n"], main = NA, axes = TRUE, graticule = TRUE) #Hexagono colorido af e bd2

#Visualizar ID e bd2
af_bd2_hex_cont <- af_grid %>% 
  dplyr::mutate(n = sf::st_intersects(x = ., bd2_I) %>% lengths())
plot(af_bd2_hex_cont)

#Associar as especies com Hexagonos
af_join_bd2 <- bd2_I[,"SPECIES"] %>% 
  sf::st_join(x=af_grid_in, y = ., join = st_intersects) 
plot(af_join_bd2)

af_join_bd2 %>%   sf::st_drop_geometry() %>% 
  dplyr::distinct(ID, SPECIES, .keep_all =TRUE)  %>% 
  dplyr::group_by(ID)
plot(af_join_bd2)

af_join_bd2_sp <- af_join_bd2 %>% 
  tidyr::drop_na(SPECIES) %>% 
  dplyr::distinct(ID, SPECIES, .keep_all = TRUE) %>% 
  dplyr::group_by(ID) %>% 
  dplyr::summarise(nsp = n())

#af_join_bd2_sp
plot(af_join_bd2_sp[, "nsp"])


 #fazer um summer das especies para saber qtos


#nativo X exotico no y
# plot
ggplot(data = a) +
  aes(x = af_join_bd_sp, y = af_join_bd2_sp) +
  geom_point() +
  geom_smooth(method = "loess", col = "red", shape = 2) +
  theme_bw() +
  labs(x = "Temperatura (º C)", y = "Tamanho da asa (cm)")


#Mapa para apresentação DADOS USADOS
fill_ma <- wes_palettes$Moonrise1[1]
dots_ma <- wes_palettes$Cavalcanti1[2]

br_2019 <- geobr::read_country(year = 2019, showProgress = FALSE)
#plot(br_2019)
 
map_bd <- tm_shape(af) +
  tm_fill(col = fill_ma, alpha = .5) +
  tm_borders(col = "black") +
  tm_shape(br_2019) +
  tm_fill(col = "gray", alpha = .0) +
  tm_borders(col = "black", alpha = .5) +
  tm_shape(bd_I) +
  tm_bubbles(size = .1, col = "forestgreen", alpha = .5, border.col = dots_ma, border.alpha = .5) +
  tm_grid(lines = FALSE, labels.format = list(big.mark = ""), labels.rot = c(0, 90)) +
  tm_compass() +
  tm_scale_bar()+
  tm_layout(legend.position = c("left", "bottom"), 
          legend.outside = TRUE,
          main.title = "Medium-and-large Mammals")

map_bd2 <- tm_shape(af) +
  tm_fill(col = fill_ma, alpha = .5) +
  tm_borders(col = "black") +
  tm_shape(br_2019) +
  tm_fill(col = "gray", alpha = .0) +
  tm_borders(col = "black", alpha = .5) +
  tm_shape(bd2_I) +
  tm_bubbles(size = .1, col = "blue", alpha = .5, border.col = dots_ma, border.alpha = .5) +
  tm_grid(lines = FALSE, labels.format = list(big.mark = ""), labels.rot = c(0, 90)) +
  tm_compass() +
  tm_scale_bar()+
  tm_layout(legend.position = c("left", "bottom"), 
            legend.outside = TRUE,
            main.title = "Alien Mammals")

#exportar mapa DADOS USADOS
tmap_save(map_bd, filename = "map_mammals_midlarge.png",
          width = 20, 
          height = 20, 
          units = "cm", 
          dpi = 300)

tmap_save(map_bd2, filename = "map_mammals_alien.png",
          width = 20, 
          height = 20, 
          units = "cm", 
          dpi = 300)


