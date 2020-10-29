# memory
rm(list = ls())

library(here)
library(readr)
library(tidyverse)
library(here)
library(readxl)
library(writexl)
library(lubridate)
library(dplyr)
library(sp)
library(sf)
library(geobr)
library(rnaturalearth)
library(tmap)
library(wesanderson)


bd <- readr::read_csv("02_dados//ATLANTIC_MAMMAL_MID_LARGE _assemblages_and_sites.csv")
spec(bd)

#bd$Latitude <- as.double(bd$Latitude)
#sort(bd$Latitude, dec = TRUE) %>% 
#  head()

bd_yr <- dplyr::filter(bd, Year_finish %in% as.character(2000:2018))

bd_drop_na <- bd_yr %>% 
  tidyr::drop_na(Latitude) %>% 
  tidyr::drop_na(Longitude)
bd_drop_na

bd_drop_na_mu <- bd_drop_na %>% 
  dplyr::mutate(lon = Longitude, lat = Latitude, .before = 1)
bd_drop_na_mu

sf_bd <- bd_drop_na_mu %>% 
  sf::st_as_sf(coords = c("lon", "lat"), crs = "+proj=longlat +datum=WGS84 +no_defs")
sf_bd



ma_sf <- sf::st_read(here::here("ma_limite_consensual_muylaert_et_al_2018_wgs84.shp"), quiet = TRUE)

ma_sf_new <- sf::st_transform(ma_sf, crs = "+proj=longlat +datum=WGS84 +no_defs")
ma_sf_new

plot(ma_sf$geometry, col = "gray", main = NA, axes = TRUE, graticule = TRUE)
plot(sf_bd$geometry, pch = 20, col = "forestgreen", main = NA, axes = TRUE, graticule = TRUE, add = TRUE)


sf_bd_inside <- sf_bd %>% 
  dplyr::filter(sf::st_intersects(x = ma_sf, y = ., sparse = FALSE))
sf_bd_inside

plot(ma_sf$geometry, col = "gray", main = NA, axes = TRUE, graticule = TRUE)
plot(sf_bd_inside$geometry, pch = 20, col = "forestgreen", main = NA, axes = TRUE, graticule = TRUE, add = TRUE)

wes_palettes <- list(
  BottleRocket1 = c("#A42820", "#5F5647", "#9B110E", "#3F5151", "#4E2A1E", "#550307", "#0C1707"),
  BottleRocket2 = c("#FAD510", "#CB2314", "#273046", "#354823", "#1E1E1E"),
  Rushmore1 = c("#E1BD6D", "#EABE94", "#0B775E", "#35274A" ,"#F2300F"),
  Rushmore = c("#E1BD6D", "#EABE94", "#0B775E", "#35274A" ,"#F2300F"),
  Royal1 = c("#899DA4", "#C93312", "#FAEFD1", "#DC863B"),
  Royal2 = c("#9A8822", "#F5CDB4", "#F8AFA8", "#FDDDA0", "#74A089"),
  Zissou1 = c("#3B9AB2", "#78B7C5", "#EBCC2A", "#E1AF00", "#F21A00"),
  Darjeeling1 = c("#FF0000", "#00A08A", "#F2AD00", "#F98400", "#5BBCD6"),
  Darjeeling2 = c("#ECCBAE", "#046C9A", "#D69C4E", "#ABDDDE", "#000000"),
  Chevalier1 = c("#446455", "#FDD262", "#D3DDDC", "#C7B19C"),
  FantasticFox1 = c("#DD8D29", "#E2D200", "#46ACC8", "#E58601", "#B40F20"),
  Moonrise1 = c("#F3DF6C", "#CEAB07", "#D5D5D3", "#24281A"),
  Moonrise2 = c("#798E87", "#C27D38", "#CCC591", "#29211F"),
  Moonrise3 = c("#85D4E3", "#F4B5BD", "#9C964A", "#CDC08C", "#FAD77B"),
  Cavalcanti1 = c("#D8B70A", "#02401B", "#A2A475", "#81A88D", "#972D15"),
  GrandBudapest1 = c("#F1BB7B", "#FD6467", "#5B1A18", "#D67236"),
  GrandBudapest2 = c("#E6A0C4", "#C6CDF7", "#D8A499", "#7294D4"),
  IsleofDogs1 = c("#9986A5", "#79402E", "#CCBA72", "#0F0D0E", "#D9D0D3", "#8D8680"),
  IsleofDogs2 = c("#EAD3BF", "#AA9486", "#B6854D", "#39312F", "#1C1718")
)

fill_ma <- wes_palettes$Moonrise1[1]
dots_ma <- wes_palettes$Cavalcanti1[2]

br_2019 <- geobr::read_country(year = 2019, showProgress = FALSE)
plot(br_2019)

map_sf_bd_inside <- tm_shape(ma_sf) +
  tm_fill(col = fill_ma, alpha = .5) +
  tm_borders(col = "black") +
  tm_shape(br_2019) +
  tm_fill(col = "gray", alpha = .0) +
  tm_borders(col = "black", alpha = .5) +
  tm_shape(sf_bd_inside) +
  tm_bubbles(size = .2, col = "forestgreen", alpha = .5, border.col = dots_ma, border.alpha = .5) +
  tm_grid(lines = FALSE, labels.format = list(big.mark = ""), labels.rot = c(0, 90)) +
  tm_compass() +
  tm_scale_bar()
map_sf_bd_inside

#-------------------------------------------------------------------------------

bd_iv <- readr::read_delim("NEOTROPICAL_ALIEN_MAMMALS_OCCURENCE_v1_0.csv", ";")
bd_iv

bd_iv_yr <- dplyr::filter(bd_iv, RECORD_YEAR %in% as.character(2000:2018))


bd_iv_drop_na <- bd_iv_yr %>% 
  tidyr::drop_na(LONG_X) %>% 
  tidyr::drop_na(LAT_Y)
bd_iv_drop_na


bd_iv_mu <- bd_iv_drop_na %>% 
  dplyr::mutate(lon = LONG_X, lat = LAT_Y, .before = 1)
bd_iv_mu

sf_bd_iv <- bd_iv_mu %>% 
  sf::st_as_sf(coords = c("lon", "lat"), crs = 4326)
sf_bd_iv

plot(ma_sf$geometry, col = "gray", main = NA, axes = TRUE, graticule = TRUE)
plot(sf_bd_iv$geometry, pch = 20, col = "forestgreen", main = NA, axes = TRUE, graticule = TRUE, add = TRUE)


sf_bd_iv_inside <- sf_bd_iv %>% 
  dplyr::filter(sf::st_intersects(x = ma_sf, y = ., sparse = FALSE))
sf_bd_iv_inside

plot(ma_sf$geometry, col = "gray", main = NA, axes = TRUE, graticule = TRUE)
plot(sf_bd_iv_inside$geometry, pch = 20, alpha = .1, col = "forestgreen", main = NA, axes = TRUE, graticule = TRUE, add = TRUE)

map_sf_bd_iv_inside <- tm_shape(ma_sf) +
  tm_fill(col = fill_ma, alpha = .5) +
  tm_borders(col = "black") +
  tm_shape(br_2019) +
  tm_fill(col = "gray", alpha = .0) +
  tm_borders(col = "black", alpha = .5) +
  tm_shape(sf_bd_iv_inside) +
  tm_bubbles(size = .2, col = "forestgreen", alpha = .5, border.col = dots_ma, border.alpha = .5) +
  tm_grid(lines = FALSE, labels.format = list(big.mark = ""), labels.rot = c(0, 90)) +
  tm_compass() +
  tm_scale_bar()
map_sf_bd_iv_inside

#--------------------------------------------------------------------------------------

geobr_ma <- geobr::read_biomes(year = 2019, showProgress = FALSE)
plot(geobr_ma)

geo_ma_so <- geobr_ma$code_biome
plot(geo_ma_so)

ma_sf_new <- sf::st_as_sf(ma_sf, crs = 102033)
sf::st_crs(ma_sf_new)
ma_sf_new

ma_sf_albers <- sf::st_transform(ma_sf_new, crs = 102033)

grid_spacing <- 0.045

ma_sf_grid <- ma_sf_new %>% 
  sf::st_make_grid(cellsize = 1, square = FALSE, crs = 4326) %>%
  sf::st_as_sf() %>% 
  dplyr::mutate(ID = 1:nrow(.))
ma_sf_grid

plot(ma_sf_new$geometry, col = "gray", main = NA, axes = TRUE, graticule = TRUE)
plot(ma_sf_grid, col = adjustcolor("red", .0), add = TRUE)

ma_sf_grid_in <- ma_sf_grid[ma_sf_new, ]

plot(ma_sf_new$geometry, col = "gray", main = NA, axes = TRUE, graticule = TRUE)
plot(ma_sf_grid_in, col = adjustcolor("red", .1), add = TRUE)

ma_sf_bd_count <- ma_sf_grid_in %>% 
  dplyr::mutate(n = sf::st_intersects(x = ., sf_bd_inside) %>% lengths())
ma_sf_bd_count

plot(ma_sf_bd_count["n"], main = NA, axes = TRUE, graticule = TRUE)



ma_join_bd <- sf_bd_inside[, "Actual_species_Name"] %>% 
  sf::st_join(x = ma_sf_grid_in, y = ., join = st_intersects) 
  #tidyr::drop_na(Actual_species_Name)
plot(ma_join_bd)
ma_join_bd

ma_join_bd_sp <- ma_join_bd %>% 
  tidyr::drop_na(Actual_species_Name) %>% 
  dplyr::distinct(ID, Actual_species_Name, .keep_all = TRUE) %>% 
  dplyr::group_by(ID) %>% 
  dplyr::summarise(nsp = n())

plot(ma_join_bd_sp[, "nsp"])
