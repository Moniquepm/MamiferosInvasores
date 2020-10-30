# memory
rm(list = ls())

library(here); library(readr)
library(tidyverse); library(readxl)
library(writexl); library(lubridate)
library(dplyr); library(tmap)
library(ggplot2); library(writexl)
library(sp); library(sf)
library(geobr); library(rnaturalearth)
library(tmap); library(wesanderson)

# open atlantic forest limit

af <- sf::st_read("ma_limite_consensual_muylaert_et_al_2018_wgs84.shp", quiet = TRUE)

# db = ATLANTIC MAMMALS dataset
# db2 = NEOTROPICAL INVASIVE MAMMALS dataset


# importing ATLANTIC MAMMALS dataset in .csv format
db <- readr::read_csv("02_dados//ATLANTIC_MAMMAL_MID_LARGE _assemblages_and_sites.csv")
db

#bd$Latitude <- as.double(bd$Latitude)
#sort(bd$Latitude, dec = TRUE) %>% 
#  head()

# filtering the db object by year (2000 - 2018)
db_yr <- dplyr::filter(db, Year_finish %in% as.character(2000:2018))

# droping NAs from the lat long columns
db_drop_na <- db_yr %>% 
  tidyr::drop_na(Latitude) %>% 
  tidyr::drop_na(Longitude)
db_drop_na

# creating two more columns with the Longitude and Latitude informations
db_drop_na_mu <- db_drop_na %>% 
  dplyr::mutate(lon = Longitude, lat = Latitude, .before = 1)
db_drop_na_mu

# transforming the db_drop_na_mu tibble in a sf object and creating the geometry based on
# the lon/lat columns
sf_db <- db_drop_na_mu %>% 
  sf::st_as_sf(coords = c("lon", "lat"), crs = "+proj=longlat +datum=WGS84 +no_defs")
sf_db

# ploting the points and the atlantic forest limit together
plot(af$geometry, col = "gray", main = NA, axes = TRUE, graticule = TRUE)
plot(sf_db$geometry, pch = 20, col = "forestgreen", main = NA, axes = TRUE, graticule = TRUE, add = TRUE)


# selecting only the points that drop inside the atlantic forest limit
sf_db_inside <- sf_db %>% 
  dplyr::filter(sf::st_intersects(x = af, y = ., sparse = FALSE))
sf_db_inside

# ploting only the points inside the atlantic forest domain
plot(af$geometry, col = "gray", main = NA, axes = TRUE, graticule = TRUE)
plot(sf_db_inside$geometry, pch = 20, col = "forestgreen", main = NA, axes = TRUE, graticule = TRUE, add = TRUE)

# creating a grid over the AF limit and putting an ID in each cell
af_sf_grid <- af %>% 
  sf::st_make_grid(cellsize = 1, square = FALSE, crs = 4326) %>%
  sf::st_as_sf() %>% 
  dplyr::mutate(ID = 1:nrow(.))
af_sf_grid

af_sf_grid_in <- af_sf_grid[af, ]

# ploting the grid and the AF limit
plot(af$geometry, col = "gray", main = NA, axes = TRUE, graticule = TRUE)
plot(af_sf_grid_in, col = adjustcolor("red", .1), add = TRUE)

# 
af_join_db <- sf_db_inside[, "Actual_species_Name"] %>% 
  sf::st_join(x = af_sf_grid_in, y = ., join = st_intersects)
plot(af_join_db)
af_join_db

af_join_db_sp <- af_join_db %>% 
  tidyr::drop_na(Actual_species_Name) %>% 
  dplyr::distinct(ID, Actual_species_Name, .keep_all = TRUE) %>% 
  dplyr::group_by(ID) %>% 
  dplyr::summarise(nsp_db = n())

plot(af_join_db_sp[, "nsp_db"])



#br_2019 <- geobr::read_country(year = 2019, showProgress = FALSE)
#plot(br_2019)

#map_sf_bd_inside <- tm_shape(ma_sf) +
#  tm_fill(col = fill_ma, alpha = .5) +
#  tm_borders(col = "black") +
#  tm_shape(br_2019) +
#  tm_fill(col = "gray", alpha = .0) +
#  tm_borders(col = "black", alpha = .5) +
#  tm_shape(sf_bd_inside) +
#  tm_bubbles(size = .2, col = "forestgreen", alpha = .5, border.col = dots_ma, border.alpha = .5) +
#  tm_grid(lines = FALSE, labels.format = list(big.mark = ""), labels.rot = c(0, 90)) +
#  tm_compass() +
#  tm_scale_bar()
#map_sf_bd_inside

#-------------------------------------------------------------------------------


# importing NEOTROPICAL INVASIVE MAMMALS dataset in .csv format
db2 <- readr::read_delim("02_Dados//NEOTROPICAL_ALIEN_MAMMALS_OCCURENCE_v1_0.csv", ";")
db2

# filtering by year (2000 - 2018)
db2_yr <- dplyr::filter(db2, RECORD_YEAR %in% as.character(2000:2018))

# droping NAs from the LAT_Y LONG_X columns
db2_drop_na <- db2_yr %>% 
  tidyr::drop_na(LONG_X) %>% 
  tidyr::drop_na(LAT_Y)
db2_drop_na

# creating two more columns with the LAT_Y LONG_X informations
db2_drop_na_mu <- db2_drop_na %>% 
  dplyr::mutate(lon = LONG_X, lat = LAT_Y, .before = 1)
db2_drop_na_mu


# transforming the db_drop_na_mu tibble in a sf object and creating the geometry based on
# the lon/lat columns
sf_db2 <- db2_drop_na_mu %>% 
  sf::st_as_sf(coords = c("lon", "lat"), crs = 4326)
sf_db2

# ploting the points and the atlantic forest limit together
plot(af$geometry, col = "gray", main = NA, axes = TRUE, graticule = TRUE)
plot(sf_db2$geometry, pch = 20, col = "forestgreen", main = NA, axes = TRUE, graticule = TRUE, add = TRUE)


# selecting only the points that drop inside the atlantic forest limit
sf_db2_inside <- sf_db2 %>% 
  dplyr::filter(sf::st_intersects(x = af, y = ., sparse = FALSE))
sf_db2_inside

# ploting only the points inside the atlantic forest domain
plot(af$geometry, col = "gray", main = NA, axes = TRUE, graticule = TRUE)
plot(sf_db2_inside$geometry, pch = 20, alpha = .1, col = "forestgreen", main = NA, axes = TRUE, graticule = TRUE, add = TRUE)


# creating a grid over the AF limit and putting an ID in each cell
#af_sf_grid <- af %>% 
#  sf::st_make_grid(cellsize = 1, square = FALSE, crs = 4326) %>%
#  sf::st_as_sf() %>% 
#  dplyr::mutate(ID = 1:nrow(.))
#af_sf_grid

#af_sf_grid_in <- af_sf_grid[af, ]


# ploting the grid and the AF limit
#plot(af$geometry, col = "gray", main = NA, axes = TRUE, graticule = TRUE)
#plot(af_sf_grid_in, col = adjustcolor("red", .1), add = TRUE)

# 
af_join_db2 <- sf_db2_inside[, "SPECIES"] %>% 
  sf::st_join(x = af_join_db_sp, y = ., join = st_intersects)
plot(af_join_db2)
af_join_db2

af_join_db2_sp <- af_join_db2 %>% 
  #tidyr::drop_na(SPECIES) %>% 
  dplyr::distinct(ID, SPECIES, .keep_all = TRUE) %>% 
  dplyr::group_by(ID) %>% 
  dplyr::summarise(nsp_db2 = n())

plot(af_join_db2_sp[, "nsp_db2"])

af_join_db2_sp_tb <- as_tibble(af_join_db2_sp)

db_db2_join <- dplyr::inner_join(af_join_db_sp, af_join_db2_sp_tb, by = "ID")


m <- lm(nsp_db ~ nsp_db2, data = db_db2_join)
summary(m)

i <- ggplot(data = db_db2_join) +
  aes(x = nsp_db2, y = nsp_db) +
  geom_point() +
  geom_smooth(cor.method = "pearson", col = "red", shape = 2) +
  theme_bw() +
  labs(x = "Richness of alien mammals", y = "Richness of native mammals")
i

library("ggpubr")

i2 <- ggscatter(db_db2_join, x = "nsp_db2", y = "nsp_db", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = FALSE, cor.method = "pearson", size = 10, alpha = .5, color = "forestgreen",
          xlab = "Richness of alien mammals", ylab = "Richness of native mammals") +
  theme(axis.text=element_text(size=32,colour="black"),
        axis.title=element_text(size=38),
        axis.ticks=element_line(size=0.5, colour="black"),
        axis.title.y=element_text(vjust=0.5))
i2

png("correlation.png", width=1000, height=800)
i2
dev.off()

#---------------------------------------------------------------------------

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
dots_ma <- wes_palettes$Cavalcanti1[1]
fil_db <- wes_palettes$Cavalcanti1[2]
fil_db2 <- wes_palettes$Rushmore1[4]

fill_nsp <- wes_palette("FantasticFox1", 5, type = "continuous")


map_nsp_db <- tm_shape(af) +
  tm_fill(col = dots_ma, alpha = .6) +
  tm_borders(col = "black") +
  tm_shape(af_join_db_sp) +
  tm_fill(col = "nsp_db", palette = "-viridis", title = "Species richness") +
  tm_borders(col = "black", alpha = .5) +
  #tm_grid(lines = FALSE, labels.format = list(big.mark = ""), labels.rot = c(0, 90)) +
  #tm_compass() +
  #tm_scale_bar() +
  tm_layout(legend.show = FALSE, frame = FALSE, legend.position = c("left", "top"), legend.title.size = 1)
map_nsp_db


map_nsp_db2 <- tm_shape(af) +
  tm_fill(col = dots_ma, alpha = .6) +
  tm_borders(col = "black") +
  tm_shape(af_join_db2_sp) +
  tm_fill(col = "nsp_db2", palette = "plasma", title = "Species richness") +
  tm_borders(col = "black", alpha = .5) +
  #tm_grid(lines = FALSE, labels.format = list(big.mark = ""), labels.rot = c(0, 90)) +
  #tm_compass() +
  #tm_scale_bar() +
  tm_layout(legend.show = FALSE, frame = FALSE, legend.position = c("left", "top"), legend.title.size = 1)
map_nsp_db2


map_grid <- tm_shape(af) +
  tm_fill(col = dots_ma, alpha = .6) +
  tm_borders(col = "black") +
  tm_shape(af_sf_grid_in) +
  tm_fill(col = "black", alpha = .2) +
  tm_borders(col = "black") +
  tm_layout(frame = FALSE)
map_grid

map_db <- tm_shape(af) +
  tm_fill(col = dots_ma, alpha = .6) +
  tm_borders(col = "black") +
  tm_shape(sf_db_inside) +
  tm_bubbles(size = .2, col = "forestgreen", alpha = .5, border.col = fil_db, border.alpha = .5) +
  tm_layout(frame = FALSE)
map_db

map_db2 <- tm_shape(af) +
  tm_fill(col = dots_ma, alpha = .6) +
  tm_borders(col = "black") +
  tm_shape(sf_db2_inside) +
  tm_bubbles(size = .2, col = "purple", alpha = .5, border.col = fil_db2, border.alpha = .5) +
  tm_layout(frame = FALSE)
map_db2


#map_sf_bd_inside <- tm_shape(ma_sf) +
#  tm_fill(col = fill_ma, alpha = .5) +
#  tm_borders(col = "black") +
#  tm_shape(br_2019) +
#  tm_fill(col = "gray", alpha = .0) +
#  tm_borders(col = "black", alpha = .5) +
#  tm_shape(sf_bd_inside) +
#  tm_bubbles(size = .2, col = "forestgreen", alpha = .5, border.col = dots_ma, border.alpha = .5) +
#  tm_grid(lines = FALSE, labels.format = list(big.mark = ""), labels.rot = c(0, 90)) +
#  tm_compass() +
#  tm_scale_bar()
#--------------------------------------------------------------------------------------

#ma_sf_albers <- sf::st_transform(ma_sf_new, crs = 102033)

#grid_spacing <- 0.045

#ma_sf_grid <- ma_sf_new %>% 
#  sf::st_make_grid(cellsize = 1, square = FALSE, crs = 4326) %>%
#  sf::st_as_sf() %>% 
#  dplyr::mutate(ID = 1:nrow(.))
#ma_sf_grid

#plot(ma_sf_new$geometry, col = "gray", main = NA, axes = TRUE, graticule = TRUE)
#plot(ma_sf_grid, col = adjustcolor("red", .0), add = TRUE)

#ma_sf_grid_in <- ma_sf_grid[ma_sf_new, ]

#plot(ma_sf_new$geometry, col = "gray", main = NA, axes = TRUE, graticule = TRUE)
#plot(ma_sf_grid_in, col = adjustcolor("red", .1), add = TRUE)

#ma_sf_bd_count <- ma_sf_grid_in %>% 
#  dplyr::mutate(n = sf::st_intersects(x = ., sf_bd_inside) %>% lengths())
#ma_sf_bd_count

#plot(ma_sf_bd_count["n"], main = NA, axes = TRUE, graticule = TRUE)



ma_join_bd <- sf_bd_inside[, "Actual_species_Name"] %>% 
  sf::st_join(x = ma_sf_grid_in, y = ., join = st_intersects)
plot(ma_join_bd)
ma_join_bd

ma_join_bd_sp <- ma_join_bd %>% 
  tidyr::drop_na(Actual_species_Name) %>% 
  dplyr::distinct(ID, Actual_species_Name, .keep_all = TRUE) %>% 
  dplyr::group_by(ID) %>% 
  dplyr::summarise(nsp = n())

plot(ma_join_bd_sp[, "nsp"])
