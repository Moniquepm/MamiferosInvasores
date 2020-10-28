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

bd <- readr::read_csv("02_Dados/ATLANTIC_MAMMAL_MID_LARGE _assemblages_and_sites.csv")
spec(bd)

bd$Latitude <- as.double(bd$Latitude)
sort(bd$Latitude, dec = TRUE) %>% 
  head()

bd_drop_na <- bd %>% 
  tidyr::drop_na(Latitude)
bd_drop_na

bd_drop_na_mu <- bd_drop_na %>% 
  dplyr::mutate(lon = Longitude, lat = Latitude, .before = 1)
bd_drop_na_mu

sf_bd <- bd_drop_na_mu %>% 
  sf::st_as_sf(coords = c("lon", "lat"), crs = 4326)
sf_bd

setwd("D:\\_data\\Lucas\\06_DISCIPLINA_GEO_R_MAURICIO_2020\\github\\disciplina-analise-geoespacial-r\\04_datapaper\\mamiferos_invasores\\03_dados_espaciais")

here::set_here()

dir.create(here::here("mamiferos_invasores"))
here::here()

ma_sf <- sf::st_read(here::here("ma_limite_consensual_muylaert_et_al_2018_wgs84.shp"), quiet = TRUE)

plot(ma_sf$geometry, col = "gray", main = NA, axes = TRUE, graticule = TRUE)
plot(sf_bd$geometry, pch = 20, col = "forestgreen", main = NA, axes = TRUE, graticule = TRUE, add = TRUE)

sf::st_intersects(x = ma_sf, y = sf_bd)

sf_bd_inside <- sf_bd %>% 
  dplyr::filter(sf::st_intersects(x = ma_sf, y = ., sparse = FALSE))
sf_bd_inside

plot(ma_sf$geometry, col = "gray", main = NA, axes = TRUE, graticule = TRUE)
plot(sf_bd_inside$geometry, add = TRUE)





