library(here)
library(readr)
library(tidyverse)
library(here)
library(readxl)
library(writexl)
library(lubridate)
library(dplyr)

DA_Brutos <- readr::read_delim("02_Dados/NEOTROPICAL_ALIEN_MAMMALS_OCCURENCE_v1_0.csv", ";")
DA_Brutos


####Explorar os dados BRUTOS - ctrl + shift + c (comenta)
# unique(DA_Brutos$COUNTRY)
# unique(DA_Brutos$RECORD_YEAR)
# unique(DA_Brutos$HABITAT_TYPE)

Da_Year <- dplyr::filter(DA_Brutos, COUNTRY == "Brazil") %>% #Filtrei só para o Brasil
           dplyr::filter(RECORD_YEAR == "2000"| RECORD_YEAR == "2001"| RECORD_YEAR == "2002"| RECORD_YEAR == "2003"|
                  RECORD_YEAR == "2004"| RECORD_YEAR == "2005"| RECORD_YEAR == "2006"| RECORD_YEAR == "2007"| 
                  RECORD_YEAR == "2008"| RECORD_YEAR == "2009"| RECORD_YEAR == "2010"| RECORD_YEAR == "2011"|
                  RECORD_YEAR == "2012"| RECORD_YEAR == "2013"| RECORD_YEAR == "2014"| RECORD_YEAR == "2015"|
                  RECORD_YEAR == "2016"| RECORD_YEAR == "2017"| RECORD_YEAR == "2018") #Só os anos que haviamos combinado, não tem 2019 e 2018

Da_Year <- dplyr::filter(DA_Brutos, COUNTRY == "Brazil") %>% #Filtrei só para o Brasil
  dplyr::filter(RECORD_YEAR %in% as.character(2000:2018)) #Só os anos que haviamos combinado, não tem 2019 e 2018
Da_Year

Da_Year %>% count(RECORD_YEAR)


# vector
Da_Year_v <- Da_Year %>% 
  dplyr::mutate(lon = LONG_X, lat = LAT_Y) %>% 
  sf::st_as_sf(coords = c("lon", "lat"), crs = 4326)
Da_Year_v

plot(Da_Year_v$geometry, pch = 20)

####Explorar os dados Brasil nos anos selecionados
table(Da_Year$HABITAT_TYPE) # Acho que vai ser complicado filtrar por Habitat tem MUUUITOS nomes diferentes
table(Da_Year$UC) # Tem mais de 5500 registros em UC, será que sua ideia incial não daria certo?
table(Da_Year$STATE) # Possibilidade de trabalhar so com a regiao SUDESTE (8239 registros)

Da_Sudeste <- dplyr::filter(Da_Year, STATE == "SP"| STATE =="MG"| STATE=="ES"| STATE =="RJ"| STATE =="Esp�rito Santo"|
                              STATE =="MG/ES"| STATE== "Minas Gerais" | STATE=="Rio de Janeiro"| STATE=="RJ and MG"| 
                              STATE=="sP"| STATE=="São Paulo") # Qual o comando para mudar esses nomes errados? Nao lembro.

