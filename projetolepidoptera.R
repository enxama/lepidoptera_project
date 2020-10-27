#'---
#'title: latitudinal lepidoptera project
#'authors: larissa teixeira and paula furlan
#'date: 2020-10-26
#'---

#carregar pacotes
library(tidyverse)
library(tidyr)
library(devtools)
library(lubridate)
library(here)
library(openxlsx)
library(readxl)
library(writexl)
library(geobr)
library(rnaturalearthdata)
library(sf)
library(raster)
library(lwgeom)
library(ggspatial)
library(tmap)
library(viridis)
library(RColorBrewer)
library(cptcity)
library(wesanderson)
library(sp)


#importar dados
datasp <- ATLANTIC_BUTTERFLIES_species
datasites <- ATLANTIC_BUTTERFLIES_sites

#remover nao-Atlantic Forest da coluna Olsonr200r

datasites_sample <- datasites[datasites$Olsong200r == "Atlantic Forests", ]
datasites_sample

#criando tabela nova

write.csv(datasites_sample, "ATLANTIC_LEPIDOPTERA_sites_Atlantic_Forest.csv", row.names = FALSE, quote = FALSE)

openxlsx::write.xlsx(datasites_sample, "ATLANTIC_LEPIDOPTERA_sites_Atlantic_Forest.xlsx", row.names = FALSE, quote = FALSE)

#abrindo tabela nova

da <- readr::read_csv(here::here("ATLANTIC_LEPIDOPTERA_sites_Atlantic_Forest.csv"))


#remover altitude less than 1000

da_alt <- da[datasites_sample$Altitude1km < 1000, ]
da_alt

#remover linhas com NA em todas as colunas 1km

da_alt_na_all <- da_alt %>% 
  tidyr::drop_na()
da_alt_na_all


#selecionar colunas pelo nome

da_select <- da_alt_na_all %>% 
  select(Altitude1km, Latitude, Longitude)
da_select

#nota: mutate() adiciona novas colunas ou adiciona resultados em colunas existentes


#criar pasta para importar dados geograficos
dir.create(here::here("geographic_data"))

#aumentar tempo de dowload
options(timeout = 600)

#espaço em branco pra importarmos os dados geograficos




#converter dados sf sample


#Dúvidas:
#2. onde fazer download dos dados
#3. como usar dados climaticos para fazer input
#4. é normal o esquema do NA ter dado ruim?
#5. baixar individualmente os dados?
#6. converter coordenadas