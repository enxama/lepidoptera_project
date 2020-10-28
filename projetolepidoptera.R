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
library(readr)
library(ggplot2)

#definir diretorio
setwd("C:\\Users\\laris\\OneDrive\\Documentos\\home\\lepidoptera_project\\Atlantic_Forest_Lepidoptera")

getwd()

dir()

#importar dados

#referencias
ATLANTIC_BUTTERFLIES_references <- read_delim("~/home/projeto-disciplina-geospacial-r/lepidoptera_project/Atlantic_Forest_Lepidoptera/ATLANTIC_BUTTERFLIES_references.csv", 
                                              ";", escape_double = FALSE, trim_ws = TRUE)
View(ATLANTIC_BUTTERFLIES_references)

#sites
ATLANTIC_BUTTERFLIES_sites <- read_delim("~/home/projeto-disciplina-geospacial-r/lepidoptera_project/Atlantic_Forest_Lepidoptera/ATLANTIC_BUTTERFLIES_sites.csv", 
                                         ";", escape_double = FALSE, trim_ws = TRUE)
View(ATLANTIC_BUTTERFLIES_sites)

#species
ATLANTIC_BUTTERFLIES_species <- read_delim("~/home/projeto-disciplina-geospacial-r/lepidoptera_project/Atlantic_Forest_Lepidoptera/ATLANTIC_BUTTERFLIES_species.csv", 
                                           ";", escape_double = FALSE, trim_ws = TRUE)
View(ATLANTIC_BUTTERFLIES_species)

#remover altitude less than 1000

da_alt <- ATLANTIC_BUTTERFLIES_sites[ATLANTIC_BUTTERFLIES_sites$Altitude1km < 1000, ]
da_alt

#criando tabelas novas

write.csv(da_alt, "ATLANTIC_LEPIDOPTERA_sites_altitudinal_cut.csv", row.names = FALSE, quote = FALSE)
openxlsx::write.xlsx(da_alt, "ATLANTIC_LEPIDOPTERA_sites_altitudinal_cut.xlsx", row.names = FALSE, quote = FALSE)

#renomear coluna problematica

sp_rename <- ATLANTIC_BUTTERFLIES_species %>% 
  rename(wing_size = `Wing size`)
sp_rename

#1. calculando media asa por lugar

#primeiro retirar linhas com NA na coluna "wing_size"

sp_drop_na <- sp_rename %>% 
  tidyr::drop_na(wing_size)
sp_drop_na

#agora calcular tamanho médio de asa por comunidade

sp_summarise_group <- sp_drop_na %>% 
  group_by(sites_ID) %>% 
  summarise(mean_wingsize = mean(wing_size),
            sd_wingsize = sd(wing_size))
sp_summarise_group

#riqueza por lugar

#precisa perguntar pro mauricio
sp_richness_group <- sp_drop_na %>% 
  group_by(sites_ID) %>% 
  
  summarise(nrow(Species))
sp_richness_group


#ATE AQUI FUNCIONA

#adicionar ao atlantic sites

sp_drop_na %>% mutate(mean_wingsize, sd_wingsize)
#duvida: como adicionar uma planilha na outra???


#criar pasta para importar dados geograficos
dir.create(here::here("geographic_data"))

#aumentar tempo de dowload
options(timeout = 600)

#espaço em branco pra importarmos os dados geograficos


