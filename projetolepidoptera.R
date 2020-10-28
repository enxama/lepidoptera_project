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

#adicionar ao dataset sites

sites_with_wingsize <- ATLANTIC_BUTTERFLIES_sites %>% 
  mutate(sp_summarise_group$mean_wingsize, sp_summarise_group$sd_wingsize)
sites_with_wingsize

#contei numero de SP por linha
sp_rich_group <- sp_drop_na %>% 
  group_by(sites_ID) %>% 
  summarise(richness = n()) %>% 
  ungroup() %>% 
  mutate(richness)
sp_rich_group

#adicionar ao dataset sites

siteswgrich <- sites_with_wingsize %>% 
  mutate(sp_rich_group$richness)
siteswgrich

#renomear colunas

sites_data <- siteswgrich %>% 
  rename(wing_size = `sp_summarise_group$mean_wingsize`, sd_wingsize = `sp_summarise_group$sd_wingsize`, richness = `sp_rich_group$richness`)

sites_data

#selecionar dados


#ATE AQUI FUNCIONA_PAULA A PARTIR DAQUI


#criar pasta para importar dados geograficos
dir.create(here::here("geographic_data"))

#aumentar tempo de dowload
options(timeout = 600)

#espaço em branco pra importarmos os dados geograficos

#SLOPE MAPA MATRICIAL

# import raster
slope  <-  raster :: raster ( here:: here ("slope_1KMmd_GMTEDmd.tif" )) %>% 
  raster :: mask ("")
slope
plot ( elev , col  =  viridis :: viridis ( 10 ))

# # ggplot2
# raster para tibble
da_slope  <-  raster :: rasterToPoints ( slope )% > %
  tibble :: as_tibble ()% > %
  dplyr :: rename ( slope  =  srtm_27_17_rc )
head ( da_slope )

# mapa de elevação
map_slope_gg  <- ggplot () +
  geom_raster ( dados  =  da_slope , aes ( x  =  x , y  =  y , preencher  =  slope )) +
  geom_sf ( data  =  "slope_1KMmd_GMTEDmd.tif" , color  =  " black " , fill  =  NA ) +
  scale_fill_gradientn ( cores  =  viridis :: viridis ( 10 )) +
  theme_bw () +
  annotation_scale ( location  =  " br " , width_hint  =  .3 ) +
  annotation_north_arrow ( location  =  " br " , which_north  =  " true " ,
                           pad_x  = unidade ( 0 , " cm " ), pad_y  = unidade ( .7 , " cm " ),
                           style  =  north_arrow_fancy_orienteering ) +
  labs ( x  =  " Longitude " , y  =  " Latitude " , title  =  " Slope Atlantic Forest " , preencher  =  " Legenda " ) +
  tema ( legend.position  = c ( .18 , .18 ),
         legend.box.background  = element_rect ( color  =  " black " ),
         axis.text.y  = element_text ( ângulo  =  90 , hjust  =  .5 ))
map_elev_gg


# # tmap
# mapa de relevo
map_slope_tmap  <- tm_shape ( slope ) +
  tm_raster ( title  =  " Legenda " ) +
  tm_shape ( slope_1KMmd_GMTEDmd.tif ) +
  tm_borders () +
  tm_grid ( linhas  =  FALSO , labels.format  =  lista ( big.mark  =  " " ), labels.rot  = C ( 0 , 90 )) +
  tm_compass () +
  tm_scale_bar () +
  tm_xlab ( " Longitude " ) +
  tm_ylab ( " Latitude " ) +
  tm_layout ( legend.position  = c ( " esquerdo " , " inferior " ),
              main.title  =  " Slope Atlantic Forest " )
map_slope_tmap

# mapa de elevação
map_slope_tmap  <- tm_shape ( slope ) +
  tm_raster ( pal  =  cptcity :: cpt ( pal  =  " gmt_GMT_dem4 " ),
              # quebras = c (400, 500, 600, 700, 800, 900),
              n  =  20 ,
              title  =  " Legenda " ) +
  tm_shape ( slope_1KMmd_GMTEDmd.tif ) +
  tm_borders () +
  tm_grid ( linhas  =  FALSO , labels.format  =  lista ( big.mark  =  " " ), labels.rot  = C ( 0 , 90 )) +
  tm_compass () +
  tm_scale_bar () +
  tm_xlab ( " Longitude " ) +
  tm_ylab ( " Latitude " ) +
  tm_layout ( legend.position  = c ( " esquerdo " , " inferior " ),
              legend.outside  =  TRUE ,
              main.title  =  " Slope Atlantic Forest " )
map_slope_tmap


# # tmap
# exportação
tmap_save ( map_use_tmap ,
            filename  =  here :: here "slope_1KMmd_GMTEDmd.tif"
            largura  =  20 ,
            altura  =  20 ,
            unidades  =  " cm " ,
            dpi  =  300 )




