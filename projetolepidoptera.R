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
setwd("C:\\Users\\paula\\OneDrive\\Documentos\\Analise Espacial com R\\GITHUB\\lepidoptera_project")

getwd()

dir()

#importar dados

#referencias
ATLANTIC_BUTTERFLIES_references <- read_delim("ATLANTIC_BUTTERFLIES_references.csv", 
                                              ";", escape_double = FALSE, trim_ws = TRUE)
View(ATLANTIC_BUTTERFLIES_references)

#sites
ATLANTIC_BUTTERFLIES_sites <- read_delim("ATLANTIC_BUTTERFLIES_sites.csv", 
                                         ";", escape_double = FALSE, trim_ws = TRUE)
View(ATLANTIC_BUTTERFLIES_sites)

#species
ATLANTIC_BUTTERFLIES_species <- read_delim("ATLANTIC_BUTTERFLIES_species.csv", 
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

ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, 'Package'])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dep = T)
  sapply(pkg, require, character.only = T)}

packages <- c('vegan', 'ggplot2', 'raster', 'plyr', 'reshape2', 'RColorBrewer', 
              'scales', 'grid', 'maptools', 'dismo', 'bbmle', 'foreign', 'rgdal')

ipak(packages)

# clean and memory size and vanish with scinot
rm(list = ls())
gc() 
memory.limit(size = 100000000) 
options(digits=7, scipen=999)

###=========================================================================================###

# Abrindo os shapes
mma2004 <- shapefile("limite_ma_mma_wgs84.shp")


plot(mma2004)

# Qual extent ? mais abrangente?

extent(mma2004)


# Nenhum, ent?o melhor fazer um extent abrangente:
# xmin    : 207854.7 
# xmax    : 3071511
# ymin    : -70735.41
# ymax    : 3207565

# Extens?o que pega todos
# extent(c(207854.7, 3071511, -70735.41, 3207565))
# Adding values to each shape

mma2004@data$Value <- c(10)

###=========================================================================================###

setwd()
# Criando rasters, se necess?rio. Caso j? tenha criado (dropbox), v? para o pr?ximo passo


# mma2004, tanto faz a proje??o base, pq todas est?o iguais

r <- raster()
r <- raster(extent(c(207854.7, 3071511, -70735.41, 3207565)), res = 1000, crs = crs(mma2004))
mma2004r <- rasterize(mma2004, r, field = 'Value')
mma2004r[is.na(mma2004r)] <- 0
plot(mma2004r)

# Plotando os rasters:

par(mfrow = c(2, 3))

plot(mma2004r)


###========================================================================================###

# exportando rasters
# criando pasta para histogramas
dir.create('rasters')
# directory
setwd('./rasters')
getwd()
writeRaster(mma2004r, 'mma2004r_1000', format = 'GTiff')


###=========================================================================================###

# Importando os rasters gerados
setwd()

mma2004r<-raster('mma2004r_1000.tif')

# Gerando cortes dos limites para a malha municipal brasileira
getwd()
dir()
brasil <- shapefile ("brasil_malha_2016_wgs84.shx")


# Area do Brasil Oficial

brasil$area_sqkm <- area(brasil) / 1000000

# Clipando os rasters pelo Brasil Oficial

list <- c("mma2004r")


bra_mma2004r <- mask(mma2004r, brasil) #10



# Juntando os rasters considerando o limite pol?tico BRASILEIRO

Sumr <- raster(extent(c(-55.66624, -28.8359, -29.95184,-3.830447), res = 1000, crs = crs(mma2004r)))

Sumr <- bra_mma2004r




#criar pasta para importar dados geograficos
dir.create(here::here("geographic_data"))

#aumentar tempo de dowload
options(timeout = 600)

#espaço em branco pra importarmos os dados geograficos





##ELEVATION MAPA MATRICIAL

# import raster
elev  <-  raster :: raster ( here:: here ("elevation_1KMmd_GMTEDmd.tif" )) %>% 
  raster :: mask (*)
elev
plot ( elev , col  =  viridis :: viridis ( 10 ))

# # ggplot2
# raster para tibble
da_elev  <-  raster :: rasterToPoints ( elev )% > %
  tibble :: as_tibble ()% > %
  dplyr :: rename ( elev  =  srtm_27_17_rc )
head ( da_elev )

# mapa de elevação
map_elev_gg  <- ggplot () +
  geom_raster ( dados  =  da_elev , aes ( x  =  x , y  =  y , preencher  =  elev )) +
  geom_sf ( data  =  "elevation_1KMmd_GMTEDmd.tif" , color  =  " black " , fill  =  NA ) +
  scale_fill_gradientn ( cores  =  viridis :: viridis ( 10 )) +
  theme_bw () +
  annotation_scale ( location  =  " br " , width_hint  =  .3 ) +
  annotation_north_arrow ( location  =  " br " , which_north  =  " true " ,
                           pad_x  = unidade ( 0 , " cm " ), pad_y  = unidade ( .7 , " cm " ),
                           style  =  north_arrow_fancy_orienteering ) +
  labs ( x  =  " Longitude " , y  =  " Latitude " , title  =  " Elevation Atlantic Forest " , preencher  =  " Legenda " ) +
  tema ( legend.position  = c ( .18 , .18 ),
         legend.box.background  = element_rect ( color  =  " black " ),
         axis.text.y  = element_text ( ângulo  =  90 , hjust  =  .5 ))
map_elev_gg


# # tmap
# mapa de elevação
map_elev_tmap  <- tm_shape ( elev ) +
  tm_raster ( title  =  " Legenda " ) +
  tm_shape ( elevation_1KMmd_GMTEDmd.tif ) +
  tm_borders () +
  tm_grid ( linhas  =  FALSO , labels.format  =  lista ( big.mark  =  " " ), labels.rot  = C ( 0 , 90 )) +
  tm_compass () +
  tm_scale_bar () +
  tm_xlab ( " Longitude " ) +
  tm_ylab ( " Latitude " ) +
  tm_layout ( legend.position  = c ( " esquerdo " , " inferior " ),
              main.title  =  " Elevation Atlantic Forest " )
map_elev_tmap

# mapa de elevação
map_elev_tmap  <- tm_shape ( elev ) +
  tm_raster ( pal  =  wesanderson :: wes_palette ( " Zissou1 " ), title  =  " Elevation" ) +
  tm_shape ( elevation_1KMmd_GMTEDmd.tif ) +
  tm_borders () +
  tm_grid ( linhas  =  FALSO , labels.format  =  lista ( big.mark  =  " " ), labels.rot  = C ( 0 , 90 )) +
  tm_compass () +
  tm_scale_bar () +
  tm_xlab ( " Longitude " ) +
  tm_ylab ( " Latitude " ) +
  tm_layout ( legend.position  = c ( " esquerdo " , " inferior " ),
              main.title  =  " Elevation Forest Atlantic " )
map_elev_tmap

# mapa de elevação
map_elev_tmap  <- tm_shape ( elev ) +
  tm_raster ( pal  =  cptcity :: cpt ( pal  =  " gmt_GMT_dem4 " ),
              # quebras = c (400, 500, 600, 700, 800, 900),
              n  =  20 ,
              title  =  " Legenda " ) +
  tm_shape ( elevation_1KMmd_GMTEDmd.tif ) +
  tm_borders () +
  tm_grid ( linhas  =  FALSO , labels.format  =  lista ( big.mark  =  " " ), labels.rot  = C ( 0 , 90 )) +
  tm_compass () +
  tm_scale_bar () +
  tm_xlab ( " Longitude " ) +
  tm_ylab ( " Latitude " ) +
  tm_layout ( legend.position  = c ( " esquerdo " , " inferior " ),
              legend.outside  =  TRUE ,
              main.title  =  " Elevation Forest Atlantic " )
map_elev_tmap


# # tmap
# exportação
tmap_save ( map_use_tmap ,
            filename  =  here :: here "elevation_1KMmd_GMTEDmd.tif" ),
largura  =  20 ,
altura  =  20 ,
unidades  =  " cm " ,
dpi  =  300 )



##SLOPE MAPA MATRICIAL

# import raster
slope  <-  raster :: raster ( here:: here ("slope_1KMmd_GMTEDmd.tif" )) %>% 
  raster :: mask (*)
slope
plot ( slope , col  =  viridis :: viridis ( 10 ))

# # ggplot2
# raster para tibble
da_slope  <-  raster :: rasterToPoints ( slope )% > %
  tibble :: as_tibble ()% > %
  dplyr :: rename ( slope  =  srtm_27_17_rc )
head ( da_slope )

# mapa de relevo
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
map_slope_gg


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

# mapa de relevo
map_slope_tmap  <- tm_shape ( slope ) +
  tm_raster ( pal  =  wesanderson :: wes_palette ( " Zissou1 " ), title  =  " Slope " ) +
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

# mapa de relevo
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
            filename  =  here :: here "slope_1KMmd_GMTEDmd.tif" ),
largura  =  20 ,
altura  =  20 ,
unidades  =  " cm " ,
dpi  =  300 )




#HOMOGENEITY MAPA MATRICIAL

# import raster
homo  <-  raster :: raster ( here:: here ("homo_1km.tif" )) %>% 
  raster :: mask (*)
homo
plot ( homo , col  =  viridis :: viridis ( 10 ))

# # ggplot2
# raster para tibble
da_homo  <-  raster :: rasterToPoints ( homo ) %>%
  tibble :: as_tibble () %>%
  dplyr :: rename ( homo  =  srtm_27_17_rc )
head ( da_homo )

# mapa de homogeinidade
map_homo_gg  <- ggplot () +
  geom_raster ( dados  =  da_homo , aes ( x  =  x , y  =  y , preencher  =  homo )) +
  geom_sf ( data  =  "homo_1km.tif" , color  =  " black " , fill  =  NA ) +
  scale_fill_gradientn ( cores  =  viridis :: viridis ( 10 )) +
  theme_bw () +
  annotation_scale ( location  =  " br " , width_hint  =  .3 ) +.
annotation_north_arrow ( location  =  " br " , which_north  =  " true " ,
                         pad_x  = unidade ( 0 , " cm " ), pad_y  = unidade ( .7 , " cm " ),
                         style  =  north_arrow_fancy_orienteering ) +
  labs ( x  =  " Longitude " , y  =  " Latitude " , title  =  " Homogeneity Atlantic Forest " , preencher  =  " Legenda " ) +
  tema ( legend.position  = c ( .18 , .18 ),
         legend.box.background  = element_rect ( color  =  " black " ),
         axis.text.y  = element_text ( ângulo  =  90 , hjust  =  .5 ))
map_homo_gg


# # tmap
# mapa de homogeinidade
map_homo_tmap  <- tm_shape ( homo ) +
  tm_raster ( title  =  " Legenda " ) +
  tm_shape ( homo_1km.tif ) +
  tm_borders () +
  tm_grid ( linhas  =  FALSO , labels.format  =  lista ( big.mark  =  " " ), labels.rot  = C ( 0 , 90 )) +
  tm_compass () +
  tm_scale_bar () +
  tm_xlab ( " Longitude " ) +
  tm_ylab ( " Latitude " ) +
  tm_layout ( legend.position  = c ( " esquerdo " , " inferior " ),
              main.title  =  " Homogeneity Atlantic Forest " )
map_homo_tmap

# mapa de homogeinidade
map_homo_tmap  <- tm_shape ( homo ) +
  tm_raster ( pal  =  wesanderson :: wes_palette ( " Zissou1 " ), title  =  " Homogeneity " ) +
  tm_shape ( homo_1km.tif ) +
  tm_borders () +
  tm_grid ( linhas  =  FALSO , labels.format  =  lista ( big.mark  =  " " ), labels.rot  = C ( 0 , 90 )) +
  tm_compass () +
  tm_scale_bar () +
  tm_xlab ( " Longitude " ) +
  tm_ylab ( " Latitude " ) +
  tm_layout ( legend.position  = c ( " esquerdo " , " inferior " ),
              main.title  =  " Homogeneity Atlantic Forest " )
map_homo_tmap

# mapa de homogeinidade
map_homo_tmap  <- tm_shape ( homo ) +
  tm_raster ( pal  =  cptcity :: cpt ( pal  =  " gmt_GMT_dem4 " ),
              # quebras = c (400, 500, 600, 700, 800, 900),
              n  =  20 ,
              title  =  " Legenda " ) +
  tm_shape ( homo_1km.tif ) +
  tm_borders () +
  tm_grid ( linhas  =  FALSO , labels.format  =  lista ( big.mark  =  " " ), labels.rot  = C ( 0 , 90 )) +
  tm_compass () +
  tm_scale_bar () +
  tm_xlab ( " Longitude " ) +
  tm_ylab ( " Latitude " ) +
  tm_layout ( legend.position  = c ( " esquerdo " , " inferior " ),
              legend.outside  =  TRUE ,
              main.title  =  " Homogeneity Atlantic Forest " )
map_homo_tmap


# # tmap
# exportação
tmap_save ( map_use_tmap ,
            filename  =  here :: here "homo_1km.tif" ),
largura  =  20 ,
altura  =  20 ,
unidades  =  " cm " ,
dpi  =  300 )









