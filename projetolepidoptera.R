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

#definir diretorio
setwd("C:\\Users\\paula\\OneDrive\\Documentos\\Analise Espacial com R\\lepidoptera_project-master\\lepidoptera_project-master")

getwd()

dir()

#importar dados

#referencias
ATLANTIC_BUTTERFLIES_references <- read_delim("~/Analise Espacial com R/lepidoptera_project-master/lepidoptera_project-master/ATLANTIC_BUTTERFLIES_references.csv", 
                                              ";", escape_double = FALSE, trim_ws = TRUE)
View(ATLANTIC_BUTTERFLIES_references)

#sites
ATLANTIC_BUTTERFLIES_sites <- read_delim("~/Analise Espacial com R/lepidoptera_project-master/lepidoptera_project-master/ATLANTIC_BUTTERFLIES_sites.csv", 
                                         ";", escape_double = FALSE, trim_ws = TRUE)
View(ATLANTIC_BUTTERFLIES_sites)

#species
ATLANTIC_BUTTERFLIES_species <- read_delim("~/Analise Espacial com R/lepidoptera_project-master/lepidoptera_project-master/ATLANTIC_BUTTERFLIES_species.csv", 
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

#calculando media asa por lugar







#ELEVATION MAPA MATRICIAL

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



#SLOPE MAPA MATRICIAL

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
plot ( elev , col  =  viridis :: viridis ( 10 ))

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







#nota: mutate() adiciona novas colunas ou adiciona resultados em colunas existentes



#criar pasta para importar dados geograficos
dir.create(here::here("geographic_data"))

#aumentar tempo de dowload
options(timeout = 600)

#espaço em branco pra importarmos os dados geograficos


#yeah

#converter dados sf sample


#Dúvidas:
#2. onde fazer download dos dados
#3. como usar dados climaticos para fazer input
#4. é normal o esquema do NA ter dado ruim?
#5. baixar individualmente os dados?
#6. converter coordenadas