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
setwd("C:\\Users\\paula\\OneDrive\\Documentos\\Analise Espacial com R\\lepidoptera_project-master")
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

#agora calcular tamanho m√©dio de asa por comunidade
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



#criar pasta para importar dados geograficos
dir.create(here::here("geographic_data"))

#aumentar tempo de dowload
options(timeout = 600)

#espaÁo em branco pra importarmos os dados geograficos

#SLOPE MAPA MATRICIAL
# mata atlantica
af <- geobr::read_biomes(year = 2004) %>% 
  dplyr::filter(name_biome == "Mata Atl‚ntica") %>% 
  sf::st_transform(crs = 4326)
af
plot(af$geom)

# import raster
slope_af <- raster::raster("slope_5KMmd_GMTEDmd.tif") %>% 
  raster::crop(af) %>%  
  raster::mask(af)
slope_af
plot(slope_af, col = viridis::viridis(10))


homo_af <- raster::raster("Homogeneity_01_05_5km_uint16.tif") %>% 
  raster::crop(af) %>%  
  raster::mask(af)
homo_af
plot(homo_af, col = viridis::viridis(10))

elev_af <- raster::raster("elevation_5KMmd_GMTEDmd.tif") %>% 
  raster::crop(af) %>%  
  raster::mask(af)
elev_af
plot(elev_af, col = viridis::viridis(10))

# extract to points
dplyr::glimpse(sites_data)

sites_data <- sites_data %>% 
  dplyr::mutate(slope = raster::extract(slope_af, sites_data[, c("Longitude", "Latitude")]),
                homo = raster::extract(homo_af, sites_data[, c("Longitude", "Latitude")]))
sites_data

dplyr::glimpse(sites_data)

# analysis ----------------------------------------------------------------
# species number ~ prec
model_prec <- lm(richness ~ A_rainfall, data = sites_data)
model_prec
summary(model_prec)

# plot
ggplot(data = sites_data) +
  aes(x = A_rainfall, y = richness) +
  geom_point() +
  geom_smooth(method = "lm", col = "purple") +
  theme_bw() +
  labs(x = "PrecipitaÁ„o (mm)", y = "N˙mero de espÈcies")

# species number ~ temp
model_temp <- lm(richness ~ A_mean_temp, data = sites_data)
model_temp
summary(model_temp)

# plot
ggplot(data = sites_data) +
  aes(x = A_mean_temp, y = richness) +
  geom_point() +
  geom_smooth(method = "lm", col = "red") +
  theme_bw() +
  labs(x = "Temperatura (∫C)", y = "N˙mero de espÈcies")

# temperatura ~ wing size
model_ws_temp <- lm(wing_size ~ A_mean_temp, data = sites_data)
model_ws_temp
summary(model_ws_temp)

# plot
ggplot(data = sites_data) +
  aes(x = A_mean_temp, y = wing_size) +
  geom_point() +
  geom_smooth(method = "lm", col = "red", shape = 2) +
  theme_bw() +
  labs(x = "Temperatura (∫C)", y = "Tamanho da asa (cm)")

# species number ~ latitude
model_sp_lat <- lm(richness ~ Latitude, data = sites_data)
model_sp_lat
summary(model_sp_lat)

# plot
ggplot(data = sites_data) +
  aes(x = Latitude, y = richness) +
  geom_point() +
  geom_smooth(method = "lm", col = "blue") +
  theme_bw() +
  labs(x = "Latitude", y = "N˙mero de espÈcies")

# tamanho da asa ~ altitude
model_wing_alt <- lm(wing_size ~ Altitude1km, data = sites_data)
model_wing_alt
summary(model_wing_alt)


# plot
ggplot(data = sites_data) +
  aes(x = Altitude1km, y = wing_size) +
  geom_point()+
  geom_smooth(method = "lm", col = "green") +
  theme_bw() +
  labs(x = "Altitude (m)", y = "Tamanho da asa (cm)")


# species number ~ homogeneidade
homo_tiff <- raster::raster (here::here ("Homogeneity_01_05_5km_uint16.tif")) %>%
  raster :: mask ("")

model_sp_homo <- lm(richness ~ "Homogeneity_01_05_5km_uint16.tif" , data = sites_data, *data = *"Homogeneity_01_05_5km_uint16.tif")
model_sp_homo
summary(model_sp_homo)


# plot
ggplot sites_data$richness ~ objeto
geom_smooth(method = "lm", col = "green") +
  theme_bw() +
  labs(x = "Homogeneidade", y = "N˙mero de espÈcies")

# -------------------------------------------------------------------------





# # ggplot2
# raster para tibble
da_slope  <-  raster :: rasterToPoints ( slope )% > %
  tibble :: as_tibble ()% > %
  dplyr :: rename ( slope  =  srtm_27_17_rc )
  'd ( da_slope )--*=]-8

# mapa de eleva√ß√£o

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
         axis.text.y  = element_text ( √¢ngulo  =  90 , hjust  =  .5 ))
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

# mapa de eleva√ß√£o
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
# exporta√ß√£o
tmap_save ( map_use_tmap ,
            filename  =  here :: here "slope_1KMmd_GMTEDmd.tif"
            largura  =  20 ,
            altura  =  20 ,
            unidades  =  " cm " ,
            dpi  =  300 )




