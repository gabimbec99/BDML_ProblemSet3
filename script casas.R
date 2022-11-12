vignette(package = "sf") # Ver viñetas disponibles

## llamar pacman (contiene la función p_load)
require(pacman)

## llamar y/o instalar librerias
p_load(tidyverse,rio,skimr,
       sf, ## datos espaciales
       leaflet, ## visualizaciones
       tmaptools, ## geocodificar
       ggsn, ## map scale bar 
       osmdata) ## packages with census data

houses_train <- import("D:/noveno semestre/big data/problem set 3/dataPS3/train.Rds")
houses_test <- import("D:/noveno semestre/big data/problem set 3/dataPS3/test.Rds")


houses_train <- st_as_sf(x = houses_train, ## datos
                         coords=c("lon","lat"), ## coordenadas
                         crs=4326) ## CRS

houses_test <- st_as_sf(x = houses_test, ## datos
                        coords=c("lon","lat"), ## coordenadas
                        crs=4326) ## CRS

leaflet() %>% addTiles() %>% addCircleMarkers(data=houses_train[1:50,])

colSums(is.na(houses_train))

###############################
########## Bogotá #############
###############################


# Primero hay que extraer las casas de bogotá 
houses_bta <- houses_train %>% subset(city=="Bogotá D.C") 

# grafico las casas
leaflet() %>% addTiles() %>% addCircles(data=houses_bta[1:50,])

# que comodidades puedo encontrar.
available_tags("amenity") %>% head(20)

# distancia al paradero SITP más cercano. 
bus = opq(bbox = getbb("Bogotá Colombia")) %>%
  add_osm_feature(key="amenity" , value="bus_station") 

bus_sf = bus %>% osmdata_sf()

bus_station = bus_sf$osm_points %>% select(osm_id,amenity) 

leaflet() %>% addTiles() %>% addCircleMarkers(data=bus_station , col="red") %>% 
  addCircles(data=houses_bta[1:50,])

matrix_dist_bus <- st_distance(x=houses_bta , y=bus_station)
min_dist_bus <- apply(matrix_dist_bus , 1 , min)
houses_bta$dist_buse = min_dist_bus

# distancia promedio al restaurante. 
restaurante = opq(bbox = getbb("Bogotá Colombia")) %>%
  add_osm_feature(key="amenity" , value="restaurant") 

restaurante_sf = restaurante %>% osmdata_sf()

restaurante = restaurante_sf$osm_points %>% select(osm_id,amenity) 

leaflet() %>% addTiles() %>% addCircleMarkers(data=restaurante , col="red") %>% 
  addCircles(data=houses_bta[1:50,])

matrix_dist_res <- st_distance(x=houses_bta , y=restaurante)
matrix_dist_res %>% head()

mean_dist_res <- apply(matrix_dist_res , 1 , mean)
houses_bta$mean_dist_res = mean_dist_res

# distancia promedio al bar 
bar = opq(bbox = getbb("Bogotá Colombia")) %>%
  add_osm_feature(key="amenity" , value="bar") 

bar_sf = bar %>% osmdata_sf()

bar = bar_sf$osm_points %>% select(osm_id,amenity) 


leaflet() %>% addTiles() %>% addCircleMarkers(data=bar , col="red") %>% 
  addCircles(data=houses_bta[1:50,])

matrix_dist_bar <- st_distance(x=houses_bta , y=bar)
mean_dist_bar <- apply(matrix_dist_bar , 1 , mean)
houses_bta$mean_dist_bar = mean_dist_bar

# distancia al hospital más cercano. 
hos = opq(bbox = getbb("Bogotá Colombia")) %>%
  add_osm_feature(key="amenity" , value="hospital") 

hos_sf = hos %>% osmdata_sf()

hos = hos_sf$osm_points %>% select(osm_id,amenity) 

leaflet() %>% addTiles() %>% addCircleMarkers(data=hos , col="red") %>% 
  addCircles(data=houses_bta[1:50,])

matrix_dist_hos <- st_distance(x=houses_bta , y=hos)
min_dist_hos<- apply(matrix_dist_hos , 1 , min)
houses_bta$min_dist_hos = min_dist_hos



# obtener la misma información pero para Medellín.

#######################################
############ Medellín #################
#######################################

# Primero hay que extraer las casas de Medellin
houses_med <- houses_train %>% subset(city=="Medellín") 

# grafico las casas
leaflet() %>% addTiles() %>% addCircles(data=houses_med)

# distancia al paradero SITP más cercano. 
bus = opq(bbox = getbb("Medellín Colombia")) %>%
  add_osm_feature(key="amenity" , value="bus_station") 

bus_sf = bus %>% osmdata_sf()

bus_station = bus_sf$osm_points %>% select(osm_id,amenity) 

leaflet() %>% addTiles() %>% addCircleMarkers(data=bus_station , col="red") %>% 
  addCircles(data=houses_med)

matrix_dist_bus <- st_distance(x=houses_med , y=bus_station)
min_dist_bus <- apply(matrix_dist_bus , 1 , min)
houses_med$dist_buse = min_dist_bus

# distancia promedio al restaurante. 
restaurante = opq(bbox = getbb("Medellín Colombia")) %>%
  add_osm_feature(key="amenity" , value="restaurant") 

restaurante_sf = restaurante %>% osmdata_sf()

restaurante = restaurante_sf$osm_points %>% select(osm_id,amenity) 

leaflet() %>% addTiles() %>% addCircleMarkers(data=restaurante , col="red") %>% 
  addCircles(data=houses_med[1:50,])

matrix_dist_res <- st_distance(x=houses_med , y=restaurante)
matrix_dist_res %>% head()

mean_dist_res <- apply(matrix_dist_res , 1 , mean)
houses_med$mean_dist_res = mean_dist_res

# distancia promedio al bar 
bar = opq(bbox = getbb("Medellín Colombia")) %>%
  add_osm_feature(key="amenity" , value="bar") 

bar_sf = bar %>% osmdata_sf()

bar = bar_sf$osm_points %>% select(osm_id,amenity) 


leaflet() %>% addTiles() %>% addCircleMarkers(data=bar , col="red") %>% 
  addCircles(data=houses_med[1:50,])

matrix_dist_bar <- st_distance(x=houses_med , y=bar)
mean_dist_bar <- apply(matrix_dist_bar , 1 , mean)
houses_med$mean_dist_bar = mean_dist_bar

# distancia al hospital más cercano. 
hos = opq(bbox = getbb("Medellín Colombia")) %>%
  add_osm_feature(key="amenity" , value="hospital") 

hos_sf = hos %>% osmdata_sf()

hos = hos_sf$osm_points %>% select(osm_id,amenity) 

leaflet() %>% addTiles() %>% addCircleMarkers(data=hos , col="red") %>% 
  addCircles(data=houses_med[1:50,])

matrix_dist_hos <- st_distance(x=houses_med , y=hos)
min_dist_hos<- apply(matrix_dist_hos , 1 , min)
houses_med$min_dist_hos = min_dist_hos




#######################################
############ Cali #################
#######################################


# grafico las casas
leaflet() %>% addTiles() %>% addCircles(data=houses_test[1:50,])

# distancia al paradero SITP más cercano. 
bus = opq(bbox = getbb("Cali Colombia")) %>%
  add_osm_feature(key="amenity" , value="bus_station") 

bus_sf = bus %>% osmdata_sf()

bus_station = bus_sf$osm_points %>% select(osm_id,amenity) 

leaflet() %>% addTiles() %>% addCircleMarkers(data=bus_station , col="red") %>% 
  addCircles(data=houses_test[1:50,])

matrix_dist_bus <- st_distance(x=houses_test , y=bus_station)
min_dist_bus <- apply(matrix_dist_bus , 1 , min)
houses_test$dist_buse = min_dist_bus

# distancia procalio al restaurante. 
restaurante = opq(bbox = getbb("Cali Colombia")) %>%
  add_osm_feature(key="amenity" , value="restaurant") 

restaurante_sf = restaurante %>% osmdata_sf()

restaurante = restaurante_sf$osm_points %>% select(osm_id,amenity) 

leaflet() %>% addTiles() %>% addCircleMarkers(data=restaurante , col="red") %>% 
  addCircles(data=houses_test[1:50,])

matrix_dist_res <- st_distance(x=houses_test , y=restaurante)
matrix_dist_res %>% head()

mean_dist_res <- apply(matrix_dist_res , 1 , mean)
houses_test$mean_dist_res = mean_dist_res

# distancia procalio al bar 
bar = opq(bbox = getbb("Cali Colombia")) %>%
  add_osm_feature(key="amenity" , value="bar") 

bar_sf = bar %>% osmdata_sf()

bar = bar_sf$osm_points %>% select(osm_id,amenity) 


leaflet() %>% addTiles() %>% addCircleMarkers(data=bar , col="red") %>% 
  addCircles(data=houses_test[1:50,])

matrix_dist_bar <- st_distance(x=houses_test , y=bar)
mean_dist_bar <- apply(matrix_dist_bar , 1 , mean)
houses_test$mean_dist_bar = mean_dist_bar

# distancia al hospital más cercano. 
hos = opq(bbox = getbb("Cali Colombia")) %>%
  add_osm_feature(key="amenity" , value="hospital") 

hos_sf = hos %>% osmdata_sf()

hos = hos_sf$osm_points %>% select(osm_id,amenity) 

leaflet() %>% addTiles() %>% addCircleMarkers(data=hos , col="red") %>% 
  addCircles(data=houses_test[1:50,])

matrix_dist_hos <- st_distance(x=houses_test , y=hos)
min_dist_hos<- apply(matrix_dist_hos , 1 , min)
houses_test$min_dist_hos = min_dist_hos


############################################################################
################### Añadir información de fuentes de las alcaldías ##########
#############################################################################

# Medellín #
#### importar el barrio de la casa. 
install.packages("rgdal")
library(rgdal)
barrios_med <- readOGR(dsn = "D:/noveno semestre/big data/problem set 3/dataPS3", layer = "planeacion_gdb", GDAL1_integer64_policy = TRUE)
barrios_med <- spTransform(barrios_med, CRS("+proj=longlat +datum=WGS84 +no_defs"))

barrios_med = st_as_sf(barrios_med)
houses_med = st_as_sf(houses_med)


inter_med <- st_join(houses_med, barrios_med)



# grafico las casas

leaflet() %>% addTiles() %>% addCircles(data=inter_med) %>% addPolygons(data=barrios_med, color= "red")

#####
#### Barrios cali

barrios_cal <- readOGR(dsn = "D:/noveno semestre/big data/problem set 3/dataPS3", layer = "mc_barrios", GDAL1_integer64_policy = TRUE)
barrios_cal <- spTransform(barrios_cal, CRS("+proj=longlat +datum=WGS84 +no_defs"))

barrios_cal = st_as_sf(barrios_cal)
houses_test = st_as_sf(houses_test)

inter_cal <- st_join(houses_test, barrios_cal)


###### Barrios Bogota y crimen bta.


barrios_bta <- readOGR(dsn = "D:/noveno semestre/big data/problem set 3/dataPS3", layer = "DAIUPZ", GDAL1_integer64_policy = TRUE)
barrios_bta <- spTransform(barrios_bta, CRS("+proj=longlat +datum=WGS84 +no_defs"))
crimen_bogota = subset(barrios_bta, select = c(CMHP19CONT, CMIUUPLA))


crimen_bogota = st_as_sf(crimen_bogota)
houses_bta = st_as_sf(houses_bta)


houses_bta <- st_join(houses_bta, crimen_bogota)


# grafico las casas

leaflet() %>% addTiles() %>% addCircles(data=houses_bta) %>% addPolygons(data=barrios_bta, color= "red")

##################################################################
################### Pegar la criminalidad a los barrios ##########################
##################################################################


### cali 
crimen_cali <- read.csv("D:/noveno semestre/big data/problem set 3/dataPS3/crimen_cali.csv", sep = ';')

crimen_cali <- crimen_cali %>%
  filter(VIGENCIA==2019)

crimen_cali <- crimen_cali %>% group_by(COD_BARRIO) %>% summarise(crimen = sum(CANTIDAD))

inter_cal <- rename(inter_cal,  COD_BARRIO = id_barrio)

houses_test= inter_cal %>% inner_join(crimen_cali,by="COD_BARRIO")



#### Medellin
crimen_med <- read.csv("D:/noveno semestre/big data/problem set 3/dataPS3/hurto_a_persona.csv", sep = ';')
library(stringr)

crimen_med$fecha_hecho = str_sub(crimen_med$fecha_hecho,1,4) 
crimen_med <- crimen_med %>%
  filter(fecha_hecho==2019)


crimen_med = subset(crimen_med, select = c(codigo_barrio, cantidad))
crimen_med$codigo_barrio <- gsub("[[:punct:]]", "", crimen_med$codigo_barrio) #this works on a single
crimen_med <- rename(crimen_med, CODIGO = codigo_barrio)

crimen_med <- crimen_med %>% group_by(CODIGO) %>% summarise(crimen = sum(cantidad))


inter_med= inter_med %>% inner_join(crimen_med,by="CODIGO")



leaflet() %>% addTiles() %>% addCircles(data=crimen_med)%>%addPolygons(data= barrios_med, color='red')





crimen_cali <- crimen_cali %>% group_by(COD_BARRIO) %>% summarise(crimen = sum(crimen))

inter_cal <- rename(inter_cal,  COD_BARRIO= id_barrio)

houses_test= inter_cal %>% inner_join(crimen_cali,by="COD_BARRIO")







##################################################################
################### unir los dataframes ##########################
##################################################################


houses_train <- rbind(houses_med, houses_bta).



##################################################################
################### información del Marco geoestadístico del DANE ##########################
##################################################################

MGDANE <- readOGR(dsn = "D:/noveno semestre/big data/problem set 3/dataPS3", layer = "MGN_ANM_SECCION_URBANA", GDAL1_integer64_policy = TRUE)
MGDANE <- spTransform(MGDANE, CRS("+proj=longlat +datum=WGS84 +no_defs"))

MGDANE = st_as_sf(MGDANE)


## quitar del MGDANE los municipios que no interesan.


MGDANE = subset(MGDANE, select = c(MPIO_CDPMP, SETU_CCNCT,SECU_CCNCT, STP9_2_1_M, STP9_2_2_M, STP9_2_3_M, STP9_3_1_N, STP9_3_2_N, STP9_3_3_N, STP9_3_5_N, STP9_3_10, STP19_ES_2, STP19_EE_1 , STP19_EE_2, STP19_EE_3, STP19_EE_4, STP19_EE_5, STP19_EE_6, STP19_ACU2, STP19_ALC2, STP19_GAS2, STP19_REC2, STP19_INT2, STP51_PRIM, STP51_SECU, STP51_SUPE, STP51_POST, STP51_13_E))


MGDANE = st_as_sf(MGDANE)
houses_test = st_as_sf(houses_test)


sf_use_s2(FALSE)
prueba_cali <- st_join(houses_test, MGDANE)


colSums(is.na(prueba_cali)) 

cali <- MGDANE %>%
  filter(MPIO_CDPMP==76001)

### hacer buffer de las casas de cali.
library(maps)
library(rgeos)
library(sp)
prueba_buffer <- prueba_cali[,20]
prueba_buffer <- sf:::as_Spatial(prueba_buffer$geometry)

prueba_buffer <- spTransform( prueba_buffer, CRS("+proj=longlat +datum=WGS84")) 
pc100km <- gBuffer( prueba_buffer, width=100)
 

leaflet() %>% addTiles() %>% addPolygons(data=pc100km, color= "red")



prueba_buffer = st_as_sf(prueba_buffer)


pointsBuffer <- gBuffer(prueba_buffer, width=.5, byid = TRUE)
leaflet() %>% addTiles() %>% addCircles(data=prueba_cali) %>% addPolygons(data=cali, color= "red")



colSums(is.na(houses_bta)) 

MGDANE <- MGDANE %>%
  filter(MPIO_CDPMP==05001 | MPIO_CDPMP==76001| MPIO_CDPMP==11001)


bta <- MGDANE %>%
  filter(MPIO_CDPMP==11001)



bta = st_as_sf(bta)
houses_bta = st_as_sf(houses_bta)


sf_use_s2(FALSE)
houses_bta <- st_join(houses_bta, bta)


colSums(is.na(houses_bta)) 





leaflet() %>% addTiles() %>% addCircles(data=houses_bta) %>% addPolygons(data=MGDANE, color= "red")





