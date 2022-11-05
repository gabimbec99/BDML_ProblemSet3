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

# Barrio industrial.
industrial = opq(bbox = getbb("Bogotá Colombia")) %>%
  add_osm_feature(key="building" , value="industrial") 

industrial_sf = industrial %>% osmdata_sf()

industrial = industrial_sf$osm_points %>% select(osm_id,building) 


leaflet() %>% addTiles() %>% addCircleMarkers(data=industrial , col="red") %>% 
  addCircles(data=houses_bta[1:50,])

matrix_dist_indu <- st_distance(x=houses_bta , y=industrial)
mean_dist_indu <- apply(matrix_dist_indu , 1 , mean)
houses_bta$mean_dist_indu = mean_dist_indu


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

# Barrio industrial.
industrial = opq(bbox = getbb("Medellín Colombia")) %>%
  add_osm_feature(key="building" , value="industrial") 

industrial_sf = industrial %>% osmdata_sf()

industrial = industrial_sf$osm_points %>% select(osm_id,building) 


leaflet() %>% addTiles() %>% addCircleMarkers(data=industrial , col="red") %>% 
  addCircles(data=houses_med[1:50,])

matrix_dist_indu <- st_distance(x=houses_med , y=industrial)
mean_dist_indu <- apply(matrix_dist_indu , 1 , mean)
houses_med$mean_dist_indu = mean_dist_indu


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

# Barrio industrial.

#### NO FUNCIONÖ PARA CALI.
industrial = opq(bbox = getbb("CaliColombia")) %>%
  add_osm_feature(key="building" , value="industrial") 

industrial_sf = industrial %>% osmdata_sf()

industrial = industrial_sf$osm_points %>% select(osm_id,building) 


leaflet() %>% addTiles() %>% addCircleMarkers(data=industrial , col="red") %>% 
  addCircles(data=houses_test[1:50,])

matrix_dist_indu <- st_distance(x=houses_test , y=industrial)
mean_dist_indu <- apply(matrix_dist_indu , 1 , mean)
houses_test$mean_dist_indu = mean_dist_indu

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


inter <- st_join(houses_med, barrios_med)



# grafico las casas

leaflet() %>% addTiles() %>% addCircles(data=inter) %>% addPolygons(data=barrios_med, color= "red")

#####
#### Barrios cali

barrios_cal <- readOGR(dsn = "D:/noveno semestre/big data/problem set 3/dataPS3", layer = "mc_barrios", GDAL1_integer64_policy = TRUE)
barrios_cal <- spTransform(barrios_cal, CRS("+proj=longlat +datum=WGS84 +no_defs"))

barrios_cal = st_as_sf(barrios_cal)
houses_test = st_as_sf(houses_test)


inter_cal <- st_join(houses_test, barrios_cal)



# grafico las casas

leaflet() %>% addTiles() %>% addCircles(data=inter_cal) %>% addPolygons(data=barrios_cal, color= "red")

##################################################################
################### Pegar la criminalidad a los barrios ##########################
##################################################################

### cali 
crimen_cali <- read.csv("D:/noveno semestre/big data/problem set 3/dataPS3/crimen_cali.csv", sep = ';')

crimen_cali <- crimen_cali %>%
  filter(VIGENCIA==2019)

crimen_cali <- crimen_cali %>% group_by(COD_BARRIO) %>% summarise(crimen = sum(CANTIDAD))

inter_cal <- rename(inter_cal,  COD_BARRIO = id_barrio)

df= inter_cal %>% inner_join(crimen_cali,by="COD_BARRIO")



#### Bogota

crimen_bogota <- readOGR(dsn = "D:/noveno semestre/big data/problem set 3/dataPS3", layer = "DAIUPZ", GDAL1_integer64_policy = TRUE)
crimen_bogota <- spTransform(crimen_bogota, CRS("+proj=longlat +datum=WGS84 +no_defs"))

crimen_bogota = st_as_sf(crimen_bogota)

crimen_bogota = subset(crimen_bogota, select = c(CMHP19CONT, CMIUUPLA))

pal <- colorNumeric(
  palette = "Blues",
  domain = crimen_bogota$CMHP19CONT)

labels <- sprintf(
  "<strong>%s</strong><br/>%g homicidios",
  crimen_bogota$CMIUUPLA, crimen_bogota$CMHP19CONT
) %>% lapply(htmltools::HTML)


leaflet() %>% addTiles() %>% addPolygons(data=crimen_bogota,
                                         fillColor = ~pal(CMHP19CONT),
                                         weight = 2,
                                         opacity = 1,
                                         color = "white",
                                         dashArray = "2",
                                         fillOpacity = 1,
                                           highlightOptions = highlightOptions(
                                           weight = 5,
                                           color = "#666",
                                           dashArray = "",
                                           fillOpacity = 0.7,
                                           bringToFront = TRUE),
                                             label = labels,
                                             labelOptions = labelOptions(
                                             style = list("font-weight" = "normal", padding = "3px 8px"),
                                             textsize = "15px",
                                             direction = "auto"))


##################################################################
################### unir los dataframes ##########################
##################################################################


houses_train <- rbind(houses_med, houses_bta).

