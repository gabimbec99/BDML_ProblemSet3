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
    
