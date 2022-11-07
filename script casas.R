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

#Esto es para mi (Gabriela)
#install the osmdata, sf, tidyverse and ggmap package
if(!require("osmdata")) install.packages("osmdata")
if(!require("tidyverse")) install.packages("tidyverse")
if(!require("sf")) install.packages("sf")
if(!require("ggmap")) install.packages("ggmap")
install.packages("sp")
#load packages
library(tidyverse)
library(osmdata)
library(sf)
library(ggmap)

houses_train <- import("/Users/gabrielamejia/Documents/GitHub/BDML_ProblemSet3/dataPS3/train.Rds")
houses_test <- import("/Users/gabrielamejia/Documents/GitHub/BDML_ProblemSet3/dataPS3/test.Rds")


houses_train <- st_as_sf(x = houses_train, ## datos
                         coords=c("lon","lat"), ## coordenadas
                         crs=4326) ## CRS

houses_test <- st_as_sf(x = houses_test, ## datos
                        coords=c("lon","lat"), ## coordenadas
                        crs=4326) ## CRS

leaflet() %>% addTiles() %>% addCircleMarkers(data=houses_train[1:50,])

colSums(is.na(houses_train))

#######################################################################
########### Extracción de variables en titulos y descripción ##########
#######################################################################

##Elementos: Caso o Apto o Casa Condominio, Apartaestudio
## En venta, En arriendo, nada
## Ubicación del Inbmueble
## Características del Inmueble
## Precio
## ¿Qué falta cuartos, area total, area cubierta, baños otros?###
colSums(is.na(houses_train))
colSums(is.na(houses_test))

houses_train$title <- toupper(houses_train$title)
houses_test$title <- toupper(houses_test$title)
houses_train$description <- toupper(houses_train$description)
houses_test$description <- toupper(houses_test$description)

##############################
##Tipo de conjunto residencial
##############################
houses_train$tipo <- NA
houses_test$tipo <- NA


for (i in c("CONDOMINIO","CONJUNTO","AGRUPACIÓN","COND")){
houses_train$tipo=ifelse(is.na(houses_train$tipo)==T,str_extract(string=houses_train$title , pattern=i),houses_train$tipo)
houses_test$tipo=ifelse(is.na(houses_test$tipo)==T,str_extract(string=houses_test$title , pattern=i),houses_test$tipo)
houses_train$tipo=ifelse(is.na(houses_train$tipo)==T,str_extract(string=houses_train$description , pattern=i),houses_train$tipo)
houses_test$tipo=ifelse(is.na(houses_test$tipo)==T,str_extract(string=houses_test$description , pattern=i),houses_test$tipo)
}

houses_train$tipo=ifelse(houses_train$tipo=="COND","CONDOMINIO",houses_train$tipo)
houses_test$tipo=ifelse(houses_test$tipo=="COND","CONDOMINIO",houses_test$tipo)


houses_train$property_type <- toupper(houses_train$property_type)
houses_test$property_type <- toupper(houses_test$property_type)

houses_train$operation_type <- toupper(houses_train$operation_type)
houses_test$operation_type <- toupper(houses_test$operation_type)

#Combinación tipo de vivienda-conjunto

houses_train$property_type <-ifelse(is.na(houses_train$tipo)==T,houses_train$property_type,paste(houses_train$property_type,houses_train$tipo,sep=" "))
houses_test$property_type <- ifelse(is.na(houses_test$tipo)==T,houses_test$property_type,paste(houses_test$property_type,houses_test$tipo,sep=" "))


##############################
##Ubicación del inmueble:Barrio y Localidad/Demasiado Especifico para sacar/Puede ser Nombre Condominio
##############################


#DESCARTADO#


##############################
##Ubicación del inmueble:Este/Oeste/Sur/Norte/Centro
##############################

houses_train$zona <- NA
houses_test$zona <- NA


for (i in c("ESTE","OESTE","OCCIDENTE","ORIENTE","CENTRO","NORTE","SUR")){
  houses_train$zona=ifelse(is.na(houses_train$zona)==T,str_extract(string=houses_train$title , pattern=i),houses_train$zona)
  houses_test$zona=ifelse(is.na(houses_test$zona)==T,str_extract(string=houses_test$title , pattern=i),houses_test$zona)
  houses_train$zona=ifelse(is.na(houses_train$zona)==T,str_extract(string=houses_train$description , pattern=i),houses_train$zona)
  houses_test$zona=ifelse(is.na(houses_test$zona)==T,str_extract(string=houses_test$description , pattern=i),houses_test$zona)
}

houses_train$zona=ifelse(houses_train$zona=="OESTE","ORIENTE",houses_train$zona)
houses_test$zona=ifelse(houses_test$zona=="ESTE","OCCIDENTE",houses_test$zona)


##############################
##Características del inmueble
##############################


houses_train$description <- str_replace_all(string = houses_train$description , "UNO" , replacement = "1")
houses_train$description <- str_replace_all(string = houses_train$description , "DOS" , replacement = "2")
houses_train$description <- str_replace_all(string = houses_train$description , "TRES" , replacement = "3")
houses_train$description <- str_replace_all(string = houses_train$description , "CUATRO" , replacement = "4")
houses_train$description <- str_replace_all(string = houses_train$description , "CINCO" , replacement = "5")
houses_train$description <- str_replace_all(string = houses_train$description , "SEIS" , replacement = "6")
houses_train$description <- str_replace_all(string = houses_train$description , "SIETE" , replacement = "7")
houses_train$description <- str_replace_all(string = houses_train$description , "OCHO" , replacement = "8")
houses_train$description <- str_replace_all(string = houses_train$description , "NUEVE" , replacement = "9")
houses_train$description <- str_replace_all(string = houses_train$description , "DIEZ" , replacement = "10")


houses_test$description <- str_replace_all(string = houses_test$description , "UNO" , replacement = "1")
houses_test$description <- str_replace_all(string = houses_test$description , "DOS" , replacement = "2")
houses_test$description <- str_replace_all(string = houses_test$description , "TRES" , replacement = "3")
houses_test$description <- str_replace_all(string = houses_test$description , "CUATRO" , replacement = "4")
houses_test$description <- str_replace_all(string = houses_test$description , "CINCO" , replacement = "5")
houses_test$description <- str_replace_all(string = houses_test$description , "SEIS" , replacement = "6")
houses_test$description <- str_replace_all(string = houses_test$description , "SIETE" , replacement = "7")
houses_test$description <- str_replace_all(string = houses_test$description , "OCHO" , replacement = "8")
houses_test$description <- str_replace_all(string = houses_test$description , "NUEVE" , replacement = "9")
houses_test$description <- str_replace_all(string = houses_test$description , "DIEZ" , replacement = "10")

houses_train$description <- str_replace_all(string = houses_train$description , "PRIMER" , replacement = "1")
houses_train$description <- str_replace_all(string = houses_train$description , "SEGUNDO" , replacement = "2")
houses_train$description <- str_replace_all(string = houses_train$description , "TERCER" , replacement = "3")
houses_train$description <- str_replace_all(string = houses_train$description , "CUARTO PISO" , replacement = "4 PISO")
houses_train$description <- str_replace_all(string = houses_train$description , "CUARTO NIVEL" , replacement = "4 NIVEL")
houses_train$description <- str_replace_all(string = houses_train$description , "QUINTO" , replacement = "5")
houses_train$description <- str_replace_all(string = houses_train$description , "SEXTO" , replacement = "6")
houses_train$description <- str_replace_all(string = houses_train$description , "SEPTIMO" , replacement = "7")
houses_train$description <- str_replace_all(string = houses_train$description , "OCTAVO" , replacement = "8")
houses_train$description <- str_replace_all(string = houses_train$description , "NOVENO" , replacement = "9")
houses_train$description <- str_replace_all(string = houses_train$description , "DÉCIMO" , replacement = "10")

houses_test$description <- str_replace_all(string = houses_test$description , "PRIMER" , replacement = "1")
houses_test$description <- str_replace_all(string = houses_test$description , "SEGUNDO" , replacement = "2")
houses_test$description <- str_replace_all(string = houses_test$description , "TERCER" , replacement = "3")
houses_test$description <- str_replace_all(string = houses_test$description , "CUARTO PISO" , replacement = "4 PISO")
houses_test$description <- str_replace_all(string = houses_test$description , "CUARTO NIVEL" , replacement = "4 NIVEL")
houses_test$description <- str_replace_all(string = houses_test$description , "QUINTO" , replacement = "5")
houses_test$description <- str_replace_all(string = houses_test$description , "SEXTO" , replacement = "6")
houses_test$description <- str_replace_all(string = houses_test$description , "SÉPTIMO" , replacement = "7")
houses_test$description <- str_replace_all(string = houses_test$description , "OCTAVO" , replacement = "8")
houses_test$description <- str_replace_all(string = houses_test$description , "NOVENO" , replacement = "9")
houses_test$description <- str_replace_all(string = houses_test$description , "DÉCIMO" , replacement = "10")


x1 <- "[:digit:]+[:space:]+"
x2 <- "[:space:]+[:digit:]+"
x3 <- "[:digit:]+"


x4 <- "+[:digit:]+[:space:]"
x5 <- "+[:space:]+[:digit:]"
x6 <- "+[:digit:]"

y1 <- "+[:upper:]"

##############################
##Pisos o niveles
##############################

houses_train$pisos <- NA
houses_test$pisos <- NA

for (i in c("NIVELES","PISOS","PISO","NIVEL","PLANTAS")){
houses_train$pisos=ifelse(is.na(houses_train$pisos)==T,str_extract(string=houses_train$description , pattern=paste0(x1,i)),houses_train$pisos)
houses_train$pisos=ifelse(is.na(houses_train$pisos)==T,str_extract(string=houses_train$description , pattern=paste0(x2,i)),houses_train$pisos)
houses_train$pisos=ifelse(is.na(houses_train$pisos)==T,str_extract(string=houses_train$description , pattern=paste0(x3,i)),houses_train$pisos)
houses_train$pisos=ifelse(is.na(houses_train$pisos)==T,str_extract(string=houses_train$description , pattern=paste0(i,x4)),houses_train$pisos)
houses_train$pisos=ifelse(is.na(houses_train$pisos)==T,str_extract(string=houses_train$description , pattern=paste0(i,x5)),houses_train$pisos)
houses_train$pisos=ifelse(is.na(houses_train$pisos)==T,str_extract(string=houses_train$description , pattern=paste0(i,x6)),houses_train$pisos)
houses_train$pisos=as.numeric(gsub(i,"",houses_train$pisos))
houses_test$pisos=ifelse(is.na(houses_test$pisos)==T,str_extract(string=houses_test$description , pattern=paste0(x1,i)),houses_test$pisos)
houses_test$pisos=ifelse(is.na(houses_test$pisos)==T,str_extract(string=houses_test$description , pattern=paste0(x2,i)),houses_test$pisos)
houses_test$pisos=ifelse(is.na(houses_test$pisos)==T,str_extract(string=houses_test$description , pattern=paste0(x3,i)),houses_test$pisos)
houses_test$pisos=ifelse(is.na(houses_test$pisos)==T,str_extract(string=houses_test$description , pattern=paste0(i,x4)),houses_test$pisos)
houses_test$pisos=ifelse(is.na(houses_test$pisos)==T,str_extract(string=houses_test$description , pattern=paste0(i,x5)),houses_test$pisos)
houses_test$pisos=ifelse(is.na(houses_test$pisos)==T,str_extract(string=houses_test$description , pattern=paste0(i,x6)),houses_test$pisos)
houses_test$pisos=as.numeric(gsub(i,"",houses_test$pisos))
}



##############################
##Garajes y Parqueaderos
##############################
houses_train$garages <- NA
houses_test$garages <- NA

for (i in c("PARQUEADERO","PARQUEADEROS","GARAJE","GARAJES","ESTACIONAMIENTO","ESTACIONAMIENTOS","PARKING","ESPACIO CARRO")){
  houses_train$garages=ifelse(is.na(houses_train$garages)==T,str_extract(string=houses_train$description , pattern=paste0(x1,i)),houses_train$garages)
  houses_train$garages=ifelse(is.na(houses_train$garages)==T,str_extract(string=houses_train$description , pattern=paste0(x2,i)),houses_train$garages)
  houses_train$garages=ifelse(is.na(houses_train$garages)==T,str_extract(string=houses_train$description , pattern=paste0(x3,i)),houses_train$garages)
  houses_train$garages=ifelse(is.na(houses_train$garages)==T,str_extract(string=houses_train$description , pattern=paste0(i,x4)),houses_train$garages)
  houses_train$garages=ifelse(is.na(houses_train$garages)==T,str_extract(string=houses_train$description , pattern=paste0(i,x5)),houses_train$garages)
  houses_train$garages=ifelse(is.na(houses_train$garages)==T,str_extract(string=houses_train$description , pattern=paste0(i,x6)),houses_train$garages)
  houses_train$garages=ifelse(is.na(houses_train$garages)==T,str_extract(string=houses_train$description , pattern=paste0(i,x6)),houses_train$garages)
  houses_train$garages=as.numeric(gsub(i,"",houses_train$garages))
  houses_train$garages=ifelse(houses_train$garages>10,NA,houses_train$garages)
  houses_test$garages=ifelse(is.na(houses_test$garages)==T,str_extract(string=houses_test$description , pattern=paste0(x1,i)),houses_test$garages)
  houses_test$garages=ifelse(is.na(houses_test$garages)==T,str_extract(string=houses_test$description , pattern=paste0(x2,i)),houses_test$garages)
  houses_test$garages=ifelse(is.na(houses_test$garages)==T,str_extract(string=houses_test$description , pattern=paste0(x3,i)),houses_test$garages)
  houses_test$garages=ifelse(is.na(houses_test$garages)==T,str_extract(string=houses_test$description , pattern=paste0(i,x4)),houses_test$garages)
  houses_test$garages=ifelse(is.na(houses_test$garages)==T,str_extract(string=houses_test$description , pattern=paste0(i,x5)),houses_test$garages)
  houses_test$garages=ifelse(is.na(houses_test$garages)==T,str_extract(string=houses_test$description , pattern=paste0(i,x6)),houses_test$garages)
  houses_test$garages=as.numeric(gsub(i,"",houses_test$garages))
  houses_test$garages=ifelse(houses_test$garages>10,NA,houses_test$garages)
}


for (j in c("PARQUEADERO","GARAJE","ESTACIONAMIENTO","PARKING","ESPACIO CARRO")){
  houses_train$garages=ifelse(is.na(houses_train$garages)==T & str_detect(string=houses_train$description , pattern=j)==T,1,houses_train$garages)
  houses_test$garages=ifelse(is.na(houses_test$garages)==T & str_detect(string=houses_test$description , pattern=j)==T,1,houses_test$garages)
}

##############################
##Pisos y Paredes
##############################

#DESCARTADO


##############################
##Precios
##############################

houses_test$description <- gsub("\\.", "",houses_test$description)
houses_test$description <- gsub("\\$", "PESOS",houses_test$description)


for (i in c("PRECIO","VENTA")){
  houses_test <- houses_test %>% 
    mutate(price = ifelse(is.na(price)==T | price>as.numeric(gsub("[[:alpha:]]", "", str_extract(string=houses_test$description,pattern=paste0(i,"+[:space:]+PESOS+[:digit:]+")))),as.numeric(gsub("[[:alpha:]]", "", str_extract(string=houses_test$description,pattern=paste0(i,"+[:space:]+PESOS+[:digit:]+")))),price))
}


##############################
##Areas
##############################

r1 <- "[:space:]+[:digit:]+[:space:]+"
r2 <- "[:space:]+[:digit:]+[:punct:]+[:digit:]+[:space:]+"
houses_train$new_surface <- NA

## replace values
for (i in c("mts","m2","mt2","mts2","metros","cuadrad","mtro","mtr2")){
  houses_train <- houses_train %>% 
    mutate(new_surface = ifelse(is.na(new_surface)==T,str_extract(string=description , pattern=paste0(r1,i)),new_surface),
           new_surface = ifelse(is.na(new_surface)==T,str_extract(string=description , pattern=paste0(r2,i)),new_surface))
}

## clean var
for (i in c("mts","m2","mt2","mts2","metros","cuadrad","mtro","mtr2"," ","\n\n")){
  houses_train$new_surface <- gsub(i,"",houses_train$new_surface)
}
houses_train$new_surface <- gsub(",",".",houses_train$new_surface)
houses_train$new_surface <- as.numeric(houses_train$new_surface)


houses_test$new_surface <- NA

## replace values
for (i in c("mts","m2","mt2","mts2","metros","cuadrad","mtro","mtr2")){
  houses_test <- houses_test %>% 
    mutate(new_surface = ifelse(is.na(new_surface)==T,str_extract(string=description , pattern=paste0(r1,i)),new_surface),
           new_surface = ifelse(is.na(new_surface)==T,str_extract(string=description , pattern=paste0(r2,i)),new_surface))
}

## clean var
for (i in c("mts","m2","mt2","mts2","metros","cuadrad","mtro","mtr2"," ","\n\n")){
  houses_test$new_surface <- gsub(i,"",houses_test$new_surface)
}
houses_test$new_surface <- gsub(",",".",houses_test$new_surface)
houses_test$new_surface <- as.numeric(houses_test$new_surface)


houses_train$surface_total <- ifelse(is.na(houses_train$surface_total),houses_train$surface_covered,houses_train$surface_total)
houses_train$surface_total <- ifelse(is.na(houses_train$surface_total),houses_train$new_surface,houses_train$surface_total)

houses_test$surface_total <- ifelse(is.na(houses_test$surface_total),houses_test$surface_covered,houses_test$surface_total)
houses_test$surface_total <- ifelse(is.na(houses_test$surface_total),houses_test$new_surface,houses_test$surface_total)
                                    

##############################
##Años de construcción:Imprecisa
##############################
houses_train$edad <- NA
houses_test$edad <- NA

for (i in c("AÑOS","CONSTRUCCIÓN","AÑOS DE CONSTRUCCIÓN","CONSTRUÍDO EL","CONSTRUÍDO EN","CONSTRUÍDO")){
  houses_train$edad=ifelse(is.na(houses_train$edad)==T,str_extract(string=houses_train$description , pattern=paste0(x1,i)),houses_train$edad)
  houses_train$edad=ifelse(is.na(houses_train$edad)==T,str_extract(string=houses_train$description , pattern=paste0(x2,i)),houses_train$edad)
  houses_train$edad=ifelse(is.na(houses_train$edad)==T,str_extract(string=houses_train$description , pattern=paste0(x3,i)),houses_train$edad)
  houses_train$edad=ifelse(is.na(houses_train$edad)==T,str_extract(string=houses_train$description , pattern=paste0(i,x4)),houses_train$edad)
  houses_train$edad=ifelse(is.na(houses_train$edad)==T,str_extract(string=houses_train$description , pattern=paste0(i,x5)),houses_train$edad)
  houses_train$edad=ifelse(is.na(houses_train$edad)==T,str_extract(string=houses_train$description , pattern=paste0(i,x6)),houses_train$edad)
  houses_train$edad=as.numeric(gsub(i,"",houses_train$edad))
  houses_test$edad=ifelse(is.na(houses_test$edad)==T,str_extract(string=houses_test$description , pattern=paste0(x1,i)),houses_test$edad)
  houses_test$edad=ifelse(is.na(houses_test$edad)==T,str_extract(string=houses_test$description , pattern=paste0(x2,i)),houses_test$edad)
  houses_test$edad=ifelse(is.na(houses_test$edad)==T,str_extract(string=houses_test$description , pattern=paste0(x3,i)),houses_test$edad)
  houses_test$edad=ifelse(is.na(houses_test$edad)==T,str_extract(string=houses_test$description , pattern=paste0(i,x4)),houses_test$edad)
  houses_test$edad=ifelse(is.na(houses_test$edad)==T,str_extract(string=houses_test$description , pattern=paste0(i,x5)),houses_test$edad)
  houses_test$edad=ifelse(is.na(houses_test$edad)==T,str_extract(string=houses_test$description , pattern=paste0(i,x6)),houses_test$edad)
  houses_test$edad=as.numeric(gsub(i,"",houses_test$edad))
}


##############################
##Baños, Habitaciones , Alcobas
##############################

###Baños###
houses_train$baños <- NA
houses_test$baños <- NA

for (i in c("BAÑOS")){
  houses_train$baños=ifelse(is.na(houses_train$baños)==T,str_extract(string=houses_train$description , pattern=paste0(x1,i)),houses_train$baños)
  houses_train$baños=ifelse(is.na(houses_train$baños)==T,str_extract(string=houses_train$description , pattern=paste0(x2,i)),houses_train$baños)
  houses_train$baños=ifelse(is.na(houses_train$baños)==T,str_extract(string=houses_train$description , pattern=paste0(x3,i)),houses_train$baños)
  houses_train$baños=ifelse(is.na(houses_train$baños)==T,str_extract(string=houses_train$description , pattern=paste0(i,x4)),houses_train$baños)
  houses_train$baños=ifelse(is.na(houses_train$baños)==T,str_extract(string=houses_train$description , pattern=paste0(i,x5)),houses_train$baños)
  houses_train$baños=ifelse(is.na(houses_train$baños)==T,str_extract(string=houses_train$description , pattern=paste0(i,x6)),houses_train$baños)
  houses_train$baños=as.numeric(gsub(i,"",houses_train$baños))
  houses_test$baños=ifelse(is.na(houses_test$baños)==T,str_extract(string=houses_test$description , pattern=paste0(x1,i)),houses_test$baños)
  houses_test$baños=ifelse(is.na(houses_test$baños)==T,str_extract(string=houses_test$description , pattern=paste0(x2,i)),houses_test$baños)
  houses_test$baños=ifelse(is.na(houses_test$baños)==T,str_extract(string=houses_test$description , pattern=paste0(x3,i)),houses_test$baños)
  houses_test$baños=ifelse(is.na(houses_test$baños)==T,str_extract(string=houses_test$description , pattern=paste0(i,x4)),houses_test$baños)
  houses_test$baños=ifelse(is.na(houses_test$baños)==T,str_extract(string=houses_test$description , pattern=paste0(i,x5)),houses_test$baños)
  houses_test$baños=ifelse(is.na(houses_test$baños)==T,str_extract(string=houses_test$description , pattern=paste0(i,x6)),houses_test$baños)
  houses_test$baños=as.numeric(gsub(i,"",houses_test$baños))
}

colSums(is.na(houses_train))
colSums(is.na(houses_test))
houses_train$bathrooms <-ifelse(is.na(houses_train$bathrooms)==T,houses_train$baños,houses_train$bathrooms)
houses_test$bathrooms <- ifelse(is.na(houses_test$bathrooms)==T,houses_train$baños,houses_test$bathrooms)
#Se reducen los missing

###Habitaciones###
houses_train$habi <- NA
houses_test$habi <- NA

for (i in c("HABITACIONES")){
  houses_train$habi=ifelse(is.na(houses_train$habi)==T,str_extract(string=houses_train$description , pattern=paste0(x1,i)),houses_train$habi)
  houses_train$habi=ifelse(is.na(houses_train$habi)==T,str_extract(string=houses_train$description , pattern=paste0(x2,i)),houses_train$habi)
  houses_train$habi=ifelse(is.na(houses_train$habi)==T,str_extract(string=houses_train$description , pattern=paste0(x3,i)),houses_train$habi)
  houses_train$habi=ifelse(is.na(houses_train$habi)==T,str_extract(string=houses_train$description , pattern=paste0(i,x4)),houses_train$habi)
  houses_train$habi=ifelse(is.na(houses_train$habi)==T,str_extract(string=houses_train$description , pattern=paste0(i,x5)),houses_train$habi)
  houses_train$habi=ifelse(is.na(houses_train$habi)==T,str_extract(string=houses_train$description , pattern=paste0(i,x6)),houses_train$habi)
  houses_train$habi=as.numeric(gsub(i,"",houses_train$habi))
  houses_test$habi=ifelse(is.na(houses_test$habi)==T,str_extract(string=houses_test$description , pattern=paste0(x1,i)),houses_test$habi)
  houses_test$habi=ifelse(is.na(houses_test$habi)==T,str_extract(string=houses_test$description , pattern=paste0(x2,i)),houses_test$habi)
  houses_test$habi=ifelse(is.na(houses_test$habi)==T,str_extract(string=houses_test$description , pattern=paste0(x3,i)),houses_test$habi)
  houses_test$habi=ifelse(is.na(houses_test$habi)==T,str_extract(string=houses_test$description , pattern=paste0(i,x4)),houses_test$habi)
  houses_test$habi=ifelse(is.na(houses_test$habi)==T,str_extract(string=houses_test$description , pattern=paste0(i,x5)),houses_test$habi)
  houses_test$habi=ifelse(is.na(houses_test$habi)==T,str_extract(string=houses_test$description , pattern=paste0(i,x6)),houses_test$habi)
  houses_test$habi=as.numeric(gsub(i,"",houses_test$habi))
}

colSums(is.na(houses_train))
colSums(is.na(houses_test))
houses_train$rooms <-ifelse(is.na(houses_train$rooms)==T,houses_train$habi,houses_train$rooms)
houses_test$rooms <- ifelse(is.na(houses_test$rooms)==T,houses_train$habi,houses_test$rooms)

#Se reducen los missing



##############################
##Balcones, Terraza y Patio:Dummies
##############################

houses_train$terra <- NA
houses_test$terra <- NA

for (i in c("BALCÓN","BALCON","BALCONES","TERRAZA","PATIO","PATIOS","TERRAZAS")){
  houses_train$terra=ifelse(is.na(houses_train$terra)==T & str_detect(string=houses_train$description , pattern=i)==T,1,houses_train$terra)
  houses_test$terra=ifelse(is.na(houses_test$terra)==T & str_detect(string=houses_test$description , pattern=i)==T,1,houses_test$terra)
  
}


##############################
##Gimnasio Y Deportes:Dummies
##############################

houses_train$gym <- NA
houses_test$gym <- NA

for (i in c("GIMNASIO","PESAS","GYM","EJERCICIO","FÚTBOL","BALONCESTO","CANCHAS","ATLÉTICO","DEPORTES","TENIS","SQUASH","TENIS DE MESA","PING PONG","CLUB","JUEGOS")){
  houses_train$gym=ifelse(is.na(houses_train$gym)==T & str_detect(string=houses_train$description , pattern=i)==T,1,houses_train$gym)
  houses_test$gym=ifelse(is.na(houses_test$gym)==T & str_detect(string=houses_test$description , pattern=i)==T,1,houses_test$gym)
}


##############################
##Salon Social:Dummies
##############################

houses_train$social <- NA
houses_test$social <- NA

for (i in c("SALON SOCIAL","SALON COMUNAL","SALA SOCIAL","SALA COMUNAL","SALA DE FIESTAS","SALA DE REUNIÓN","SALAS DE REUNIÓN")){
  houses_train$social=ifelse(is.na(houses_train$social)==T & str_detect(string=houses_train$description , pattern=i)==T,1,houses_train$social)
  houses_test$social=ifelse(is.na(houses_test$social)==T & str_detect(string=houses_test$description , pattern=i)==T,1,houses_test$social)
}


##############################
##Parques, Parques Infantiles y Reservas:Dummies
##############################

houses_train$parks <- NA
houses_test$parks <- NA

for (i in c("PARQUE","PARQUE INFANTIL","PARQUES","RESERVA","ZONA VERDE","ZONAS VERDES")){
  houses_train$parks=ifelse(is.na(houses_train$parks)==T & str_detect(string=houses_train$description , pattern=i)==T,1,houses_train$parks)
  houses_test$parks=ifelse(is.na(houses_test$parks)==T & str_detect(string=houses_test$description , pattern=i)==T,1,houses_test$parks)
}


##############################
##Deposito:Dummies
##############################


houses_train$deposit <- NA
houses_test$deposit <- NA

for (i in c("DEPOSITO","DEPOSITOS","ARMARIO")){
  houses_train$deposit=ifelse(is.na(houses_train$deposit)==T & str_detect(string=houses_train$description , pattern=i)==T,1,houses_train$deposit)
  houses_test$deposit=ifelse(is.na(houses_test$deposit)==T & str_detect(string=houses_test$description , pattern=i)==T,1,houses_test$deposit)
}



##############################
##Remodelación o cambios:Dummies
##############################


houses_train$renov <- NA
houses_test$renov <- NA

for (i in c("RENOVADO","REMODELADO","RENOVADA","REMODELADA","RENOV","REMOD","REMODELACIÓN","RENOVACIÓN")){
  houses_train$renov=ifelse(is.na(houses_train$renov)==T & str_detect(string=houses_train$description , pattern=i)==T,1,houses_train$renov)
  houses_test$renov=ifelse(is.na(houses_test$renov)==T & str_detect(string=houses_test$description , pattern=i)==T,1,houses_test$renov)
}

##############################
##Chimenea:Dummies
##############################

houses_train$chim <- NA
houses_test$chim <- NA

for (i in c("CHIMENEA","CHÍMENEA","FOGATA")){
  houses_train$chim=ifelse(is.na(houses_train$chim)==T & str_detect(string=houses_train$description , pattern=i)==T,1,houses_train$chim)
  houses_test$chim=ifelse(is.na(houses_test$chim)==T & str_detect(string=houses_test$description , pattern=i)==T,1,houses_test$chim)
}

##############################
##Ascensor:Dummies
##############################


houses_train$ele <- NA
houses_test$ele <- NA

for (i in c("ASCENSOR","ELEVADOR","ASCENSORES","ELEVADORES")){
  houses_train$ele=ifelse(is.na(houses_train$ele)==T & str_detect(string=houses_train$description , pattern=i)==T,1,houses_train$ele)
  houses_test$ele=ifelse(is.na(houses_test$ele)==T & str_detect(string=houses_test$description , pattern=i)==T,1,houses_test$ele)
}

##############################
##Sala de estar:Dummies
##############################

houses_train$estar <- NA
houses_test$estar <- NA

for (i in c("ESTAR","SALA","SALÓN DE TELEVISIÓN")){
  houses_train$estar=ifelse(is.na(houses_train$estar)==T & str_detect(string=houses_train$description , pattern=i)==T,1,houses_train$estar)
  houses_test$estar=ifelse(is.na(houses_test$estar)==T & str_detect(string=houses_test$description , pattern=i)==T,1,houses_test$estar)
}


##############################
##Comedor:Dummies
##############################


houses_train$comedor <- NA
houses_test$comedor <- NA

for (i in c("COMEDOR","MESÓN","ISLA")){
  houses_train$comedor=ifelse(is.na(houses_train$comedor)==T & str_detect(string=houses_train$description , pattern=i)==T,1,houses_train$comedor)
  houses_test$comedor=ifelse(is.na(houses_test$comedor)==T & str_detect(string=houses_test$description , pattern=i)==T,1,houses_test$comedor)
}

##############################
##Lavanderia:Dummies
##############################

houses_train$lavan <- NA
houses_test$lavan <- NA

for (i in c("LAVANDERÍA","ROPAS","ROPA","LAVAR","LAVADORA")){
  houses_train$lavan=ifelse(is.na(houses_train$lavan)==T & str_detect(string=houses_train$description , pattern=i)==T,1,houses_train$lavan)
  houses_test$lavan=ifelse(is.na(houses_test$lavan)==T & str_detect(string=houses_test$description , pattern=i)==T,1,houses_test$lavan)
}

##############################
##Estudio:Dummies
##############################


houses_train$estudio <- NA
houses_test$estudio <- NA

for (i in c("ESTUDIO","ESTUDIAR")){
  houses_train$estudio=ifelse(is.na(houses_train$estudio)==T & str_detect(string=houses_train$description , pattern=i)==T,1,houses_train$estudio)
  houses_test$estudio=ifelse(is.na(houses_test$estudio)==T & str_detect(string=houses_test$description , pattern=i)==T,1,houses_test$estudio)
}


##############################
##Cocina:Dummies
##############################


houses_train$cocina <- NA
houses_test$cocina <- NA

for (i in c("COCINA")){
  houses_train$cocina=ifelse(is.na(houses_train$cocina)==T & str_detect(string=houses_train$description , pattern=i)==T,1,houses_train$cocina)
  houses_test$cocina=ifelse(is.na(houses_test$cocina)==T & str_detect(string=houses_test$description , pattern=i)==T,1,houses_test$cocina)
}

##############################
##Portería y Seguridad:Dummies
##############################

houses_train$port <- NA
houses_test$port <- NA

for (i in c("PORTERÍA","PORTERIA","RECEPCION","RECEPCIÓN","SEGURIDAD PRIVADA","VIGILANTE","VIGILANCIA")){
  houses_train$port=ifelse(is.na(houses_train$port)==T & str_detect(string=houses_train$description , pattern=i)==T,1,houses_train$port)
  houses_test$port=ifelse(is.na(houses_test$port)==T & str_detect(string=houses_test$description , pattern=i)==T,1,houses_test$port)
}



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
##OTRAS VARIABLES IMPORTANTES##
# cercanía con universidades
universidad = opq(bbox = getbb("Bogotá Colombia")) %>%
  add_osm_feature(key="amenity" , value="	university") 

universidad_sf = 	universidad %>% osmdata_sf()

university = 	universidad_sf$osm_points %>% select(osm_id,amenity) 

leaflet() %>% addTiles() %>% addCircleMarkers(data=	university , col="red") %>% 
  addCircles(data=houses_bta[1:50,])

matrix_dist_uni <- st_distance(x=houses_bta , y=university)
min_dist_uni <- apply(matrix_dist_uni , 1 , min)
houses_bta$dist_uni = min_dist_uni

# cercanía con mujeres de la vida galante y moteles
burdel = opq(bbox = getbb("Bogotá Colombia")) %>%
  add_osm_feature(key="amenity" , value="	brothel") 

burdel_sf = 	burdel %>% osmdata_sf()

brothel = 	burdel_sf$osm_points %>% select(osm_id,amenity) 

leaflet() %>% addTiles() %>% addCircleMarkers(data=	brothel , col="red") %>% 
  addCircles(data=houses_bta[1:50,])

matrix_dist_put <- st_distance(x=houses_bta , y=brothel)
min_dist_put <- apply(matrix_dist_put , 1 , min)
houses_bta$dist_put = min_dist_put

motel = opq(bbox = getbb("Bogotá Colombia")) %>%
  add_osm_feature(key="amenity" , value="	love_hotel") 

motel_sf = 	motel %>% osmdata_sf()

love_hotel = 	motel_sf$osm_points %>% select(osm_id,amenity) 

leaflet() %>% addTiles() %>% addCircleMarkers(data=	love_hotel , col="red") %>% 
  addCircles(data=houses_bta[1:50,])

matrix_dist_mot <- st_distance(x=houses_bta , y=love_hotel)
min_dist_mot <- apply(matrix_dist_mot , 1 , min)
houses_bta$dist_mot = min_dist_mot

# cercanía con cárceles
carcel = opq(bbox = getbb("Bogotá Colombia")) %>%
  add_osm_feature(key="amenity" , value="	prison") 

carcel_sf = 	carcel %>% osmdata_sf()

prison = 	carcel_sf$osm_points %>% select(osm_id,amenity) 

leaflet() %>% addTiles() %>% addCircleMarkers(data=	prison , col="red") %>% 
  addCircles(data=houses_bta[1:50,])

matrix_dist_pri <- st_distance(x=houses_bta , y=prison)
min_dist_pri <- apply(matrix_dist_pri , 1 , min)
houses_bta$dist_pri = min_dist_pri

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


crimen_cali <- read.csv("D:/noveno semestre/big data/problem set 3/dataPS3/crimen_cali.csv", sep = ';')

crimen_cali <- crimen_cali %>%
  filter(VIGENCIA==2019)

crimen_cali <- crimen_cali %>% group_by(COD_BARRIO) %>% summarise(crimen = sum(CANTIDAD))

inter_cal <- rename(inter_cal,  COD_BARRIO = id_barrio)

df= inter_cal %>% inner_join(crimen_cali,by="COD_BARRIO")

##################################################################
################### unir los dataframes ##########################
##################################################################


houses_train <- rbind(houses_med, houses_bta).

