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


library(kableExtra)
library("readxl")

#No abrir esto

{setwd("C:/Users/mrozo/OneDrive - Universidad de los Andes/Maestría/Big Data/BDML_ProblemSet3-main/dataPS3")


houses_train2 <- read_csv(file="houses_train_final2.csv")
houses_test <- read_excel("houses_tests.xlsx")


houses_train <- st_as_sf(x = houses_train, ## datos
                         coords=c("lon","lat"), ## coordenadas
                         crs=4326) ## CRS

houses_test <- st_as_sf(x = houses_test, ## datos
                        coords=c("lon","lat"), ## coordenadas
                        crs=4326) ## CRS



########################################################
############### MODELOS ###############################
#######################################################

##########################
##### Ajuste de base #####
##########################
houses_train <- houses_train[,-1]
houses_test <- houses_test[,-1]


houses_train <- subset(houses_train, select = -c(edad,new_surface,pisos))
houses_test <- subset(houses_test, select = -c(zona,numbers,edad,pisos))

houses_train <- subset(houses_train, select = -c(habi,zona))
houses_test <- subset(houses_test, select = -c(habi))
houses_train <- subset(houses_train, select = -c(city))
houses_test <- subset(houses_test, select = -c(city))
houses_train <- subset(houses_train, select = -c(baños))
houses_test <- subset(houses_test, select = -c(baños))

houses_train$property_type<-ifelse(houses_train$property_type=="APARTAMENTO AGRUPACIÓN","APARTAMENTO",houses_train$property_type)
houses_train$property_type<-ifelse(houses_train$property_type=="CASA AGRUPACIÓN","CASA",houses_train$property_type)


houses_train$property_type= as.factor(houses_train$property_type)
houses_test$property_type= as.factor(houses_test$property_type)

houses_train <- houses_train %>%
  mutate(garages = if_else(is.na(garages), 0, garages))

houses_test <- houses_test %>%
  mutate(garages = if_else(is.na(garages), 0, garages))

houses_train <- houses_train %>%
  mutate(terra = if_else(is.na(terra), 0, terra))

houses_test <- houses_test %>%
  mutate(terra = if_else(is.na(terra), 0, terra))

houses_train <- houses_train %>%
  mutate(gym = if_else(is.na(gym), 0, gym))

houses_test <- houses_test %>%
  mutate(gym = if_else(is.na(gym), 0, gym))


houses_train <- houses_train %>%
  mutate(social = if_else(is.na(social), 0, social))

houses_test <- houses_test %>%
  mutate(social = if_else(is.na(social), 0, social))


houses_train <- houses_train %>%
  mutate(parks = if_else(is.na(parks), 0, parks))

houses_test <- houses_test %>%
  mutate(parks = if_else(is.na(parks), 0, parks))


houses_train <- houses_train %>%
  mutate(deposit = if_else(is.na(deposit), 0, deposit))

houses_test <- houses_test %>%
  mutate(deposit = if_else(is.na(deposit), 0, deposit))


houses_train <- houses_train %>%
  mutate(renov = if_else(is.na(renov), 0, renov))

houses_test <- houses_test %>%
  mutate(renov = if_else(is.na(renov), 0, renov))


houses_train <- houses_train %>%
  mutate(chim = if_else(is.na(chim), 0, chim))

houses_test <- houses_test %>%
  mutate(chim = if_else(is.na(chim), 0, chim))

houses_train <- houses_train %>%
  mutate(ele = if_else(is.na(ele), 0, ele))

houses_test <- houses_test %>%
  mutate(ele = if_else(is.na(ele), 0, ele))


houses_train <- houses_train %>%
  mutate(estar = if_else(is.na(estar), 0, estar))

houses_test <- houses_test %>%
  mutate(estar = if_else(is.na(estar), 0, estar))


houses_train <- houses_train %>%
  mutate(comedor = if_else(is.na(comedor), 0, comedor))

houses_test <- houses_test %>%
  mutate(comedor = if_else(is.na(comedor), 0, comedor))


houses_train <- houses_train %>%
  mutate(lavan = if_else(is.na(lavan), 0, lavan))

houses_test <- houses_test %>%
  mutate(lavan = if_else(is.na(lavan), 0, lavan))

houses_train <- houses_train %>%
  mutate(estudio = if_else(is.na(estudio), 0, estudio))

houses_test <- houses_test %>%
  mutate(estudio = if_else(is.na(estudio), 0, estudio))


houses_train <- houses_train %>%
  mutate(cocina = if_else(is.na(cocina), 0, cocina))

houses_test <- houses_test %>%
  mutate(cocina = if_else(is.na(cocina), 0, cocina))

houses_train <- houses_train %>%
  mutate(port = if_else(is.na(port), 0, port))

houses_test <- houses_test %>%
  mutate(port = if_else(is.na(port), 0, port))

houses_train <- houses_train %>%
  mutate(rooms = if_else(is.na(rooms), mean(rooms, na.rm=TRUE), rooms))

houses_test <- houses_test %>%
  mutate(rooms = if_else(is.na(rooms), mean(rooms, na.rm=TRUE), rooms))

houses_train <- houses_train %>%
  mutate(bathrooms = if_else(is.na(bathrooms), mean(bathrooms, na.rm=TRUE), bathrooms))

houses_test <- houses_test %>%
  mutate(bathrooms = if_else(is.na(bathrooms), mean(bathrooms, na.rm=TRUE), bathrooms))

houses_train <- houses_train %>%
  mutate(surface_total = ifelse(surface_total>198000,NA, surface_total))

houses_test <- houses_test %>%
  mutate(surface_total = ifelse(surface_total>198000,NA, surface_total))

houses_train <- houses_train %>%
  mutate(surface_total = if_else(is.na(surface_total),mean(surface_total, na.rm=TRUE), surface_total))

houses_test <- houses_test %>%
  mutate(surface_total = if_else(is.na(surface_total),mean(surface_total, na.rm=TRUE), surface_total))

houses_train <- houses_train %>%
  mutate(crimen_norm = if_else(crimen_norm="          NA",mean(crimen_norm, na.rm=TRUE), crimen_norm))

houses_test <- houses_test %>%
  mutate(crimen_norm = if_else(is.na(crimen_norm),mean(crimen_norm, na.rm=TRUE), crimen_norm))

###########################
#### Casos completos ######
###########################
houses_train<-houses_train[complete.cases(houses_train), ]###Quita muchas observaciones


###############################################
#### Casos completos: Quitando variables ######
###############################################

colSums(is.na(houses_train))
houses_train <- subset(houses_train, select = -c(habi,zona))
houses_test <- subset(houses_test, select = -c(habi))
houses_train <- subset(houses_train, select = -c(city))
houses_test <- subset(houses_test, select = -c(city))
houses_train <- subset(houses_train, select = -c(city))
houses_test <- subset(houses_test, select = -c(city))

houses_train$property_type<-ifelse(houses_train$property_type=="APARTAMENTO AGRUPACIÓN","APARTAMENTO",houses_train$property_type)
houses_train$property_type<-ifelse(houses_train$property_type=="CASA AGRUPACIÓN","CASA",houses_train$property_type)

colSums(is.na(houses_test))

estrato_train <- read_csv(file="train_estrato.csv")
estrato_test <- read_csv(file="test_estrato.csv")

crimen_train <- read_csv(file="crimens_train.csv")
crimen_test <- read_csv(file="crimen_test.csv")


estrato_train <- subset(estrato_train, select = -c(geometry))
estrato_test <- subset(estrato_test, select = -c(geometry))

houses_train <- subset(houses_train, select = -c(geometry))
houses_test <- subset(houses_test, select = -c(geometry))

houses_train <- cbind(houses_train, estrato_train)
houses_test <- cbind(houses_test, estrato_test)


houses_train <- cbind(houses_train,crimen_train)
houses_test <- cbind(houses_test,crimen_test)



#######################################
#####Unión de crimen por Stata#########
#######################################

write_csv(houses_train2,file="houses_train_finalisimo.csv")
write_csv(houses_test,file="houses_test_finalisimo.csv")}

######################################################
########### A partir de aquí deben correr ############
######################################################

setwd("la carpeta donde tengan los datos")

houses_train<-read_csv(file="houses_train_finalisimo.csv")
houses_test<-read_csv(file="houses_test_finalisimo.csv")




##############################################
####### Ejemplo A Réplicar: Linear model #####
##############################################

################################################################################################################
######### Consideraciones: Se ajustaron algunas variables para que la base sea más completa y toda corra, ######
######### inicialmente plantee como fuera de muestra las pocas observaciones de Cali con precios y completas. ##
######### No obstante, podemos hacer un validación cruzada, LOOCV o partir en 70-30 base de entrenamiento.######
################################################################################################################
################################################################################################################
######### Ajustes de variables: Para las dummies se reemplazo missing con 0 y para los valores que eran   ######
######### continuos se reemplzao con la media para evitar la pérdida de la mayoria de las observaciones.      ##
################################################################################################################


modelo1 <- lm(formula = price ~ ., data = houses_train)

#Métricas de predicción y precios(dentro de muestra)
insample_mse1= mean(modelo1$residuals^2)
insample_rtmse1= sqrt(mean(modelo1$residuals^2))
insample_total1real=sum(modelo1$model$price)/1000000000000 #billones de pesos
insample_total1pred=sum(modelo1$fitted.values)/1000000000000 #billones de pesos

#Métricas de predicción y precios(fuera de muestra) ##Versión contra Cali, no dividiendo la muestra de entrenamiento
houses_test<-houses_test[complete.cases(houses_test$price), ]
outsample_modelo1=predict(modelo1,houses_test)
outsample_modelo1=ifelse(is.na(outsample_modelo1),houses_test$price,outsample_modelo1)
outsample_mse1= mean((houses_test$price-outsample_modelo1)^2)
outsample_rtmse1= sqrt(outsample_mse1)
outsample_total1real=sum(houses_test$price)/1000000000000 #billones de pesos
outsample_total1pred=sum(outsample_modelo1)/1000000000000 #billones de pesos




metricas_insample1 <- data.frame(Modelo = "Linear Model", 
                                 "Evaluación" = "Dentro de muestra",
                                  "MSE"=insample_mse1,
                                  "RMSE"=insample_rtmse1,
                                 "Costo_Billones_Real"=insample_total1real,
                                 "Costo_Billones_Predicho"=insample_total1pred)

metricas_outsample1 <- data.frame(Modelo = "Linear Model", 
                                 "Evaluación" = "Fuera de muestra-Test Cali",
                                 "MSE"=outsample_mse1,
                                 "RMSE"=outsample_rtmse1,
                                 "Costo_Billones_Real"=outsample_total1real,
                                 "Costo_Billones_Predicho"=outsample_total1pred)


metricas1 <- bind_rows(metricas_insample1, metricas_outsample1)
metricas <- bind_rows(metricas1)

metricas %>%
  kbl(digits = 2)  %>%
  kable_styling(full_width = T)





















#################################



model5 <- train(price ~ poly(bedrooms,2):poly(bathrooms,3):centair:fireplace
                :brick:lnland:lnbldg+garage1+garage2+rr+
                  yrbuilt+factor(carea)+poly(latitude,8):poly(longitude,8),
                data = matchdata,
                trControl = trainControl(method = "cv", number = 5),
                method = "lm")

fitControl <- trainControl(
  method = 'cv', 
  summaryFunction = fiveStats,     # k-fold cross validation
  number = 5,                      # number of folds
  savePredictions = 'final',       # saves predictions for optimal tuning parameter
  returnResamp = "all"
)


loocvrmse2 <- NULL
for(i in 1:nrow(datap3c)){
  #you did this part right
  testcv<-datap3c[i,]
  traincv<-datap3c[-i,]
  modelo5<-lm("res_y_a~res_s_a-1", data=traincv, weights= w)
  testcv$modelo5<-predict(modelo5,newdata = testcv)
  rmse<-with(testcv,sqrt((y-modelo5)^2))
  loocvrmse2[i]<-rmse}
rmsemodelos2[2,] <- c(2,mean(loocvrmse2))
