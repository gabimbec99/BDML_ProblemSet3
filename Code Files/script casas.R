vignette(package = "sf") # Ver viñetas disponibles

## llamar pacman (contiene la función p_load)
require(pacman)
require(caret)
## llamar y/o instalar librerias
p_load(tidyverse,rio,skimr,
       sf, ## datos espaciales
       leaflet, ## visualizaciones
       tmaptools, ## geocodificar
       ggsn, ## map scale bar 
       osmdata) ## packages with census data


library(kableExtra)
library(glmnet)

#######################################################################
########### Nota: En otro documento tengo todo lo que hice ############
############ para dejar base así, yo lo subo al GitHub después ########
#######################################################################

setwd("C:/Users/mrozo/OneDrive - Universidad de los Andes/Maestría/Big Data/BDML_ProblemSet3-main/dataPS3")

houses_train<-read_csv(file="houses_train_final.csv")
houses_test<-read_csv(file="houses_test_finalf.csv")

houses_train <- subset(houses_train, select = -c(baños))
houses_test$property_id <- houses_test$property_id...60
houses_test <- subset(houses_test, select = -c(property_id))
houses_train <- subset(houses_train, select = -c(property_id))

houses_train$property_type<-ifelse(houses_train$property_type=="APARTAMENTO AGRUPACIÓN","APARTAMENTO",houses_train$property_type)
houses_train$property_type<-ifelse(houses_train$property_type=="CASA AGRUPACIÓN","CASA",houses_train$property_type)

houses_train$property_type= as.factor(houses_train$property_type)
houses_test$property_type= as.factor(houses_test$property_type)



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

names(houses_test)[names(houses_test) == "_merge"] = "m"

names(houses_test) <- tolower(names(houses_test))

##############################################
####### Linear model #########################
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
houses_test<-houses_test[complete.cases(houses_test$price), ]

modelo1 <- lm(formula = price ~ ., data = houses_train)

#Métricas de predicción y precios(dentro de muestra)
insample_mse1= mean(modelo1$residuals^2)
insample_rtmse1= sqrt(mean(modelo1$residuals^2))
insample_total1real=sum(modelo1$model$price)/1000000000 #billones de pesos
insample_total1pred=sum(modelo1$fitted.values)/1000000000 #billones de pesos

#Métricas de predicción y precios(fuera de muestra) ##Versión contra Cali, no dividiendo la muestra de entrenamiento

outsample_modelo1=predict(modelo1,houses_test)
outsample_mse1= mean((houses_test$price-outsample_modelo1)^2)
outsample_rtmse1= sqrt(outsample_mse1)
outsample_total1real=sum(houses_test$price)/1000000000 #billones de pesos
outsample_total1pred=sum(outsample_modelo1)/1000000000 #billones de pesos -mil millones




metricas_insample1 <- data.frame(Modelo = "Linear Model", 
                                 "Evaluación" = "Dentro de muestra",
                                  "MSE"=insample_mse1,
                                  "RMSE"=insample_rtmse1,
                                 "Costo_MMillones_Real"=insample_total1real,
                                 "Costo_MMillones_Predicho"=insample_total1pred)

metricas_outsample1 <- data.frame(Modelo = "Linear Model", 
                                 "Evaluación" = "Fuera de muestra-Test Cali",
                                 "MSE"=outsample_mse1,
                                 "RMSE"=outsample_rtmse1,
                                 "Costo_MMillones_Real"=outsample_total1real,
                                 "Costo_MMillones_Predicho"=outsample_total1pred)


metricas1 <- bind_rows(metricas_insample1, metricas_outsample1)
metricas <- bind_rows(metricas1)

metricas %>%
  kbl(digits = 2)  %>%
  kable_styling(full_width = T)


##############################################
####### Elastic Net ##########################
##############################################
x<-subset(houses_train, select = -c(price))
y<-subset(houses_train, select = c(price))
x_test<-subset(houses_test, select = -c(price))

modelo2 <- cv.glmnet(data.matrix(x),data.matrix(y))

modelo2fitted <- predict(modelo2, data.matrix(x), s = 'lambda.1se')
modelo2residuals <- y - modelo2fitted

#Métricas de predicción y precios(dentro de muestra)
insample_mse2= mean(modelo2residuals$price^2)
insample_rtmse2= sqrt(mean(modelo2residuals$price^2))
insample_total2real=sum(y)/1000000000 #billones de pesos
insample_total2pred=sum(modelo2fitted)/1000000000 #billones de pesos

#Métricas de predicción y precios(fuera de muestra) 



outsample_modelo2=as.data.frame(predict(modelo2,data.matrix(x_test)))
outsample_mse2= mean((houses_test$price-outsample_modelo2$lambda.1se)^2)
outsample_rtmse2= sqrt(outsample_mse2)
outsample_total2real=sum(houses_test$price)/1000000000 #billones de pesos
outsample_total2pred=sum(outsample_modelo2)/1000000000 #billones de pesos




metricas_insample2 <- data.frame(Modelo = "GLM-Net", 
                                 "Evaluación" = "Dentro de muestra",
                                 "MSE"=insample_mse2,
                                 "RMSE"=insample_rtmse2,
                                 "Costo_MMillones_Real"=insample_total2real,
                                 "Costo_MMillones_Predicho"=insample_total2pred)

metricas_outsample2 <- data.frame(Modelo = "GLM-Net", 
                                  "Evaluación" = "Fuera de muestra-Test Cali",
                                  "MSE"=outsample_mse2,
                                  "RMSE"=outsample_rtmse2,
                                  "Costo_MMillones_Real"=outsample_total2real,
                                  "Costo_MMillones_Predicho"=outsample_total2pred)


metricas2 <- bind_rows(metricas_insample2, metricas_outsample2)

metricas <- bind_rows(metricas1,metricas2)

metricas %>%
  kbl(digits = 2)  %>%
  kable_styling(full_width = T)




##############################################
####### Random Forest #########################
##############################################
x<-subset(houses_train, select = -c(price))
y<-subset(houses_train, select = c(price))
x_test<-subset(houses_test, select = -c(price))

install.packages("randomForest")
library(randomForest)
fiveStats <- function(...) c(twoClassSummary(...), defaultSummary(...))


fitControl <- trainControl(
  method = 'cv', 
  summaryFunction = fiveStats,     # k-fold cross validation
  number = 10,                      # number of folds
  savePredictions = 'final',       # saves predictions for optimal tuning parameter
  returnResamp = "all"
) 

modelo3 <- caret::train(x,y$price, method = "ranger",
                        preProcess = c("center", "scale"),
                        ntrees = 100, metric="RMSE")

modelo3fitted <- predict(odelo3, x)

#Métricas de predicción y precios(dentro de muestra)
insample_mse3= odelo3$results$RMSE[6]^2
insample_rtmse3=odelo3$results$RMSE[6]
insample_total3real=sum(y)/1000000000 #billones de pesos
insample_total3pred=sum(modelo3fitted)/1000000000 #billones de pesos

#Métricas de predicción y precios(fuera de muestra) 

outsample_modelo3=as.data.frame(predict(odelo3,x_test))
names(outsample_modelo3)[names(outsample_modelo3) == "predict(odelo3,x_test)"] = "predict"

outsample_mse3= mean((houses_test$price-outsample_modelo3$`predict(odelo3, x_test)`)^2)

outsample_rtmse3= sqrt(outsample_mse3)
outsample_total3real=sum(houses_test$price)/1000000000 #billones de pesos
outsample_total3pred=sum(outsample_modelo3)/1000000000 #billones de pesos


metricas_insample3 <- data.frame(Modelo = "RF", 
                                 "Evaluación" = "Dentro de muestra",
                                 "MSE"=insample_mse3,
                                 "RMSE"=insample_rtmse3,
                                 "Costo_MMillones_Real"=insample_total3real,
                                 "Costo_MMillones_Predicho"=insample_total3pred)

metricas_outsample3 <- data.frame(Modelo = "RF", 
                                  "Evaluación" = "Fuera de muestra-Test Cali",
                                  "MSE"=outsample_mse3,
                                  "RMSE"=outsample_rtmse3,
                                  "Costo_MMillones_Real"=outsample_total3real,
                                  "Costo_MMillones_Predicho"=outsample_total3pred)


metricas3 <- bind_rows(metricas_insample3, metricas_outsample3)

metricas <- bind_rows(metricas1,metricas2,metricas3)

metricas %>%
  kbl(digits = 2)  %>%
  kable_styling(full_width = T)





##############################################
####### XGBoost ##############################
##############################################


modelo4<-read_csv(file="xgb_dentro.csv")
modelo4_outsample<-read_csv(file="xgb_cali_test.csv")

modelo4residuals <- modelo4$real - modelo4$dento_muestra

#Métricas de predicción y precios(dentro de muestra)
insample_mse4=mean(modelo4residuals^2)
insample_rtmse4=sqrt(insample_mse4)
insample_total4real=sum(modelo4$real)/1000000000 #billones de pesos
insample_total4pred=sum(modelo4$dento_muestra)/1000000000 #billones de pesos

#Métricas de predicción y precios(fuera de muestra) 
modelo4residuals2 <- modelo4_outsample$real - modelo4_outsample$cali_test

outsample_mse4=mean(modelo4residuals2^2)
outsample_rtmse4=sqrt(outsample_mse4)
outsample_total4real=sum(modelo4_outsample$real)/1000000000 #billones de pesos
outsample_total4pred=sum(modelo4_outsample$cali_test)/1000000000 #billones de pesos

metricas_insample4 <- data.frame(Modelo = "XGBOOST", 
                                 "Evaluación" = "Dentro de muestra",
                                 "MSE"=insample_mse4,
                                 "RMSE"=insample_rtmse4,
                                 "Costo_MMillones_Real"=insample_total4real,
                                 "Costo_MMillones_Predicho"=insample_total4pred)

metricas_outsample4 <- data.frame(Modelo = "XGBOOST", 
                                  "Evaluación" = "Fuera de muestra-Test Cali",
                                  "MSE"=outsample_mse4,
                                  "RMSE"=outsample_rtmse4,
                                  "Costo_MMillones_Real"=outsample_total4real,
                                  "Costo_MMillones_Predicho"=outsample_total4pred)


metricas4 <- bind_rows(metricas_insample4, metricas_outsample4)

metricas <- bind_rows(metricas1,metricas2,metricas3,metricas4)

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
  number = 10,                      # number of folds
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
