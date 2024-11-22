#Importacion de librerias
library(readr)      # Para leer el CSV
library(tidyr)
library(dplyr)      # Para manipulación de datos
library(ggplot2)    # Para visualización
library(forecast)   # Para análisis de series temporales
library(tseries)    # Para pruebas estadísticas en series temporales
library(lubridate)  # modificar los datos de fechas y dias
library(gridExtra)  # Para graficcos ggplot


#Importación de los datos
setwd("/Users/MuzDog/Desktop/MasterBigData/0_Estadistica_Adv/Actividad_2")
data <- read_csv("export (1).csv")


#View(data)
head(data)

data$date <- as.Date(data$date, format="%Y-%m")

str(data)
summary(data)

#Preprocesamiento (Limpieza filas y columnas vacias):
colSums(is.na(data)) #Comprobamos si hay valores nulos
data_clean <- data %>% drop_na(tavg, tmin, tmax) # Limpieza filas vacias
data_clean <- data_clean %>% select(-snow, -wdir, -wspd, -wpgt, -pres, -tsun) # Limpieza columnas vacias
colSums(is.na(data_clean))

# Visualizacion inicial

plot1 <- ggplot(data_clean, aes(x = date)) +
  geom_line(aes(y = tmax, color = "Tmax")) +
  geom_line(aes(y = tmin, color = "Tmin")) +
  geom_line(aes(y = tavg, color = "Tavg")) +
  labs(title = "Variables Meteorológicas en Madrid (2015-2024)",
       x = "Fecha", y = "Valor") +
  scale_color_manual(values = c("Tmax" = "red", "Tmin" = "green", "Tavg" = "yellow")) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

plot2 <- ggplot(data_clean, aes(x = date)) +
  geom_line(aes(y = prcp, color = "Precipitation")) +
  labs(x = "Fecha", y = "Precipitación (mm)") +
  scale_color_manual(values = c("Precipitation" = "blue")) +
  theme_minimal()

grid.arrange(plot1, plot2, ncol = 1)

#Visallizacion de tavg
tavg.ts <-ts(data_clean[,2], start = c(2015, 305), freq = 365)

#Decompose:
plot(decompose(tavg.ts))

# Serie

acf(tavg.ts, main="Función de Autocorrelación (ACF)", lwd=4, col=4) 
pacf(tavg.ts, main="Función de Autocorrelación Parcial (PACF)", lwd=4, col=4) # p = 10

# Aplicamos diferenciación
# Primero se hace la diferenciación estacional

ts_data_seasonal_diff <- diff(tavg.ts, lag = 365) # Diferenciación estacional

plot(ts_data_seasonal_diff)

plot(decompose(ts_data_seasonal_diff))

acf(ts_data_seasonal_diff, main="ACF de la Serie Diferenciada", lwd=4, col=4) # q = Mal
pacf(ts_data_seasonal_diff, main="PACF de la Serie Diferenciada", lwd=4, col=4) # P = 3

# A continuacion se hace diferenciacion no estacional

ts_tavg_diff = diff(ts_data_seasonal_diff) # Diferenciacion no estacional

plot(ts_tavg_diff)

plot(decompose(ts_tavg_diff))

acf(ts_tavg_diff, main="ACF de la Serie Diferenciada", lwd=4, col=4) # q = 6
pacf(ts_tavg_diff, main="PACF de la Serie Diferenciada", lwd=4, col=4) # p = 8

# Hasta aqui, se ve que el mejor ajuste es c(8,1,6,*,1,*)
fit <- auto.arima(tavg.ts, seasonal = TRUE)
summary(fit)

#Problemas con el lag: No puede haber un lag mayor que 350... Se agrupa por mes para tener un lag mas pequeño.

# Modificamos los datos para que se agrupen por mes en lugar de por dia:
data_clean$AñoMes <- format(data_clean$date, "%Y-%m") # Crear una nueva columna que solo tenga el año y el mes
data_clean
data_mensual <- data_clean %>% # Calcular la temperatura media para cada mes
  group_by(AñoMes) %>%
  summarise(Temp_Media_Mensual = mean(tavg, na.rm = TRUE), Temp_Minima_Mensual = mean(tmin, na.rm = TRUE), Temp_Maxima_Mensual = mean(tmax, na.rm = TRUE))

data_mensual$AñoMes <- as.Date(paste0(data_mensual$AñoMes, "-01"))
data_mensual
# Mostrar los primeros resultados
head(data_mensual)
View(data_mensual)

#generamos la serie temporal
tavgm.ts <-ts(data_mensual[,2], start = c(2015, 11), freq = 12)

#Decompose:
plot(decompose(tavgm.ts))

acf(tavgm.ts, main="Función de Autocorrelación (ACF)", lwd=4, col=4) #q = 3
pacf(tavgm.ts, main="Función de Autocorrelación Parcial (PACF)", lwd=4, col=4) # p = 6

# Aplicamos diferenciación
# Primero se hace la diferenciación estacional

ts_data_seasonal_diff <- diff(tavgm.ts, lag = 12) # Diferenciación estacional
plot(decompose(ts_data_seasonal_diff))

acf(ts_data_seasonal_diff, main="ACF de la Serie Diferenciada", lwd=4, col=4, lag.max = 50) # Q = 2
pacf(ts_data_seasonal_diff, main="PACF de la Serie Diferenciada", lwd=4, col=4, lag.max = 50) # P = 2

# A continuación se hace diferenciación no estacional

ts_tavgm_diff = diff(ts_data_seasonal_diff)
plot(decompose(ts_tavgm_diff))

acf(ts_tavgm_diff, main="ACF de la Serie Diferenciada", lwd=4, col=4, lag.max = 50) # q = 2
pacf(ts_tavgm_diff, main="PACF de la Serie Diferenciada", lwd=4, col=4, lag.max = 50) # p = 3

#Seleccion del modelo SARIMA

#auto.arima
auto.arima(ts_data_seasonal_diff)
auto.arima(tavgm.ts, seasonal = TRUE)

#funcion optimizada con AIC
get.best.arima <- function(x.ts, maxord = c(1,1,1,1,1,1)) 
{
  best.aic <- Inf
  best.fit <- NULL
  best.model <- NULL
  n <- length(x.ts)
  
  for (p in 0:maxord[1]) for(d in 0:maxord[2]) for(q in 0:maxord[3])
   for (P in 0:maxord[4]) for(D in 0:maxord[5]) for(Q in 0:maxord[6]) 
    {
      # Se ajusta el modelo y captura posibles errores
      fit <- tryCatch({
        arima(x.ts, order = c(p,d,q), 
              seas = list(order = c(P,D,Q), 
                          frequency = frequency(x.ts)))
      }, error = function(e) {
        message("Error en el ajuste del modelo con (", p, d, q, ")(", P, D, Q, "): ", e)
        return(NULL)
      })
      
      # Se realiza AIC y captura de posibles errores
      if (!is.null(fit)) {
        fit.aic <- tryCatch({
          AIC(fit)  # Calcular AIC
        }, error = function(e) {
          message("Error al calcular AIC: ", e)
          return(NA)
        })
        
        message("Valores actuales: p=", p, ", d=", d, ", q=", q, ", P=", P, ", D=", D, ", Q=", Q, " AIC=", fit.aic)
        
        # Imprime nuevo valor AIC y guarda datos nuevos
        if (!is.na(fit.aic) && is.finite(fit.aic) && fit.aic < best.aic) 
        {
          message("Nuevo mejor modelo encontrado con AIC=", best.aic)
          best.aic <- fit.aic
          best.fit <- fit
          best.model <- c(p,d,q,P,D,Q)
        }}}
  list(best.aic,best.fit, best.model)
}

best.arima.tavgm <- best.arima.elec

best.arima.tavgm <- get.best.arima(tavgm.ts, maxord = c(3,1,3,3,1,3)) 
best.fit.tavgm <- best.arima.tavgm[[2]]
acf(resid(best.fit.tavgm))
best.arima.tavgm [[3]]
best.arima.tavgm [[2]]
best.arima.tavgm [[1]]

#El predict:
ts.plot(cbind( window(tavgm.ts, start = c(2015, 11)), predict(best.fit.tavgm, 12)$pred) , lty = 1:2)


#Pruebas de AIC:


AIC (arima(tavgm.ts, order = c(1,1,1), seas = list(order = c(0,1,3), 12))) #AIC = 360.70
AIC (arima(tavgm.ts, order = c(1,0,0), seas = list(order = c(1,1,1), 12))) #AIC = 362.34

