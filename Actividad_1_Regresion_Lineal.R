# Instalacion de paquetes(no hacer si ya se tienen)
install.packages("readr")
install.packages("ggplot2")
install.packages("tidyverse")
install.packages("GGally")
install.packages('corrplot')
Install.packages('car') 
install.packages("carData") 
install.packages("scatterplot3d")
install.packages("plotly")
install.packages("dplyr")

# Cargado de librerias
library(readr)
library(ggplot2)
library(tidyverse)
library(dplyr)
library(car)
library(scatterplot3d)
library(plotly)
library(MASS)
library(plotly)
library(corrplot)

# Cargar el dataset
HousePrices <- read_csv("Desktop/MasterBigData/0_Estadistica_Adv/Actividad/HousePrices.csv")
View(HousePrices)

#Seleccion de columnas numericas
numeric_data <- HousePrices %>% select(where(is.numeric))

# Verificacion valores nulos
any(is.na(numeric_data))

# Reemplazar valores nulos con la media de la columna
numeric_data <- numeric_data %>% mutate(across(everything(), ~ ifelse(is.na(.), mean(., na.rm = TRUE), .)))

# Visualizar el dataset en otra pestaña en R
View(numeric_data)

# Visualizar los primeros datos y un analisis preliminar
head(numeric_data)
summary(numeric_data) 

#plot de los datos entre todas las columnas
pairs(numeric_data, pch=".")

# Modelo Lineal

plot(SalePrice ~ GrLivArea, data = numeric_data,
     xlab = "Calidad de los acabados", 
     ylab = "Precio de venta", 
     main = "Price vs Calidad", 
     pch = 20,
     cex = 2,
     col = "grey")

#GrLivArea: 
modelo_lineal <- lm(SalePrice ~ GrLivArea, data = numeric_data)
summary(modelo_lineal_2)

#GrLivArea sale decente
plot(SalePrice ~ GrLivArea, data = numeric_data,
     xlab = "Superficie habitable(sf)", 
     ylab = "Precio de venta ($)", 
     main = "Price vs Area", 
     pch = 20,
     cex = 1,
     col = "blue")
abline(modelo_lineal, col = "darkorange")

plot(modelo_lineal, which = 1) #Linealidad
plot(modelo_lineal, which = 2) #Muestra si los residuos se distribuyen normalmente
plot(modelo_lineal, which = 3) #Homocedasticidad: como de constante es la varianza de los residuos
plot(modelo_lineal, which = 4) #Residuals vs. leverage: La distancia de cook
plot(modelo_lineal, which = 5) #Residuals vs. leverage: para encontrar observaciones influyentes

confint(modelo_lineal, level = 0.99)

modelo_lineal$residuals

#Analisis de correlaccion
M = cor(numeric_data) 
corrplot.mixed(M, order = 'AOE')

#Modelo multilineal (preliminar)
modelo_multilineal <- lm(SalePrice ~ OverallQual + GrLivArea, data = numeric_data)
summary(modelo_multilineal)

# Modelo lineal stepwise
# Ajustar el modelo completo con todas las variables numéricas
modelo_completo <- lm(SalePrice ~ ., data = numeric_data)
summary(modelo_completo)

# Realizar la selección de variables con stepwise
modelo_stepwise <- step(modelo_completo, direction = "both", k = 25)

modelo_final <- lm(formula(modelo_stepwise), data = numeric_data)
summary(modelo_final)

# Calcular los residuos y los valores predichos del modelo final
numeric_data$predicted_final <- predict(modelo_final)
numeric_data$residuals_final <- residuals(modelo_final)

# Crear el gráfico de dispersión 3D con los residuos proyectados solo sobre el eje SalePrice
plot <- plot_ly(numeric_data, x = ~GrLivArea, y = ~OverallQual, z = ~SalePrice, 
                type = 'scatter3d', mode = 'markers', marker = list(color = 'blue')) %>%
  # Añadir el plano de regresión
  add_trace(x = ~GrLivArea, y = ~OverallQual, z = ~predicted_final, type = 'mesh3d', opacity = 0.5, 
           color = I('blue'), cex = 1) %>%
  # Añadir líneas de los residuos proyectadas sobre el eje SalePrice
  add_trace(x = rep(numeric_data$GrLivArea, each = 2), 
            y = rep(numeric_data$OverallQual, each = 2),
            z = c(rbind(numeric_data$SalePrice, numeric_data$predicted_final)), 
            type = 'scatter3d', mode = 'lines', line = list(color = 'black', width = 2)) %>%
  layout(title = 'Modelo Multilineal 3D',
         scene = list(
           xaxis = list(title = 'Superficie habitable (sq ft)'),
           yaxis = list(title = 'Calidad general'),
           zaxis = list(title = 'Precio de venta')
           ))
plot

coef(modelo_final)

summary(modelo_final)

plot(modelo_final ,which = 1)
plot(modelo_final ,which = 2)
plot(modelo_final ,which = 3)
plot(modelo_final ,which = 4)
plot(modelo_final ,which = 5)

# VIF del modelo multilineal con 10 variables
vif (modelo_final)
vif_values <- vif(modelo_final)
par(mar = c(5, 8, 4, 2))
barplot(vif_values , main = 'VIF Values', horiz = TRUE, col = 'steelblue', las = 1)
abline(v = 1, lwd = 3, lty = 2)


# Regresión logística

numeric_data$CentralAir <- ifelse(HousePrices$CentralAir == 'Y', 1, 0)

fit_glm = glm(CentralAir ~ SalePrice, data = numeric_data, family = binomial)
fit_glm


plot(CentralAir ~ SalePrice, data = numeric_data,
     pch = 20, ylab = "Probabilad de Aire Centralizado", main = "Regresion Logística",
     xlim = c(-50000, max(numeric_data$SalePrice)))
grid()

curve(predict(fit_glm, newdata = data.frame(SalePrice = x), type = "response"),
      add = TRUE, col = "dodgerblue", lty = 2)

legend("bottomright", c("Log Regresion", "Data"), lty = c(1, 2, 0),
       pch = c(NA, NA, 20), lwd = 2, col = c("dodgerblue", "black")) 
