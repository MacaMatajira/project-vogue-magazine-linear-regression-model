
  library(magrittr)
  library(stringr)
  library(lubridate)
  library(scales)
  library(testthat)
  library(readxl)
  library(RColorBrewer)
  library(tidyverse)
  library(knitr)
  library(dplyr)
  library(car)
  library(corrplot)
  library(lmtest)


  #Leer datos archivo excel
  #Renombramos las variables como X1 a X10
  datos <- read_excel("C:/Users/Federico Acosta Leon/Downloads/Revistas.xlsx")
  datos$X9<-factor(datos$X9)#Declarar variable como factor
  datos$X10<-factor(datos$X10)#Declarar variable como factor
  # Adicionalmente, podemos decirle a R cuál queremos que sea la base
  # Si omitimos este paso, R toma como base la categoría que esté en la primera observación
  datos$X9<-relevel(datos$X9,ref="Milán")
  datos$X10<-relevel(datos$X10,ref="Accesorios")
  
  #mirar estructura datos 
  str(datos)
  #ver estadisticas 
  summary(datos)
  
  
  ##MULTICOLINEALIDAD
  
  ## ¿Que es?Xses relacionadas entre si 
  pairs(~Y+X1+X2+X3+X4+X5+X6+X7+X8,data=datos, 
        main="Simple Scatterplot Matrix")
  
  ##Aca vemos patrones lineales, esto nos indica multicolinealidad
  ##Regresión lineal
  reg <- lm(Y ~ ., data = datos)
  summary(reg)
  plot(reg, which=c(1,1))
  
  #Se calcula el VIF
  
  ##Si VIF ES MAYOR A 10 PROBLEMA MULTICOLINEALIDAD
  vif(reg)
  #frame de las variables X
  matriz<-data.frame(datos[,2:9])
  #Matriz de correlaciones 
  matriz_cor <- cor(matriz)
  # Conclusión: Hay segmentos de color oscuro y tamaño considerable, por lo que hay MULTICOLINEALIDAD
  
  corrplot(matriz_cor, method = "circle")
  
  ###solución
  # Crear la nueva variable Z promediando X3 y X5
  datos$Z <- (datos$X3 + datos$X5) / 2
  
  # Regresión sin X5 (utilizando Z en lugar de X3 y X5)
  fit <- lm(Y ~ X1 + X2 + X4 + Z, data = datos)
  summary(fit)
  
  # Evaluar nuevamente VIF
  vif(fit)

#####HETEROCEDASTICIDAD
## Residuales^2 Vs otra cosa
## Residuales vs otro


##residuales vs fitted values se espera que no haya relación que sea similar a linea punteada

plot(reg)
# Contar la cantidad de observaciones en el data frame 'datos'
cantidad_observaciones <- nrow(datos)
# Imprimir el resultado
print(cantidad_observaciones)
plot(reg$fitted.values, rstandard(reg))
reg <- lm(Y ~ X1 + X2 + X3 + X4 + X5 + X6 + X7 + X8 + X9 + X10, data = datos)
# Obtener el término de error del modelo 'reg'
error_term <- residuals(reg)
# Realizar la prueba de Breusch-Pagan
bptest(error_term ~ X1 + X2 + X3 + X4 + X5 + X6 + X7 + X8 + X9 + X10, data = datos)
##Conclusión:Observamos que no rechazo por lo que no hay heterosedasticidad

###NORMALIDAD RESIDUOS
#Gráfico de histograma de los residuos
hist(resid(reg))
 #Gráfico Q-Q de los residuos
qqnorm(resid(reg))
qqline(resid(reg))
shapiro.test(reg$residuals)
##conclusión :no hay problema

##AUTOCORRELACIÓN
#Comparación de residuales
res <- cbind(residuals(reg))
res1 <- c(res[-1], 0)

plot(res, res1)
#Prueba de Durbin-Watson
dwtest(reg)
##Conclusión:No rechazo la hipotesis nula, no tengo autocorrelación

##Especificación
resettest(reg, power = 2:3, type = "fitted", data = datos)
##Conclusión:No tengo especificación




###2. Determine si el efecto de las interacciones en línea es mayor al efecto del alcance que tiene la
#revista y, con base a sus resultados, recomiende a Vogue en cuál de estos dos se debe enfocar
#para maximizar sus ventas.

#Efecto interaccion>efecto alcance
##X4>X8


# Planteamos el modelo de regresión lineal
modelo <- lm(Y ~ X4 + X8, data = datos)

# Obtenemos el resumen del modelo
summary(modelo)

# Realizamos la prueba de hipótesis para comparar los coeficientes
coeftest(modelo, hypothesis = c("X4 <= X8"))

#Conclusión: No se rechaza hipotesis nula por lo que Alcance tiene efecto más fuerte en ventas




# 3. Vogue quiere saber si el impacto en las ventas de una revista que se publica en la ciudad de
# Londres es el doble de una que se publica en París.
# Planteamos el modelo de regresión lineal con interacciones de Londres y París
modelo_interacciones <- lm(Y ~ X1 + X2 + X3 + X4 + X5 + X6 + X7 + X8 + X9:X10, data = datos)

# Obtenemos el resumen del modelo
summary(modelo_interacciones)

# Realizamos la prueba de hipótesis para comparar el efecto de Londres y París
coeftest(modelo_interacciones, hypothesis = c("X9Londres:X10Accesorios = 2*X9París:X10Accesorios"))

#Conclusión: Los valores de la prueba para Londres da 54,164 y la de París da 106,24, por lo cual se rechaza la hipótesis nula 


#4. Confirme o refute si el efecto del la satisfacción del lector sobre las ventas es mayor cuando la
#revista va dirigida a una audiencia interesada en Vestuario, que cuando va dirigida a una
#audiencia interesada en Belleza y Maquillaje.


#Ho:Satisfaccion_vest=Satisfacción_Bell+Satisfacción_Maq
#H1:Satisfaccion_vest>Satisfacción_Bell+Satisfacción_Maq

# Planteamos el modelo de regresión lineal con la interacción entre la satisfacción del lector (X8) y la audiencia (X10)
modelo_interaccion <- lm(Y ~ X1 + X2 + X3 + X4 + X5 + X6 + X7 + X8 + X8:X10, data = datos)

# Obtenemos el resumen del modelo
summary(modelo_interaccion)

# Realizamos la prueba de hipótesis para comparar el efecto de la satisfacción del lector en las ventas entre las audiencias de Vestuario y Belleza y Maquillaje
coeftest(modelo_interaccion, hypothesis = c("X8Vestuario:X10 = X8Belleza y Maquillaje:X10"))

#Conclusión: Satisfacción vestuario es significativa ya que tiene un pvalue chiquito,
#Mientras que Satisfacción Belleza y Maquillaje no son significativas


# Un especialista en alta costura afirma que las top 10 revistas de moda a nivel mundial generan
# ventas de $40.000 USD. Realice intervalos para la media del valor de las ventas de cada una de
# ellas (información disponible en la hoja “Top” del archivo de Excel) y determine para cuáles de
# las revistas la media concuerda con el valor propuesto por el especialista.


# Leer los datos de la hoja "Top" del archivo de Excel
revistas <- read_excel("C:/Users/Federico Acosta Leon/Downloads/Revistas.xlsx", sheet = "Top")

# Calcular la media y el desviación estándar de las ventas de cada revista
medias_ventas <- tapply(revistas$Ventas, revistas$Revista, mean)
desviaciones_ventas <- tapply(revistas$Ventas, revistas$Revista, sd)

# Nivel de confianza para los intervalos (por ejemplo, 95%)
confianza <- 0.95

# Tamaño de la muestra
n <- length(revistas$Ventas)

# Valor propuesto por el especialista
valor <- 40000

# Calcular los intervalos de confianza para la media de las ventas de cada revista
intervalos_confianza <- t.test(revistas$Ventas, conf.level = confianza)$conf.int

# Crear un data frame para las gráficas
datos_graficas <- data.frame(Revista = names(medias_ventas),
                             MediaVentas = medias_ventas,
                             IntervaloInf = intervalos_confianza[1],
                             IntervaloSup = intervalos_confianza[2])

# Determinar para cuáles revistas la media concuerda con el valor propuesto por el especialista
cumplen <- datos_graficas$Revista[abs(datos_graficas$MediaVentas - valor) <= (datos_graficas$IntervaloSup - datos_graficas$IntervaloInf) / 2]

# Imprimir los resultados
print(datos_graficas)
print("Medias de ventas por revista:")
print(medias_ventas)
print("Intervalos de confianza para la media de ventas de cada revista:")
print(intervalos_confianza)
print("Revistas con medias que concuerdan con el valor propuesto por el especialista:")
print(cumplen)






# Gráfico de barras para mostrar las medias de ventas por revista
ggplot(datos_graficas, aes(x = Revista, y = MediaVentas)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_hline(yintercept = valor, linetype = "dashed", color = "red", size = 1) +
  labs(title = "Medias de Ventas por Revista",
       x = "Revista",
       y = "Media de Ventas") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  geom_text(aes(label = MediaVentas), vjust = -0.3, color = "black", size = 4) +
  geom_errorbar(aes(ymin = IntervaloInf, ymax = IntervaloSup), width = 0.2, color = "black", size = 0.7) +
  geom_hline(yintercept = valor, linetype = "dashed", color = "red", size = 1)

# Gráfico de intervalo para visualizar los intervalos de confianza para la media de ventas de cada revista
ggplot(datos_graficas, aes(x = Revista, y = MediaVentas)) +
  geom_point(color = "steelblue", size = 4) +
  geom_errorbar(aes(ymin = IntervaloInf, ymax = IntervaloSup), width = 0.2, color = "black", size = 0.7) +
  geom_hline(yintercept = valor, linetype = "dashed", color = "red", size = 1) +
  labs(title = "Intervalos de Confianza para la Media de Ventas por Revista",
       x = "Revista",
       y = "Media de Ventas") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  geom_hline(yintercept = valor, linetype = "dashed", color = "red", size = 1)
