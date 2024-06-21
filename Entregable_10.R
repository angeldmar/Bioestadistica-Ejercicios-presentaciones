# Set de datos sobre melanoma

# Fisher y Belle (1993) informan sobre las tasas de mortalidad debidas al
# melanoma maligno en hombres blancos durante el período 1950-1969, para
# cada estado en el territorio continental de Estados Unidos. Los datos
# incluyen el número de defunciones por melanoma maligno en el estado
# correspondiente, la longitud y latitud del centro geográfico de cada estado
# y una variable binaria indicando la contigüidad a un océano, es decir, si
# el estado limita con uno de los océanos. Las preguntas de interés sobre
# estos datos incluyen: 
   
# 1. ¿Se parecen las tasas de mortalidad para los estados
# oceánicos y no oceánicos? 

load("melanoma.Rdat")
View(USmelanoma)

# Tratemos los datos
USmelanoma$ocean <- as.factor(USmelanoma$ocean)
str(USmelanoma)
# Separemos datos

oceanUSmelanoma <- subset(USmelanoma, ocean == "yes")
notOceanUSmelanoma <- subset(USmelanoma, ocean == "no")

str(oceanUSmelanoma)
str(notOceanUSmelanoma)

# Probemos test de proporciones no es util por solo haber una variable categorica
# Test de independencia y correlacion tampoco es util. 
# Lo mas adecuado seria un test para comparar si existe una diferencia significativa
# entre los valores de dos grupos.

# Miremos si es parametrica o no parametrica
# Test de shapiro-wilk
# Hipotesis nula es una distribucion normal
# Hipotesis alternativa no es una distribucion normal

testShapiroOceanUSmelanoma <- shapiro.test(oceanUSmelanoma$mortality)
testShapiroNotOceanUSmelanoma <- shapiro.test(notOceanUSmelanoma$mortality)

testShapiroOceanUSmelanoma # Es Parametrica
testShapiroNotOceanUSmelanoma # Es parametrica

# Test de Student
# Hipotesis nula no hay diferencia significativa
# Hipotesis alternativa Hay diferencia significativa

tTestOceanMortalityMelanoma <- t.test(oceanUSmelanoma$mortality, notOceanUSmelanoma$mortality)
tTestOceanMortalityMelanoma # Hay diferencia significativa

# str(notOceanUSmelanoma)
# str(oceanUSmelanoma)
# pwr.t.test(d=1, n =22,sig.level=0.05)
# No es tan relevante al ser tasas y no datos recopilados. 


# Respondiendo la pregunta inicial, no se parecen las tasas de mortalidad
# entre los estados oceanicos y no oceanicos

mean(oceanUSmelanoma$mortality)
tTestOceanUSMelanoma <- t.test(oceanUSmelanoma$mortality)
tTestOceanUSMelanoma
IC95OceanUSMelanoma <- (tTestOceanUSMelanoma$conf.int[2]-tTestOceanUSMelanoma$conf.int[1])/2


mean(notOceanUSmelanoma$mortality)
tTestNotOceanUSMelanoma <- t.test(notOceanUSmelanoma$mortality)
tTestNotOceanUSMelanoma

IC95NotOceanUSMelanoma <- (tTestNotOceanUSMelanoma$conf.int[2]-NotOceanUSMelanoma$conf.int[1])/2
IC95OceanUSMelanoma
IC95NotOceanUSMelanoma 

# De hecho es mayor en los estados oceanicos.  

# GRAFICAS

# Hagamos comparacion de densidades y boxplot

densityOceanUSmelanoma <- density(oceanUSmelanoma$mortality)
densityNotOceanUSmelanoma <- density(notOceanUSmelanoma$mortality) 

plot(densityOceanUSmelanoma, 
     main = c("Grafico de densidad de tasa de mortalidad por melanoma", 
              "en estados oceanicos contra estados no oceanicos"), 
     col = "firebrick", 
     lwd = 2,
     ylim = c(0, 0.02),
     ylab = "Densidad",
     xlab = "Tasa de mortalidad",
     sub = "Realizado por: Angel Martínez")
lines(densityNotOceanUSmelanoma, 
      lty = 4, 
      lwd = 2, 
      col = "cornflowerblue")
legend("topright", 
       c("Estado oceanico", "Estado no oceanico"),
       lty = c(1, 4),
       col = c("firebrick", "cornflowerblue"),
       text.col =  c("firebrick", "cornflowerblue"),
       bty = "n",
)
# El que tenga valores mas altos a la derecha se puede interpretar como el que
# presenta mayor mortalidad.

# Probemos comparacion de boxplot

boxplot(oceanUSmelanoma$mortality, notOceanUSmelanoma$mortality, 
        names = c("Estados oceanicos", "Estados no oceanicos"),
        main = c("Gráfico de cajas de tasa de mortalidad de melanoma", 
                 "en Estados Oceanicos contra Estados no oceanicos"), 
        ylab = "Tasa de mortalidad", 
        col = c("firebrick", "cornflowerblue"),
        sub = "Realizado por: Angel Martínez"
)

# 2. ¿existe algún efecto de la latitud o de la longitud 
# en la tasa de mortalidad? 

# ¿Que prueba usar?
# Algun Test para diferencia de medias?  no porque no estamos comparando el mismo dato en dos grupos
# Algun Test de correlacion? Probemos esto

# Latitud se puede considerar como el eje vertical o y de la tierra
# Longitud se puede considerar como el eje horizontal o x de la tierra

# Empezemos con el coeficiente de Pearson y el de determinacion

correlacionLatitudMortalidad <- cor(USmelanoma$latitude, USmelanoma$mortality)
correlacionLatitudMortalidad
correlacionLatitudMortalidad^2

correlacionLongitudMortalidad <- cor(USmelanoma$longitude, USmelanoma$mortality)  
correlacionLongitudMortalidad
correlacionLongitudMortalidad^2

# Probemos el test de correlacion
# hipotesis nula: No hay correlacion significativa
# Hipotesis alternativa: Hay correlacion significativa

testCorrelacionLatitudMortalidad <- cor.test(USmelanoma$latitude, USmelanoma$mortality)
testCorrelacionLatitudMortalidad
# Hay correlacion significativa


testCorrelacionLongitudMortalidad <-cor.test(USmelanoma$longitud, USmelanoma$mortality)
testCorrelacionLongitudMortalidad
# No Hay correlacion significativa

# Respondiendo a la pregunta
# Si hay un efecto significativo de la latidud en la tasa de mortalidad, pero
# no hay un efecto significativo de  la longitud en la tasa de mortalidad. 

# Hagamos regresiones lineales por la gracia

# Graficas

# Quizas graficos de dispersion y regresiones lineales

lmLatitudMortalidad <- lm(USmelanoma$mortality ~ USmelanoma$latitude)
summary(lmLatitudMortalidad)
lmLatitudMortalidadOceanica <- lm(oceanUSmelanoma$mortality ~ oceanUSmelanoma$latitude)
summary(lmLatitudMortalidadOceanica)
lmLatitudMortalidadNoOceanica <- lm(notOceanUSmelanoma$mortality ~ notOceanUSmelanoma$latitude)
summary(lmLatitudMortalidadNoOceanica)

lmLongitudMortalidad <- lm(USmelanoma$mortality ~ USmelanoma$longitude)
summary(lmLongitudMortalidad)
lmLongitudMortalidadOceanica <- lm(oceanUSmelanoma$mortality ~ oceanUSmelanoma$longitude)
summary(lmLongitudMortalidadOceanica)
lmLongitudMortalidadNoOceanica <- lm(notOceanUSmelanoma$mortality ~ notOceanUSmelanoma$longitude)
summary(lmLongitudMortalidadNoOceanica)

plot(mortality ~ latitude,
     data = USmelanoma,
     pch = ifelse(USmelanoma$ocean == "yes", 16, 17),
     col = ifelse(USmelanoma$ocean == "yes", "cornflowerblue", "orangered"),
     main = "Relación entre la tasa de mortalidad de melanoma y latitud",
     xlab = "Latitud",
     ylab = "Tasa de mortalidad",
     sub = "Elaborado por Angel Martínez")
abline(lmLatitudMortalidad, 
       col = "grey",
       lty = 1,
       lwd = 1)
abline(lmLatitudMortalidadOceanica, 
       col = "cornflowerblue",
       lty = 2,
       lwd = 1)
abline(lmLatitudMortalidadNoOceanica, 
       col = "orangered",
       lty = 3,
       lwd = 1)
legend("bottomleft",
       legend = c("Modelo lineal general", "Modelo lineal Estados oceanicos", 
                  "Modelo lineal Estados no oceanicos", "Estados oceanicos", "Estados no oceanicos"), 
       col = c("grey", "cornflowerblue","orangered", "cornflowerblue","orangered"),
       text.col = c("grey", "cornflowerblue","orangered", "cornflowerblue","orangered"),
       lty = c(1, 2, 3, NA, NA),
       # ncol = 2,
       lwd = 1,
       bty = "n",
       pch = c(NA, NA, NA, 16, 17))


plot(mortality ~ longitude,
     data = USmelanoma,
     pch = ifelse(USmelanoma$ocean == "yes", 16, 17),
     col = ifelse(USmelanoma$ocean == "yes", "cornflowerblue", "orangered"),
     main = "Relación entre la tasa de mortalidad de melanoma y longitud",
     xlab = "Latitud",
     ylab = "Tasa de mortalidad",
     sub = "Elaborado por Angel Martínez")
abline(lmLongitudMortalidad, 
       col = "grey66",
       lty = 1,
       lwd = 1)
abline(lmLongitudMortalidadOceanica, 
       col = "cornflowerblue",
       lty = 2,
       lwd = 1)
abline(lmLongitudMortalidadNoOceanica, 
       col = "orangered",
       lty = 3,
       lwd = 1)
legend("topright",
       legend = c("Modelo lineal general", "Modelo lineal Estados oceanicos", 
                  "Modelo lineal Estados no oceanicos"), 
       col = c("grey", "cornflowerblue","orangered"),
       text.col = c("grey", "cornflowerblue","orangered"),
       lty = c(1, 2, 3),
       lwd = 1,
       bty = "n")
legend("bottomleft",
       legend = c("Estados oceanicos", "Estados no oceanicos"), 
       col = c("cornflowerblue","orangered"),
       text.col = c("cornflowerblue","orangered"),
       bty = "n",
       pch = c(16, 17))

# Set de datos de una encuesta 

# La Encuesta China sobre Salud y Vida Familiar tomó una muestra de 60 aldeas
# y barrios urbanos elegidos de tal manera que sean representativos de la
# totalidad de la geografía y el rango socioeconómico de la China
# contemporánea, excluyendo Hong Kong y Tíbet. Ochenta y tres individuos
# fueron escogidos al azar para cada ubicación a partir de registros
# oficiales de adultos de entre 20 y 64 años (muestra de 5000 individuos en
# total). 

# 1.¿cuál es la relación entre las variables felicidad y salud? 

load("encuesta.Rdat")

# Transformemos datos
encuestaChinos <- CHFLS
any(is.na(encuestaChinos$R_happy))
any(is.na(encuestaChinos$R_health))
any(is.na(encuestaChinos$R_income))
any(is.na(encuestaChinos$A_income))
encuestaChinos <- subset(encuestaChinos, !is.na(encuestaChinos$R_happy))
encuestaChinos <- subset(encuestaChinos, !is.na(encuestaChinos$R_happy))
View(encuestaChinos)
str(encuestaChinos)
# Ya estan por defecto en factores ordinales

Felicidad <- encuestaChinos$R_happy
Salud <- encuestaChinos$R_health

# Podriamos usar cor pero estos datos son categorias y no numeros
# Probemos con tablas de contingencia y proporciones entonces

tablaContingenciaFelicidadSalud <- xtabs(~ Felicidad + Salud, data = encuestaChinos)
tablaContingenciaFelicidadSalud
addmargins(tablaContingenciaFelicidadSalud)


# prop.test(tablaContigenciaFelicidadSalud)
# Da error porque solo admite mas de dos columnas
# Igual no estan util para relaciones, es mas para comparar proporciones

# Prueba de independencia de chi cuadrado
# Hipotesis nula es independiente
# Hipotesis alternativa es dependiente


chisqTestFelicidadSalud <- chisq.test(tablaContingenciaFelicidadSalud)
chisqTestFelicidadSalud # Es dependiente

# Miremos el poder 
# El numero de muestras es de 1534 (n)
# La significancia es de 0.05 (sig.level)
# Tamaño del efecto, probemos 0.1 (w)

# df <- (5-1)*(4-1)
# df <- 12
library(pwr)
poderChisqTestFelicidadSalud <- pwr.chisq.test(w = 0.1 , N = 1534, df= 12, sig.level = 0.05)
poderChisqTestFelicidadSalud

poderMedianoChisqTestFelicidadSalud <- pwr.chisq.test(w = 0.5 , N = 1534, df= 12, sig.level = 0.05)
poderMedianoChisqTestFelicidadSalud
# Se pueden convertir en numericas y aplicar un cor.test usando el metodo de kendall
# pero no tenia ganas y no lo vimos en clase
# igual probemos
# Kendall se usa para datos ordinales

felicidadNum <- as.numeric(Felicidad)
saludNum <- as.numeric(Salud)

corTestFelicidadSalud <- cor.test(felicidadNum, saludNum, method = "kendall")
corTestFelicidadSalud
# 0.3191523 
# Hay correlacion

# GRAFICOS

# Probablemente mosaicos
rownames(tablaContingenciaFelicidadSalud) <- c("MI*", "No demasiado Feliz", "Algo Feliz", "Muy Feliz")
colnames(tablaContingenciaFelicidadSalud) <- c("Pobre", "Mala", "Regular", "B*", "E*")

plot(tablaContingenciaFelicidadSalud,
     main = "Composición de Felicidad contra Salud",
     ylab = "Salud",
     xlab = "Felicidad",
     cex = 0.5,
     off = 1,
     col = c("midnightblue", "tomato"),
     border = c("midnightblue", "tomato"),
     sub = "Realizado por Angel Martínez"
)



# 2. ¿existe relación entre las variables felicidad e ingresos? 

# Felicidad ya la habiamos guardado antes
# Ingesos miro uno con R_income y otro con A_income
# Me imagino que la r es de raw, y la de a es adjusted
# Probemos con r

Ingresos <- encuestaChinos$R_income
# Esta vez ordinal contra continua (numerica)
# Tablas de contingencia o chi cuadrado no van a servir. 

# Probemos una test de correlacion
# Spearman es se usa para datos ordinales y continuos

corIngresosFelicidad <- cor.test(felicidadNum, Ingresos, method = "spearman")
corIngresosFelicidad

# Probemos con Anova

modelo_anova <- aov(Ingresos ~ Felicidad, data = encuestaChinos)
modelo_anova
summary(modelo_anova)
# Hay una diferencia significativa entre grupos

library(ggplot2)

ggplot(encuestaChinos, aes(x = R_happy, y = R_income)) +
  geom_boxplot(outlier.shape = 16,      
               outlier.size = 2,         
               outlier.color = "red",    
               notch = TRUE,
               varwidth = TRUE) +     
  labs(title = "Grafico de cajas de Felicidad contra Ingresos",
       x = "Felicidad",
       y = "Ingresos")

# colour = c("midnightblue","dodgerblue4","dodgerblue3", "dodgerblue2", "dodgerblue")

levels(encuestaChinos$R_happy) <- c("Muy Infeliz", "No demasiado Feliz", "Algo Feliz", "Muy Feliz")
Felicidad <- encuestaChinos$R_happy
ggplot(encuestaChinos, 
      aes(x = Felicidad, 
          y = R_income, 
          fill = Felicidad))+
  geom_boxplot(varwidth = TRUE,
               notch = TRUE) +
  labs(title = "Grafico de cajas de Felicidad contra Ingresos",
       x = "Felicidad",
       y = "Ingresos") +
  theme_minimal()
  
