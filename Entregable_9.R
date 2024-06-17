library(readxl)
library(coin)
library(pwr)
Anderson <- read_excel("Anderson_-_datos.xlsx")
View(Anderson)
data(glioma, package = "coin")

# Set de ANDERSON

Anderson$Vaccine12mo <- as.factor(Anderson$Vaccine12mo)
Anderson$Flu_RT_PCR_posneg <- as.factor(Anderson$Flu_RT_PCR_posneg)
Anderson$Gender <- as.factor(Anderson$Gender)

# 1. ¿La vacunación es efectiva? (I)

# Compare las proporciones de positividad de la prueba en los grupos de
# vacunados y no vacunados y determine si hay diferencia significativa.

Vacuna <- Anderson$Vaccine12mo
PCR <- Anderson$Flu_RT_PCR_posneg


tablaContingenciaVacunaPCR <- xtabs(~ Vacuna + PCR, data = Anderson)
tablaContingenciaVacunaPCR
addmargins(tablaContingenciaVacunaPCR)

# testProporcionesVacunaPCR <- prop.test(tablaContingenciaVacunaPCR)
# testProporcionesVacunaPCR


prop.test(c(1283,210), c(3610,959))

# Esto podria verse como 

PCR_Positivos <- c(1283,210)
totales <- c(3610,959)

ProporcionesVacunaPCR <-prop.test(PCR_Positivos, totales)


# Es analisis del valor P

# 2. ¿La vacunación es efectiva? (II)

# Estime las proporciones de positividad de la prueba en los grupos de
# vacunados y no vacunados y determine los intervalos de confianza al 95%.



IC95ProporcionesVacunaPCR <- (testProporcionesVacunaPCR$conf.int[2]-testProporcionesVacunaPCR$conf.int[1])/2
IC95ProporcionesVacunaPCR

# 3. Resúmenes numéricos, gráficos y evaluación de hipótesis


# 4. Elabore una tabla de contingencia con la función
# xtabs() y genere un gráfico de mosaico a partir de este
# resumen.
rownames(tablaContingenciaVacunaPCR) <- c("No", "Sí")
colnames(tablaContingenciaVacunaPCR) <- c("Negativo", "Positivo")
plot(tablaContingenciaVacunaPCR,
     main = "Efectividad de la vacunación",
     xlab = "Vacunados",
     ylab = "Resultado RT-PCR de Influenza",
     col = c("sienna4", "aquamarine4"),
     border = c("sienna4", "aquamarine4"),
     off = 1,
     cex = 1,
     sub = "Realizado por Angel Martínez",
)


# 5. Evalúe si hay influencia de la vacunación en la
# positividad con la prueba chi cuadrado, función
# chisq.test().

chisqTestVacunaPCR <- chisq.test(tablaContingenciaVacunaPCR)
chisqTestVacunaPCR

# 6. Estime el poder de la prueba aplicada con la
# función pwr.chisq.test() implementada en el paquete
# “pwr”. Puede suponer que el tamaño del efecto es medio
# o grande.

str(Anderson)
# El numero de muestras es de 4569 (n)
# La significancia es de 0.05 (sig.level)
# Tamaño del efecto, probemos 0.1(w)

# df <- (2-1)*(2-1)
# df

poderChisqTestVacunaPCR <- pwr.chisq.test(w = 0.1, N = 4569, df= 1, sig.level = 0.05)
poderChisqTestVacunaPCR

poderChisqTestVacunaPCRmediano <- pwr.chisq.test(w = 0.5, N = 4569, df= 1, sig.level = 0.05)
poderChisqTestVacunaPCRmediano
# 7. Estime las proporciones indicadas a continuación-
# con un nivel de confianza del 95% con la función
# prop.test()
# hay 2327 Negativos No vacunados
# hay 1283 Positivos No vacunados
# hay 749 Negativos Vacunados
# hay 210 Positivos Vacunados

# hay que modificarlo en base a add margins

testProporcionNegativoNoVacunado <- prop.test(2327, 3610)
testProporcionPositivoNoVacunado <- prop.test(1283, 3610)
testProporcionNegativoVacunado <- prop.test(749, 959)
testProporcionPositivoVacunado <- prop.test(210, 959)

testProporcionNegativoNoVacunado 
testProporcionPositivoNoVacunado 
testProporcionNegativoVacunado
testProporcionPositivoVacunado

# Hay que ver IC95

IC95NegativoNoVacunado <- (testProporcionNegativoNoVacunado$conf.int[2]-testProporcionNegativoNoVacunado$conf.int[1])/2
IC95PositivoNoVacunado <- (testProporcionPositivoNoVacunado$conf.int[2]-testProporcionPositivoNoVacunado$conf.int[1])/2
IC95NegativoVacunado <- (testProporcionNegativoVacunado$conf.int[2]-testProporcionNegativoVacunado$conf.int[1])/2
IC95PositivoVacunado <- (testProporcionPositivoVacunado$conf.int[2]-testProporcionPositivoVacunado$conf.int[1])/2

IC95NegativoNoVacunado
IC95PositivoNoVacunado
IC95NegativoVacunado
IC95PositivoVacunado

View(glioma)

# Set de Glioma

# 1. Se desea conocer el efecto de dos terapias en este estudio piloto, para lo
# cual se emplean datos de supervivencia: ¿alguna terapia es mejor que la
# otra?
# Se responde en base a las graficas

glioma$sex <- as.factor(glioma$sex)
glioma$histology <- as.factor(glioma$histology)
glioma$group <- as.factor(glioma$group)
# El evento se deja logico

# 2. Genere los gráficos de supervivencia de acuerdo con la pregunta de
# investigación. Emplee una prueba de rango logarítmico evaluar hipótesis en
# torno a las curvas de distribución de dos tratamientos en los dos grados
# de avance del glioma (grado 3 y grado 4).


# 2.1 Grafique los datos empleando la función plot() sobre las funciones anidadas
# survfit(Surv()). La función Surv() crea un objeto especial de supervivencia
# empleando los datos de campo, mientras la función survfit() traza las
# curvas de supervivencia. Identifique las dos curvas con la función legend()
# empleada con anterioridad.

plot(survfit(Surv(time, event) ~ group, data = glioma), 
     lty =  c(1,2),
     lwd = 2,
     col = c("cornflowerblue", "tomato4"),
     main = "Comparacion entre terapias",
     ylab = "Probabilidad de Supervivencia",
     xlab = "Tiempo",
     sub = "Elaborado por Angel Martínez"
     )

legend("bottomleft",
       legend = c("Control", "RIT"),
       lty =  c(1,2),
       lwd = 2,
       bty = "n",
       col = c("cornflowerblue", "tomato4"),
       text.col =  c("cornflowerblue", "tomato4")
)

logrank_test(Surv(time,event) ~ group, data = glioma, distrtibution = "exact")


# 2.2. Supervivencia según el tipo de tratamiento y grado
# de evolución del glioma.

grado3 <- subset(glioma, histology == "Grade3")
GBM <- subset(glioma, histology == "GBM")

plot(survfit(Surv(time, event) ~ group, data = grado3),
     lty =  c(1,2),
     lwd = 2,
     col = c("cornflowerblue", "tomato4"),
     main = "Comparacion entre terapias para glioma Grado III",
     ylab = "Probabilidad de Supervivencia",
     xlab = "Tiempo",
     sub = "Elaborado por Angel Martínez"
)

legend("bottomleft",
       legend = c("Control", "RIT"),
       lty =  c(1,2),
       lwd = 2,
       bty = "n",
       col = c("cornflowerblue", "tomato4"),
       text.col =  c("cornflowerblue", "tomato4")
)


plot(survfit(Surv(time, event) ~ group, data = GBM),
     lty =  c(1,2),
     lwd = 2,
     col = c("cornflowerblue", "tomato4"),
     main = "Comparacion entre terapias para glioma GBM",
     ylab = "Probabilidad de Supervivencia",
     xlab = "Tiempo",
     sub = "Elaborado por Angel Martínez"
)

legend("bottomleft",
       legend = c("Control", "RIT"),
       lty =  c(1,2),
       lwd = 2,
       bty = "n",
       col = c("cornflowerblue", "tomato4"),
       text.col =  c("cornflowerblue", "tomato4")
)

logrank_test(Surv(time,event) ~ group, data = grado3, distrtibution = "exact")
logrank_test(Surv(time,event) ~ group, data = GBM, distrtibution = "exact")

# 3. De acuerdo con los resultados de la evaluación de hipótesis y de lo
# observado en los gráficos de supervivencia, concluya sobre la efectividad
# de la radioinmunoterapia (RIT) evn comparación con el tratamiento Control.


