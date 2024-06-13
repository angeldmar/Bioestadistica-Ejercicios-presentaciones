library(readxl)
library(coin)
Anderson <- read_excel("Anderson_-_datos.xlsx")
View(Anderson)
data(glioma, package = "coin")

# Set de ANDERSON

# 1. ¿La vacunación es efectiva? (I)

Vacuna <- Anderson$Vaccine12mo
PCR <- Anderson$Flu_RT_PCR_posneg

tablaContingenciaVacunaPCR <- xtabs(~ Vacuna + PCR, data = Anderson)
tablaContingenciaVacunaPCR

testProporcionesVacunaPCR <- prop.test(tablaContingenciaVacunaPCR)

# 2. ¿La vacunación es efectiva? (II)




# 3. Resúmenes numéricos, gráficos y evaluación de hipótesis

# 4. Elabore una tabla de contingencia con la función
# xtabs() y genere un gráfico de mosaico a partir de este
# resumen.

# 5. Evalúe si hay influencia de la vacunación en la
# positividad con la prueba chi cuadrado, función
# chisq.test().

# 6. Estime el poder de la prueba aplicada con la
# función pwr.chisq.test() implementada en el paquete
# “pwr”. Puede suponer que el tamaño del efecto es medio
# o grande.

# 7. Estime las proporciones indicadas a continuación
# con un nivel de confianza del 95% con la función
# prop.test()




# Set de Glioma

# 1. Se desea conocer el efecto de dos terapias en este estudio piloto, para lo
# cual se emplean datos de supervivencia: ¿alguna terapia es mejor que la
# otra?

# 2. Genere los gráficos de supervivencia de acuerdo con la pregunta de
# investigación. Emplee una prueba de rango logarítmico evaluar hipótesis en
# torno a las curvas de distribución de dos tratamientos en los dos grados
# de avance del glioma (grado 3 y grado 4).


# 2.1 Grafique los datos empleando la función plot() sobre las funciones anidadas
# survfit(Surv()). La función Surv() crea un objeto especial de supervivencia
# empleando los datos de campo, mientras la función survfit() traza las
# curvas de supervivencia. Identifique las dos curvas con la función legend()
# empleada con anterioridad.


# 2.2. Supervivencia según el tipo de tratamiento y grado
# de evolución del glioma.


# 3. De acuerdo con los resultados de la evaluación de hipótesis y de lo
# observado en los gráficos de supervivencia, concluya sobre la efectividad
# de la radioinmunoterapia (RIT) en comparación con el tratamiento Control.