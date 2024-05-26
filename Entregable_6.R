library(readxl)
library(ggplot2)
Dennison_datos <- read_excel("Dennison_-_datos.xlsx")
View(Dennison_datos)

# ¿Cuánto jugo (oz) consume el grupo de niños que
# participaron en el estudio?

# Dennison_datos$GENDER = as.factor(Dennison_datos$GENDER)
str(Dennison_datos)

Boys_Dennison <- subset(Dennison_datos, GENDER == 0)
Boys_Dennison$JUICE
str(Boys_Dennison)


# Shapiro-Wilk postula que este test es no parametrico
# p < 0.05 
# No parametrico
# Hipotesis nula 
# p > 0.05 Que es parametrico

# No es parametrico  usamos el test de wilcoxon t.wilcox

# Es parametrico usamos el test de Student  t.test
shapiro.test(Boys_Dennison$JUICE) # Es no paramétrico
meanOZJuiceBoys <- mean(Boys_Dennison$JUICE)
tTestOzJuiceBoys <- t.test(Boys_Dennison$JUICE)
tTestOzJuiceBoys

IC95OzJuiceBoys <- (tTestOzJuiceBoys$conf.int[2]-tTestOzJuiceBoys$conf.int[1])/2

paste0(meanOZJuiceBoys, " +- ", IC95OzJuiceBoys)

sum(Boys_Dennison$JUICE)

#  ¿El grupo de niños menores de 3.5 años consume la
# misma cantidad de jugo (oz) que el grupo de niños
# mayores de 3.5 años

BoysUnderAge35 <- subset(Dennison_datos, GENDER == 0 & AGE < 3.5)
BoysUnderAge35

BoysOverAge35 <- subset(Dennison_datos, GENDER == 0 & AGE >= 3.5)
BoysOverAge35

shapiro.test(BoysUnderAge35$JUICE) # Es no parametrico
shapiro.test(BoysOverAge35$JUICE) # Es no parametrico

wilcox.test(BoysUnderAge35$JUICE, BoysOverAge35$JUICE) # Son diferentes
# ¿Niños y niñas menores de 3.5 años consume la
# misma cantidad de jugo (oz)?

GirlsUnderAge35 <- subset(Dennison_datos, GENDER == 1 & AGE < 3.5)
GirlsUnderAge35

shapiro.test(GirlsUnderAge35$JUICE) # Es no parametrico


wilcox.test(BoysUnderAge35$JUICE, GirlsUnderAge35$JUICE) # Son diferentes
# Niños y niñas mayores de 3.5 años consume la
# misma cantidad de jugo (oz)?

GirlsOverAge35 <- subset(Dennison_datos, GENDER == 1 & AGE >= 3.5)
GirlsOverAge35

shapiro.test(GirlsOverAge35$JUICE) # Es no parametrica

wilcox.test(BoysOverAge35$JUICE, GirlsOverAge35$JUICE, correct = TRUE) # No son diferentes. 


# Graficos
#Pregunta 1
# Histograma y su densidad

hist(Boys_Dennison$JUICE, 
     freq = FALSE, 
     main = "Histograma y densidad de consumo de onzas de jugo en niños", 
     col = "cornflowerblue", 
     border="white", 
     xlab = "Onzas de Jugo", 
     ylab = "Densidad",
     sub = "Realizado por: Angel Martínez")


dens_Boys_Dennison <- density(Boys_Dennison$JUICE)
lines(dens_Boys_Dennison, 
     col = "red3",
     lty = 1,
     lwd = 2
     )

# Boxplot

boxplot(Boys_Dennison$JUICE,
        main = c("Grafico de cajas de consumo de onzas de jugo en niños"), 
        ylab = "Onzas de Jugo", 
        col = c("cornflowerblue"),
        sub = "Realizado por: Angel Martínez")

stripchart(BoysUnderAge35$JUICE, method = "jitter", add = TRUE, pch = 1,col = "firebrick", vertical = TRUE)

#Pregunta 2

# Histogramas de grupos

densBoysUnderAge35 <- density(BoysUnderAge35$JUICE)
densBoysOverAge35 <- density(BoysOverAge35$JUICE)

hist(BoysUnderAge35$JUICE, 
     freq = FALSE, 
     main = c("Histograma y grafico de densidad de consumo de onzas de jugo", 
              "en niños menores de 3.5 años"), 
     col = "cornflowerblue", 
     border="white", 
     xlab = "Onzas de Jugo", 
     ylab = "Densidad",
     sub = "Realizado por: Angel Martínez")

lines(densBoysUnderAge35, 
      col = "red3",
      lty = 1,
      lwd = 2
)

hist(BoysOverAge35$JUICE, 
     freq = FALSE, 
     main = c("Histograma y grafico de densidad de consumo de onzas de jugo",
            "en niños mayores de 3.5 años"), 
     col = "cornflowerblue", 
     border="white", 
     xlab = "Onzas de Jugo", 
     ylab = "Densidad",
     sub = "Realizado por: Angel Martínez")

lines(densBoysOverAge35, 
      col = "red3",
      lty = 1,
      lwd = 2
)

# Comparacion de densidades

plot(densBoysUnderAge35, 
     main = c("Grafico de densidad de consumo de onzas de jugo", 
              "en niños menores de 3.5 años contra niños mayores de 3.5 años"), 
     col = "firebrick", 
     lwd = 2,
     ylim = c(0, 0.12),
     ylab = "Densidad", 
     sub = "Realizado por: Angel Martínez")
lines(densBoysOverAge35, 
      lty = 4, 
      lwd = 2, 
      col = "cornflowerblue")
legend("topleft", c("Menores de 3.5 años", "Mayores de 3.5 años"),
       lty = c(1, 4),
       col = c("firebrick", "cornflowerblue"),
       text.col =  c("firebrick", "cornflowerblue"),
       bty = "n",
       title = "Grupos de edades",
       title.col = "black"
)

# Comparacion de boxplots


boxplot(BoysUnderAge35$JUICE, BoysOverAge35$JUICE, 
        names = c("Menores de 3.5 años", "Mayores de 3.5 años"),
        main = c("Gráfico de cajas de consumo de jugo", 
                 "de niños menores de 3.5 años contra niños mayores de 3.5 años"), 
        ylab = "Onzas de Jugo", 
        col = c("firebrick", "cornflowerblue"),
        sub = "Realizado por: Angel Martínez"
        )

# Pregunta 3


# Histogramas de grupos

densGirlsUnderAge35 <- density(GirlsUnderAge35$JUICE)

hist(GirlsUnderAge35$JUICE, 
     freq = FALSE, 
     main = c("Histograma y grafico de densidad de consumo de onzas de jugo", 
              "en niñas menores de 3.5 años"), 
     col = "cornflowerblue", 
     border="white", 
     xlab = "Onzas de Jugo", 
     ylab = "Densidad",
     sub = "Realizado por: Angel Martínez")

lines(densGirlsUnderAge35, 
      col = "red3",
      lty = 1,
      lwd = 2
)

# Comparacion de densidades

plot(densBoysUnderAge35, 
     main = c("Grafico de densidad de consumo de onzas de jugo", 
              "en niños menores de 3.5 años contra niñas menores de 3.5 años"), 
     col = "firebrick", 
     lwd = 2,
     ylim = c(0, 0.12),
     ylab = "Densidad", 
     sub = "Realizado por: Angel Martínez")
lines(densGirlsUnderAge35, 
      lty = 4, 
      lwd = 2, 
      col = "cornflowerblue")
legend("topleft", c("Niños", "Niñas"),
       lty = c(1, 4),
       col = c("firebrick", "cornflowerblue"),
       text.col =  c("firebrick", "cornflowerblue"),
       bty = "n",
       title = "Género",
       title.col = "black"
)

# Comparacion de boxplots


boxplot(BoysUnderAge35$JUICE, GirlsOverAge35$JUICE, 
        names = c("Niños", "Niñas"),
        main = c("Gráfico de cajas de consumo de jugo", 
                 "de niños menores de 3.5 años contra niñas menores de 3.5 años"), 
        ylab = "Onzas de Jugo", 
        col = c("firebrick", "cornflowerblue"),
        sub = "Realizado por: Angel Martínez"
)

# Pregunta 4

# Histogramas de grupos

densGirlsOverAge35 <- density(GirlsOverAge35$JUICE)

hist(GirlsOverAge35$JUICE, 
     freq = FALSE, 
     main = c("Histograma y grafico de densidad de consumo de onzas de jugo",
              "en niñas mayores de 3.5 años"), 
     col = "cornflowerblue", 
     border="white", 
     xlab = "Onzas de Jugo", 
     ylab = "Densidad",
     sub = "Realizado por: Angel Martínez")

lines(densGirlsOverAge35, 
      col = "red3",
      lty = 1,
      lwd = 2
)

# Comparacion de densidades

plot(densBoysOverAge35, 
     main = c("Grafico de densidad de consumo de onzas de jugo", 
              "en niños mayores de 3.5 años contra niñas mayores de 3.5 años"), 
     col = "firebrick", 
     lwd = 2,
     ylim = c(0, 0.12),
     ylab = "Densidad", 
     sub = "Realizado por: Angel Martínez")
lines(densGirlsOverAge35, 
      lty = 4, 
      lwd = 2, 
      col = "cornflowerblue")
legend("topleft", c("Niños", "Niñas"),
       lty = c(1, 4),
       col = c("firebrick", "cornflowerblue"),
       text.col =  c("firebrick", "cornflowerblue"),
       bty = "n",
       title = "Género",
       title.col = "black"
)

# Comparacion de boxplots


boxplot(BoysUnderAge35$JUICE, GirlsOverAge35$JUICE, 
        names = c("Niños", "Niñas"),
        main = c("Gráfico de cajas de consumo de jugo", 
                 "de niños mayores de 3.5 años contra niñas mayores de 3.5 años"), 
        ylab = "Onzas de Jugo", 
        col = c("firebrick", "cornflowerblue"),
        sub = "Realizado por: Angel Martínez"
)

