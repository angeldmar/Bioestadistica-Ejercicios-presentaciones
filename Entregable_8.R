# Seguimos usando base de dato de Neidert

library(readr)
NeidertBD<- read_csv("Kluess-plasma DPPIV-data.csv")
View(NeidertBD)

NeidertBD$Sex <- as.factor(NeidertBD$Sex)

# Variables  son libres el nombre que quieran
# Columnas de renombrarse
# Filas
# kg..mg/kg
# `kg mg/kg`

# Funciones son palabras reservados

as.factor

# Preguntas de Investigacion

# 1. Sabiendo que existe correlación entre el índice de masa corporal 
# (BMI..kg.m2.) y el porcentaje de grasa corporal (X..Fat) (Véase la guía de la
# SESIÓN No. 7, Sección 2.2.1.1.1.), ¿Cuál es el modelo de regresión lineal de 
# la relación entre estas dos variables (independientemente del sexo del 
# participante)?

# f= m(pendiente)x + b (intercepto)
# 
# punto 1 (2, 3)
# punto 2 (4, 5)
# 
# % Fat =  0.9742x(IMC) + 5.1539
# 
# Si es demasiado pequeno se puede despreciar a 0  
# Si tiene valor significativo es que no es  0 
# p < 0.05  Es  que no es 0 se refiere es significativo
# p >= 0.05 Es  que es 0 se refiere que no es significativo
# 
# La pendiente del imc es significativo (p < 0.001)
# 
# IMC <- NeidertBD$`BMI (kg/m2)`
# FAT <- NeidertBD$`% Fat`

lmNeidert <- lm(NeidertBD$`% Fat` ~ NeidertBD$`BMI (kg/m2)`, data = NeidertBD)
lmNeidert <- lm(`% Fat` ~ `BMI (kg/m2)`, data = NeidertBD)
lmNeidert <- lm(FAT~IMC, data= NeidertBD)

summary(lmNeidert)



# Hipotesis nula No hay relacion (p > 0.05)
# Hipotesis alternativa Si hay relacion (p < 0.05)

# Si hay una relacion entre las dos variables ( p < 0.05)

# 2. ¿Cuáles son los modelos de regresión lineal de la relación entre estas dos 
# variables según el sexo del participante?

Neidert_F <- subset(NeidertBD, NeidertBD$Sex=="F")
Neidert_M <- subset(NeidertBD, NeidertBD$Sex=="M")

lmNeidert_F <- lm(`% Fat`~`BMI (kg/m2)`, data = Neidert_F)
lmNeidert_M <- lm(`% Fat`~`BMI (kg/m2)`, data = Neidert_M)

summary(lmNeidert_F)
summary(lmNeidert_M)

# 3. Represente graficamente los tres modelos lineales generados. 

plot(FAT~IMC, data = NeidertBD,
     pch = ifelse(NeidertBD$Sex == "M", 16, 17),
     col = ifelse(NeidertBD$Sex == "M", "midnightblue","indianred4"),
     main = "Relación entre variables",
     xlab = "IMC",
     ylab = "% Grasa",
     sub = "Elaborado por Angel Martínez")

abline(lmNeidert, 
       col = "goldenrod4",
       lty = 1,
       lwd = 1)
abline(lmNeidert_M, 
       col = "midnightblue",
       lty = 4,
       lwd = 1)
abline(lmNeidert_F, 
       col = "indianred4",
       lty = 5,
       lwd = 1)


legend("topleft",
       legend = c("Modelo global", "Modelo hombres", "Modelo mujeres", "Hombres", "Mujeres"), 
       col = c("goldenrod4", "midnightblue","indianred4", "midnightblue","indianred4"),
       text.col = c("goldenrod4", "midnightblue","indianred4", "midnightblue","indianred4"),
       lty = c(1, 4, 5, NA, NA),
       lwd = 1,
       bty = "n",
       pch = c(NA, NA, NA, 16, 17))


# 4. ¿Cuál es el efecto de la edad en el el porcentaje
# de grasa corporal (X..Fat)?

lmNeidert_Age <- lm(`% Fat`~`BMI (kg/m2)` + `Age (yrs)`, 
                    data = NeidertBD)




lmNeidert_Age_F <- lm(`% Fat`~`BMI (kg/m2)` + `Age (yrs)`, 
                      data = Neidert_F)

lmNeidert_Age_M <- lm(`% Fat`~`BMI (kg/m2)` + `Age (yrs)`, 
                     data = Neidert_M)

summary(lmNeidert_Age)
summary(lmNeidert_Age_F)
summary(lmNeidert_Age_M)

plot(lmNeidert_Age)
