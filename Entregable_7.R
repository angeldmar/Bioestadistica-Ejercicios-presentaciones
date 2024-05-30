library(readr)
NeidertBD <- read_csv("Kluess-plasma DPPIV-data.csv")
View(NeidertBD)

# Preguntas de investigación
# 1. ¿La relación observada gráficamente entre el IMC
#(BMI Kg/m2) y el % de grasa (%Fat) es estadísticamente
# significativa?

str(NeidertBD)
NeidertBD$Sex <- as.factor(NeidertBD$Sex)

CorBMIFAT <- cor(NeidertBD$`BMI (kg/m2)`, NeidertBD$`% Fat`)
CorBMIFAT

# 0.4210956
# correlacion moderada

CorBMIFATtest <- cor.test(NeidertBD$`BMI (kg/m2)`, NeidertBD$`% Fat`)
CorBMIFATtest 
# valor p = 4.181e-06
# 95 percent confidence interval:
# 0.2546933 0.5632773
# La correlacion es sestadisticamente significativa


# 2.¿Cuán fuerte es la correlación entre (BMI Kg/m2)
# y (%Fat)

#coeficiente de Pearson
# R
#coeficiente de determinacion 
# R^2

R2BMIFAT <- CorBMIFAT^2
R2BMIFAT
# R^2 = 0.1773215
# 17.73% de variabilidad en y es explicada por la variabilidad en x.



# 3. Existe correlación entre (BMI Kg/m2) y (%Fat)?
# Aplique la prueba t para correlación aplicando la
# función cor.test() y haga la prueba de hipótesis:

# creo que se aplica a la pregunta 1


# 4. ¿Cuál es el IC95 de la correlación entre (BMI Kg/m2) y (%Fat)?

# 95 percent confidence interval:
# 0.2546933 0.5632773

# 5. ¿Existe evidencia gráfica de relación entre las
# variables (BMI Kg/m2) y (%Fat)?

# A graficar

Fat <- NeidertBD$`% Fat`
BMI <- NeidertBD$`BMI (kg/m2)`
lmNeidert <- lm(Fat ~ BMI)

plot(Fat ~ BMI, 
     data = NeidertBD,
     pch = ifelse(NeidertBD$Sex == "M", 16, 17),
     col = ifelse(NeidertBD$Sex == "M", "cornflowerblue","orangered"),
     main = "Relación entre variables",
     xlab = "IMC",
     ylab = "% Grasa",
     sub = "Elaborado por Angel Martínez")
abline(lmNeidert, 
       col = "grey66",
       lty = 1,
       lwd = 2)

legend("bottomright", 
       legend = c("Regresion", "Hombre", "Mujer"), 
       col = c("grey66", "cornflowerblue","orangered"),
       lty = c(1, NA, NA),
       lwd = 2,
       pch = c(NA, 16, 17))


