library(readr)
NeidertBD <- read_csv("Kluess-plasma DPPIV-data.csv")
View(NeidertBD)

# Preguntas de investigación

# 1. Existe correlación entre (BMI Kg/m2) y (%Fat)?
# Aplique la prueba t para correlación aplicando la
# función cor.test() y haga la prueba de hipótesis:


str(NeidertBD)
NeidertBD$Sex <- as.factor(NeidertBD$Sex)

CorBMIFAT <- cor(NeidertBD$`BMI (kg/m2)`, NeidertBD$`% Fat`)
CorBMIFAT

# 0.4210956
# correlacion moderada


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

# 2. ¿La relación observada gráficamente entre el IMC
#(BMI Kg/m2) y el % de grasa (%Fat) es estadísticamente
# significativa?

CorBMIFATtest <- cor.test(NeidertBD$`BMI (kg/m2)`, NeidertBD$`% Fat`)
CorBMIFATtest 
# valor p = 4.181e-06
# 95 percent confidence interval:
# 0.2546933 0.5632773
# La correlacion es sestadisticamente significativa



# 4. ¿Cuál es el IC95 de la correlación entre (BMI Kg/m2) y (%Fat)?

# 95 percent confidence interval:
# 0.2546933 0.5632773

# 5. ¿Existe evidencia gráfica de relación entre las
# variables (BMI Kg/m2) y (%Fat)?

# A graficar

Fat <- NeidertBD$`% Fat`
BMI <- NeidertBD$`BMI (kg/m2)`
lmNeidert <- lm(Fat ~ BMI)

FatMale <- subset(NeidertBD, Sex == "M", select = `% Fat`)
FatMale <- FatMale$`% Fat`
BMIMale <- subset(NeidertBD, Sex == "M", select = `BMI (kg/m2)`)
BMIMale <- BMIMale$`BMI (kg/m2)`
lmNeidertMale <- lm(FatMale ~ BMIMale)

FatFemale <- subset(NeidertBD, Sex == "F", select = `% Fat`)
FatFemale <- FatFemale$`% Fat`
BMIFemale <- subset(NeidertBD, Sex == "F", select = `BMI (kg/m2)`)
BMIFemale <- BMIFemale$`BMI (kg/m2)`
lmNeidertFemale <- lm(FatFemale ~ BMIFemale)


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
       lwd = 1)
abline(lmNeidertMale, 
       col = "cornflowerblue",
       lty = 2,
       lwd = 1)
abline(lmNeidertFemale, 
       col = "orangered",
       lty = 3,
       lwd = 1)


legend("topleft", 
       legend = c("Modelo lineal general", "Modelo lineal hombres", "Modelo lineal mujeres", "Hombres", "Mujeres"), 
       col = c("grey66", "cornflowerblue","orangered", "cornflowerblue","orangered"),
       text.col = c("grey66", "cornflowerblue","orangered", "cornflowerblue","orangered"),
       lty = c(1, 2, 3, NA, NA),
       lwd = 1,
       bty = "n",
       pch = c(NA, NA, NA, 16, 17))


