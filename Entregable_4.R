BD_Falanges <- read.csv("~/Documentos/Bioestadistica/Bioestadistica-Ejercicios-presentaciones/BD_Falanges.csv")
View(BD_Falanges)

BD_Falanges$ID <- as.factor(BD_Falanges$ID)
BD_Falanges$DEDO <- as.factor(BD_Falanges$DEDO)
BD_Falanges$FALANGES <- as.factor(BD_Falanges$FALANGES)
BD_Falanges$MANO <- as.factor(BD_Falanges$MANO)
BD_Falanges$Genero<- as.factor(BD_Falanges$Genero)

BD_Falanges <- BD_Falanges[, -which(names(BD_Falanges) == "X")]

str(BD_Falanges)

save(BD_Falanges, file = "BD_falanges.Rdat")
dir()

# Resúmenes numéricos y gráficos de unos datos

mean(BD_Falanges$mm)
median(BD_Falanges$mm)
min(BD_Falanges$mm)
max(BD_Falanges$mm)
sd(BD_Falanges$mm)
var(BD_Falanges$mm)
range(BD_Falanges$mm)
summary(BD_Falanges$mm)
fivenum(BD_Falanges$mm)

# Resúmenes Gráficos

hist(BD_Falanges$mm)

hist(BD_Falanges$mm, freq = FALSE, main = "Histograma de frecuencias relativas de la longitud de falanges", col = "lightblue", border="white", xlab = "Milimetros", ylab = "Densidad")

dens_mm <- density(BD_Falanges$mm)

hist(BD_Falanges$mm, freq = FALSE, ylim = c(0,0.075),main = "Histograma de frecuencias relativas de la longitud de falanges", col = "lightblue", border="white", xlab = "Milimetros", ylab = "Densidad")

lines(dens_mm, lty = 2, col = "blue", lwd = 2)

rug(BD_Falanges$mm, col = "red")

boxplot(BD_Falanges$mm, main = "Boxplot de longitud de falanges", ylab = "milimetros", col = "red")

stripchart(BD_Falanges$mm, method = "jitter", add = TRUE, pch = 1,col = "gray", vertical = TRUE)



