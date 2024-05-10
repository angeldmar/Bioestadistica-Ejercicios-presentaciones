# Cargar Base de datos y darle un vistazo

load("BD_falanges.Rdat")
View(BD_Falanges)
str(BD_Falanges)
dir()
ls()
BD_Falanges$mm <- as.numeric(BD_Falanges$mm)


# Empleo filtros

filtro <- BD_Falanges$mm[BD_Falanges$MANO == "izq" & BD_Falanges$DEDO == "ind"]
str(filtro)

subset(BD_Falanges, DEDO == "ind" & FALANGES == "dis" & MANO == "izq")


# Aplicación de funciones

dens_mm <- density(BD_Falanges$mm)
hist(BD_Falanges$mm, freq = FALSE, ylim = c(0,0.075),main = "Histograma de frecuencias relativas de la longitud de falanges", col = "lightblue", border="white", xlab = "Milimetros", ylab = "Densidad")
lines(dens_mm, lty = 2, col = "blue", lwd = 2)
rug(BD_Falanges$mm, col = "red")

abline(v = mean(BD_Falanges$mm), col = "green", lwd = 2)


men_pro <- BD_Falanges$mm[BD_Falanges$DEDO == "men" & BD_Falanges$FALANGE == "pro"]
ic95_men_pro <- t.test(men_pro)
int_ic95_men_pro <- (ic95_men_pro$conf.int[2]-ic95_men_pro$conf.int[1])/2
int_ic95_men_pro


pul_pro <- subset(BD_Falanges, DEDO == "pul" & FALANGES == "pro")
pul_dis <- subset(BD_Falanges, DEDO == "pul" & FALANGES == "dis")
pul_med <- subset(BD_Falanges, DEDO == "pul" & FALANGES == "med")
t.test (pul_pro$mm, pul_dis$mm)


library(psych)
describe(BD_Falanges)

# Inspección de datos
ind_pro <- subset(BD_Falanges, DEDO == "ind" & FALANGES == "pro")
ind_dis <- subset(BD_Falanges, DEDO == "ind" & FALANGES == "dis")
ind_med <- subset(BD_Falanges, DEDO == "ind" & FALANGES == "med")
med_pro <- subset(BD_Falanges, DEDO == "med" & FALANGES == "pro")
med_dis <- subset(BD_Falanges, DEDO == "med" & FALANGES == "dis")
med_med <- subset(BD_Falanges, DEDO == "med" & FALANGES == "med")
anu_pro <- subset(BD_Falanges, DEDO == "anu" & FALANGES == "pro")
anu_dis <- subset(BD_Falanges, DEDO == "anu" & FALANGES == "dis")
anu_med <- subset(BD_Falanges, DEDO == "anu" & FALANGES == "med")
men_pro <- subset(BD_Falanges, DEDO == "men" & FALANGES == "pro")
men_dis <- subset(BD_Falanges, DEDO == "men" & FALANGES == "dis")
men_med <- subset(BD_Falanges, DEDO == "men" & FALANGES == "med")


str(ind_pro)
str(ind_dis)
str(ind_med)
str(med_pro)
str(med_dis)
str(med_med)
str(anu_pro)
str(anu_dis)
str(anu_med)
str(men_pro)
str(men_dis)
str(men_med)


Frecuencias <- table(BD_Falanges$ID)
print(Frecuencias)

Frecuencias_Falanges <- table(BD_Falanges$FALANGES)
print(Frecuencias_Falanges)

Frecuencias_Dedo <- table(BD_Falanges$DEDO)
print(Frecuencias_Dedo)

Frecuencias_Mano<- table(BD_Falanges$MANO)
print(Frecuencias_Dedo)

# Resúmenes numéricos y gráficos
# Sección del entregable
