# Cargar Base de datos y darle un vistazo
library("ggplot2")
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
hist(BD_Falanges$mm, freq = FALSE, ylim = c(0,0.075),main = "Histograma de frecuencias relativas de la longitud de falanges", col = "lightblue", border="white", xlab = "Milimetros", ylab = "Densidad", sub = "Realizado por Angel Martínez")
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

str(pul_pro)
str(pul_dis)
str(pul_med)
library(psych)
describe(BD_Falanges)

# Inspección de datos
pul_pro <- subset(BD_Falanges, DEDO == "pul" & FALANGES == "pro")
pul_dis <- subset(BD_Falanges, DEDO == "pul" & FALANGES == "dis")
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
# Parte 1 IC95

ic95_pul_pro <- t.test(pul_pro$mm)
int_ic95_pul_pro <- (ic95_pul_pro$conf.int[2]-ic95_pul_pro$conf.int[1])/2
int_ic95_pul_pro

ic95_pul_dis <- t.test(pul_dis$mm)
int_ic95_pul_dis <- (ic95_pul_dis$conf.int[2]-ic95_pul_dis$conf.int[1])/2
int_ic95_pul_dis

ic95_ind_pro <- t.test(ind_pro$mm)
int_ic95_ind_pro <- (ic95_ind_pro$conf.int[2]-ic95_ind_pro$conf.int[1])/2
int_ic95_ind_pro

ic95_ind_med <- t.test(ind_med$mm)
int_ic95_ind_med <- (ic95_ind_med$conf.int[2]-ic95_ind_med$conf.int[1])/2
int_ic95_ind_med

ic95_ind_dis <- t.test(ind_dis$mm)
int_ic95_ind_dis <- (ic95_ind_dis$conf.int[2]-ic95_ind_dis$conf.int[1])/2
int_ic95_ind_dis

ic95_med_pro <- t.test(med_pro$mm)
int_ic95_med_pro <- (ic95_med_pro$conf.int[2]-ic95_med_pro$conf.int[1])/2
int_ic95_med_pro

ic95_med_med <- t.test(med_med$mm)
int_ic95_med_med <- (ic95_med_med$conf.int[2]-ic95_med_med$conf.int[1])/2
int_ic95_med_med

ic95_med_dis <- t.test(med_dis$mm)
int_ic95_med_dis <- (ic95_med_dis$conf.int[2]-ic95_med_dis$conf.int[1])/2
int_ic95_med_dis

ic95_anu_pro <- t.test(anu_pro$mm)
int_ic95_anu_pro <- (ic95_anu_pro$conf.int[2]-ic95_anu_pro$conf.int[1])/2
int_ic95_anu_pro

ic95_anu_med <- t.test(anu_med$mm)
int_ic95_anu_med <- (ic95_anu_med$conf.int[2]-ic95_anu_med$conf.int[1])/2
int_ic95_anu_med

ic95_anu_dis <- t.test(anu_dis$mm)
int_ic95_anu_dis <- (ic95_anu_dis$conf.int[2]-ic95_anu_dis$conf.int[1])/2
int_ic95_anu_dis

ic95_men_pro <- t.test(men_pro$mm)
int_ic95_men_pro <- (ic95_men_pro$conf.int[2]-ic95_men_pro$conf.int[1])/2
int_ic95_men_pro

ic95_men_med <- t.test(men_med$mm)
int_ic95_med_med <- (ic95_men_med$conf.int[2]-ic95_men_med$conf.int[1])/2
int_ic95_med_med

ic95_men_dis <- t.test(men_dis$mm)
int_ic95_men_dis <- (ic95_men_dis$conf.int[2]-ic95_men_dis$conf.int[1])/2
int_ic95_men_dis

media_pul_pro <- mean(pul_pro$mm)
media_pul_dis <- mean(pul_dis$mm)
media_ind_pro <- mean(ind_pro$mm)
media_ind_med <- mean(ind_med$mm)
media_ind_dis <- mean(ind_dis$mm)
media_med_pro <- mean(med_pro$mm)
media_med_dis <- mean(med_dis$mm)
media_med_med <- mean(med_med$mm)
media_anu_pro <- mean(anu_pro$mm)
media_anu_dis <- mean(anu_dis$mm)
media_anu_med <- mean(anu_med$mm)
media_men_pro <- mean(men_pro$mm)
media_men_dis <- mean(men_dis$mm)
media_men_med <- mean(men_med$mm)



round(mean(pul_pro$mm), 2)
round(mean(pul_dis$mm), 2)
round(mean(ind_pro$mm), 2)
round(mean(ind_dis$mm), 2)
round(mean(ind_med$mm), 2)
round(mean(med_pro$mm), 2)
round(mean(med_dis$mm), 2)
round(mean(med_med$mm), 2)
round(mean(anu_pro$mm), 2)
round(mean(anu_dis$mm), 2)
round(mean(anu_med$mm), 2)
round(mean(men_pro$mm), 2)
round(mean(men_dis$mm), 2)
round(mean(men_med$mm), 2)

# Parte 2 t test entre datos

t.test (ind_pro$mm, anu_pro$mm)

t.test (ind_dis$mm, anu_dis$mm)

t.test (ind_med$mm, anu_med$mm)

t.test (men_dis$mm, med_dis$mm)

t.test (men_pro$mm, med_pro$mm)

# Graficas IC50

ggplot()


identificacion = c("pul_pro", "pul_dis", "ind_pro", "ind_dis", "ind_med", "med_pro", "med_dis", "med_med", "anu_pro", "anu_dis", "anu_med", "men_pro", "men_dis", "men_med")
media = c(media_pul_pro,
  media_pul_dis,
  media_ind_pro,
  media_ind_med,
  media_ind_dis,
  media_med_pro,
  media_med_dis,
  media_med_med,
  media_anu_pro,
  media_anu_dis,
  media_anu_med,
  media_men_pro,
  media_men_dis,
  media_men_med
)

Intervalo_bajo = c(
  ic95_pul_pro$conf.int[1],
  ic95_pul_dis$conf.int[1],
  ic95_ind_pro$conf.int[1],
  ic95_ind_med$conf.int[1],
  ic95_ind_dis$conf.int[1],
  ic95_med_pro$conf.int[1],
  ic95_med_dis$conf.int[1],
  ic95_med_med$conf.int[1],
  ic95_anu_pro$conf.int[1],
  ic95_anu_dis$conf.int[1],
  ic95_anu_med$conf.int[1],
  ic95_men_pro$conf.int[1],
  ic95_men_dis$conf.int[1],
  ic95_men_med$conf.int[1]
)

Intervalo_alto = c(
  ic95_pul_pro$conf.int[2],
  ic95_pul_dis$conf.int[2],
  ic95_ind_pro$conf.int[2],
  ic95_ind_med$conf.int[2],
  ic95_ind_dis$conf.int[2],
  ic95_med_pro$conf.int[2],
  ic95_med_dis$conf.int[2],
  ic95_med_med$conf.int[2],
  ic95_anu_pro$conf.int[2],
  ic95_anu_dis$conf.int[2],
  ic95_anu_med$conf.int[2],
  ic95_men_pro$conf.int[2],
  ic95_men_dis$conf.int[2],
  ic95_men_med$conf.int[2]
)

Datos_ic95_grafica <- data.frame(identificacion, media, Intervalo_bajo, Intervalo_alto)

ggplot(Datos_ic95_grafica, aes(y = reorder(identificacion, -media), x = media)) +
  geom_point() +
  geom_errorbarh(aes(xmin = Intervalo_bajo, xmax = Intervalo_alto )) +
  geom_vline(xintercept = mean(BD_Falanges$mm), linetype = "dotted", color = "red") +
  annotate("text", x= mean(BD_Falanges$mm)-0.5, y= 13, label="Media Falanges", angle=90) +
  xlab("ic95") + ylab("Dedo_Falange") +
  labs(title = "Media e intervalo de confianza 95% de falanges", caption = "Realizado por: Angel Martinez")


# Graficas de histograma
  

dens_ind_pro <- density(ind_pro$mm)
dens_anu_pro <- density(anu_pro$mm)

plot(dens_ind_pro, main = "Gráfico de densidad de las falanges del índice proximal contra anular proximal", col = "firebrick4", lwd = 2, ylab = "Densidad", sub = "Realizado por: Angel Martínez")
lines(dens_anu_pro, lty = 4  , lwd = 2, col = "darkblue")

legend("topleft", c("Índice proximal", "Anular proximal"),
       lty = c(1, 4),
       col = c("firebrick4", "darkblue"),
       text.col =  c("firebrick4", "darkblue"),
       bty = "n",
       title = "Funciones",
       title.col = "black"
       )


dens_ind_dis <- density(ind_dis$mm)
dens_anu_dis <- density(anu_dis$mm)

plot(dens_ind_dis, main = "Gráfico de densidad de las falanges del índice distal contra anular distal", col = "firebrick4", lwd = 2, ylab = "Densidad", sub = "Realizado por: Angel Martínez")
lines(dens_anu_dis, lty = 4  , lwd = 2, col = "darkblue")

legend("topleft", c("Índice distal", "Anular distal"),
       lty = c(1, 4),
       col = c("firebrick4", "darkblue"),
       text.col =  c("firebrick4", "darkblue"),
       bty = "n",
       title = "Funciones",
       title.col = "black"
)


dens_ind_med <- density(ind_med$mm)
dens_anu_med <- density(anu_med$mm)

plot(dens_ind_med, main = "Gráfico de densidad de las falanges del índice medio contra anular medio", col = "firebrick4", lwd = 2, ylab = "Densidad" , sub = "Realizado por: Angel Martínez")
lines(dens_anu_med, lty = 4  , lwd = 2, col = "darkblue")

legend("topleft", c("Índice medio", "Anular medio"),
       lty = c(1, 4),
       col = c("firebrick4", "darkblue"),
       text.col =  c("firebrick4", "darkblue"),
       bty = "n",
       title = "Funciones",
       title.col = "black"
)

dens_men_pro <- density(men_pro$mm)
dens_med_pro <- density(med_pro$mm)

plot(dens_men_pro, main = "Gráfico de densidad de las falanges del meñique proximal contra medio proximal", col = "firebrick4", lwd = 2, ylab = "Densidad", sub = "Realizado por: Angel Martínez")
lines(dens_med_pro, lty = 4  , lwd = 2, col = "darkblue")

legend("topleft", c("Meñique proximal", "Medio proximal"),
       lty = c(1, 4),
       col = c("firebrick4", "darkblue"),
       text.col =  c("firebrick4", "darkblue"),
       bty = "n",
       title = "Funciones",
       title.col = "black"
)


dens_men_dis <- density(men_dis$mm)
dens_med_dis <- density(med_dis$mm)

plot(dens_men_dis, main = "Gráfico de densidad de las falanges del meñique distal contra medio distal", col = "firebrick4", lwd = 2, ylab = "Densidad", sub = "Realizado por: Angel Martínez")
lines(dens_med_dis, lty = 4  , lwd = 2, col = "darkblue")

legend("topleft", c("Meñique distal", "Medio distal"),
       lty = c(1, 4),
       col = c("firebrick4", "darkblue"),
       text.col =  c("firebrick4", "darkblue"),
       bty = "n",
       title = "Funciones",
       title.col = "black"
)

# Graficas de boxplot

boxplot(ind_pro$mm, anu_pro$mm, 
        names = c("Índice proximal", "Anular proximal"),
        main = c("Gráfico de cajas de las falanges", "del índice proximal contra anular proximal"), 
        ylab = "milimetros", 
        col = c("firebrick", "cyan"),
        sub = "Realizado por: Angel Martínez")

boxplot(ind_dis$mm, anu_dis$mm, 
        names = c("Índice distal", "Anular distal"),
        main = c("Gráfico de cajas de las falanges", "del índice distal contra anular distal"), 
        ylab = "milimetros", 
        col = c("firebrick", "cyan"),
        sub = "Realizado por: Angel Martínez")

boxplot(ind_med$mm, anu_med$mm, 
        names = c("Índice medio", "Anular medio"),
        main = c("Gráfico de cajas de las falanges", "del índice medio contra anular medio"), 
        ylab = "milimetros", 
        col = c("firebrick", "cyan"),
        sub = "Realizado por: Angel Martínez")

boxplot(men_pro$mm, med_pro$mm, 
        names = c("Meñique proximal", "Medio proximal"),
        main = c("Gráfico de cajas de las falanges", "del meñique proximal contra medio proximal"), 
        ylab = "milimetros", 
        col = c("firebrick", "cyan"),
        sub = "Realizado por: Angel Martínez")

boxplot(men_dis$mm, med_dis$mm, 
        names = c("Meñique distal", "Medio distal"),
        main = c("Gráfico de cajas de las falanges", "del meñique distal contra medio distal"), 
        ylab = "milimetros", 
        col = c("firebrick", "cyan"),
        sub = "Realizado por: Angel Martínez")

