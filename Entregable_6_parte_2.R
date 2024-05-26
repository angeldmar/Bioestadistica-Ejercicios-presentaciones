library(pwr)
# Esta función es útil para calcular el número de muestras necesario por
# grupo para alcanzar un poder determinado con un tamaño del efecto definido.
# Se emplea cuando se ha establecido que se llevará a cabo una prueba t de
# dos grupos.
# Ajuste los valores del tamaño del efecto y poder en los argumentos “d=” y
# “power=” respectivamente para obtener el valor “n” (número de muestras)

pwr.t.test(d=0.1, sig.level=0.05, power=0.8)

# Esta función también es útil para calcular el poder según un número de
# muestras determinado. Sustituya el argumento “power=” con el argumento
# “n=”

pwrt_5 <- pwr.t.test(d=0.1, sig.level=0.05, n=5)
str(pwrt_5)
pwrt_5$power * 100


# Esta función es útil para calcular el número de muestras necesario por
# grupo para alcanzar un poder determinado con un tamaño del efecto definido.
# Se emplea cuando se ha establecido que se llevará a cabo un análisis de
# varianza (tres o más grupos).
# La lógica del uso es la misma que para la función pwr.t.test()

pwr.anova.test(k = 3, n = 20, f = 0.1, sig.level = 0.05)

dat <- data.frame("N" = c ("5", "50", "500", "1000", "2000", "3000"), "Poder" = 1:6)
dat

#Vamos a calcular el poder por numero de muestras con t.test

pwrt_50 <- pwr.t.test(d=0.1, sig.level=0.05, n=50)
pwrt_500 <- pwr.t.test(d=0.1, sig.level=0.05, n=500)
pwrt_1000 <- pwr.t.test(d=0.1, sig.level=0.05, n=1000)
pwrt_2000 <- pwr.t.test(d=0.1, sig.level=0.05, n=2000)
pwrt_3000 <- pwr.t.test(d=0.1, sig.level=0.05, n=3000)


dat$Poder[1] <- pwrt_5$power * 100
dat$Poder[2] <- pwrt_50$power * 100
dat$Poder[3] <- pwrt_500$power * 100
dat$Poder[4] <- pwrt_1000$power * 100
dat$Poder[5] <- pwrt_2000$power * 100
dat$Poder[6] <- pwrt_3000$power * 100

dat

plot(Poder ~ N, 
     data = dat, 
     type = "b",
     col = "darkblue",
     lty = 2,
     main = "Poder contra número de muestras para el test T de Student",
     sub = "Realizado por: Angel Martínez")


# Ahora poder con Anova

dat2 <- data.frame("N" = c ("5", "50", "500", "1000", "2000", "3000"), "Poder" = 1:6)
dat2

pwranova_5 <- pwr.anova.test(k =3, f=0.1,sig.level=0.05, n=5)
pwranova_50 <- pwr.anova.test(k =3, f=0.1,sig.level=0.05, n=50)
pwranova_500 <- pwr.anova.test(k =3, f=0.1, sig.level=0.05, n=500)
pwranova_1000 <- pwr.anova.test(k =3, f=0.1, sig.level=0.05, n=1000)
pwranova_2000 <- pwr.anova.test(k =3, f=0.1, sig.level=0.05, n=2000)
pwranova_3000 <- pwr.anova.test(k =3, f=0.1, sig.level=0.05, n=3000)


dat2$Poder[1] <- pwranova_5$power * 100
dat2$Poder[2] <- pwranova_50$power * 100
dat2$Poder[3] <- pwranova_500$power * 100
dat2$Poder[4] <- pwranova_1000$power * 100
dat2$Poder[5] <- pwranova_2000$power * 100
dat2$Poder[6] <- pwranova_3000$power * 100

dat2

plot(Poder ~ N, 
     data = dat2, 
     type = "b",
     col = "red4",
     lty = 3,
     main = "Poder contra número de muestras para el test de ANOVA",
     sub = "Realizado por: Angel Martínez")
