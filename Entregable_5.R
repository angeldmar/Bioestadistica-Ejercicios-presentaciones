# Generar datos
datos <- rnorm(1000, mean = 500, sd = 3)
bd <- data.frame( "No." = 1:1000, "Valor"=datos)
head(bd)
str(bd)

# Resumen grafico

n_10 <- sample(x = bd$Valor, size = 10)
n_10

n_50 <- sample(x = bd$Valor, size = 50)
n_50

n_100 <- sample(x = bd$Valor, size = 100)
n_100

n_500 <- sample(x = bd$Valor, size = 500)
n_500

n_1000 <- sample(x = bd$Valor, size = 1000)
n_1000

n_10_t <- t.test(n_10)
n_10_t

ic_95_n10 <- (n_10_t$conf.int[2] - n_10_t$conf.int[1])/2
ic_95_n10

n_50_t <- t.test(n_50)
n_50_t

ic_95_n50 <- (n_50_t$conf.int[2] - n_50_t$conf.int[1])/2
ic_95_n50

n_100_t <- t.test(n_100)
n_100_t

ic_95_n100 <- (n_100_t$conf.int[2] - n_100_t$conf.int[1])/2
ic_95_n100

n_500_t <- t.test(n_500)
n_500_t

ic_95_n500 <- (n_500_t$conf.int[2] - n_500_t$conf.int[1])/2
ic_95_n500

n_1000_t <- t.test(n_1000)
n_1000_t

ic_95_n1000 <- (n_1000_t$conf.int[2] - n_1000_t$conf.int[1])/2
ic_95_n1000

cuadro_ic95 <- data.frame("N"= c(10, 50, 100, 500, 1000), "Amplitud"= c(ic_95_n10, ic_95_n50, ic_95_n100, ic_95_n500, ic_95_n1000))
cuadro_ic95

plot(cuadro_ic95$N, cuadro_ic95$Amplitud, type="b")
