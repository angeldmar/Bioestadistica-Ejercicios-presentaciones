Enteros <- 1:10 
Flotantes <- Enteros*0.1
Booleanos <- (Enteros<5)
Dias_Semana <- c("Lunes", "Martes", "Miercoles", "Jueves", "Viernes", "Sabado", "Domingo", "Lunes", "Martes", "Miercoles")
Binarios <- Booleanos*1
Estaciones <- c("Primavera", "Invierno", "Verano", "Otono", "Primavera", "Invierno", "Verano", "Otono", "Verano", "Otono")
Logaritmo <- log(Flotantes)
Coseno <- cos(Enteros)
Exponencial <- exp(Flotantes)
Operacion <- Logaritmo + Coseno

BD02 <- data.frame(Enteros, Flotantes, Booleanos, Dias_Semana, Binarios, Estaciones, Logaritmo, Coseno, Exponencial, Operacion)

BD02