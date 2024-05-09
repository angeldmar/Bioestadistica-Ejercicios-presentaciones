library(readxl)
Base_de_Datos_Falanges <- read_excel("Base de Datos Falanges.xlsx")
View(Base_de_Datos_Falanges)

# se podría exportar con el portapapeles usando
# BD <- read.delim("clipboard", header = TRUE)
# head(BD)

#Poniendo en minúsculas

head(Base_de_Datos_Falanges)
str(Base_de_Datos_Falanges)

Base_de_Datos_Falanges$DEDO <- tolower(Base_de_Datos_Falanges$DEDO)
Base_de_Datos_Falanges$FALANGES <- tolower(Base_de_Datos_Falanges$FALANGES)
Base_de_Datos_Falanges$MANO <- tolower(Base_de_Datos_Falanges$MANO)
Base_de_Datos_Falanges$Genero <- tolower(Base_de_Datos_Falanges$Genero)

# Unificando Valores

Base_de_Datos_Falanges$DEDO <- ifelse(Base_de_Datos_Falanges$DEDO == "meñ", "men", Base_de_Datos_Falanges$DEDO)
Base_de_Datos_Falanges$DEDO <- ifelse(Base_de_Datos_Falanges$DEDO == "an", "anu", Base_de_Datos_Falanges$DEDO)

Base_de_Datos_Falanges$FALANGES <- ifelse(Base_de_Datos_Falanges$FALANGES == "distal" | Base_de_Datos_Falanges$FALANGES == "dist", "dis", Base_de_Datos_Falanges$FALANGES)
Base_de_Datos_Falanges$FALANGES <- ifelse(Base_de_Datos_Falanges$FALANGES == "proximal" | Base_de_Datos_Falanges$FALANGES == "prox", "pro", Base_de_Datos_Falanges$FALANGES)
Base_de_Datos_Falanges$FALANGES <- ifelse(Base_de_Datos_Falanges$FALANGES == "media" | Base_de_Datos_Falanges$FALANGES == "men" , "med", Base_de_Datos_Falanges$FALANGES)

Base_de_Datos_Falanges$MANO <- ifelse(Base_de_Datos_Falanges$MANO== "iz", "izq", Base_de_Datos_Falanges$MANO)

Base_de_Datos_Falanges$Genero <- ifelse(Base_de_Datos_Falanges$Genero == "fem", "f", Base_de_Datos_Falanges$Genero)

Base_de_Datos_Falanges$DEDO <- as.factor(Base_de_Datos_Falanges$DEDO)
Base_de_Datos_Falanges$FALANGES <- as.factor(Base_de_Datos_Falanges$FALANGES)
Base_de_Datos_Falanges$MANO <- as.factor(Base_de_Datos_Falanges$MANO)
Base_de_Datos_Falanges$Genero <- as.factor(Base_de_Datos_Falanges$Genero)

str(Base_de_Datos_Falanges)

write.csv(Base_de_Datos_Falanges, "BD_Falanges.csv", row.names = TRUE)

#Trabajo solicitado en el entregable

stem(Base_de_Datos_Falanges$mm)

hist(Base_de_Datos_Falanges$mm, main = "Histograma de longitud de falanges", col = "lightblue", border="white", xlab = "Milimetros", ylab = "Frecuencia")

boxplot(Base_de_Datos_Falanges$mm, main = "Boxplot de longitud de falanges", ylab = "milimetros", col = "red")
