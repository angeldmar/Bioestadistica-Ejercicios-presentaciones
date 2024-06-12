library(readxl)
StromBD <- read_excel("Progredi_Excel_2.xlsx")
View(StromBD)

StromBD$Group <- as.factor(StromBD$Group)
StromBD$Sex <- as.factor(StromBD$Sex)
StromBD$Marital_status <- as.factor(StromBD$Marital_status)
StromBD$Education <- as.factor(StromBD$Education)
StromBD$Medication <- as.factor(StromBD$Medication)
StromBD$Psychotherapy <- as.factor(StromBD$Psychotherapy)
# StromBD$Imputed_post <- as.factor(StromBD$Imputed_post)
# StromBD$Imputed_followup <- as.factor(StromBD$Imputed_followup)
# StromBD$BDI_post_under14 <- as.factor(StromBD$BDI_post_under14)

# 1. ¿Cuáles son las proporciones de los sexos en los
# participantes del estudio?

# Tabla de contigencia
tablaContigenciaSexo <- xtabs(~Sex, data = StromBD)
tablaContigenciaSexo

# Proporciones
proporcionesSexo <- proportions(tablaContigenciaSexo)
proporcionesSexo

# Intervalos de confianza
# Sexo 1 : 8 participante, de 48 participantes.
testProporcionesSexo01 <- prop.test(8, 48)
# Sexo 2: 40 participante, de 48 participantes.
testProporcionesSexo02 <- prop.test(40, 48)

testProporcionesSexo01
testProporcionesSexo02

IC95ProporcionesSexo01 <- (testProporcionesSexo01$conf.int[2]-testProporcionesSexo01$conf.int[1])/2
IC95ProporcionesSexo02 <- (testProporcionesSexo02$conf.int[2]-testProporcionesSexo02$conf.int[1])/2

IC95ProporcionesSexo01
IC95ProporcionesSexo02



# 2. Los participantes fueron asignados
# (aleatoriamente) a dos grupos ¿Cuál es la proporción
# de sexos en cada grupo?

tablaContigenciaSexoGrupo <- xtabs(~Sex + Group, data = StromBD) 
tablaContigenciaSexoGrupo

testProporcionesSexoGrupo <- prop.test(tablaContigenciaSexoGrupo)
testProporcionesSexoGrupo

IC95ProporcionesSexoGrupo <- (testProporcionesSexoGrupo$conf.int[2]-testProporcionesSexoGrupo$conf.int[1])/2

IC95ProporcionesSexo02

plot(tablaContigenciaSexoGrupo,
     main = "Composición de la muestra",
     ylab = "Grupo",
     xlab = "Sexo",
     col = c("#109EFB", "#D98880"),
     sub = "Realizado por Angel Martínez"
     )

# 3. ¿Las variables Sexo y Grupo son independientes?.

# xi cuadrado

chisqTestSexoGrupo <- chisq.test(tablaContigenciaSexoGrupo)
chisqTestSexoGrupo

# 4. ¿La variable es independiente de otras variables
# contempladas en el estudio (educación, medicación, psicoterapia)?

# Asumo que se refiere a Sexo

tablaContigenciaSexoEducacion <- xtabs(~Sex + Education, data = StromBD)
tablaContigenciaSexoEducacion
chisqTestSexoEducacion <- chisq.test(tablaContigenciaSexoEducacion)
chisqTestSexoEducacion

tablaContigenciaSexoMedicacion <- xtabs(~Sex + Medication, data = StromBD)
tablaContigenciaSexoMedicacion
chisqTestSexoMedicacion <- chisq.test(tablaContigenciaSexoMedicacion)
chisqTestSexoMedicacion

tablaContigenciaSexoPsicoterapia <- xtabs(~Sex + Psychotherapy, data = StromBD)
tablaContigenciaSexoPsicoterapia
chisqTestSexoPsicoterapia <- chisq.test(tablaContigenciaSexoPsicoterapia)
chisqTestSexoPsicoterapia

# 4.1. En general ¿El sexo de los participantes
# del estudio influye en su estado civil?

#Cambio en el tono de la pregunta mismo metodo. 


tablaContigenciaSexoEstadoCivil <- xtabs(~Sex + Marital_status, data = StromBD)
tablaContigenciaSexoEstadoCivil
chisqTestSexoEstadoCivil <- chisq.test(tablaContigenciaSexoEstadoCivil)
chisqTestSexoEstadoCivil

# 4.2. En general ¿Hay evidencia de que el nivel
# educativo esté influenciado por el sexo en los participantes del estudio?

# Ya lo habiamos realizado, pero es diferente pregunta

chisqTestSexoEducacion


# 4.2. En general ¿Hay evidencia de que el nivel
# educativo esté influenciado por el sexo en los participantes del estudio?

chisqTestSexoPsicoterapia
