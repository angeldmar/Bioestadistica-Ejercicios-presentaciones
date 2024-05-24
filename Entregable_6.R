library(readxl)
Dennison_datos <- read_excel("Dennison_-_datos.xlsx")
View(Dennison_datos)

# ¿Cuánto jugo (oz) consume el grupo de niños que
# participaron en el estudio?

# Dennison_datos$GENDER = as.factor(Dennison_datos$GENDER)
str(Dennison_datos)
str(Boys_Dennison)
Boys_Dennison <- subset(Dennison_datos, GENDER == 0)
Boys_Dennison

meanOZJuiceBoys <- mean(Boys_Dennison$JUICE)
tTestOzJuiceBoys <- t.test(Boys_Dennison$JUICE)
tTestOzJuiceBoys

IC95OzJuiceBoys <- (tTestOzJuiceBoys$conf.int[2]-tTestOzJuiceBoys$conf.int[1])/2

paste0(meanOZJuiceBoys, " +- ", IC95OzJuiceBoys)



#  ¿El grupo de niños menores de 3.5 años consume la
# misma cantidad de jugo (oz) que el grupo de niños
# mayores de 3.5 años

BoysUnderAge35  <- subset(Dennison_datos, GENDER == 0 & AGE < 3.5)
BoysUnderAge35

BoysOverAge35 <- subset(Dennison_datos, GENDER == 0 & AGE >= 3.5)
BoysOverAge35

shapiro.test(BoysUnderAge35$JUICE)
shapiro.test(BoysOverAge35$JUICE)

t.test(BoysUnderAge35$JUICE, BoysOverAge35$JUICE)

wilcox.test(BoysUnderAge35$JUICE, BoysOverAge35$JUICE)
# ¿Niños y niñas menores de 3.5 años consume la
# misma cantidad de jugo (oz)?

GirlsUnderAge35 <- subset(Dennison_datos, GENDER == 1 & AGE < 3.5)
GirlsUnderAge35

shapiro.test(GirlsUnderAge35$JUICE)

t.test(BoysUnderAge35$JUICE, GirlsUnderAge35$JUICE)

wilcox.test(BoysUnderAge35$JUICE, GirlsUnderAge35$JUICE)
# Niños y niñas mayores de 3.5 años consume la
# misma cantidad de jugo (oz)?

GirlsOverAge35 <- subset(Dennison_datos, GENDER == 1 & AGE < 3.5)
GirlsOverAge35

shapiro.test(GirlsOverAge35$JUICE)

t.test(BoysOverAge35$JUICE, GirlsOverAge35$JUICE)

wilcox.test(BoysOverAge35$JUICE, GirlsOverAge35$JUICE)
