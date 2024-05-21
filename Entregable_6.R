library(readxl)
Dennison_datos <- read_excel("Dennison_-_datos.xlsx")
View(Dennison_datos)

# ¿Cuánto jugo (oz) consume el grupo de niños que
# participaron en el estudio?



Dennison_datos$GENDER = as.factor(Dennison_datos$GENDER)
str(Dennison_datos)

Boys_Dennison <- subset(Dennison_datos, GENDER =)

#  ¿El grupo de niños menores de 3.5 años consume la
# misma cantidad de jugo (oz) que el grupo de niños
# mayores de 3.5 años
# 
# ¿Niños y niñas menores de 3.5 años consume la
# misma cantidad de jugo (oz)?
#   
# Niños y niñas mayores de 3.5 años consume la
# misma cantidad de jugo (oz)?