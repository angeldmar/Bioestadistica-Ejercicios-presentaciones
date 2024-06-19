load("ejemplo.Rdat")
View(datos_factor_ordinal)

ej_fact_ordinal <- datos_factor_ordinal

str(ej_fact_ordinal)

ej_fact_ordinal$Nivel_felicidad <-
  factor(ej_fact_ordinal$Nivel_felicidad, 
         order=TRUE, 
         levels=c("Nada", "Poco", "Medio", "Mucho"))

ej_fact_ordinal$Categoría_ingreso <-
  factor(ej_fact_ordinal$Categoría_ingreso, 
  order=TRUE, 
  levels=c("Muy bajo", "Bajo", "Medio", "Alto", "Muy alto"))

str(ej_fact_ordinal)
