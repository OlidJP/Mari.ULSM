#================================================================================================================================================================================================================================================#
#   CONFIRMATORY FACTOR ANALYSIS (CFA) -  Estimacion de Minimos Cuadrados no Ponderados (ULS: Unweighted Least Squares)
#================================================================================================================================================================================================================================================#

#Funcion Para Instalar Varios Paquetes.

ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

# Paquetes a Usar
packages <- c("dplyr","readxl","parameters","apa", "apaTables","haven","ggplot2","ggpubr","gridExtra",
              "apaTables", "reshape", "GPArotation", "mvtnorm", "psych", "psychometric", "nFactors",
              "semPlot", "lavaan", "MVN", "semTools", "diagram")
ipak(packages)

#Importar la Base de Datos (Original)
BD_M <- read_excel("G:/GitHub/M/BaseDatosMariVia.xlsx")
View(BD_M)


#Especificación del modelo conceptual
SixFactor<-'EA=~ ITEM08 + ITEM09 + ITEM14 + ITEM16 + ITEM18 + ITEM19 + ITEM25 + ITEM40 + ITEM45
EC=~ ITEM15 + ITEM21 + ITEM22 + ITEM23 + ITEM24 + ITEM26 + ITEM27 + ITEM31 + ITEM33 + ITEM37 + ITEM38
EPE=~ ITEM20 + ITEM29 + ITEM34 + ITEM35 + ITEM36 + ITEM42 
EPt=~ ITEM01 + ITEM02 + ITEM04 + ITEM28 + ITEM30 + ITEM41
ECp=~ ITEM03 + ITEM05 + ITEM06 + ITEM07 + ITEM11 + ITEM13 + ITEM32 + ITEM43
EP=~ ITEM10 + ITEM12 + ITEM17 + ITEM39 + ITEM44'

#Análisis Factorial Confirmatorio para seis dimensiones. Estimadores

#Usaremos el paquete lavaan

AFCSixFactor<-cfa(SixFactor,orthogonal=FALSE, data = BD_M, estimator="ULSM", ordered = names(BD_M))
lavInspect(SixFactor, "cov.lv")

#Muestra los Indices de Ajuste de la estimacion
summary(AFCSixFactor, fit.measures=TRUE, standardized=TRUE)


#Mostar todos los Indices de Ajuste de la Estimacion
fitMeasures(AFCSixFactor)

#Graficar el AFC del Modelo
semPaths(AFCSixFactor, intercepts = FALSE,edge.label.cex=1, optimizeLatRes = TRUE,
         groups = "lat",pastel = TRUE, exoVar = FALSE, sizeInt=5,edge.color ="black",
         esize = 6, label.prop=1,sizeLat = 7,"std", layout="circle2")


semPaths(AFCSixFactor, intercepts = FALSE,edge.label.cex=1.5, optimizeLatRes = TRUE, groups = "lat",pastel = TRUE, exoVar = FALSE, sizeInt=5,edge.color ="black",esize = 6, label.prop=2,sizeLat = 6,"std", layout="circle2")




#================================================================================================================================================================================================================================================#
#                       AYUDAS
#================================================================================================================================================================================================================================================#

# https://germangfeler.github.io/datascience/barras-y-tortas/                               ++++++++
# https://www.youtube.com/watch?v=aJBiXcjQZiA                                               ++ Barras
# https://www.youtube.com/watch?v=EQNm0Dcte3Y                                               ++ Pastel
# http://rstudio-pubs-static.s3.amazonaws.com/5312_98fc1aba2d5740dd849a5ab797cc2c8d.html    +++ Colores
# https://es.r4ds.hadley.nz/comunicar-con-gr%C3%A1ficos.html#escalas                        +++ Colores y Mas
#https://www.youtube.com/watch?v=1QTMide5wUA                                                +++ Editor de ejes

#================================================================================================================================================================================================================================================#
#                       Guias GGPLOP2
#================================================================================================================================================================================================================================================#

# https://bookdown.org/gboccardo/manual-ED-UCH/construccion-de-graficos-usando-rstudio-funcionalidades-basicas-y-uso-del-paquete-ggplot2.html       +++Guia Ggplop2
# https://arcruz0.github.io/libroadp/dataviz.html