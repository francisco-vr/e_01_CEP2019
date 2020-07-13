## ELECTIVO: HERRAMIENTAS PARA EL ANÁLISIS CUANTITATIVO: CIENCIA ABIERTA Y SOFTWARE LIBRE ##
## MAGISTER EN CIENCIAS SOCIALES ##
## UNIVERSIDAD DE CHILE ##
## Profesores: Cristobal Moya y Felipe Ruiz ##

##  MASTER SCRIPT ##

# Instalación y carga de todos los paquetes
if (!require("pacman")) {
  install.packages("pacman")
}
pacman::p_load(tidyverse,
               haven,
               dplyr,
               car,
               summarytools,
               psych)

#Parte 1: carga y recorte de base de datos
# Expulsa archivo de datos para editar
source("Scripts/Processing/limpieza-base.R")

# Parte 2: Limpieza de base de datos
# recodifica y agrupa. expulsa base de datos lista para analizar

source("Scripts/Processing/preparacion-base.R")

# Parte 3: Construcción de resultados - Tablas
# realiza tablas descriptivas y cruzadas de variables seleccionadas. expulsa objeto con todas las tablas

source("Scripts/Analysis/Construccion-tablas.R")

# Parte 4: Construcción de resultados - Gráficos
# Realiza gráficos por cada variable analizada. Expulsa imagenes por cada gráfico y un objeto con todos

source("Scripts/Analysis/Construccion-graficos.R")

# Parte 5: Crea reporte de investigación
# En base a los resultados genera reporte reproducible

rmarkdown::render('reporte_reproducible.Rmd')

# Limpiar entorno de trabajo
rm(list=ls())



