## ELECTIVO: HERRAMIENTAS PARA EL ANÁLISIS CUANTITATIVO: CIENCIA ABIERTA Y SOFTWARE LIBRE ##
## MAGISTER EN CIENCIAS SOCIALES ##
## UNIVERSIDAD DE CHILE ##
## Profesores: Cristobal Moya y Felipe Ruiz ##

#Parte 1: Carga de base de datos

CEP_Electivo <-as.data.frame(read_spss("Data/InputData/DataSources/Encuesta CEP 84 Dic 2019 v1.sav"))

CEP_Electivo <- select(CEP_Electivo,ESP_5_1,ESP_5_3,ESP_5_4,ESP_6_1,
                       ESP_6_2,DS_P1,DS_P2_EXACTA,DS_P4,ESP_30_1:ESP_30_4, PONDERADOR)

save(CEP_Electivo, file = "Data/intermediate Data/P_CEP2019.RData")

# Limpiar entorno de trabajo
rm(list=ls())