## ELECTIVO: HERRAMIENTAS PARA EL ANÁLISIS CUANTITATIVO: CIENCIA ABIERTA Y SOFTWARE LIBRE ##
## MAGISTER EN CIENCIAS SOCIALES ##
## UNIVERSIDAD DE CHILE ##
## Profesores: Cristobal Moya y Felipe Ruiz ##

# Construcción de tablas

load("Data/intermediate Data/P_CEP2019.RData")

# Descripción de la muestra de Exposición a info de RRSS ------------------

## Descriptivos de la muestra. Colocar en Anexo N°1
freq(CEP_Electivo$Expo_RRSS, weights = CEP_Electivo$POND, na.rm = TRUE)

#Exposición a información de RRSS, Según sexo, edad y Nivel Educativo, en Anexo N°2

ctable(CEP_Electivo$Expo_RRSS, CEP_Electivo$SexoRecod, prop = "c", weights = CEP_Electivo$POND)
ctable(CEP_Electivo$Expo_RRSS, CEP_Electivo$Edadrec, prop = "c", weights = CEP_Electivo$POND)
ctable(CEP_Electivo$Expo_RRSS, CEP_Electivo$NivelEduc, prop = "c", weights = CEP_Electivo$POND)

# Resultados sobre percepción de democracia según Exposición a info de RRSS --------


## [RESULTADO N°1] ##
## Grado de acuerdo con pregunta:
##"Las políticas públicas generalmente reflejan lo que quiere la mayoría de los ciudadanos"
## 1 = Muy de acuerdo  5 = muy en desacuerdo

Tabla1.1 <-descr(CEP_Electivo$PPubMayo, transpose = T, stats = c("mean", "med", "sd", "cv"),
                 weights = CEP_Electivo$POND, style = 'rmarkdown')

Tabla1.2 <-ctable(CEP_Electivo$Expo_RRSS, CEP_Electivo$PPubMayo,prop = "r", weights = CEP_Electivo$POND, na.rm = T,
                  style = 'rmarkdown')

#Varianza y curtosis pregunta 1
TabAnexo1 <-CEP_Electivo %>%
  group_by(Expo_RRSS) %>%
  summarise(VarGrupal1 = var(PPubMayo, na.rm = T),
            KurtoGrupal1 = psych::kurtosi(PPubMayo, na.rm = T))

## [RESULTADO N°2] ##
#Grado de acuerdo con pregunta:
##"Los ciudadanos más ricos tienen más influencia que los demás ciudadanos en las políticas públicas" según exposición
## a información en RRSS
## 1 = Muy de acuerdo  5 = muy en desacuerdo

#desriptivos univariados:
Tabla2.1 <-descr(CEP_Electivo$InflRicos, transpose = T, stats = c("mean", "med", "sd", "cv"),
                 weights = CEP_Electivo$POND, style = 'rmarkdown')

#bivariado según exposición a RRSS
Tabla2.2 <-ctable(CEP_Electivo$Expo_RRSS, CEP_Electivo$InflRicos,prop = "r", weights = CEP_Electivo$POND,
                  style = 'rmarkdown')
#Varianza y curtosis pregunta 2
TabAnexo2 <-CEP_Electivo %>%
  group_by(Expo_RRSS) %>%
  summarise(VarGrupal1 = var(InflRicos, na.rm = T),
            KurtoGrupal1 = psych::kurtosi(InflRicos, na.rm = T))

## [RESULTADO N°3] ##
##"Las empresas y los grupos de interés influyen enormemente en las políticas públicas" según exposición a RRSS
## 1 = Muy de acuerdo  5 = muy en desacuerdo

#Descriptivo univariado
Tabla3.1 <-descr(CEP_Electivo$InflEmpre, transpose = T, stats = c("mean", "med", "sd", "cv"),
                 weights = CEP_Electivo$POND, style = 'rmarkdown')

#Bivariado según exposición a RRSS
Tabla3.2 <-ctable(CEP_Electivo$Expo_RRSS, CEP_Electivo$InflEmpre, prop = "r", weights = CEP_Electivo$POND,
                  style = 'rmarkdown')

#Varianza y curtosis pregunta 3
TabAnexo3 <-CEP_Electivo %>%
  group_by(Expo_RRSS) %>%
  summarise(VarGrupal1 = var(InflEmpre, na.rm = T),
            KurtoGrupal1 = psych::kurtosi(InflEmpre, na.rm = T))

# Resultados sobre percepción de partidos políticos, según exposición a info de RRSS --------

## [RESULTADO N°4] ##
##Grado de acuerdo con pregunta
##"A Los partidos políticos reflejan las demandas de los ciudadanos"
## 1 = Muy de acuerdo  5 = muy en desacuerdo

Tabla4.1 <-descr(CEP_Electivo$DemanPPol, transpose = T, stats = c("mean","med","sd","cv"), weights = CEP_Electivo$POND,
                 style = 'rmarkdown')
Tabla4.2 <-ctable(CEP_Electivo$Expo_RRSS, CEP_Electivo$DemanPPol, prop = "r", weights = CEP_Electivo$POND,
                  style = 'rmarkdown')

#Varianza y curtosis pregunta 4
TabAnexo4 <-CEP_Electivo %>%
  group_by(Expo_RRSS) %>%
  summarise(VarGrupal1 = var(DemanPPol, na.rm = T),
            KurtoGrupal1 = psych::kurtosi(DemanPPol, na.rm = T))

## [RESULTADO N°5] ##
##Grado de acuerdo con pregunta
##"Los partidos políticos son indispensables para la democracia"
## 1 = Muy de acuerdo  5 = muy en desacuerdo

Tabla5.1 <-descr(CEP_Electivo$PPolIndis, transpose = T, stats = c("mean", "med", "sd", "cv"),
                 weights = CEP_Electivo$POND, style = 'rmarkdown')
Tabla5.2 <-ctable(CEP_Electivo$Expo_RRSS, CEP_Electivo$PPolIndis, prop = "r", weights = CEP_Electivo$POND,
                  style = 'rmarkdown')

#Varianza y curtosis pregunta 5
TabAnexo5 <-CEP_Electivo %>%
  group_by(Expo_RRSS) %>%
  summarise(VarGrupal1 = var(PPolIndis, na.rm = T),
            KurtoGrupal1 = psych::kurtosi(PPolIndis, na.rm = T))

#generación de un solo objeto para tablas
ResulTablas <- list(Tabla1.1, Tabla1.2, TabAnexo1, Tabla2.1, Tabla2.2, TabAnexo2, Tabla3.1, Tabla3.2,
                    TabAnexo3, Tabla4.1, Tabla4.2, TabAnexo4, Tabla5.1, Tabla5.2, TabAnexo5)

#Resultados de tablas
saveRDS(ResulTablas, file = "Data/Analysis-Data/Tablas-reporte.rds")

# Limpiar entorno de trabajo
rm(list=ls())


