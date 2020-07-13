# [Entrega N°2]
# ELECTIVO: CIENCIA ABIERTA Y SOFTWARE LIBRE.
# MAGÍSTER EN CCSS UNIVERSIDAD DE CHILE - 2020 

# Carga de paquetes -------------------------------------------------------

#Carga de paquetes y archivo de base de datos original

library(haven)
library(tidyr)
library(tidyverse)
library(dplyr)

CEP_Electivo <-as.data.frame(read_spss("Data/InputData/DataSources/Encuesta CEP 84 Dic 2019 v1.sav"))


# Limpieza y recodificación de variables ----------------------------------


#Selección de variables a usar y creación de nueva base

CEP_Electivo <- select(CEP_Electivo,ESP_5_1,ESP_5_3,ESP_5_4,ESP_6_1,
                       ESP_6_2,DS_P1,DS_P2_EXACTA,DS_P4,ESP_30_1:ESP_30_4, PONDERADOR)

save(CEP_Electivo, file = "Data/intermediate Data/P_CEP2019.RData")

load("Data/intermediate Data/P_CEP2019.RData")

#   [Modificación de base de Datos]

#Cambiar nombres

CEP_Electivo <- rename(CEP_Electivo,
                       PPubMayo = ESP_5_1,
                       InflRicos = ESP_5_3,
                       InflEmpre = ESP_5_4,
                       DemanPPol = ESP_6_1,
                       PPolIndis = ESP_6_2,
                       ExpoTV = ESP_30_1,
                       ExpoDiario = ESP_30_2,
                       ExpoRadio = ESP_30_3,
                       ExpoRRSS = ESP_30_4,
                       Sexo = DS_P1,
                       Edad = DS_P2_EXACTA,
                       NivEduc = DS_P4,
                       POND = PONDERADOR)

# Recodificación sexo

CEP_Electivo$sexo <- as.numeric(CEP_Electivo$Sexo)
CEP_Electivo <- mutate(CEP_Electivo, SexoRecod = recode(CEP_Electivo$sexo, "1" = "hombre",
                                                        "2" = "mujer"))
class(CEP_Electivo$SexoRecod) #Queda como Character

#Recodificación edad
CEP_Electivo$Edad <- as.numeric(CEP_Electivo$Edad)
class(CEP_Electivo$Edad)
CEP_Electivo <- mutate(CEP_Electivo, Edadrec = car::recode(CEP_Electivo$Edad, "18:39 = 1; 40:64 = 2;
                                                           65:99 = 3"))
CEP_Electivo <- mutate(CEP_Electivo, Edadrec = recode(CEP_Electivo$Edadrec, "1" = "Postdictadura",
                                                      "2" = "Dictadura","3" = "Predictadura"))

#Las edades se agrupan según criterios expuestos en Lindh et.al(2019)donde observa en qué momento histórico
#Las personas se vuelven adultas. así está en "pre dictadura", donde se hicieron adultos antes de 1973, los
#"Dictadura" que se hicieron adultos entre 1973/1989 y los "postdictadura" que se hicieron adultos desde 1990

#Recodificación Nivel Educativo
CEP_Electivo$NivEduc<- as.numeric(CEP_Electivo$NivEduc)
table(CEP_Electivo$NivEduc)
CEP_Electivo <- mutate(CEP_Electivo, NivelEduc = car::recode(CEP_Electivo$NivEduc,
                                                             "0:1 = 1; 2:5 = 2; 6 = 3; 7:9 = 4; else = NA"))
CEP_Electivo <- mutate(CEP_Electivo, NivelEduc = recode(CEP_Electivo$NivelEduc, "1" = "Sin estudios",
                                                      "2" = "Secundaria e inferior","3" = "Técnica",
                                                      "4" = "Universitaria o superior"))
table(CEP_Electivo$NivelEduc)

#Recodificación exposición a información - RRSS
CEP_Electivo$ExpoRRSS <- as.numeric(CEP_Electivo$ExpoRRSS)
CEP_Electivo <-mutate(CEP_Electivo,Expo_RRSS = car::recode(CEP_Electivo$ExpoRRSS, "1:2 = 1; 3:5 = 2; else = NA"))
CEP_Electivo <- mutate(CEP_Electivo, Expo_RRSS = recode(CEP_Electivo$Expo_RRSS,"1" = "Mucha información",
                                                        "2" = "Poca información"))
table(CEP_Electivo$Expo_RRSS)

#Recodificación exposición a información - TV
CEP_Electivo$ExpoTV <-as.numeric(CEP_Electivo$ExpoTV)
CEP_Electivo <- mutate(CEP_Electivo, Expo_TV = car::recode(CEP_Electivo$ExpoTV,"1:2 = 1; 3:5 = 2; else = NA"))
CEP_Electivo <- mutate(CEP_Electivo, Expo_TV = recode(CEP_Electivo$Expo_TV, "1" = "Mucha información",
                                                      "2" = "Poca información"))
table(CEP_Electivo$Expo_TV)

#Recodificación Exposición - Radio
CEP_Electivo$ExpoRadio <- as.numeric(CEP_Electivo$ExpoRadio)
CEP_Electivo <- mutate(CEP_Electivo, Expo_Radio = car::recode(CEP_Electivo$ExpoRadio, "1:2 = 1; 3:5 = 2;
                                                             else = NA"))
CEP_Electivo <- mutate(CEP_Electivo, Expo_Radio = recode(CEP_Electivo$Expo_Radio,"1" = "Mucha información",
                                                         "2" = "Poca información"))

#Recodificación Exposición - Diarios

CEP_Electivo$ExpoDiario <- as.numeric(CEP_Electivo$ExpoDiario)
CEP_Electivo <- mutate(CEP_Electivo, Expo_Diario = car::recode(CEP_Electivo$ExpoDiario,"1:2 = 1; 3:5 = 2;
                                                             else = NA" ))
CEP_Electivo <- mutate(CEP_Electivo, Expo_Diario = recode(CEP_Electivo$Expo_Diario,"1" = "Mucha información",
                                                          "2" = "Poca información"))
# recodificar variables - acuerdo sobre democracia y partidos políticos

## Las políticas públicas son lo que quiere la mayoría de los ciudadanos
CEP_Electivo$PPubMayo <-as.numeric(CEP_Electivo$PPubMayo)
CEP_Electivo$PPubMayo[CEP_Electivo$PPubMayo==8] <- NA #se asigna NA a "8", equivalente a "No sabe"
CEP_Electivo$PPubMayo[CEP_Electivo$PPubMayo==9] <- NA #se Asigna NA a "9", equivalente a "No contesta"

## Influencia de los ricos en las políticas públicas
CEP_Electivo$InflRicos <-as.numeric(CEP_Electivo$InflRicos)
CEP_Electivo$InflRicos[CEP_Electivo$InflRicos==8]<- NA #se asigna NA a "8", equivalente a "No sabe"
CEP_Electivo$InflRicos[CEP_Electivo$InflRicos==9]<- NA #se Asigna NA a "9", equivalente a "No contesta"

## Empresas y grupos de interés influyen en las Políticas públicas
CEP_Electivo$InflEmpre <-as.numeric(CEP_Electivo$InflEmpre)
CEP_Electivo$InflEmpre[CEP_Electivo$InflEmpre==8] <- NA #se asigna NA a "8", equivalente a "No sabe"
CEP_Electivo$InflEmpre[CEP_Electivo$InflEmpre==9] <- NA #se Asigna NA a "9", equivalente a "No contesta"

#Los partidos políticos relejan las demandas de los ciudadanos
CEP_Electivo$DemanPPol <-as.numeric(CEP_Electivo$DemanPPol)
CEP_Electivo$DemanPPol[CEP_Electivo$DemanPPol==8] <-NA #se asigna NA a "8", equivalente a "No sabe"
CEP_Electivo$DemanPPol[CEP_Electivo$DemanPPol==9] <-NA #se Asigna NA a "9", equivalente a "No contesta"

#Los partidos políticos son indispensables para la democracia
CEP_Electivo$PPolIndis <-as.numeric(CEP_Electivo$PPolIndis)
CEP_Electivo$PPolIndis[CEP_Electivo$PPolIndis==8] <-NA #se asigna NA a "8", equivalente a "No sabe"
CEP_Electivo$PPolIndis[CEP_Electivo$PPolIndis==9] <-NA #se Asigna NA a "9", equivalente a "No contesta"

# Guardado de base de datos limpia en formato R.Data ----------------------



CEP_Electivo <- select(CEP_Electivo,PPubMayo, InflRicos, InflEmpre, DemanPPol, PPolIndis, Edadrec, POND, SexoRecod,
                       NivelEduc, Expo_RRSS, Expo_TV,Expo_Radio, Expo_Diario)

save(CEP_Electivo, file = "Data/intermediate Data/P_CEP2019.RData")


# Análisis dscriptivo de los datos #

# Análisis descriptivo de datos - Carga de paquetes -----------------------
library(summarytools)
library(ggplot2)
library(psych)

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

descr(CEP_Electivo$PPubMayo, transpose = T, stats = c("mean", "med", "sd", "cv"),
      weights = CEP_Electivo$POND)

ctable(CEP_Electivo$Expo_RRSS, CEP_Electivo$PPubMayo, weights = CEP_Electivo$POND, na.rm = T)

## [RESULTADO N°2] ##
#Grado de acuerdo con pregunta:
##"Los ciudadanos más ricos tienen más influencia que los demás ciudadanos en las políticas públicas" según exposición
## a información en RRSS
## 1 = Muy de acuerdo  5 = muy en desacuerdo

#desriptivos univariados:
descr(CEP_Electivo$InflRicos, transpose = T, stats = c("mean", "med", "sd", "cv"),
      weights = CEP_Electivo$POND)

#bivariado según exposición a RRSS
ctable(CEP_Electivo$Expo_RRSS, CEP_Electivo$InflRicos, weights = CEP_Electivo$POND)

## [RESULTADO N°3] ##
##"Las empresas y los grupos de interés influyen enormemente en las políticas públicas" según exposición a RRSS
## 1 = Muy de acuerdo  5 = muy en desacuerdo

#Descriptivo univariado
descr(CEP_Electivo$InflEmpre, transpose = T, stats = c("mean", "med", "sd", "cv"),
      weights = CEP_Electivo$POND)

#Bivariado según exposición a RRSS
ctable(CEP_Electivo$Expo_RRSS, CEP_Electivo$InflEmpre, prop = "r", weights = CEP_Electivo$POND)


# Resultados sobre percepción de partidos políticos, según exposición a info de RRSS --------

## [RESULTADO N°4] ##
##Grado de acuerdo con pregunta
##"A Los partidos políticos reflejan las demandas de los ciudadanos"
## 1 = Muy de acuerdo  5 = muy en desacuerdo

descr(CEP_Electivo$DemanPPol, transpose = T, stats = c("mean","med","sd","cv"), weights = CEP_Electivo$POND)
ctable(CEP_Electivo$Expo_RRSS, CEP_Electivo$DemanPPol, prop = "r", weights = CEP_Electivo$POND)

## [RESULTADO N°5] ##
##Grado de acuerdo con pregunta
##"Los partidos políticos son indispensables para la democracia"
## 1 = Muy de acuerdo  5 = muy en desacuerdo

descr(CEP_Electivo$PPolIndis, transpose = T, stats = c("mean", "med", "sd", "cv"), weights = CEP_Electivo$POND)
ctable(CEP_Electivo$Expo_RRSS, CEP_Electivo$PPolIndis, prop = "r", weights = CEP_Electivo$POND)


# Graficos de resultados --------------------------------------------------

## [RESULTADO N°1] ##
## Gráfico de Barras bivariado sobre grado de acuerdo con la frase:
##"Las políticas públicas generalmente reflejan lo que quiere la mayoría de los ciudadanos"
## 1 = Muy de acuerdo  5 = muy en desacuerdo

grafico1 <-CEP_Electivo%>% 
   filter(!is.na(Expo_RRSS))%>% 
   ggplot() +
   geom_bar(mapping = aes(x = PPubMayo, weight = POND, y = (..count..)/sum(..count..), fill = Expo_RRSS),
            position = "dodge") +
   scale_y_continuous(labels=scales::percent) + 
   scale_x_continuous(labels = c("Muy de acuerdo", "De Acuerdo",
                                 "Ni acuerdo ni desacuerdo","En desacuerdo", "Muy en desacuerdo"),
                      breaks = c(1,2,3,4,5)) +
   labs(title = "Gráfico 2. Nivel de acuerdo con la frase",
        subtitle = "'Las políticas públicas generalmente reflejan lo que quiere la mayoría de los ciudadanos'",
        x = "Nivel de Acuerdo", y = "Proporción",
        caption = "Fuente: Elaboración propia, en base a los datos de Encuesta CEP, Diciembre 2019") +
   xlab("Nivel de acuerdo con frase") + ylab("Porcentaje") + 
   scale_fill_manual("Consumo de información vía Redes Sociales",
                     values = c("#E41A1C", "#377EB8"),
                     labels = c("Mucha Información", "Poca información"))
#Imprimir gráfico 1
ggsave(grafico1, filename = "grafico1.png",
       dpi = 400, width = 11, height = 6)

## [RESULTADO N°2] ##
## Gráfico de barras bivariado sobre el grado de acuerdo con la frase: 
##"Los ciudadanos más ricos tienen más influencia que los demás ciudadanos en las políticas públicas"
## 1 = Muy de acuerdo  5 = muy en desacuerdo

grafico2 <-CEP_Electivo%>% 
   filter(!is.na(Expo_RRSS))%>% 
   ggplot() +
   geom_bar(mapping = aes(x = InflRicos, weight = POND,  y = (..count..)/sum(..count..), fill = factor(Expo_RRSS)),
            position = "dodge") +
   scale_y_continuous(labels=scales::percent) + 
   scale_x_continuous(labels = c("Muy de acuerdo", "De Acuerdo",
                                 "Ni acuerdo ni desacuerdo","En desacuerdo", "Muy en desacuerdo"),
                      breaks = c(1,2,3,4,5)) +
   labs(title = "Gráfico 2. Nivel de acuerdo con la frase",
        subtitle = "'Los ciudadanos más ricos tienen más influencia que los demás ciudadanos en las políticas públicas'",
        x = "Nivel de Acuerdo", y = "Proporción",
        caption = "Fuente: Elaboración propia, en base a los datos de Encuesta CEP, Diciembre 2019") +
   xlab("Nivel de acuerdo con frase") + ylab("Porcentaje") + 
   scale_fill_manual("Consumo de información vía Redes Sociales",
                     values = c("#E41A1C", "#377EB8"),
                     labels = c("Mucha Información", "Poca información"))
# Imprimir gráfico 2
ggsave(grafico2, filename = "grafico2.png",
       dpi = 400, width = 11, height = 6)

## [RESULTADO N°3] ##
# Gráfico de barras bivariado sobre el grado de acuerdo con la frase:
##"Las empresas y los grupos de interés influyen enormemente en las políticas públicas" según exposición a RRSS
## 1 = Muy de acuerdo  5 = muy en desacuerdo

grafico3 <- CEP_Electivo%>% 
   filter(!is.na(Expo_RRSS))%>% 
   ggplot() +
   geom_bar(mapping = aes(x = InflEmpre, weight = POND, y = (..count..)/sum(..count..), fill = factor(Expo_RRSS)),
            position = "dodge") +
   scale_y_continuous(labels=scales::percent) + 
   scale_x_continuous(labels = c("Muy de acuerdo", "De Acuerdo",
                                 "Ni acuerdo ni desacuerdo","En desacuerdo", "Muy en desacuerdo"),
                      breaks = c(1,2,3,4,5)) +
   labs(title = "Gráfico 3. Nivel de acuerdo con la frase",
        subtitle = "'Las empresas y los grupos de interés influyen enormemente en las políticas públicas'",
        x = "Nivel de Acuerdo", y = "Proporción",
        caption = "Fuente: Elaboración propia, en base a los datos de Encuesta CEP, Diciembre 2019") +
   xlab("Nivel de acuerdo con frase") + ylab("Porcentaje") + 
   scale_fill_manual("Consumo de información vía Redes Sociales",
                     values = c("#E41A1C", "#377EB8"),
                     labels = c("Mucha Información", "Poca información"))

#Imprimir gráfico 3
ggsave(grafico3, filename = "grafico3.png",
       dpi = 400, width = 11, height = 6)


##  [RESULTADO N°4]  ##
# Gráfico de barras bivariado sobre el grado de acuerdo con la frase:
##"A Los partidos políticos reflejan las demandas de los ciudadanos"
## 1 = Muy de acuerdo  5 = muy en desacuerdo

grafico4 <-CEP_Electivo%>% 
   filter(!is.na(Expo_RRSS))%>% 
   ggplot() +
   geom_bar(mapping = aes(x = DemanPPol, weight = POND, y = (..count..)/sum(..count..), fill = factor(Expo_RRSS)),
            position = "dodge") +
   scale_y_continuous(labels=scales::percent) + 
   scale_x_continuous(labels = c("Muy de acuerdo", "De Acuerdo",
                                 "Ni acuerdo ni desacuerdo","En desacuerdo", "Muy en desacuerdo"),
                      breaks = c(1,2,3,4,5)) +
   labs(title = "Gráfico 4. Nivel de acuerdo con la frase",
   subtitle = "'A Los partidos políticos reflejan las demandas de los ciudadanos'",
        x = "Nivel de Acuerdo", y = "Proporción",
        caption = "Fuente: Elaboración propia, en base a los datos de Encuesta CEP, Diciembre 2019") +
   xlab("Nivel de acuerdo con frase") + ylab("Porcentaje") + 
   scale_fill_manual("Consumo de información vía Redes Sociales",
                     values = c("#E41A1C", "#377EB8"),
                     labels = c("Mucha Información", "Poca información"))
#Imprimir gráfico 4
ggsave(grafico4, filename = "grafico4.png",
       dpi = 400, width = 11, height = 6)


## [RESULTADO N°5]  ##
#Histograma simple univariado
##"Los partidos políticos son indispensables para la democracia"
## 1 = Muy de acuerdo  5 = muy en desacuerdo
grafico5 <-CEP_Electivo%>% 
   filter(!is.na(Expo_RRSS))%>% 
   ggplot() +
   geom_bar(mapping = aes(x = PPolIndis, weight = POND, y = (..count..)/sum(..count..), fill = factor(Expo_RRSS)),
            position = "dodge") +
   scale_y_continuous(labels=scales::percent) + 
   scale_x_continuous(labels = c("Muy de acuerdo", "De Acuerdo",
                                 "Ni acuerdo ni desacuerdo","En desacuerdo", "Muy en desacuerdo"),
                      breaks = c(1,2,3,4,5)) +
   labs(title = "Gráfico 5. Nivel de acuerdo con la frase",
        subtitle = "'Los partidos políticos son indispensables para la democracia'",
        x = "Nivel de Acuerdo", y = "Proporción",
        caption = "Fuente: Elaboración propia, en base a los datos de Encuesta CEP, Diciembre 2019") +
   xlab("Nivel de acuerdo con frase") + ylab("Porcentaje") + 
   scale_fill_manual("Consumo de información vía Redes Sociales",
                     values = c("#E41A1C", "#377EB8"),
                     labels = c("Mucha Información", "Poca información"))
#Imprimir gráfico 5
ggsave(grafico5, filename = "grafico5.png",
       dpi = 400, width = 11, height = 6)

# Tareas pendientes a resolver y vertedero --------------------------------------------

# En lo que respecta a los resultados
# Revisar los resultados de varianza y curtosis que siguen sin salir bien (Quizás falta comando filter?)

## PRUEBA DE VARIANZA Y CURTOSIS

#Varianza y curtosis pregunta 1
group_by(CEP_Electivo, Expo_RRSS)%>%
   summarise(VarGrupal1 = var(CEP_Electivo$PPubMayo,na.rm = T), KurtoGrupal1 = kurtosi(CEP_Electivo$PPubMayo, na.rm = T))

#Varianza y curtosis pregunta 2
group_by(CEP_Electivo, Expo_RRSS)%>%
  summarise(VarGrupal2 = var(CEP_Electivo$InflRicos, na.rm = T), KurtoGrupal2 = kurtosi(CEP_Electivo$InflRicos, na.rm = T))

#Varianza y curtosis pregunta 3
group_by(CEP_Electivo, Expo_RRSS)%>%
  summarise(VarGrupal3 = var(CEP_Electivo$InflEmpre, na.rm = T), KurtoGrupal3 = kurtosi(CEP_Electivo$InflEmpre, na.rm = T))

#Varianza y curtosis pregunta 4
group_by(CEP_Electivo, Expo_RRSS)%>%
  summarise(VarGrupal4 = var(CEP_Electivo$DemanPPol, na.rm = T), KurtoGrupal4 = kurtosi(CEP_Electivo$DemanPPol,na.rm = ))

  #Varianza y curtosis pregunta 5
group_by(CEP_Electivo, Expo_RRSS)%>%
  summarise(VarGrupal5 = var(CEP_Electivo$PPolIndis, na.rm = T), KurtoGrupal5 = kurtosi(CEP_Electivo$DemanPPol, na.rm = T))
