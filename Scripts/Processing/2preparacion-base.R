## ELECTIVO: HERRAMIENTAS PARA EL ANÁLISIS CUANTITATIVO: CIENCIA ABIERTA Y SOFTWARE LIBRE ##
## MAGISTER EN CIENCIAS SOCIALES ##
## UNIVERSIDAD DE CHILE ##
## Profesores: Cristobal Moya y Felipe Ruiz ##


#   [Modificación de base de Datos]
load("Data/intermediate Data/P_CEP2019.RData")

CEP_Electivo <- select(CEP_Electivo,ESP_5_1,ESP_5_3,ESP_5_4,ESP_6_1,
                       ESP_6_2,DS_P1,DS_P2_EXACTA,DS_P4,ESP_30_1:ESP_30_4, PONDERADOR)

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

# Limpiar entorno de trabajo
rm(list=ls())

