## ELECTIVO: HERRAMIENTAS PARA EL ANÁLISIS CUANTITATIVO: CIENCIA ABIERTA Y SOFTWARE LIBRE ##
## MAGISTER EN CIENCIAS SOCIALES ##
## UNIVERSIDAD DE CHILE ##
## Profesores: Cristobal Moya y Felipe Ruiz ##

# Construcción de gráficos

CEP_Electivo <-load("Data/intermediate Data/P_CEP2019.RData")

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

#Listado de gráficos
ResultGraf <- list(grafico1, grafico2, grafico3, grafico4, grafico5)
saveRDS(ResultGraf, file = "Data/Analysis-Data/gráficos-reporte.rds")

