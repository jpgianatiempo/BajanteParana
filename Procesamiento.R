rm(list = ls())


# Librerias ---------------------------------------------------------------

library(tidyverse)
library(tidyquant)
library(ggdist)
library(gghighlight)
library(ggridges)
library(viridis)
library(hrbrthemes)

# Data --------------------------------------------------------------------

df <- readxl::read_excel("./Data/Data.xlsx", skip=1) %>% mutate(Fecha = as.Date(Fecha)) %>% select(Fecha, `Altura [m]`)

df <- df %>% mutate(
  Mes = lubridate::month(df$Fecha),
  Ano = lubridate::year(df$Fecha))




# Graf Mensual ------------------------------------------------------------


df %>% filter(Ano != 2021) %>% 
  ggplot(aes(x=`Altura [m]`, y=Mes, fill=factor(Mes)))+
  stat_halfeye(
    adjust = 0.5,
    justification = -.2,
    .width = 0,
    point_colour = NA,
  ) +
  geom_boxplot(
    width = .12,
    outlier.color = NA,
    alpha = 0.5
  )+
  stat_dots(data = df %>% filter(Ano == 2021) %>% group_by(Mes) %>% summarise(`Altura [m]` = mean(`Altura [m]`)),
    side = "left",
    justification = 1.1,
    binwidth = .25
  )+
  scale_fill_tq()+
  theme_tq()+
  labs(
    title = "Distribución de la altura en la Estación de Timbúes - Río Paraná",
    subtitle = "Datos históricos desde 1900 a 2020 y en círculos el promedio mensual de 2021",
    x = "Altura en metros",
    y = "Mes",
    caption = "Fuente: Elaboración propia en base a datos del Sistema Nacional de Información Hídrica.",
    fill = "2021"
  )+
  theme(legend.position = "none")+
  scale_y_discrete(limits=factor(c(1:12)))+
  coord_flip()
  



# Graf Anual --------------------------------------------------------------

# Plot
df %>% filter(Ano >= 1980, Mes %in% c(1,2,3,4,5,6,7)) %>% 
ggplot(mapping = aes(x =`Altura [m]`, y = as.factor(Ano),fill=stat(x))) +
  geom_density_ridges_gradient(scale=5, size = 0.3, rel_min_height = 0.01, alpha=0.2) +
  scale_fill_viridis(discrete = F,option="C")+
  #scale_fill_tq()+
  theme_tq()+
  labs(
    title = "Distribución de la altura en la Estación de Timbúes - Río Paraná",
    subtitle = "Datos históricos de enero a julio",
    x = "Altura en metros",
    y = "Año",
    caption = "Fuente: Elaboración propia en base a datos del Sistema Nacional de Información Hídrica.",
    #fill = "2021"
  )+
  theme(legend.position = "none")


#Dias por encima de los 2.47 metros, que permiten los 34 pies de calado
b <- df %>% filter(Ano == 2021) %>% mutate("Si" = ifelse(`Altura [m]` >= 2.47,1,0),
                                           "No" = ifelse(`Altura [m]` < 2.47,1,0))

print(paste0("En el ",scales::label_percent()(sum(b$Si)/sum(b$No)),"de los días del 2021, el nivel del río se ubicó por debajo de los 2,47 mts"))


# Camiones ----------------------------------------------------------------


historico <- readxl::read_excel("./Data/camiones.xlsx") %>% filter(TOTALES != "TOTALES")

historico <- historico[,1:13]
historico$Puerto1[historico$Puerto1=="Darsenas y Bs As"] <- "Dársena - Bs As - Entre Rios"
historico$Puerto1[historico$Puerto1=="Darsena - Bs As - Entre Rios"] <- "Dársena - Bs As - Entre Rios"
historico$Puerto1[historico$Puerto1=="Puertos-B.Blanca"] <- "Bahía Blanca"
historico$Puerto1[historico$Puerto1=="Puertos de Necochea"] <- "Necochea"
#historico$Puerto1[historico$Puerto1=="Rosario Y Zona"] <- "Darsena - Bs As - Entre Rios"

historicot <- historico %>%
  group_by(Puerto1,fecha = lubridate::floor_date(Fecha, "month")) %>%
  summarize(totalpuerto = sum(as.numeric(TOTALES),na.rm = T))

historicot$totalpuerto[historicot$totalpuerto==4490835]=44908.35


# Gráfico total puertos ---------------------------------------------------


historicot %>% 
  ggplot(aes(x=fecha, y=totalpuerto, color=Puerto1))+
  geom_line(size=0.75)+
  facet_wrap(~Puerto1, scales = "free_y")+
  #scale_color_viridis(option = "D" ,discrete = T)+
  scale_color_tq()+
  theme_tq()+
  labs(
    title = "Número de camiones arribados a puertos",
    subtitle = "Segmentado en los principales puertos graneleros",
    x = "Fecha",
    y = "Número de camiones",
    caption = "Fuente: Elaboración propia en base a datos de Williams Entregas S.A.",
    #fill = "2021"
  )+
  theme(legend.position = "none")+
  scale_y_continuous(labels = scales::comma)+
  scale_x_datetime(date_breaks = "1 year",date_labels = "%Y")



# Acumulado los primeros 7 meses ------------------------------------------


acum <- historico %>% filter(Puerto1 %in% c("Bahía Blanca","Necochea")) %>% 
 pivot_longer(
    !c(Año,Semana,Fecha,Puerto1,Puerto2,Localidad,TOTALES), names_to = "producto", values_to = "camiones"
  )

acum <- acum %>% group_by(
  Puerto1,fecha = lubridate::floor_date(Fecha, "month"),producto) %>%
    summarize(camioness = sum(as.numeric(camiones),na.rm = T)
)

acum <- acum %>% mutate(Mes = month(fecha),
                          Ano = year(fecha))

acum <- acum %>% group_by(
  Puerto1,producto,Ano) %>%
  mutate(camionesAcum = cumsum(camioness)) %>% 
  filter(Mes== 7, Ano !=2013)

acum <- acum %>% mutate(
  producto = case_when(producto=="CEBADA"~"Cebada",
                       producto=="GIRAS"~"Girasol",
                       producto=="MAIZ"~"Maíz",
                       producto=="SOJA"~"Soja",
                       producto=="SORG"~"Sorgo",
                       T ~ "Trigo")
  )


# Gráfico acumulado Bahía Blanca ------------------------------------------

acum %>% filter(Puerto1 == "Bahía Blanca") %>% 
  ggplot(aes(x=Ano, y=camionesAcum, fill=producto))+
  geom_col()+
  facet_wrap(~producto, scales = "free_y")+
  #scale_color_viridis(option = "D" ,discrete = T)+
  scale_fill_tq()+
  theme_tq()+
  labs(
    title = "Acumulado de camiones arribados al puerto de Bahía Blanca en los primeros 7 meses",
    subtitle = "Segmentado en los principales granos",
    x = "Fecha",
    y = "Número de camiones",
    caption = "Fuente: Elaboración propia en base a datos de Williams Entregas S.A.",
    #fill = "2021"
  )+
  theme(legend.position = "none")+
  scale_y_continuous(labels = scales::comma)+
  scale_x_continuous(breaks = c(2014,2015,2016,2017,2018,2019,2020,2021))



# Gráfico acumulado Necochea ----------------------------------------------


acum %>% filter(Puerto1 == "Necochea") %>% 
  ggplot(aes(x=Ano, y=camionesAcum, fill=producto))+
  geom_col()+
  facet_wrap(~producto, scales = "free_y")+
  #scale_color_viridis(option = "D" ,discrete = T)+
  scale_fill_tq()+
  theme_tq()+
  labs(
    title = "Acumulado de camiones arribados al puerto de Necochea en los primeros 7 meses",
    subtitle = "Segmentado en los principales granos",
    x = "Fecha",
    y = "Número de camiones",
    caption = "Fuente: Elaboración propia en base a datos de Williams Entregas S.A.",
    #fill = "2021"
  )+
  theme(legend.position = "none")+
  scale_y_continuous(labels = scales::comma)+
  scale_x_continuous(breaks = c(2014,2015,2016,2017,2018,2019,2020,2021))
