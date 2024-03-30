library(tidyverse)
library(eph)
library(ggplot2)

df <- get_microdata(year = 2021, trimester = 2, type = 'individual')

df <- organize_labels(df=df, type='individual') 

df <- df %>% mutate_at(vars(REGION, ESTADO), ~as.character(.))%>%
  mutate_at(vars(NIVEL_ED), ~as.factor(.))

tabla_frecuencias_simple_tv <- df %>% 
  group_by(REGION) %>% 
  summarise(n=n())

tabla_frec_porcentaje_tv <- df %>% 
  group_by(REGION) %>% 
  summarise(n=n()) %>% 
  mutate(n=round(n/sum(n)*100,2))


df %>% 
  ggplot(aes(x=REGION))+
  geom_bar(colour="blue", fill = "white")+
  labs(title = "Tabla de frecuencias",
       subtitle = "por region",
       x = "Region",
       y = "Frencuencia",
       caption = "Fuente: Encuesta Permanente de Hogares")

df %>% 
  filter(ESTADO != "Menor de 10 anios." & 
           ESTADO != "Entrevista individual no realizada (no respuesta al cuestionario individual)")%>% 
  ggplot(aes(x=NIVEL_ED, fill=ESTADO))+
  geom_bar(position = "fill")+
  labs(title = "Estado ocupacional",
       subtitle = "SEGUN NIVEL EDUCATIVO",
       x = "Nivel Educativo",
       y = "Porcentaje",
       caption = "Fuente: Encuesta Permanente de Hogares")+
  theme(axis.text.x = element_text(angle = 90))+
  coord_flip()

df %>% 
  filter(ESTADO != "Menor de 10 anios." & 
           ESTADO != "Entrevista individual no realizada (no respuesta al cuestionario individual)")%>% 
  ggplot(aes(x=NIVEL_ED, fill=ESTADO))+
  geom_bar(position = "fill")+
  labs(title = "Estado ocupacional",
       subtitle = "SEGUN NIVEL EDUCATIVO",
       x = "Nivel Educativo",
       y = "Porcentaje",
       caption = "Fuente: Encuesta Permanente de Hogares")+
  theme(axis.text.x = element_text(angle = 90))+
  coord_flip()+
  facet_wrap(.~REGION)

