library(openxlsx)
library(tidyverse) #carga muchos paquetes
library(magrittr)
library(kableExtra)
library(lubridate)
data_banco<-read.xlsx("Base/Data_Banco (1).xlsx")
View(data_banco)
names(data_banco)
str(data_banco)
data_banco <- data_banco %>%
  mutate( Monto= str_replace(Monto, pattern = ",", replacement = ".") ) %>%
  mutate(Sucursal= as.character(Sucursal),
         Cajero = as.character(Cajero),
         Satisfaccion = parse_factor(Satisfaccion, 
                                     levels= c('Muy Malo', 
                                               'Malo', 
                                               'Regular', 
                                               'Bueno', 
                                               'Muy Bueno'), ordered = T),
         Monto= parse_number(Monto, locale = locale(decimal_mark = ".")))
data_banco<-data_banco %>% mutate(Tiempo_Servicio_Min= Tiempo_Servicio_seg/60)
data_banco %>% transmute(Tiempo_Servicio_Min=Tiempo_Servicio_seg/60)
str(data_banco)

##Pregunta 2
data_banco %>% 
  rename(TiempoServicioMin= Tiempo_Servicio_Min) %>% 
  group_by(Sexo) %>% 
  summarise_at( vars(TiempoServicioMin), 
                funs (
                  Media= mean(., na.rm=TRUE),
                  Cantidad= n()
                )) %>% 
  gather(-c("Sexo"),key="Var",value= valor)%>% #reunir
  separate("Var", c("Variable","Medida"), sep = "_") %>% 
  spread(Medida,value= valor) #separar 


data_banco %>% 
  rename(TiempoServicioMin= Tiempo_Servicio_Min) %>% 
  group_by(Ubicación) %>% 
  summarise_at( vars(TiempoServicioMin), 
                funs (
                  Media= mean(., na.rm=TRUE),
                  Cantidad= n()
                )) %>% 
  gather(-c("Ubicación"),key="Var",value= valor)%>% #reunir
  separate("Var", c("Variable","Medida"), sep = "_") %>% 
  spread(Medida,value= valor) #separar 

data_banco %>% 
  rename(TiempoServicioMin= Tiempo_Servicio_Min) %>% 
  group_by(Nuevo_Sistema) %>% 
  summarise_at( vars(TiempoServicioMin), 
                funs (
                  Media= mean(., na.rm=TRUE),
                  Cantidad= n()
                )) %>% 
  gather(-c("Nuevo_Sistema"),key="Var",value= valor)%>% #reunir
  separate("Var", c("Variable","Medida"), sep = "_") %>% 
  spread(Medida,value= valor) #separar 


#####pregunta 3
data <- data_banco
data <- data %>% mutate(
  Peso=case_when(Satisfaccion=="Muy Bueno"~5,
                 Satisfaccion=="Bueno"~4,
                 Satisfaccion=="Regular"~3,
                 Satisfaccion=="Malo"~2,
                 Satisfaccion=="Muy Malo"~1)
)%>% group_by(Ubicación,Satisfaccion) %>% summarise(Total=n(), Peso=sum(Peso))
data <- data %>% group_by(Ubicación) %>% summarise(Indicador=sum(Peso)/sum(Total))
data<-arrange(data,Indicador)
data


###pregunta 4
source("Script/funciones.R")
###pregunta 5
Fecha<-ymd(Sys.Date())
Fecha
AAAA<-year(Fecha)
data_banco<-data_banco %>% mutate(Experiencia= AAAA-Año.Ingreso)
str(data_banco)
data1<-data_banco %>% mutate(
  Peso=case_when(
    Satisfaccion== "Muy Bueno" ~ 5,
    Satisfaccion== "Bueno" ~ 4,
    Satisfaccion=="Regular" ~ 3,
    Satisfaccion=="Malo" ~ 2,
    Satisfaccion=="Muy Malo" ~ 1
  ))  %>% group_by(Cajero,Sexo,Experiencia,Año.Ingreso) %>% 
  summarise(Media_TiempoServicioMin=mean(Tiempo_Servicio_Min),
  Total_Monto=sum(Monto),                                                           
  Numero_Transacciones=n(),Satisfacción =sum(Peso)/n()) %>%arrange(as.numeric(Cajero))
data1  %>%  kable() %>% kable_styling()


