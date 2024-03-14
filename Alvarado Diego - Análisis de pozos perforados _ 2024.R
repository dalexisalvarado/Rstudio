################### ANALISIS DE PRODUCCION DE NO CONVENCIONAL ----
#Secretaria de energía
#Dataset: Producción de Pozos de Gas y Petróleo No Convencional

# http://datos.energia.gob.ar/dataset/produccion-de-petroleo-y-gas-por-pozo/archivo/b5b58cdc-9e07-41f9-b392-fb9ec68b0725

#Selección del directorio de trabajo
setwd("C:/Users/a/Desktop/Data/2. R/Diego - R/R - Pozos perforados/R - Análisis de no convencional")

#carga de librerías
library(tidyverse)



#carga del dataset
produccion_no_convencional<-read.csv("produccin-de-pozos-de-gas-y-petrleo-no-convencional.csv",header = TRUE, sep = ",", dec=",")
# ok, se cargaron 287733 registros

#0. Transformacion de datos -----
#0.1 Trasnformar en numerico la produccion de gas, de agua y petroleo

produccion_no_convencional$prod_gas<-as.numeric(produccion_no_convencional$prod_gas)
produccion_no_convencional$prod_pet<-as.numeric(produccion_no_convencional$prod_pet)
produccion_no_convencional$prod_agua<-as.numeric(produccion_no_convencional$prod_agua)

#0.3 Transformar en número la profundidad del pozo
produccion_no_convencional$profundidad<-as.numeric(produccion_no_convencional$profundidad)

#0.4 Hay un dato con profundidad de 380000 m lo que es poco probable. se elimina ese dato

produccion_no_convencional<-produccion_no_convencional%>%
  filter(profundidad<300000)

#0.5 año as date para poder hacer el boxplot

produccion_no_convencional$anio<-as.factor(produccion_no_convencional$anio)

# 1. Nombre de las columnas ----
colnames(produccion_no_convencional)
# 
# [1] "idempresa"              "anio"                  
# [3] "mes"                    "idpozo"                
# [5] "prod_pet"               "prod_gas"              
# [7] "prod_agua"              "iny_agua"              
# [9] "iny_gas"                "iny_co2"               
# [11] "iny_otro"               "tef"                   
# [13] "vida_util"              "tipoextraccion"        
# [15] "tipoestado"             "tipopozo"              
# [17] "observaciones"          "fechaingreso"          
# [19] "rectificado"            "habilitado"            
# [21] "idusuario"              "empresa"               
# [23] "sigla"                  "formprod"              
# [25] "profundidad"            "formacion"             
# [27] "idareapermisoconcesion" "areapermisoconcesion"  
# [29] "idareayacimiento"       "areayacimiento"        
# [31] "cuenca"                 "provincia"             
# [33] "coordenadax"            "coordenaday"           
# [35] "tipo_de_recurso"        "proyecto"              
# [37] "clasificacion"          "subclasificacion"      
# [39] "sub_tipo_recurso"       "fecha_data"   

#2. Empresas ----
unique(produccion_no_convencional$empresa)
# [1] "YSUR ENERGÍA ARGENTINA S.R.L."                          
# [2] "YPF S.A."                                               
# [3] "WINTERSHALL ENERGIA S.A."                               
# [4] "WINTERSHALL DEA ARGENTINA S.A"                          
# [5] "VISTA ENERGY ARGENTINA SAU"                             
# [6] "Vista Oil & Gas Argentina SA"                           
# [7] "VENOIL S.A."                                            
# [8] "VISTA OIL & GAS ARGENTINA SAU"                          
# [9] "TECPETROL S.A."                                         
# [10] "TOTAL AUSTRAL S.A."                                     
# [11] "SHELL ARGENTINA S.A."                                   
# [12] "ROCH S.A."                                              
# [13] "PETROLERA EL TREBOL S.A."                               
# [14] "PRESIDENT PETROLEUM S.A."                               
# [15] "PLUSPETROL S.A."                                        
# [16] "PATAGONIA ENERGY S.A."                                  
# [17] "PETROLERA ENTRE LOMAS S.A."                             
# [18] "PETROBRAS ARGENTINA S.A."                               
# [19] "PAMPA ENERGIA S.A."                                     
# [20] "PAN AMERICAN ENERGY SL"                                 
# [21] "PAN AMERICAN ENERGY (SUCURSAL ARGENTINA) LLC"           
# [22] "OILSTONE ENERGIA S.A."                                  
# [23] "O&G DEVELOPMENTS LTD S.A."                              
# [24] "MEDANITO S.A."                                          
# [25] "MADALENA AUSTRAL S.A."                                  
# [26] "KILWER S.A."                                            
# [27] "MADALENA ENERGY ARGENTINA SRL "                         
# [28] "GRECOIL y CIA. S.R.L."                                  
# [29] "GAS Y PETROLEO DEL NEUQUEN S.A."                        
# [30] "ENERGICON S.A."                                         
# [31] "EXXONMOBIL EXPLORATION ARGENTINA S.R.L."                
# [32] "CAPETROL ARGENTINA S.A."                                
# [33] "CHEVRON ARGENTINA S.R.L."                               
# [34] "COMPAÑÍA GENERAL DE COMBUSTIBLES S.A."                  
# [35] "CGC ENERGIA SAU"                                        
# [36] "CAPEX S.A."                                             
# [37] "APCO OIL AND GAS INTERNATIONAL INC (SUCURSAL ARGENTINA)"
# [38] "APACHE ENERGIA ARGENTINA S.R.L."                        
# [39] "AMERICAS PETROGAS ARGENTINA S.A."                       
# [40] "ARGENTA ENERGIA S.A."                                   
# [41] "Petrolera Aconcagua Energia S.A."  

#3. Provincias -----
unique(produccion_no_convencional$provincia)
# [1] "Rio Negro"  "Neuquén"    "Mendoza"    "Chubut"    
# [5] "Santa Cruz" "Salta"

#4. Cuencas ----
unique(produccion_no_convencional$cuenca)
# "NEUQUINA"        
# "GOLFO SAN JORGE" 
# "NOROESTE"       
# "AUSTRAL"    

#5. Tipo de extracción ----
unique(produccion_no_convencional$tipoextraccion)
# [1] "Surgencia Natural"         "Bombeo Mecánico"          
# [3] "Plunger Lift"              "Sin Sistema de Extracción"
# [5] "Otros Tipos de Extracción" "Gas Lift"                 
# [7] "Pistoneo (Swabbing)"       "Bombeo Hidráulico"        
# [9] "Jet Pump"                  ""                         
# [11] "Electrosumergible"         "Cavidad Progresiva"    

#6. Clasificacion y subclasificacion ----
unique(produccion_no_convencional$clasificacion)
# [1] "EXPLOTACION" "EXPLORACION" "SERVICIO"    ""  

unique(produccion_no_convencional$subclasificacion)
# [1] "AVANZADA"              "DESARROLLO"           
# [3] "EXPLORACION"           "EXPLORATORIO PROFUNDO"
# [5] "EXTENSION"             "CONTROL"              
# [7] "ESTUDIO"               ""                     
# [9] "SUMIDERO"        


###### GRAFICAS ----
#7. Gráficas de producción de gas por provincia y año -----

ggplot(data=produccion_no_convencional)+
  geom_col(mapping=aes(x=anio,y=prod_gas,color=provincia))+
  labs(title='Produccion de gas por año y provincia',
       subtitle='Datos de la Secretaría de Energía',
       x='Año',y='Producción de gas')+
  theme_bw()

#8. Gráfica de boxplot de profundidad de pozo, evolucion interanaual y por provincia - Santa Cruz y Neuquen

produccion_no_convencional_nqn_sc<-produccion_no_convencional%>%
  filter(provincia=="Santa Cruz"|provincia=="Neuquén")%>%
  filter(clasificacion=="EXPLOTACION")

ggplot(data=produccion_no_convencional_nqn_sc)+
  geom_boxplot(mapping=aes(x=anio,y=profundidad,color=cuenca))+
  labs(title='Dispersión de la profundidad de pozo en Explotación, por año y provincia - Neuquen y Santa Cruz',
       subtitle='Datos de la Secretaría de Energía',
       x='Año',y='Profundidad',axis.text.x=element_text(angle = 90, hjust = 1))+
  facet_wrap(~provincia,nrow=2)
  
  
  

  
  
