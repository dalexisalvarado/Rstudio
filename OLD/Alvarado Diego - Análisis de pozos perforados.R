#### Activación de paquetes básicos ----
library(tidyverse)
library(dplyr)
library(ggplot2)

#### Selección del espacio de trabajo ----
setwd("C:/Users/a/Desktop/Data/2. R/Diego - R/R - Pozos perforados")

#otra alternativa es Session > Set Working Directory > Choose Directory

#### Obtención de datos e importacion del archivo ----

# http://datos.energia.gob.ar/dataset/perforacion-de-pozos-de-petroleo-y-gas/archivo/712805f3-35d4-4825-93c6-98d03aeca203

pozos_perforados<-read.csv("metros-perforados.csv",header = TRUE, sep = ",", dec=",")
# ok, se cargaron 423296 registros


#### Análisis descriptivo el dataset y ajustes preliminares----

#0. Ajustes del dataset ----

#0.1 Transformacion de fecha_data en variable tipo fecha
pozos_perforados$fecha_data<-as.Date(pozos_perforados$fecha_data)

#0.2 Cantidad aparece como caracter, se debe pasar a numero

pozos_perforados$cantidad<-as.numeric(pozos_perforados$cantidad)


#1. Nombre de las columnas ----
colnames(pozos_perforados)
#Resultado:
# "indice_tiempo"          "anio"                  
# [3] "mes"                    "idempresa"
# [5] "empresa"                "idareapermisoconcesion"
# [7] "areapermisoconcesion"   "idareayacimiento"
# [9] "areayacimiento"         "idcuenca"
# [11] "cuenca"                 "idprovincia"
# [13] "provincia"              "idubicacion"
# [15] "ubicacion"              "idconcepto"
# [17] "concepto"               "cantidad"
# [19] "observaciones"          "fecha_data"

#2. Empresas presentes -----
unique(pozos_perforados$empresa)

# [1] "YPF S.A."                                                       
# [2] "PETROBRAS ARGENTINA S.A."                                       
# [3] "ALIANZA PETROLERA ARGENTINA S.A."                               
# [4] "ENAP SIPETROL ARGENTINA S.A."                                   
# [5] "PETROLERA ENTRE LOMAS S.A."                                     
# [6] "ROCH S.A."                                                      
# [7] "CAPEX S.A."                                                     
# [8] "COMPAÑÍA GENERAL DE COMBUSTIBLES S.A."                          
# [9] "PAN AMERICAN ENERGY (SUCURSAL ARGENTINA) LLC"                   
# [10] "ARGENTA ENERGIA S.A."                                           
# [11] "PETROFARO S.A."                                                 
# [12] "TECPETROL S.A."                                                 
# [13] "APCO ARGENTINA INC."                                            
# [14] "PETROLERA LF COMPANY S.R.L."                                    
# [15] "APACHE ENERGIA ARGENTINA S.R.L."                                
# [16] "EZ HOLDINGS S.A."                                               
# [17] "PLUSPETROL ENERGY S.A."                                         
# [18] "PLUSPETROL S.A."                                                
# [19] "PETRO ANDINA RESOURCES LTD."                                    
# [20] "TOTAL AUSTRAL S.A."                                             
# [21] "OCCIDENTAL ARGENTINA EXPLORATION AND PRODUCTION, INC."          
# [22] "EHRENCAP S.A."                                                  
# [23] "CLEAR S.R.L."                                                   
# [24] "PETROQUIMICA COMODORO RIVADAVIA S.A."                           
# [25] "OIL M&S S.A."                                                   
# [26] "COMPAÑÍAS ASOCIADAS PETROLERAS S.A."                            
# [27] "CHEVRON ARGENTINA S.R.L."                                       
# [28] "INTERENERGY ARGENTINA S.A."                                     
# [29] "ENERGIAL S.A."                                                  
# [30] "PETROLERA DEL COMAHUE S.A."                                     
# [31] "CHAÑARES ENERGIA S.A."                                          
# [32] "MADALENA PETROLEUM (AMERICAS) LTD. (SUCURSAL ARGENTINA)"        
# [33] "INGENIERIA ALPA S.A."                                           
# [34] "ENARSA ENERGIA ARGENTINA S.A."                                  
# [35] "INGENIERIA SIMA S.A."                                           
# [36] "NECON S.A."                                                     
# [37] "GRECOIL y CIA. S.R.L."                                          
# [38] "MEDANITO S.A."                                                  
# [39] "APACHE PETROLERA ARGENTINA S.A."                                
# [40] "PETROLERA CERRO NEGRO S.A."                                     
# [41] "PETROLEOS SUDAMERICANOS S.A."                                   
# [42] "AMERICAS PETROGAS ARGENTINA S.A."                               
# [43] "COLHUE HUAPI S.A."                                              
# [44] "SAN JORGE PETROLEUM S.A."                                       
# [45] "EXXONMOBIL EXPLORATION ARGENTINA S.R.L."                        
# [46] "SINOPEC ARGENTINA EXPLORATION AND PRODUCTION, INC."             
# [47] "OILSTONE ENERGIA S.A."                                          
# [48] "COPESA CIA CONSTRUCTORA PETROLERA SA"                           
# [49] "HIGH LUCK GROUP LTD. - SUCURSAL ARGENTINA"                      
# [50] "PANAPETROLEO S.A."                                              
# [51] "PETROLERA EL TREBOL S.A."                                       
# [52] "PETROLERA SAN JOSE S.R.L."                                      
# [53] "CROWN POINT OIL & GAS S.A."                                     
# [54] "UNITEC ENERGY S.A."                                             
# [55] "GEOPARK ARGENTINA LTD. (SUCURSAL ARGENTINA)"                    
# [56] "JHP INTERNATIONAL PETROLEUM ENGINEERING LTD"                    
# [57] "PETROLERA PAMPA S.A. "                                          
# [58] "MADALENA ENERGY ARGENTINA SRL "                                 
# [59] "O&G DEVELOPMENTS LTD S.A."                                      
# [60] "QUINTANA E&P ARGENTINA S.R.L."                                  
# [61] "PETROLERA PIEDRA DEL AGUILA S.A."                               
# [62] "KILWER S.A."                                                    
# [63] "DAPETROL S.A."                                                  
# [64] "CENTRAL INTERNATIONAL CORPORATION (SUCURSAL ARGENTINA)"         
# [65] "CROWN POINT ENERGIA S.A."                                       
# [66] "PETROLERA PATAGONIA S.R.L."                                     
# [67] "GAS Y PETROLEO DEL NEUQUEN S.A."                                
# [68] "WINTERSHALL ENERGIA S.A."                                       
# [69] "PETRO AP S.A."                                                  
# [70] "FOMICRUZ S.E."                                                  
# [71] "ENERGICON S.A."                                                 
# [72] "PRESIDENT PETROLEUM S.A."                                       
# [73] "PAMPETROL S.A.P.E.M"                                            
# [74] "YSUR ENERGÍA ARGENTINA S.R.L."                                  
# [75] "YSUR PETROLERA ARGENTINA S.A."                                  
# [76] "CAPETROL S.A."                                                  
# [77] "SELVA MARIA OIL S.A."                                           
# [78] "PAMPA ENERGIA S.A."                                             
# [79] "Vista Oil & Gas Argentina SA"                                   
# [80] "PAN AMERICAN ENERGY SL"                                         
# [81] "SHELL ARGENTINA S.A."                                           
# [82] "VISTA OIL & GAS ARGENTINA SAU"                                  
# [83] "EXXONMOBIL EXPLORATION AND PRODUCTION OFFSHORE ARGENTINA S.R.L."
# [84] "CRI HOLDING, INC."                                              
# [85] "WINTERSHALL DEA ARGENTINA S.A"                                  
# [86] "VISTA ENERGY ARGENTINA SAU"                                     
# [87] "INTEROIL ARGENTINA S A"                                         
# [88] "VENOIL S.A."                                                    
# [89] "PATAGONIA ENERGY S.A."                                          
# [90] "CGC ENERGIA SAU"                                                
# [91] "Petrolera Aconcagua Energia S.A."  

#3. Cuencas y provincias ----

unique(pozos_perforados$cuenca)
# [1] "GOLFO SAN JORGE" "NEUQUINA"        "AUSTRAL"        
# [4] "CUYANA"          "NOROESTE"        "LOS BOLSONES"   
# [7] "MALVINAS"        "CAÑADON ASFALTO" "ÑIRIHUAU"       
# [10] "DEL COLORADO"    "NORESTE"         "LAS SALINAS"    
# [13] "FUERA DE CUENCA" "GENERAL LEVALLE" "MALVINAS OESTE" 
# [16] "ARGENTINA NORTE"

unique(pozos_perforados$provincia)
# [1] "Santa Cruz"       "La Pampa"         "Mendoza"         
# [4] "Estado Nacional"  "Rio Negro"        "Neuquén"         
# [7] "Tierra del Fuego" "Chubut"           "Salta"           
# [10] "San Juan"         "Formosa"          "Jujuy"           
# [13] "La Rioja"         "Córdoba"        

unique(pozos_perforados$concepto)
# [1] "Exploración" "Avanzada"    "Servicio"    "Explotación"

#4. Cantidad de pozos por cuenca y tipo - Diciembre 2023 ----
pozos_perforados_12_2023<-pozos_perforados%>%
  filter(mes==12&anio==2023)

print(summarise(group_by(pozos_perforados_12_2023,provincia,cuenca,concepto),mean(cantidad)),n=30)

#5. Cantidad de pozos por tipo, cuenca y provincia - Evolución temporal ----

pozos_perforados%>%
  group_by(provincia,anio,concepto)%>%
  count()

chubut_2009<-pozos_perforados%>%
  filter((provincia=="Chubut")&(anio==2009))

chubut_2009%>%
  group_by(provincia,anio,concepto)%>%
  count()

#6. Evolución de pozos en exploración por cuenca-----

# 6.1 Generando DF de pozos en exploración
pozos_perforados_exploracion<-pozos_perforados%>%
  filter(concepto=='Exploración')

# 6.2 Generando gráfico

ggplot(data=pozos_perforados_exploracion)+
  geom_bar(mapping=aes(x=fecha_data,color=cuenca))

#7. Evolución de pozos en explotación por cuenca -----

pozos_perforados_explotacion<-pozos_perforados%>%
  filter(concepto=='Explotación')%>%
  filter(cantidad>0)%>%
  filter(anio==2023)

#8. Boxplot de las longitudes de los pozos de explotación por cuenca -----

ggplot(data=pozos_perforados_explotacion)+
  geom_boxplot(mapping=aes(x=provincia,y=cantidad))




################### ANALISIS DE PRODUCCION DE NO CONVENCIONAL ----
#Secretaria de energía
#Dataset: Producción de Pozos de Gas y Petróleo No Convencional

# http://datos.energia.gob.ar/dataset/produccion-de-petroleo-y-gas-por-pozo/archivo/b5b58cdc-9e07-41f9-b392-fb9ec68b0725
produccion_no_convencional<-read.csv("produccin-de-pozos-de-gas-y-petrleo-no-convencional.csv",header = TRUE, sep = ",", dec=",")
# ok, se cargaron 287733 registros

#0. Transformacion de datos -----
#0.1 Trasnformar en numerico la produccion de gas, de agua y petroleo

produccion_no_convencional$prod_gas<-as.numeric(produccion_no_convencional$prod_gas)
produccion_no_convencional$prod_pet<-as.numeric(produccion_no_convencional$prod_pet)
produccion_no_convencional$prod_agua<-as.numeric(produccion_no_convencional$prod_agua)

#0.3 Transformar en número la profundidad del pozo
produccion_no_convencional$profundidad<-as.numeric(produccion_no_convencional$profundidad)

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
  geom_col(mapping=aes(x=anio,y=prod_gas,color=provincia,fill=TRUE))+
  labs(title='Produccion de gas por año y provincia',
       subtitle='Datos de la Secretaría de Energía',
       x='Año',y='Producción de gas')+
  theme_bw()

#8. Gráfica de boxplot de profundidad de pozo, evolucion interanaual y por provincia

produccion_no_convencional_2018<-produccion_no_convencional%>%
  filter(anio>=2018)

ggplot(data=produccion_no_convencional_2018)+
  geom_boxplot(mapping=aes(x=anio,y=profundidad))+
  labs(title='Dispersión de la profundidad por año y provincia',
       subtitle='Datos de la Secretaría de Energía',
       x='Año',y='Profundidad')+
  theme_bw()
  
