rm(list = ls())

install.packages("tidyverse")
install.packages("dplyr")
library(tidyr)
library(tidyverse)


##______________________________________________________________________________
### Tenencia de tierra, infraestructura y salud ###

Tenencia_tirras_NAF <- read.csv("C:/Agricultura familiar/Tenencia tierra, infraestructura y salud del NAF/datos-tenencia-tierra-nafs.csv", header = T, sep = ",", dec = ".")
Infraestructura_NAF <- read.csv("C:/Agricultura familiar/Tenencia tierra, infraestructura y salud del NAFdatos-infraestructura-nafs.csv", header = T, sep = ";", dec = ".")
Cobertura_Salud_NAF <- read.csv("C:/Agricultura familiar/Tenencia tierra, infraestructura y salud del NAF/datos-cobertura-salud-nafs.csv", header = T, sep = ";", dec = ".")


# Tenencia de tierras NAF:
Tenencia_tirras_NAF %>% 
  select(Region, Año.del.dato, Tenencia.de.la.tierra, Cantidad.de.NAFs) %>% 
  filter( Tenencia.de.la.tierra != "Posesión"  ) %>% 
  group_by(Año.del.dato) %>% 
  summarise(cant_tierra_año = n_distinct(Tenencia.de.la.tierra)) #tipo de tenencias por años

Tenencia_tirras_NAF %>%
  select(Region, Año.del.dato, Tenencia.de.la.tierra, Cantidad.de.NAFs) %>%
  filter(Tenencia.de.la.tierra != "Posesión") %>%
  group_by(Año.del.dato) %>%
  summarise(count = n()) #Por cada año, cuantas NAFS diferentes hay.
  


# Infraestructura NAF:



# Cobertura de salud NAF:




##______________________________________________________________________________

### Produccion de los nucleos ###

Actividad_NAF <- read.csv("C:/Agricultura familiar/Produccion del NAF/actividades-del-nucleo-de-agricultura-familiar-por-grupos-de-produccion.csv", header = T , sep = ",", dec = ".", fileEncoding = "latin1") 

Produccion_veg_NAF <- read.csv("C:/Agricultura familiar/Produccion del NAF/datos-de-produccion-vegetal-de-los-naf.csv", header = T , sep = ",", dec = ".")

Prodiccion_x_tipo_agro<- read.csv("C:/Agricultura familiar/Produccion del NAF/produccion-por-tipo-de-agroindustria.csv", header = T , sep = ";", dec = ".", fileEncoding = "latin1")

# Cuales son las producciones en la provincia de Buenos Aires:

Tipo_cultivo_BsAs <- Produccion_veg_NAF %>% 
  filter( nom_provincia == "BUENOS AIRES") %>% 
  select( produccion) %>% 
  distinct()


#Seleccion, del total, de frutas y verduras:
#sascar: "AVENA", "ARVEJAS", HUERTA AUTOCONSUMO, HUERTA MERCADO (5 ESPECIES O MAS), SOJA 1RA, TRIGO PAN, AJI / PIMIENTO FRESCO, AROMATICAS, ALFALFA, MAIZ, OTROS CEREALES, PECAN, OTROS FORESTALES, VIVERO - ORNAMENTALES, SORGO GRANIFERO, FLORES, VID DE MESA, VID VINIFERA, PERENNES, OTRAS AROMATICAS, OTRAS FORRAJERAS PERENNES, FORRAJERAS ANUALES, (en blanco), PINO, CASTA\xd1O, MAIZ PISINGALLO, SOJA 2DA, SAUCE, OTROS VIVEROS, MEDICINALES, NOGAL, PIMIENTO PARA PIMENTON, POROTO BLANCO, MIJO, LUPINO, TARTAGO, TABACO, ARROZ, AVELLANO, GIRASOL, ALPISTE, CENTENO, VID PARA PASAS, POROTO NEGRO, OTRA, FORRAJERAS PERENNES, ORNAMENTALES, EUCALIPTO, OTRAS OLEAGINOSAS, VIVERO - FORESTALES, COLZA, OTROS INDUSTRIALES,  , ALGODON, CA\xd1AMO, VIVERO - INDUSTRIALES, MANI, QUINOA O QUINUA, POROTO COLORADO, YERBA MATE, GREVILLEA, CULTIVOS PARA SEMILLAS, PARAISO, LINO, OTROS CULTIVOS ANDINOS, OTROS CULTIVOS ANDINOS, ROMERO, MAICES ANDINOS  

Produccion_veg_NAF %>% 
  filter( nom_provincia == "BUENOS AIRES" & !(produccion %in% c("AVENA", "ARVEJAS", "HUERTA AUTOCONSUMO", "HUERTA MERCADO (5 ESPECIES O MAS)", "SOJA 1RA", "TRIGO PAN", "AJI / PIMIENTO FRESCO", "AROMATICAS", "ALFALFA", "MAIZ", "OTROS CEREALES", "PECAN", "OTROS FORESTALES", "VIVERO - ORNAMENTALES", "SORGO GRANIFERO", "FLORES", "VID DE MESA", "VID VINIFERA", "PERENNES", "OTRAS AROMATICAS", "OTRAS FORRAJERAS PERENNES", "FORRAJERAS ANUALES", "(en blanco)", "PINO", "CASTA\xd1O", "MAIZ PISINGALLO", "SOJA 2DA", "SAUCE", "OTROS VIVEROS", "MEDICINALES", "NOGAL", "PIMIENTO PARA PIMENTON", "POROTO BLANCO", "MIJO", "LUPINO", "TARTAGO", "TABACO", "ARROZ", "AVELLANO", "GIRASOL", "ALPISTE", "CENTENO", "VID PARA PASAS", "POROTO NEGRO", "OTRA", "FORRAJERAS PERENNES", "ORNAMENTALES", "EUCALIPTO", "OTRAS OLEAGINOSAS", "VIVERO - FORESTALES", "COLZA", "OTROS INDUSTRIALES", "                  " ,"CA\xd1A DE AZUCAR", "ALGODON", "CA\xd1AMO", "VIVERO - INDUSTRIALES", "MANI", "QUINOA O QUINUA", "POROTO COLORADO", "YERBA MATE", "GREVILLEA", "CULTIVOS PARA SEMILLAS", "PARAISO", "LINO", "OTROS CULTIVOS ANDINOS", "OTROS CULTIVOS ANDINOS", "ROMERO", "MAICES ANDINOS"))) %>% 
  select( produccion) %>% 
  distinct()

table(Tipo_cultivo_BsAs)

Produccion_vegetal_BsAs <- Produccion_veg_NAF %>%
  select(anio, nom_provincia,nom_depto, produccion, cantidad_de_producciones, cod_unid_superficie) %>% 
  filter(nom_provincia == "BUENOS AIRES" & !(produccion %in% c("AVENA", "ARVEJAS", "HUERTA AUTOCONSUMO", "HUERTA MERCADO (5 ESPECIES O MAS)", "SOJA 1RA", "TRIGO PAN", "AJI / PIMIENTO FRESCO", "AROMATICAS", "ALFALFA", "MAIZ", "OTROS CEREALES", "PECAN", "OTROS FORESTALES", "VIVERO - ORNAMENTALES", "SORGO GRANIFERO", "FLORES", "VID DE MESA", "VID VINIFERA", "PERENNES", "OTRAS AROMATICAS", "OTRAS FORRAJERAS PERENNES", "FORRAJERAS ANUALES", "(en blanco)", "PINO", "CASTA\xd1O", "MAIZ PISINGALLO", "SOJA 2DA", "SAUCE", "OTROS VIVEROS", "MEDICINALES", "NOGAL", "PIMIENTO PARA PIMENTON", "POROTO BLANCO", "MIJO", "LUPINO", "TARTAGO", "TABACO", "ARROZ", "AVELLANO", "GIRASOL", "ALPISTE", "CENTENO", "VID PARA PASAS", "POROTO NEGRO", "OTRA", "FORRAJERAS PERENNES", "ORNAMENTALES", "EUCALIPTO", "OTRAS OLEAGINOSAS", "VIVERO - FORESTALES", "COLZA", "OTROS INDUSTRIALES", "                  " ,"CA\xd1A DE AZUCAR", "ALGODON", "CA\xd1AMO", "VIVERO - INDUSTRIALES", "MANI", "QUINOA O QUINUA", "POROTO COLORADO", "YERBA MATE", "GREVILLEA", "CULTIVOS PARA SEMILLAS", "PARAISO", "LINO", "OTROS CULTIVOS ANDINOS", "OTROS CULTIVOS ANDINOS", "ROMERO", "MAICES ANDINOS"))) %>% 
  group_by(anio)

# Cuantas hectareas de produccion total fruti-horticola hubieron por año, en los años datados?

Produccion_vegetal_BsAs %>% 
  select(anio, produccion, cantidad_de_producciones) %>% 
  group_by(anio) %>% 
  summarise(Produccion_hec = sum(cantidad_de_producciones))
   

# Cantidad de hectareas por produccion

Produccion_vegetal_BsAs %>% 
  group_by(produccion) %>% 
  summarise(Produccion_hec = sum(cantidad_de_producciones) ) %>% 
  arrange(desc(Produccion_hec)) %>% 
  print( n = 83)
  


# Cantidad de hectareas por produccion por año, de forma ascendente en años y de mayor a menor produccion


Produccion_vegetal_BsAs %>% 
  filter( anio != "Sin dato") %>% 
  mutate( anio = as.numeric(anio)) %>% 
  group_by(anio, produccion) %>% 
  summarise(Produccion_hec = sum(cantidad_de_producciones) ) %>% 
  ungroup() %>% 
  arrange(anio, desc(Produccion_hec)) %>% 
  print( n = 83)



##______________________________________________________________________________

### Integrantes de los nucleos ###


