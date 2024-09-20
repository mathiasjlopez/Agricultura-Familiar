rm(list = ls())

install.packages("tidyverse")
install.packages("dplyr")
library(tidyr)
library(tidyverse)


##______________________________________________________________________________
### Tenencia de tierra, infraestructura y salud ###

Tenencia_tirras_NAF <- read.csv("C:/Users/Dell/Documents/Agricultura-Familiar/Tenencia tierra, infraestructura y salud del NAF/datos-tenencia-tierra-nafs.csv", header = T, sep = ",", dec = ".")
Infraestructura_NAF <- read.csv("C:/Users/Dell/Documents/Agricultura-Familiar/Tenencia tierra, infraestructura y salud del NAFdatos-infraestructura-nafs.csv", header = T, sep = ";", dec = ".")
Cobertura_Salud_NAF <- read.csv("C:/Users/Dell/Documents/Agricultura-Familiar/Tenencia tierra, infraestructura y salud del NAF/datos-cobertura-salud-nafs.csv", header = T, sep = ";", dec = ".")


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

Actividad_NAF <- read.csv("C:/Users/Dell/Documents/Agricultura-Familiar/Produccion del NAF/actividades-del-nucleo-de-agricultura-familiar-por-grupos-de-produccion.csv", header = T , sep = ",", dec = ".", fileEncoding = "latin1") 

Produccion_veg_NAF <- read.csv("C:/Users/Dell/Documents/Agricultura-Familiar/Produccion del NAF/datos-de-produccion-vegetal-de-los-naf.csv", header = T , sep = ",", dec = ".")

Prodiccion_x_tipo_agro<- read.csv("C:/Users/Dell/Documents/Agricultura-Familiar/Produccion del NAF/produccion-por-tipo-de-agroindustria.csv", header = T , sep = ";", dec = ".", fileEncoding = "latin1")

# Cuales son las producciones en la provincia de Buenos Aires:

Tipo_cultivo_BsAs <- Produccion_veg_NAF %>% 
  filter( nom_provincia == "BUENOS AIRES") %>% 
  select( produccion) %>% 
  distinct()


#Seleccion, del total, de frutas y verduras:
#sascar: "AVENA", "ARVEJAS", HUERTA AUTOCONSUMO, HUERTA MERCADO (5 ESPECIES O MAS), SOJA 1RA, TRIGO PAN, AJI / PIMIENTO FRESCO, AROMATICAS, ALFALFA, MAIZ, OTROS CEREALES, PECAN, OTROS FORESTALES, VIVERO - ORNAMENTALES, SORGO GRANIFERO, FLORES, VID DE MESA, VID VINIFERA, PERENNES, OTRAS AROMATICAS, OTRAS FORRAJERAS PERENNES, FORRAJERAS ANUALES, (en blanco), PINO, CASTA\xd1O, MAIZ PISINGALLO, SOJA 2DA, SAUCE, OTROS VIVEROS, MEDICINALES, NOGAL, PIMIENTO PARA PIMENTON, POROTO BLANCO, MIJO, LUPINO, TARTAGO, TABACO, ARROZ, AVELLANO, GIRASOL, ALPISTE, CENTENO, VID PARA PASAS, POROTO NEGRO, OTRA, FORRAJERAS PERENNES, ORNAMENTALES, EUCALIPTO, OTRAS OLEAGINOSAS, VIVERO - FORESTALES, COLZA, OTROS INDUSTRIALES,  , ALGODON, CA\xd1AMO, VIVERO - INDUSTRIALES, MANI, QUINOA O QUINUA, POROTO COLORADO, YERBA MATE, GREVILLEA, CULTIVOS PARA SEMILLAS, PARAISO, LINO, OTROS CULTIVOS ANDINOS, OTROS CULTIVOS ANDINOS, ROMERO, MAICES ANDINOS  

# Produccion_veg_NAF %>% 
#   filter( nom_provincia == "BUENOS AIRES" & !(produccion %in% c("AVENA", "ARVEJAS", "HUERTA AUTOCONSUMO", "HUERTA MERCADO (5 ESPECIES O MAS)", "SOJA 1RA", "TRIGO PAN", "AJI / PIMIENTO FRESCO", "AROMATICAS", "ALFALFA", "MAIZ", "OTROS CEREALES", "PECAN", "OTROS FORESTALES", "VIVERO - ORNAMENTALES", "SORGO GRANIFERO", "FLORES", "VID DE MESA", "VID VINIFERA", "PERENNES", "OTRAS AROMATICAS", "OTRAS FORRAJERAS PERENNES", "FORRAJERAS ANUALES", "(en blanco)", "PINO", "CASTA\xd1O", "MAIZ PISINGALLO", "SOJA 2DA", "SAUCE", "OTROS VIVEROS", "MEDICINALES", "NOGAL", "PIMIENTO PARA PIMENTON", "POROTO BLANCO", "MIJO", "LUPINO", "TARTAGO", "TABACO", "ARROZ", "AVELLANO", "GIRASOL", "ALPISTE", "CENTENO", "VID PARA PASAS", "POROTO NEGRO", "OTRA", "FORRAJERAS PERENNES", "ORNAMENTALES", "EUCALIPTO", "OTRAS OLEAGINOSAS", "VIVERO - FORESTALES", "COLZA", "OTROS INDUSTRIALES", "                  " ,"CA\xd1A DE AZUCAR", "ALGODON", "CA\xd1AMO", "VIVERO - INDUSTRIALES", "MANI", "QUINOA O QUINUA", "POROTO COLORADO", "YERBA MATE", "GREVILLEA", "CULTIVOS PARA SEMILLAS", "PARAISO", "LINO", "OTROS CULTIVOS ANDINOS", "OTROS CULTIVOS ANDINOS", "ROMERO", "MAICES ANDINOS"))) %>% 
#   select( produccion) %>% 
#   distinct()

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
  


# Cantidad de hectareas por produccion para los años 2009 y 2013, de forma ascendente en años y de mayor a menor produccion


Produccion_vegetal_BsAs %>% 
  filter( anio %in% c(2009, 2013) ) %>% 
  mutate( anio = as.numeric(anio)) %>% 
  group_by(anio, produccion) %>% 
  summarise(Produccion_hec = sum(cantidad_de_producciones) ) %>% 
  ungroup() %>% 
  arrange(anio, desc(Produccion_hec)) %>% 
  print( n = 83)


# En la base "produccion vegetal buenos aires" solo nos vamos a quedar con los años 2009 y 2018, vamos a sumar todas las producciones frutihorticola(voy a sumar todo en una sola columna, sin discriminar) y voy a replicar para las otras provincias



Produccion_vegetal_BsAs %>% 
  group_by(anio) %>% 
  filter(anio %in% c(2009, 2013)) %>% 
  summarise(Ha_totales_FyV = sum(cantidad_de_producciones)) %>% 
  print(n = 100)
  

## Armamos la nueva base de datos con la que nos vamos a quedar:

Prod_NAF_FyV_09y13 <- Produccion_veg_NAF %>% 
  group_by(nom_provincia, anio) %>% 
  select(cod_provincia, nom_provincia, anio, produccion, cantidad_de_producciones, cod_unid_superficie) %>% 
  filter(anio %in% c(2009, 2013) & !(produccion %in% c("AVENA", "ARVEJAS", "SOJA 1RA", "TRIGO PAN", "AJI / PIMIENTO FRESCO", "AROMATICAS", "ALFALFA", "MAIZ", "OTROS CEREALES", "OTROS FORESTALES", "VIVERO - ORNAMENTALES", "SORGO GRANIFERO", "FLORES", "PERENNES", "OTRAS AROMATICAS", "OTRAS FORRAJERAS PERENNES", "FORRAJERAS ANUALES", "(en blanco)", "PINO", "MAIZ PISINGALLO", "SOJA 2DA", "SAUCE", "OTROS VIVEROS", "MEDICINALES", "PIMIENTO PARA PIMENTON", "POROTO BLANCO", "MIJO", "LUPINO", "TARTAGO", "TABACO", "ARROZ", "AVELLANO", "GIRASOL", "ALPISTE", "CENTENO", "POROTO NEGRO", "OTRA", "FORRAJERAS PERENNES", "ORNAMENTALES", "EUCALIPTO", "OTRAS OLEAGINOSAS", "VIVERO - FORESTALES", "COLZA", "                  " ,"CA\xd1A DE AZUCAR", "ALGODON", "CA\xd1AMO", "VIVERO - INDUSTRIALES", "MANI", "QUINOA O QUINUA", "POROTO COLORADO", "YERBA MATE", "GREVILLEA", "CULTIVOS PARA SEMILLAS", "PARAISO", "LINO", "OTROS CULTIVOS ANDINOS", "OTROS CULTIVOS ANDINOS", "ROMERO", "MAICES ANDINOS"))) %>% 
  summarise(Ha_total_FyV = sum(cantidad_de_producciones), .groups = "drop") %>% 
  arrange(anio, nom_provincia) 

Prod_NAF_FyV_09y13 %>%
  group_by(anio) %>% 
  select(anio, Ha_total_FyV) %>% 
  summarise(Suma_total_FyV_año = sum(Ha_total_FyV))

# anio        Suma_total_FyV_año
#               
# 2009               13461
# 2013               11741

# Chequeando la suma de las producciones: Cambiamos la provincia y da los resultados
Produccion_veg_NAF %>%
  group_by(anio) %>% 
  select(anio, nom_provincia,nom_depto, produccion, cantidad_de_producciones, cod_unid_superficie) %>% 
  filter(nom_provincia == "RIO NEGRO" & anio %in% c(2009, 2013) & !(produccion %in% c("AVENA", "ARVEJAS", "HUERTA AUTOCONSUMO", "HUERTA MERCADO (5 ESPECIES O MAS)", "SOJA 1RA", "TRIGO PAN", "AJI / PIMIENTO FRESCO", "AROMATICAS", "ALFALFA", "MAIZ", "OTROS CEREALES", "PECAN", "OTROS FORESTALES", "VIVERO - ORNAMENTALES", "SORGO GRANIFERO", "FLORES", "VID DE MESA", "VID VINIFERA", "PERENNES", "OTRAS AROMATICAS", "OTRAS FORRAJERAS PERENNES", "FORRAJERAS ANUALES", "(en blanco)", "PINO", "CASTA\xd1O", "MAIZ PISINGALLO", "SOJA 2DA", "SAUCE", "OTROS VIVEROS", "MEDICINALES", "NOGAL", "PIMIENTO PARA PIMENTON", "POROTO BLANCO", "MIJO", "LUPINO", "TARTAGO", "TABACO", "ARROZ", "AVELLANO", "GIRASOL", "ALPISTE", "CENTENO", "VID PARA PASAS", "POROTO NEGRO", "OTRA", "FORRAJERAS PERENNES", "ORNAMENTALES", "EUCALIPTO", "OTRAS OLEAGINOSAS", "VIVERO - FORESTALES", "COLZA", "OTROS INDUSTRIALES", "                  " ,"CA\xd1A DE AZUCAR", "ALGODON", "CA\xd1AMO", "VIVERO - INDUSTRIALES", "MANI", "QUINOA O QUINUA", "POROTO COLORADO", "YERBA MATE", "GREVILLEA", "CULTIVOS PARA SEMILLAS", "PARAISO", "LINO", "OTROS CULTIVOS ANDINOS", "OTROS CULTIVOS ANDINOS", "ROMERO", "MAICES ANDINOS"))) %>% 
  summarise(Ha_totales = sum(cantidad_de_producciones))

# YA CHEQUEADO, DA BIEN EL CODIGO Prod_NAF_FyV_09y13



## Guardamos el nuevo dataset

write.csv(Prod_NAF_FyV_09y13, "C:/Users/Dell/Documents/Agricultura-Familiar/Prod_NAF_FyV_09y13.csv", row.names = FALSE)

##______________________________________________________________________________

### Integrantes de los nucleos ###


