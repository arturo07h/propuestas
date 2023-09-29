
# Base de secciones electorales para CDMX
# MORANT
# 29-septiembre-2023

# Librerías a utilizar ------------------------------------------------------

library(openxlsx)
library(tidyverse)
library(sf)

# Bases de datos a utilizar -------------------------------------------------

lista_nominal_ine <- 
  read.xlsx("Data-raw/base_ine.xlsx",sheet = "ine_re") %>% 
  filter(NOMBRE.ENTIDAD=="CIUDAD DE MEXICO") %>% 
  collect() %>% 
  as_tibble()

censo_inegi_2020 <- 
  read.xlsx("Data-raw/base_ine.xlsx",sheet = "cens_geo_2020") %>% 
  filter(ENTIDAD==9) %>% 
  collect() %>% 
  as_tibble()

part_pol <- 
  read.xlsx("Data-raw/base_ine.xlsx",sheet = "alcaldes") %>% 
  mutate(Partido=case_when(Partido==" Partido Acción Nacional"~"PAN",
                           Partido==" Partido Revolucionario Institucional"~"PRI",
                           Partido==" Partido de la Revolución Democrática"~"PRD",
                           Partido==" Movimiento Regeneración Nacional"~"MORENA")) %>% 
  collect() %>% 
  as_tibble()

votos_partidos <- 
  read.csv("Data-raw/cdmex_partido_21.csv") %>% 
  select(seccion2,ele_total_pm_21,ele_mc_pm_21,ele_pan_pm_21,
         ele_prd_pm_21,ele_morena_pm_21) %>% 
  collect() %>% 
  as_tibble()

shp_secc_cdmx <- read_sf("Data-raw/Shapes/SECCION.shp")

# Creación de base solicitada -----------------------------------------------

base_secc_cdmx <- 
  
  data.frame(
    
    distrito = lista_nominal_ine$CLAVE.DISTRITO,
    alcaldia = lista_nominal_ine$NOMBRE.MUNICIPIO,
    seccion = lista_nominal_ine$SECCION,
    lista_nominal = lista_nominal_ine$LISTA.NOMINAL, # 1. Lista nominal
    lista_hombres = lista_nominal_ine$LISTA.HOMBRES, # 2. Lista nominal Hombres
    lista_mujeres = lista_nominal_ine$LISTA.MUJERES, # 3. Lista nominal Mujeres
    lista_18_29 = rowSums(lista_nominal_ine[, 52:63]), # 4. Lista nominal 18-29
    lista_30_59 = rowSums(lista_nominal_ine[, 64:81]), # 5. Lista nominal 30-59
    lista_60y_mas = rowSums(lista_nominal_ine[, 82:87]) # 6. Lista nominal 60 y más
    
  ) %>% as_tibble()


# viviendas particulares habitadas y grado promedio de escolaridad -----------

# VIVPAR_HAB
# GRAPROES
# Tipo de sección electoral (2. Urbana; 3. Mixta;  4. Rural).

base_secc_cdmx <- 
  left_join(base_secc_cdmx,censo_inegi_2020[,c(5,6,140,177)],
            by=c("seccion"="SECCION")) 

base_secc_cdmx <- 
  base_secc_cdmx %>%
  mutate(TIPO=case_when(TIPO==2~"Urbana",TIPO==3~"Mixta",TIPO==4~"Rural"))

# Partido que gobierna ------------------------------------------------------

part_pol <- mutate(part_pol,Alcaldía=toupper(Alcaldía))

part_pol$Alcaldía <- gsub("Á","A",part_pol$Alcaldía)
part_pol$Alcaldía <- gsub("É","E",part_pol$Alcaldía)
part_pol$Alcaldía <- gsub("Í","I",part_pol$Alcaldía)
part_pol$Alcaldía <- gsub("Ó","O",part_pol$Alcaldía)
part_pol$Alcaldía <- gsub("Ú","U",part_pol$Alcaldía)

base_secc_cdmx <- 
  left_join(base_secc_cdmx,part_pol,by=c("alcaldia"="Alcaldía"))

base_secc_cdmx %>% view

# votación por partido político en elecciones intermedias -------------------

base_secc_cdmx <- 
  left_join(base_secc_cdmx,votos_partidos,by=c("seccion"="seccion2"))

# Exportación de base ------------------------------------------------------


base_secc_cdmx <- base_secc_cdmx %>% 
  rename(grado_promedio_escolaridad=GRAPROES,
         viv_hab_part=VIVPAR_HAB,
         tipo_localidad=TIPO)

write.csv(base_secc_cdmx,"Data/base secciones cdmx.csv")

















