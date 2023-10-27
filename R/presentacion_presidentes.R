
library(tidyverse)
library(flextable)
library(officer)

shp_chis_dof |>
  filter(DISTRITO==1) |>
  ggplot() +
  # geom_sf(data=shp_chis_mun)+
  geom_sf(aes(fill=NULL)) +
  theme_void()


shp_chis_mun |>
  ggplot() +
  geom_sf()+
  geom_sf(data = shp_chis_dof, fill = NA, linewidth =1.2) +
  theme_void()


shp_chis_dof |>
  filter(DISTRITO==1) |>
  ggplot() +
  geom_sf()+
  geom_sf(data = shp_chis_mun, fill = NA, linewidth = 1.2) +
  theme_void()


shp_chis_secc %>% 
  st_join(shp_chis_mun, by=c("MUNICIPIO")) %>% 
  st_join(shp_chis_dof, by = c("DISTRITO")) %>% 
  select(-GEOMETRY1_) %>% 
  filter(DISTRITO.y==1) %>% 
  ggplot()+
  geom_sf()


shp_chis_secc %>%
  select(DISTRITO,MUNICIPIO, -geometry) %>%
  as_tibble() -> b

distrito <- as.vector(shp_chis_secc$DISTRITO)
mun <- as.vector(shp_chis_secc$MUNICIPIO)

b <- 
  data.frame(
    distrito=distrito,
    mun=mun
  ) %>% as_tibble()


shp_mun_chis %>% 
  left_join(b, by=c("MUNICIPIO"="mun")) -> b1


b1 %>% 
  filter(distrito==6) %>% 
  ggplot()+
  geom_sf()+
  geom_sf_label(aes(label=NOMBRE))


# ----------------------------------------------------------------------------


partido_ganador <- readr::read_csv("Data-raw/chis_partido_21.csv")

base_pg <- partido_ganador %>% select(nombre_municipio,last_col())

cuestionario_path <-  "Data-raw/presidentes_municipales.xlsx"
cuestionario_direccionWeb <- "https://docs.google.com/spreadsheets/d/1DbP1XLmUJTHgwtpjD6gh_YUhfqvLTbOlZ28MCit4lnU/edit#gid=1723974365"
archivo_xlsx <- googledrive::drive_download(googledrive::as_id(cuestionario_direccionWeb), path = cuestionario_path, overwrite = TRUE)

colores_partido <- 
  readr::read_csv("Data-raw/chis_partido_21_color.csv") %>% 
  select(nombre_municipio,pm_21)

presidentes_municipales <- readxl::read_excel(path = cuestionario_path,sheet = "municipio") 
diputados <- readxl::read_excel(path = cuestionario_path,sheet = "distrito") |> rename(por=...6)
mapas_sh <- b1 %>% left_join(colores_partido,by=c("NOMBRE"="nombre_municipio"))

tabla_presidentes_mapa <- function(distrito, data){
  
  tabla <- 
    {{data}} %>% 
    filter(distrito=={{distrito}}) %>% 
    select(-distrito) %>% 
    mutate(municipio=str_to_title(municipio),
           presidente=str_to_title(presidente),
           votos_f=format(votos,big.mark = ","),
           t_votos=votos/lista_nominal,
           total_votos=scales::percent(t_votos,accuracy=1)) %>% 
    arrange(desc(votos)) %>% 
    select(municipio,presidente,partido,votos_f,total_votos) %>% 
    head(n = 5)
  
  return(tabla)
  
}
tabla_presidentes_dis <- function(distrito, data){
  
  tabla <- 
    {{data}} %>% 
    filter(distrito=={{distrito}}) %>% 
    select(-distrito) %>% 
    mutate(municipio=str_to_title(municipio),
           presidente=str_to_title(presidente),
           votos_f=format(votos,big.mark = ","),
           t_votos=votos/lista_nominal,
           total_votos=scales::percent(t_votos,accuracy=1)) %>% 
    arrange(desc(votos)) %>% 
    select(municipio,presidente,partido,votos_f,total_votos) %>% 
    head(n = 5)
  
 tabla_f <- 
   tabla %>% 
   flextable() %>% 
   flextable::width(j = 1,width = 1) %>%
   flextable::width(j = 2,width = 2.5) %>%
   flextable::width(j = 3:4,width = 1) %>%
   flextable::width(j = 5,width = 0.7) %>%
   flextable::line_spacing(space = 0.9) %>%
   flextable::fontsize(size = 10,part = "body") %>%
   set_header_labels(
     municipio="Municipio",
     presidente="Presidente Municipal",
     partido="Partido\nPolítico",
     votos_f="Total de\nvotos",
     total_votos="Porcentaje"
   )
  
  return(tabla_f)
  
}
mapa_distrito <- function(distrito,data,data_top5){
  
  top5 <- 
    tabla_presidentes_mapa(distrito = {{distrito}},data = {{data_top5}}) |> 
    select(municipio) |> 
    mutate(top="top5",
           municipio=str_to_upper(municipio))

  mapa <- 
    {{data}} %>% 
    left_join(top5,by=c("NOMBRE"="municipio")) %>%
    filter(distrito=={{distrito}}) %>% 
    ggplot()+
    geom_sf(aes(fill=pm_21))+
    geom_sf_label(aes(x = -1,y = -1,label=ifelse(top=="top5",str_wrap(str_to_title(NOMBRE),width = 10),NA)),
                 size=3) +
    scale_fill_identity(guide = NULL)+
    theme_void()
  
  return(mapa)
  
}

generar_slide <- function(distrito,data_mapa,data_tabla,data_diputado){

  pptx <- officer::read_pptx("Data-raw/DistritosChiapas_plantilla.pptx")
 
  for(i in distrito){
    
    mapa_distrital <- mapa_distrito(distrito = {{i}},data = {{data_mapa}},data_top5={{data_tabla}})
    tabla_distrital <- tabla_presidentes_dis(distrito = {{i}},data = {{data_tabla}})
    titulo_sl <- paste("Distrito electoral federal",{{i}})
  
    base_diputado <- {{data_diputado}} |> filter(Distrito=={{i}})
    nombre_diputado <- c(str_to_title(base_diputado[1,3])) |> as.character()
  
    votos <- c({{data_diputado}} |> filter(Distrito=={{i}}) |> select(votos))
    votos_l <- paste0(round(votos$votos/1000),"K")
  
    por_dip <- c({{data_diputado}} |> filter(Distrito=={{i}}) |> select(por))
    por_dip_l <- paste0("(",scales::percent(por_dip$por,accuracy=1),")")
  
    indig <- c({{data_diputado}} |> filter(Distrito=={{i}}) |> select(indigena))
  
    titulo_diputado <- paste0("Diputada actual: ", nombre_diputado,"."," Votos: ", votos_l," ",por_dip_l)
  
   add_slide(pptx, layout = "cualitativas_2", master = "Office Theme") |> 
     ph_with(value = titulo_sl,location = ph_location_label(ph_label = "titulo")) |> 
     ph_with(value = mapa_distrital,location = ph_location_label(ph_label = "distrito")) |> 
     ph_with(value = tabla_distrital,location = ph_location_label(ph_label = "tabla")) |> 
     ph_with(value = titulo_diputado,location = ph_location_label(ph_label = "subtitulo")) |> 
     ph_with(value = ifelse(!is.na(indig),paste("Distrito",indig),""),location = ph_location_label(ph_label = "indigena"))
 
  }
  
  dir_actual <- getwd()
  doc_pptx <- glue::glue("{dir_actual}/presentacion_presidentes/reporte_distritos.pptx")
  print(pptx, doc_pptx)
  
  doc_pdf <- glue::glue("{dir_actual}\\presentacion_presidentes\\reporte_distritos.pdf")
  docxtractr::convert_to_pdf(doc_pptx, doc_pdf) 
  
  
}

generar_slide(distrito = 1:13,
              data_mapa = mapas_sh,
              data_tabla = presidentes_municipales,
              data_diputado = diputados)



# Presentación por secciones ------------------------------------------------

shp_secc_chis

colores_partido_seccion <- 
  readr::read_csv("Data-raw/seccion_color_df21.csv") |> 
  select(seccion,df_21) |> 
  mutate(seccion=gsub("07_","",seccion),
         seccion=as.double(seccion))

shp_secc_chis |> 
  left_join(colores_partido_seccion, by=c("SECCION"="seccion")) |> 
  group_by(DISTRITO) |> 
  filter(DISTRITO==2) |> 
  ggplot(aes(fill=df_21),Show.le)+
  geom_sf()+
  scale_fill_identity(guide = NULL)
  
diputados <- readxl::read_excel(path = cuestionario_path,sheet = "distrito") |> rename(por=...6)


mapa_presidentes_seccion <- function(data,distrito){
  
  mapa <- 
    shp_secc_chis |> 
    left_join({{data}}, by=c("SECCION"="seccion")) |> 
    group_by(DISTRITO) |> 
    filter(DISTRITO=={{distrito}}) |> 
    ggplot(aes(fill=df_21),Show.le)+
    geom_sf()+
    scale_fill_identity(guide = NULL)+
    theme_void()
  
  return(mapa)

}

generar_slide_seccion <- function(distrito,data_colores,data_diputado){
  
  pptx <- officer::read_pptx("Data-raw/DistritosChiapas_plantilla.pptx")
  
  for(i in distrito){
    
    mapa_seccion <- mapa_presidentes_seccion(data = {{data_colores}},distrito = {{i}})
    titulo_sl <- paste("Distrito electoral federal",{{i}})
    
    base_diputado <- {{data_diputado}} |> filter(Distrito=={{i}})
    nombre_diputado <- c(str_to_title(base_diputado[1,3])) |> as.character()
    
    votos <- c({{data_diputado}} |> filter(Distrito=={{i}}) |> select(votos))
    votos_l <- paste0(round(votos$votos/1000),"K")
    
    por_dip <- c({{data_diputado}} |> filter(Distrito=={{i}}) |> select(por))
    por_dip_l <- paste0("(",scales::percent(por_dip$por,accuracy=1),")")
    
    indig <- c({{data_diputado}} |> filter(Distrito=={{i}}) |> select(indigena))
    
    titulo_diputado <- paste0("Diputada actual: ", nombre_diputado,"."," Votos: ", votos_l," ",por_dip_l)
    
    add_slide(pptx, layout = "cualitativas_2_secc", master = "Office Theme") |> 
      ph_with(value = titulo_sl,location = ph_location_label(ph_label = "titulo")) |> 
      ph_with(value = mapa_seccion,location = ph_location_label(ph_label = "distrito")) |> 
      ph_with(value = titulo_diputado,location = ph_location_label(ph_label = "subtitulo")) |> 
      ph_with(value = ifelse(!is.na(indig),paste("Distrito",indig),""),location = ph_location_label(ph_label = "indigena"))

  }
  
  dir_actual <- getwd()
  doc_pptx <- glue::glue("{dir_actual}/presentacion_presidentes/reporte_distritos_secc.pptx")
  print(pptx, doc_pptx)
  
  doc_pdf <- glue::glue("{dir_actual}\\presentacion_presidentes\\reporte_distritos_secc.pdf")
  docxtractr::convert_to_pdf(doc_pptx, doc_pdf) 
  
  
  
  
}

generar_slide_seccion(distrito = 1:13,
                      data_colores = colores_partido_seccion,
                      data_diputado = diputados)
















#----------------------------------------------

"#E9F3E8"

shp_chis_secc |> 
  filter(DISTRITO==6,MUNICIPIO==102) |> 
  ggplot()+
  geom_sf(aes(fill="#F3D0CE",color="#F3D0CE"),show.legend = F)+
  theme_void()

shp_chis_secc |> 
  filter(DISTRITO==6) |> 
  ggplot()+
  geom_sf(aes(fill=ifelse(MUNICIPIO==102,"#F3D0CE",NA),
              ),color="#F3D0CE",show.legend = F)+
  geom_sf(data=mapa,aes(fill=pm_21))+
  geom_sf_label(data=mapa,aes(x = -1,y = -1,
                              label=ifelse(top=="top5",str_wrap(str_to_title(NOMBRE),width = 10),NA)),
                size=3) +
  scale_fill_identity(guide = NULL)+
  theme_void() -> mapa7

shp_chis_secc |> 
  filter(DISTRITO==11) |> 
  ggplot()+
  geom_sf(aes(fill=ifelse(MUNICIPIO==77,"#E9F3E8",NA),
  ),color="white",show.legend = F, linewidth=0.01)+
  geom_sf(data=mapa,aes(fill=pm_21))+
  geom_sf_label(data=mapa,aes(x = -1,y = -1,
                              label=ifelse(top=="top5",str_wrap(str_to_title(NOMBRE),width = 10),NA)),
                size=3) +
  scale_fill_identity(guide = NULL)+
  theme_void() -> mapa6

mapa <- 
  mapas_sh %>% 
  left_join(top6,by=c("NOMBRE"="municipio")) %>%
  filter(distrito==11, NOMBRE!="SAN CRISTOBAL DE LAS CASAS")

top6 <- 
  tabla_presidentes_mapa(distrito = 11,data = presidentes_municipales) |> 
  select(municipio) |> 
  mutate(top="top5",
         municipio=str_to_upper(municipio))


shp_chis_secc |> 
  filter(DISTRITO==9,MUNICIPIO==102) |> 
  ggplot()+
  geom_sf(fill="#F3D0CE",color="#F3D0CE")+
  theme_void() -> mapa9

top12 <- 
  tabla_presidentes_mapa(distrito = 12,data = presidentes_municipales) |> 
  select(municipio) |> 
  mutate(top="top5",
         municipio=str_to_upper(municipio))


mapa <- 
  mapas_sh %>% 
  left_join(top12,by=c("NOMBRE"="municipio")) %>%
  filter(distrito==12, NOMBRE!="SAN CRISTOBAL DE LAS CASAS") |> 
  ggplot()+
  geom_sf(aes(fill=ifelse(NOMBRE=="TUXTLA GUTIERREZ","#F3D0CE",pm_21)))+
  geom_sf_label(aes(x = -1,y = -1,label=ifelse(top=="top5",str_wrap(str_to_title(NOMBRE),width = 10),NA)),
                size=3) +
  scale_fill_identity(guide = NULL)+
  theme_void()

tabla_presidentes_dis(distrito = 6,data = presidentes_municipales) -> tabla7
tabla_presidentes_dis(distrito = 5,data = presidentes_municipales) -> tabla8
tabla_presidentes_dis(distrito = 11,data = presidentes_municipales) -> tabla8
tabla_presidentes_dis(distrito = 12,data = presidentes_municipales) -> tabla12
tabla_presidentes_dis(distrito = 13,data = presidentes_municipales) -> tabla13
tabla_presidentes_dis(distrito = 6,data = presidentes_municipales) -> tabla6


pptx <- officer::read_pptx("Data-raw/DistritosChiapas_plantilla.pptx")

add_slide(pptx, layout = "cualitativas_2", master = "Office Theme") |> 
  ph_with(value = tabla6,location = ph_location_label(ph_label = "tabla"))


dir_actual <- getwd()
doc_pptx <- glue::glue("{dir_actual}/presentacion_presidentes/reporte_distrito_6.pptx")
print(pptx, doc_pptx)



shp_chis_secc |> 
  filter(DISTRITO==13) |> 
  ggplot()+
  geom_sf()+
  geom_sf_text(aes(label=MUNICIPIO))


sugerencias <- openxlsx::read.xlsx("Data-raw/base_sugerencias.xlsx")


pptx <- officer::read_pptx("Data-raw/DistritosChiapas_plantilla.pptx")


sugerencias |> 
  flextable() %>% 
  flextable::width(j = 2,width = 2.5) %>%
  flextable::fontsize(size = 10,part = "body") %>%
  set_header_labels(
    municipio="Municipio",
    presidente="Presidente Municipal",
    partido="Partido\nPolítico",
    votos_f="Total de\nvotos",
    total_votos="Porcentaje") -> tabla_sugerencias

pptx <- officer::read_pptx("Data-raw/DistritosChiapas_plantilla.pptx")


add_slide(pptx, layout = "sugerencias", master = "Office Theme") |> 
  ph_with(value = tabla_sugerencias,location = ph_location_label(ph_label = "distrito"))

dir_actual <- getwd()
doc_pptx <- glue::glue("{dir_actual}/presentacion_presidentes/reporte_distrito_sugerencias.pptx")
print(pptx, doc_pptx)





