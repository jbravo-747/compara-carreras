# CrearTablasSitio
# Este script convierte los resultados de compara carreras al formato de las tablas que el equipo de tecnologia usa para subir los resultados a la base de datos del sitio

library(stringr)
library(tidyr)
library(dplyr)
library(readxl)

# Importar scripts y fuentes de datos

source("02_scripts\\011_RealizarEquivalenciasCMPE.R")

## Resultados de compara
resultados <- readRDS("03_resultados//temporales//resultados_121_221_321_421.RDS")

## base de costos
costos_universidades <- readRDS("03_resultados//temporales//costos_bd.RDS")

## clasificacion de carreras
clas.car <- read.csv("01_fuentes//cmpe 2011.csv", encoding = "UTF-7")
clas.car <- clas.car %>% mutate_all(as.character)

## formatos 911 de educacion superior y bachillerato tecnologico
f911 <- list()
f911$carreras <- readRDS("01_fuentes//sep//CARRERA_I2021_F1920.RDS")
f911$escuelas <- readRDS("01_fuentes//sep//ESCUELA_I2021_F1920.RDS")
f911$bach <- readRDS("01_fuentes//sep//BACH_TECNOLOGICO_I2021_F1920.RDS")

## directorio de escuelas
dir <- readRDS("01_fuentes//sep//directorio_escuelas.RDS")

## machotes de tablas de BD del sitio
ImportarMachotesTablas <- function(){
   
   folder <- "01_fuentes\\sitio\\"
   lista_archivos <- list.files(folder, 
                                all.files = FALSE, 
                                full.names = TRUE, 
                                pattern = "(.csv|.xlsx|.xls|.DBF|.dbf)")
   
   ImportarArchivo <- function(ruta_archivo){
      archivo <- read.csv(ruta_archivo, encoding = "UTF-7", na.strings = c("NA"))
   }
   
   tablas_sitio <- lapply(lista_archivos, ImportarArchivo)
   names(tablas_sitio) <- substring(lista_archivos, 
                                    str_locate(lista_archivos, "sitio")[, 2] + 2, 
                                    nchar(lista_archivos) - 4) 
   return(tablas_sitio)
}

tablas_sitio <- ImportarMachotesTablas()

# dar formato a las fuentes
## filtrar y seleccionar f911 y directorio de escuelas
f911$carreras <- f911$carreras %>% 
   select(CCT_INS_PLA, NOMBRE_INS_PLA, CV_CCT, NOMBRECT, CONTROL,
          CV_CARRERA, C_NOM_CARRERA, CV_ENT_INMUEBLE, ENTIDAD_INMUEBLE, C_CARRERA_ESTATUS, 
          C_NOM_MUN, CV_MUN, V177, V175, V176, 
          V23, V21, V22, V82, V85, 
          V89, V88)

f911$bach <- f911$bach %>% select(CCT_INS_PLA, NOMBRE_INS_PLA, CV_CCT, NOMBRECT, CONTROL, 
                             C_CARRERA_ESTATUS, CV_CARRERA, C_NOM_CARRERA, CV_ENT_INMUEBLE, ENTIDAD_INMUEBLE, 
                             C_NOM_MUN, CV_MUN, V472, V470, V471,
                             V204, V202, V203, V333, V336, 
                             V413, V412)

dir <- dir %>% 
   filter(C_TIPO == "ESCUELA" & TIPONIVELSUB_C_SERVICION1 == "SUPERIOR") %>% 
   select(CV_CCT, INMUEBLE_C_VIALIDAD_PRINCIPAL, INMUEBLE_N_EXTNUM, INMUEBLE_LATITUD, INMUEBLE_LONGITUD, CONTACTO_C_TELEFONO)

# definir el valor de año a usar
año <- "2021"

# crear lista para guardar las tablas nuevas
tablas_nuevas <- list()

## crear tabla de datos nacionales
CrearDatosNacionales <- function(resultados){
   
   ### extraer de los resultados los valores necesarios
   ing_prepa <- resultados$niveles %>% 
      filter(ESC == "Bachillerato") %>% 
      pull(INGRESO)
   
   t_ocup <- resultados$niveles %>% 
      filter(ESC == "Todos") %>% 
      pull(TASA_DESOCUPACION)
   
   t_ocup_prof <- resultados$niveles %>% 
      filter(ESC == "Profesional") %>% 
      pull(TASA_DESOCUPACION)
   
   t_inf <- resultados$niveles %>% 
      filter(ESC == "Todos") %>% 
      pull(TASA_INFORMALIDAD)
   
   t_inf_prof <- resultados$niveles %>% 
      filter(ESC == "Profesional") %>% 
      pull(TASA_INFORMALIDAD)
      
   sueldo_prom <- resultados$niveles %>% 
      filter(ESC == "Todos") %>% 
      pull(INGRESO)
      
   sueldo_prom_prof <- resultados$niveles %>% 
      filter(ESC == "Profesional") %>% 
      pull(INGRESO)
   
   ### crear el data frame
   datos_nacionales <- data.frame(año = año, 
                                  trimestre = 1,
                                  vida_laboral = 45,
                                  ingreso_prepa = ing_prepa,
                                  tasa_ocupacion_nacional = t_ocup,
                                  tasa_ocupacion_profesionistas_nacional = t_ocup_prof,
                                  tasa_informalidad_nacional = t_inf,
                                  tasa_informalidad_profesionistas_nacional = t_inf_prof,
                                  sueldo_promedio_nacional = sueldo_prom,
                                  sueldo_promedio_profesionistas_nacional = sueldo_prom_prof)
   
   ### ajustar formato
   #### crear una funcion para ajustar los decimales
   Decimales <- function(valor){
      valor <- ifelse(valor <= 1, round(valor * 100, 1), valor)
   }
   
   datos_nacionales <- datos_nacionales %>% 
      mutate_at(vars(tasa_ocupacion_nacional, tasa_ocupacion_profesionistas_nacional, tasa_informalidad_nacional, tasa_informalidad_profesionistas_nacional),
                Decimales) 
   
   #### verificar que todas las columnas necesarios se encuentren presentes
   tabla <- pivot_wider(tablas_sitio$datos_nacionales, 
                        names_from = 1, 
                        values_from = 2)
   
   if(sum(colnames(tabla) %in% colnames(datos_nacionales)) - ncol(tabla) > 0){
      warning("Columnas faltantes en datos nacionales")
   }
   
   datos_nacionales <- pivot_longer(datos_nacionales,
                                    !matches("año"),
                                    values_to = año,
                                    names_to = "indicador")
   
   res <- list(datos_nacionales, ing_prepa)
   
   return(res)
}

tablas_nuevas$datos_nacionales <- CrearDatosNacionales(resultados)[[1]]

ing_prepa <- CrearDatosNacionales(resultados)[2][[1]][1]

# crear tabla de estadisticas
CrearEstadisticas <- function(resultados, tablas_sitio){
   
   ## selecionar en los resultados solo las columnas de la tabla de estadisticas
   lic <- resultados$carreras 

   colnames(lic) <- str_to_sentence(colnames(lic))
   colnames(lic) <- str_replace_all(colnames(lic), "_", ".")
   
   lic <- lic %>% 
      mutate(Tasa.de.participacion = round((Activa / Total) * 100, 1),
             Tasa.de.ocupacion = round((Ocupado / Activa) * 100, 1),
             Tasa.de.informalidad = round((Informal / Ocupado) * 100, 1),
             Nivel = "Licenciatura") %>% 
      rename(Clave.carrera = Carrera.clave,
             Carrera = Carrera.nombre,
             Hombres = Hombre,
             Mujeres = Mujer,
             PEA = Activa,
             PNEA = `No activa`,
             Ocupados = Ocupado,
             Desocupados = Desocupado,
             Formales = Formal,
             Informales = Informal,
             Desanimados = Desanimado,
             Menores.de.30 = `Menor de 30`,
             Mayores.de.30 = `30 o mas`,
             Ocupados.con.ingreso.igual.a.cero = `Ingreso cero`,
             Con.licenciatura = `Carrera profesional`,
             Con.Posgrado = Posgrado,
             Subordinados = Subordinado,
             Empleadores = Empleador,
             Trabajadores.por.cuenta.propia = Cuentapropia,
             Trabajadores.sin.pago = `Sin pago`,
             Tasa.de.Riesgo = Tasa.riesgo,
             ing.prom = Ingreso,
             ing.prom.hombre = Ingreso.hombre,
             ing.prom.mujer = Ingreso.mujer,
             ing.prom.formal = Ingreso.formal,
             ing.prom.informal = Ingreso.informal,
             ing.prom.posg = Ingreso.posgrado,
             ing.prom.lice = `Ingreso.carrera profesional`,
             ing.prom.menores.30 = `Ingreso.menor de 30`,
             ing.prom.mayores.30 = `Ingreso.30 o mas`,
             ing.percentil.25 = Ingreso.q25,
             ing.percentil.50 = Ingreso.q50,
             ing.percentil.75 = Ingreso.q75,
             ing.percentil.100 = Ingreso.q100) 
   
   lic <- lic[, colnames(lic) %in% colnames(tablas_sitio$estadisticas)]
   
   tsu <- resultados$tsu
   
   colnames(tsu) <- str_to_sentence(colnames(tsu))
   colnames(tsu) <- str_replace_all(colnames(tsu), "_", ".")
   
   tsu <- tsu %>% mutate(Tasa.de.participacion = round((Activa / Total) * 100, 1),
                         Tasa.de.ocupacion = round((Ocupado / Activa) * 100, 1),
                         Tasa.de.informalidad = round((Informal / Ocupado) * 100, 1),
                         Nivel = "TSU",
                         Con.licenciatura = NA,
                         Con.Posgrado = NA,
                         ing.prom.posg = NA,
                         ing.prom.lice = NA) %>% 
      rename(Clave.carrera = Carrera.clave,
             Carrera = Carrera.nombre,
             Hombres = Hombre,
             Mujeres = Mujer,
             PEA = Activa,
             PNEA = `No activa`,
             Ocupados = Ocupado,
             Desocupados = Desocupado,
             Formales = Formal,
             Informales = Informal,
             Desanimados = Desanimado,
             Menores.de.30 = `Menor de 30`,
             Mayores.de.30 = `30 o mas`,
             Ocupados.con.ingreso.igual.a.cero = `Ingreso cero`,
             Subordinados = Subordinado,
             Empleadores = Empleador,
             Trabajadores.por.cuenta.propia = Cuentapropia,
             Trabajadores.sin.pago = `Sin pago`,
             Tasa.de.Riesgo = Tasa.riesgo,
             ing.prom = Ingreso,
             ing.prom.hombre = Ingreso.hombre,
             ing.prom.mujer = Ingreso.mujer,
             ing.prom.formal = Ingreso.formal,
             ing.prom.informal = Ingreso.informal,
             ing.prom.menores.30 = `Ingreso.menor de 30`,
             ing.prom.mayores.30 = `Ingreso.30 o mas`,
             ing.percentil.25 = Ingreso.q25,
             ing.percentil.50 = Ingreso.q50,
             ing.percentil.75 = Ingreso.q75,
             ing.percentil.100 = Ingreso.q100) 
   
   tsu <- tsu[, colnames(tsu) %in% colnames(tablas_sitio$estadisticas),]
   
   estadisticas <- bind_rows(lic, tsu)
   
   estadisticas <- estadisticas %>% mutate(periodo = año)
   estadisticas <- estadisticas %>% filter(is.na(Carrera) == FALSE)
   
   ## ajustar formato
   
   Decimales <- function(valor){
      valor <- ifelse(valor <= 1, round(valor * 100, 1), valor)
   }
   
   estadisticas <- estadisticas %>% mutate_at(vars(matches("^Tasa")),
                                              Decimales)
   
   #### verificar que todas las columnas necesarios se encuentren presentes
   tabla <- tablas_sitio$estadisticas
   
   if(sum(colnames(tabla) %in% colnames(estadisticas)) - ncol(tabla) < 0){
      warning("Columnas faltantes en estadisticas")
   }
   
   
   return(estadisticas)
   
}

tablas_nuevas$estadisticas <- CrearEstadisticas(resultados, tablas_sitio)

# crear tabla de participacion sector
CrearParticipacionSector <- function(resultados, tablas_sitio){
   
   sectores <- c("Agricultura y ganaderia",
                 "Industria extractiva",
                 "Industria manufacturera",
                 "Construccion",
                 "Comercio",
                 "Restaurantes y alojamientos",
                 "Transportes y comunicaciones",
                 "Servicios profesionales, financieros y corporativos",
                 "Servicios sociales",
                 "Servicios diversos",
                 "Gobierno y organismos internacionales")
   
   ## obtener resultados por carrera con solo columnas de OCUPADO por sector por carrera
   sector_lic <- resultados$carreras
   
   colnames(sector_lic) <- str_to_sentence(colnames(sector_lic))
   
   sector_lic <- sector_lic[, colnames(sector_lic) %in% c("Carrera_clave", sectores)]
   
   ## modificar los nombres de columnas y crear nuevas columnas
   sector_lic <- sector_lic %>% mutate(Nivel = "Licenciatura",
                                       año  = año,
                                       trimestre = 1)
   ## pasar a formato largo
   sector_lic <- pivot_longer(sector_lic,
                              -c("Carrera_clave", "año", "trimestre", "Nivel"),
                              names_to = "id_sector",
                              values_to = "ocupados")
   
   ## obtener resultados por carrera con solo columnas de OCUPADO por sector por tsu
   sector_tsu <- resultados$tsu 
   
   colnames(sector_tsu) <- str_to_sentence(colnames(sector_tsu))
   
   sector_tsu <- sector_tsu[, colnames(sector_tsu) %in% c("Carrera_clave", sectores)]
   
   ## modificar los nombres de columnas y crear nuevas columnas
   sector_tsu <- sector_tsu %>% 
      mutate(Nivel = "TSU",
             año  = año,
             trimestre = 1)
   
   ## pasar a formato largo
   sector_tsu <- pivot_longer(sector_tsu,
                              -c("Carrera_clave", "año", "trimestre", "Nivel"),
                              names_to = "id_sector",
                              values_to = "ocupados")
   
   ## unir ambas 
   participacion_sector <- bind_rows(sector_lic, sector_tsu)
   
   ## ajustar formato
   participacion_sector$ocupados <- replace_na(participacion_sector$ocupados, 0)
   
   participacion_sector$id_sector <- recode(participacion_sector$id_sector,
                                            "Agricultura y ganaderia" = "1",
                                            "Industria extractiva" = "2",
                                            "Industria manufacturera" = "3",
                                            "Construccion" = "4",
                                            "Comercio" = "5",
                                            "Restaurantes y alojamientos" = "6",
                                            "Transportes y comunicaciones" = "7",
                                            "Servicios profesionales, financieros y corporativos" = "8",
                                            "Servicios sociales" = "9",
                                            "Servicios diversos" = "10",
                                            "Gobierno y organismos internacionales" = "11") 
   
   participacion_sector <- rename(participacion_sector,
                                  "Clave.carrera" = "Carrera_clave")
   
   #### verificar que todas las columnas necesarios se encuentren presentes
   tabla <- tablas_sitio$participacion_sector
   
   if(sum(colnames(tabla) %in% colnames(participacion_sector)) - ncol(tabla) < 0){
      warning("Columnas faltantes en participacion sector")
   }
   
   return(as.data.frame(participacion_sector))
   
}

tablas_nuevas$participacion_sector <- CrearParticipacionSector(resultados, tablas_sitio)

# crear tabla de costos universidades

tablas_nuevas$costos_universidades <- costos_universidades

# crear tabla de campos
CrearCampos <- function(resultados, clas.car, año){
   
   ## campos amplios
   campo_amplio <- resultados$areas %>% select(AREA_CLAVE)
   campo_amplio <- left_join(campo_amplio, 
                             unique(select(clas.car, c.amp.cve, c.amp.nom)),
                             by = c("AREA_CLAVE" = "c.amp.cve"))
   
   campo_amplio <- campo_amplio %>% 
      rename(id_campo_amplio = AREA_CLAVE,
             campo_amplio = c.amp.nom) %>% 
      mutate(año = año,
             trimestre = 1,
             campo_amplio = str_to_sentence(campo_amplio))
   
   ## campos especificos
   campo_especifico <- resultados$subareas %>% select(SUBAREA_CLAVE)
   campo_especifico <- left_join(campo_especifico, 
                                 unique(select(clas.car, c.esp.cve, c.esp.nom)),
                                 by = c("SUBAREA_CLAVE" = "c.esp.cve"))
   
   campo_especifico <- campo_especifico %>% 
      rename(id_campo_especifico = SUBAREA_CLAVE,
             campo_especifico = c.esp.nom) %>% 
      mutate(año = año,
             trimestre = 1,
             campo_amplio = substring(id_campo_especifico, 1, 1),
             campo_especifico = str_to_sentence(campo_especifico))
   
   ## campos detallados
   ### licenciaturas
   campo_detallado_lic <- resultados$carreras %>% 
      ungroup() %>% 
      select(CARRERA_CLAVE)
   
   campo_detallado_lic <- left_join(campo_detallado_lic, 
                                    unique(select(clas.car, c.det.cve, c.det.nom)),
                                    by = c("CARRERA_CLAVE" = "c.det.cve"))
   
   ### tsu
   campo_detallado_tsu <- resultados$tsu %>% 
      ungroup() %>% 
      select(CARRERA_CLAVE)
   
   campo_detallado_tsu <- left_join(campo_detallado_tsu, 
                                    unique(select(clas.car, c.det.cve, c.det.nom)),
                                    by = c("CARRERA_CLAVE" = "c.det.cve"))
   
   ### unir licenciatura y tsu, seleccionar registros unicos y con nombre
   campo_detallado <- bind_rows(campo_detallado_lic, campo_detallado_tsu)
   campo_detallado <- unique(campo_detallado)   

   campo_detallado <- filter(campo_detallado, is.na(c.det.nom) == FALSE)
   
   campo_detallado <- campo_detallado %>% 
      rename(id_campo_detallado = CARRERA_CLAVE,
             campo_detallado = c.det.nom) %>% 
      mutate(año = año,
             trimestre = 1,
             campo_especifico = substring(id_campo_detallado, 1, 2),
             campo_detallado = str_to_sentence(campo_detallado))
   
   campos <- list(campo_amplio, campo_especifico, campo_detallado)

   }

tablas_nuevas$campo_amplio <- CrearCampos(resultados, clas.car, año)[[1]]
tablas_nuevas$campo_especifico <- CrearCampos(resultados, clas.car, año)[[2]]
tablas_nuevas$campo_detallado <- CrearCampos(resultados, clas.car, año)[[3]]

# crear tablas de carreras y universidades
Procesar911 <- function(f911, tablas_sitio){
   
   ## crear lista carreras
   ### licenciaturas y tsu
   carreras <- f911$carreras
   
   carreras <- carreras %>% rename(PLANTEL = CCT_INS_PLA,
                                   NOMPLANTEL = NOMBRE_INS_PLA,
                                   ESCUELA = CV_CCT,
                                   NOMESCUELA = NOMBRECT,
                                   NOMREGIMEN = CONTROL,
                                   CARRERA = CV_CARRERA,
                                   NOMBRECAR = C_NOM_CARRERA,
                                   ENTIDAD = CV_ENT_INMUEBLE,
                                   MUNICIPIO = CV_MUN,
                                   NOMENTIDAD = ENTIDAD_INMUEBLE,
                                   NOMMUNICIPIO = C_NOM_MUN,
                                   MATRICULA = V177,
                                   M.H = V175,
                                   M.M = V176,
                                   E = V23,
                                   E.H = V21,
                                   E.M = V22,
                                   LO = V82,
                                   SNI = V85,
                                   NI.M = V89,
                                   NI.H = V88)
   
   carreras <- carreras %>% mutate(NIVEL = 3,
                                   NOMNIVEL = "Universidad",
                                   CVE_NIV = substring(CARRERA, 1, 1),
                                   NOMSUBNIVEL = recode(CVE_NIV,
                                                        "4" = "TÉCNICO SUPERIOR UNIVERSITARIO",
                                                        "5" = "LICENCIATURA UNIVERSITARIA, NORMAL Y TECNOLÓGICA"),
                                   SUBNIVEL = recode(NOMSUBNIVEL, 
                                                     "TÉCNICO SUPERIOR UNIVERSITARIO" = "30", 
                                                     "LICENCIATURA UNIVERSITARIA, NORMAL Y TECNOLÓGICA" = "31"),
                                   NOMREGIMEN = str_replace(NOMREGIMEN, "PÚBLICO", "PUBLICO"),
                                   REGIMEN = recode(NOMREGIMEN,
                                                    "PUBLICO" = "1",
                                                    "PRIVADO" = "2"),
                                   CAMPO = RealizarEquivalenciasCMPE(substring(carreras$CARRERA, 2, 5), "cmpe2011"),
                                   AREA = substring(CAMPO, 1, 1))
   
   carreras <- carreras %>% filter(C_CARRERA_ESTATUS == "Activa")
   
   carreras <- carreras[, colnames(carreras) %in% colnames(tablas_sitio$lista_carreras)]
   
   #### dar formato a las columnas
   carreras <- carreras %>% mutate_at(vars(PLANTEL, NOMPLANTEL, ESCUELA, NOMESCUELA, NOMREGIMEN, CARRERA, NOMENTIDAD, ENTIDAD, NOMMUNICIPIO, MUNICIPIO, NOMNIVEL, NOMSUBNIVEL, CAMPO, AREA), 
                                      as.character)
   carreras <- carreras %>% mutate_at(vars(M.H, M.M, E.H, E.M, LO, SNI, NI.M, NI.H, NIVEL, REGIMEN, SUBNIVEL), 
                                      as.integer)
   
   ### bachillerato
   bach <- f911$bach 
   
   bach <- bach %>% rename(PLANTEL = CCT_INS_PLA,
                           NOMPLANTEL = NOMBRE_INS_PLA,
                           ESCUELA = CV_CCT,
                           NOMESCUELA = NOMBRECT,
                           NOMREGIMEN = CONTROL,
                           CARRERA = CV_CARRERA,
                           NOMBRECAR = C_NOM_CARRERA,
                           ENTIDAD = CV_ENT_INMUEBLE,
                           MUNICIPIO = CV_MUN,
                           NOMENTIDAD = ENTIDAD_INMUEBLE,
                           NOMMUNICIPIO = C_NOM_MUN,
                           MATRICULA = V472,
                           M.H = V470,
                           M.M = V471,
                           E = V204,
                           E.H = V202,
                           E.M = V203,
                           LO = V333,
                           SNI = V336,
                           NI.M = V413,
                           NI.H = V412)
   
   bach <- bach %>% mutate(NIVEL = 3,
                           NOMNIVEL = "Bachillerato",
                           NOMSUBNIVEL = "Bachillerato Tecnologico",
                           SUBNIVEL = 24, 
                           NOMREGIMEN = str_replace(NOMREGIMEN, "PÚBLICO", "PUBLICO"),
                           REGIMEN = recode(NOMREGIMEN,
                                            "PUBLICO" = "1",
                                            "PRIVADO" = "2"),
                           CAMPO = RealizarEquivalenciasCMPE(substring(CARRERA, 2, 5), "cmpe2011"),
                           AREA = substring(CAMPO, 1, 1),
                           CARRERA = as.character(CARRERA))
   
   bach <- bach %>% filter(C_CARRERA_ESTATUS == "Activa" & is.na(CAMPO) == FALSE)
   
   bach <- bach[, colnames(bach) %in% colnames(tablas_sitio$lista_carreras)]
   
   #### dar formato a las columnas
   bach <- bach %>% mutate_at(vars(PLANTEL, NOMPLANTEL, ESCUELA, NOMESCUELA, NOMREGIMEN, CARRERA, NOMENTIDAD, ENTIDAD, NOMMUNICIPIO, MUNICIPIO, NOMNIVEL, NOMSUBNIVEL, CAMPO, AREA), 
                                      as.character)
   bach <- bach %>% mutate_at(vars(M.H, M.M, E.H, E.M, LO, SNI, NI.M, NI.H, NIVEL, REGIMEN, SUBNIVEL), 
                                      as.integer)
   
   
   ### unir bachillero con educacion superior
   lista_carreras <- bind_rows(carreras, bach)
   
   ### unir con nombres de area y subareas
   lista_carreras <- left_join(lista_carreras, unique(select(clas.car, c.amp.cve, c.amp.nom)),
                               by = c("AREA" = "c.amp.cve"))
   lista_carreras <- left_join(lista_carreras, unique(select(clas.car, c.det.cve, c.det.nom)),
                               by = c("CAMPO" = "c.det.cve"))
   
   lista_carreras <- rename(lista_carreras,
                            NOMAREA = c.amp.nom,
                            NOMCAMPO = c.det.nom)
   
   # crear universidades
   ## filtrar y seleccionar datos de los f911
   universidades <- lista_carreras %>% 
      select(PLANTEL, NOMPLANTEL, ESCUELA, NOMESCUELA, NIVEL, 
             NOMNIVEL, NOMSUBNIVEL, SUBNIVEL, REGIMEN, NOMREGIMEN, 
             ENTIDAD, MUNICIPIO, NOMENTIDAD, NOMMUNICIPIO) 
   
   universidades <- universidades %>% rename(clave_universidad = PLANTEL,
                                             universidad = NOMPLANTEL,
                                             cct = ESCUELA,
                                             nombre_escuela_campus = NOMESCUELA,
                                             nivel = NIVEL,
                                             nomnivel = NOMNIVEL, 
                                             subnivel = SUBNIVEL,
                                             nomsubnivel = NOMSUBNIVEL,
                                             regimen = REGIMEN, 
                                             nomsostenimiento = NOMREGIMEN,
                                             entidad = ENTIDAD,
                                             clave_municipio = MUNICIPIO,
                                             nombre_entidad = NOMENTIDAD,
                                             nombre_municipio = NOMMUNICIPIO)
   
   universidades <- universidades %>% mutate(nombre_escuela_campus = str_to_title(nombre_escuela_campus),
                                             universidad = str_to_title(universidad),
                                             nombre_entidad = str_to_title(nombre_entidad),
                                             nombre_municipio = str_to_title(nombre_municipio))
   
   ## filtrar y seleccionar datos del directorio nacional de escuelas
   dir <- dir %>% rename(latitud = INMUEBLE_LATITUD, 
                         longitud = INMUEBLE_LONGITUD, 
                         telefono = CONTACTO_C_TELEFONO)
   
   dir <- dir %>% mutate(domicilio = ifelse(is.na(INMUEBLE_N_EXTNUM) == FALSE,
                                            paste(str_to_title(INMUEBLE_C_VIALIDAD_PRINCIPAL), 
                                                  INMUEBLE_N_EXTNUM,
                                                  sep = " "),
                                            INMUEBLE_C_VIALIDAD_PRINCIPAL)) %>% select(-c(INMUEBLE_C_VIALIDAD_PRINCIPAL, INMUEBLE_N_EXTNUM))
   
   ## unir datos de 911 y el directorio
   universidades <- left_join(universidades,
                              dir,
                              by = c("cct" = "CV_CCT"))
   
   ## unir datos de universidades con la tabla anterior, para obtner columnas que ya no existen en el directorio ni 911
   universidades <- left_join(universidades,
                              select(tablas_sitio$universidades, cct, email, website),
                              by = "cct")
   
   universidades <- universidades[, colnames(universidades) %in% colnames(tablas_sitio$universidades)]
   
   universidades <- unique(universidades)
   
   # crear universidades matricula
   universidades_matricula <- lista_carreras %>% 
      select(CAMPO, NOMBRECAR, NOMPLANTEL, NOMESCUELA, ESCUELA, 
             NIVEL, NOMNIVEL, SUBNIVEL, NOMSUBNIVEL, PLANTEL, 
             NOMENTIDAD, NOMMUNICIPIO, ENTIDAD, MUNICIPIO, REGIMEN, 
             NOMREGIMEN)
   
   universidades_matricula <- universidades_matricula %>% 
      rename(campo_detallado = CAMPO,
             nombre_carrera = NOMBRECAR,
             universidad = NOMPLANTEL,
             nombre_campus = NOMESCUELA,
             cct = ESCUELA,
             nivel = NIVEL,
             nomnivel = NOMNIVEL,
             subnivel = SUBNIVEL,
             nomsubnivel = NOMSUBNIVEL,
             clave_universidad = PLANTEL,
             nomentidad = NOMENTIDAD,
             nombre_municipio = NOMMUNICIPIO,
             id_entidad = ENTIDAD,
             clave_municipio = MUNICIPIO,
             regimen = REGIMEN,
             nomregimen = NOMREGIMEN)
   
   universidades_matricula <- universidades_matricula %>% 
      mutate_at(vars(nombre_carrera, universidad, nombre_campus, nomentidad, nombre_municipio), 
                str_to_title) 
   
   universidades_matricula <- unique(universidades_matricula)

   matricula <- lista_carreras %>% select(CAMPO, ESCUELA, M.H, M.M, E.H, E.M, LO, SNI, NI.H, NI.M)
   
   suppressMessages(
   matricula <- matricula %>% group_by(CAMPO, ESCUELA) %>% summarise(matricula_hombres = sum(M.H, na.rm = T),
                                                                     matricula_mujeres = sum(M.M, na.rm = T),
                                                                     matricula_nuevo_ingreso_hombres = sum(NI.H, na.rm = T),
                                                                     matricula_nuevo_ingreso_mujeres = sum(NI.M, na.rm = T),
                                                                     egresados_hombres = sum(E.H, na.rm = T),
                                                                     egresados_mujeres = sum(E.M, na.rm = T),
                                                                     lugares_ofertados = sum(LO, na.rm = T),
                                                                     solicitudes_nuevo_ingreso = sum(SNI, na.rm = TRUE))
   )
   
   universidades_matricula <- left_join(universidades_matricula,
                                        matricula,
                                        by = c("campo_detallado" = "CAMPO", "cct" = "ESCUELA"))
   
   universidades_matricula <- universidades_matricula %>% 
      mutate(año = año,
             trimestre = 1)
   
   # unir todos los resultados en una lista
   l911 <- list(lista_carreras, universidades, universidades_matricula)
   names(l911) <- c("lista_carreras", "universidades", "universidades_matricula")
   
   return(l911)
}

l911 <- Procesar911(f911, tablas_sitio)

tablas_nuevas$lista_carreras <- l911$lista_carreras
tablas_nuevas$universidades <- l911$universidades
tablas_nuevas$universidades_matricula <- l911$universidades_matricula

for(i in 1:length(tablas_nuevas)){
   
   write.csv(tablas_nuevas[[i]], paste0("03_resultados\\sitio web\\", names(tablas_nuevas)[i], ".csv"),
             fileEncoding = "UTF-8")
}                                                        

                                 