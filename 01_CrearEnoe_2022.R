CrearEnoe <- function(sdem, periodo_enoe){
   
# La funcion CreateEnoeTable toma como argumento la tabla sdem de los microdatos de inegi y crea la tabla enoe que contiene la informacion necesaria para los calculos de Compara Carreras

 source("02_scripts\\011_RealizarEquivalenciasCMPE.R")
   
   
# Si el trimestre es el 320 o 420 (cuando se utilizo la ENOEN), ajustar el nombre de las variables del estrato de diseño y el factor de expansion
   if(periodo_enoe %in% c("320", "420", "121", "221", "321", "421")){
      enoe <- sdem %>% rename(EST_D = EST_D_TRI,
                              FAC = FAC_TRI) 
   }

# Ajustar el formato de la tabla 
## Seleccionar las variables necesarias
   enoe <- enoe %>% 
      select(ENT, R_DEF, C_RES, EST, EST_D, UPM, SEX, EDA, CS_P13_1, 
             CS_P13_2, CS_P14_C, CS_P15, CS_P16, FAC, CLASE1, CLASE2, 
             POS_OCU, C_OCU11C, RAMA_EST1, RAMA_EST2,
             DUR_EST, HRSOCUP, INGOCUP, 
             ING_X_HRS, SCIAN, EMP_PPAL, TUE_PPAL, C_INAC5C)
   
## Renombrar variables
   enoe <- enoe %>% 
      rename(NIVEL_ESC = CS_P13_1, 
             A_APROB = CS_P13_2, 
             TER_EST = CS_P16, 
             C_CAR = CS_P14_C,
             EDAD = EDA,
             SEXO = SEX,
             ACTIVIDAD = CLASE1,
             OCUPACION = CLASE2,
             POSICION = POS_OCU,
             FORMALIDAD = EMP_PPAL,
             EST_REQ = CS_P15,
             SECTOR = RAMA_EST2)

## Modificar variables para recodificar valores
   enoe <- enoe %>% 
      mutate_at(vars(R_DEF, C_RES, ACTIVIDAD, POSICION, FORMALIDAD, NIVEL_ESC, TER_EST, C_CAR, EST_REQ, EDAD, A_APROB, SECTOR),
                as.character) %>%
      mutate_at(vars(EST_REQ, A_APROB), 
                na_if, y = "9") %>% 
      mutate_at(vars(NIVEL_ESC), 
                na_if, y = "99") %>%
      mutate_at(vars(ACTIVIDAD, POSICION, OCUPACION, FORMALIDAD, SECTOR), 
                na_if, y = "0") %>% 
      mutate_at(vars(EDAD, A_APROB), 
                as.numeric) %>%
      mutate(ACTIVIDAD = recode(ACTIVIDAD, 
                                 "1" = "activa", 
                                 "2" = "no activa"),
             POSICION = recode(POSICION, 
                                "1" = "subordinado", 
                                "2" = "empleador", 
                                "3" = "cuentapropia", 
                                "4" = "sin pago", 
                                "5" = "no especificado"),
             OCUPACION = recode(OCUPACION, 
                                "1" = "ocupado", 
                                "2" = "desocupado", 
                                "3" = "disponible", 
                                "4" = "no disponible"),
             FORMALIDAD = recode(FORMALIDAD, 
                                  "1" = "informal", 
                                  "2" = "formal"),
             SEXO = recode(SEXO, 
                            "1" = "hombre", 
                            "2" = "mujer"),
             NIVEL_ESC = recode(NIVEL_ESC,
                                "00" = "ninguno",
                                "01" = "preescolar",
                                "02" = "primaria",
                                "03" = "secundaria",
                                "04" = "bachillerato",
                                "05" = "normal",
                                "06" = "carrera tecnica",
                                "07" = "profesional",
                                "08" = "maestria",
                                "09" = "doctorado"),
             TER_EST = recode(TER_EST,
                              "1" = "si",
                              "2" = "no",
                              "9" = "no sabe"),
             EST_REQ = recode(EST_REQ,
                              "1" = "primaria",
                              "2" = "secundaria",
                              "3" = "preparatoria",
                              "9" = "no sabe"),
             SECTOR = recode(SECTOR,
                              "1" = "agricultura y ganaderia",
                              "2" = "industria extractiva",
                              "3" = "industria manufacturera",
                              "4" = "construccion",
                              "5" = "comercio",
                              "6" = "restaurantes y alojamientos",
                              "7" = "transportes y comunicaciones",
                              "8" = "servicios profesionales, financieros y corporativos",
                              "9" = "servicios sociales",
                              "10" = "servicios diversos",
                              "11" = "gobierno y organismos internacionales"))
   
   # Filtrar seleccionar entrevistas completas a residentes habituales de entre 15 a 97 años
   enoe <- enoe %>% 
      filter(R_DEF == "00" & C_RES != "2" & EDAD >= 15 & EDAD <= 98)
   
   # Codificar maximo nivel de estudios completado
   enoe <- enoe %>% 
      mutate(ESC = NA,
             ESC = replace(ESC, NIVEL_ESC == "ninguno" | NIVEL_ESC == "preescolar",  "Ninguno"),
             ESC = replace(ESC, NIVEL_ESC == "primaria" & (A_APROB < 6 | is.na(A_APROB)),  "Ninguno"),
             
             ESC = replace(ESC, NIVEL_ESC == "primaria" & A_APROB == 6,  "Primaria"),
             ESC = replace(ESC, NIVEL_ESC == "secundaria" & (A_APROB < 3 | is.na(A_APROB)),  "Primaria"),
             ESC = replace(ESC, (NIVEL_ESC == "normal" | NIVEL_ESC == "carrera tecnica") & TER_EST == "no" & EST_REQ == "primaria",  "Primaria"),
             
             ESC = replace(ESC, NIVEL_ESC == "secundaria" & A_APROB == 3,  "Secundaria"),
             ESC = replace(ESC, NIVEL_ESC == "bachillerato" & (A_APROB < 3 | is.na(A_APROB)),  "Secundaria"),
             ESC = replace(ESC, (NIVEL_ESC == "normal" | NIVEL_ESC == "carrera tecnica") & TER_EST == "no" & EST_REQ == "secundaria",  "Secundaria"),
             
             ESC = replace(ESC, NIVEL_ESC == "bachillerato" & A_APROB == 3,  "Bachillerato"),
             ESC = replace(ESC, NIVEL_ESC == "profesional" & TER_EST == "no",  "Bachillerato"),
             ESC = replace(ESC, NIVEL_ESC == "profesional" & TER_EST == "no sabe" & EST_REQ == "preparatoria",  "Bachillerato"),
             ESC = replace(ESC, (NIVEL_ESC == "normal" | NIVEL_ESC == "carrera tecnica") & TER_EST == "no" & EST_REQ == "preparatoria",  "Bachillerato"),
             
             ESC = replace(ESC, NIVEL_ESC == "carrera tecnica" & TER_EST == "si", "Carrera tecnica"),
             
             ESC = replace(ESC, NIVEL_ESC == "normal" & TER_EST == "si",  "Carrera profesional"),
             ESC = replace(ESC, NIVEL_ESC == "profesional" & TER_EST == "si",  "Carrera profesional"),
             ESC = replace(ESC, (NIVEL_ESC == "maestria" | NIVEL_ESC == "doctorado") & (TER_EST == "no" | TER_EST == "no sabe"),  "Carrera profesional"),
             ESC = replace(ESC, (NIVEL_ESC == "doctorado" | NIVEL_ESC == "maestria") & TER_EST == "si",  "Posgrado"),
             PROF = 0,
             PROF = replace(PROF, ESC == "Carrera profesional" | ESC == "Posgrado", 1))
             
# Codificar variable de grupos poblacionales y desanimados
   enoe <- enoe %>% 
      mutate(GRUPO_EDAD = NA,
             GRUPO_EDAD = ifelse(EDAD < 30,
                                 "menor de 30",
                                 "30 o mas"),
             DESANIMADOS = ifelse(C_INAC5C == 5,
                                  "desanimado",
                                  NA))
   
# Codificar el ingreso de ocupados con más de 30 hrs por semana e ingreso mayor a 0
   enoe <- enoe %>% 
      mutate(INGRESO = ifelse(OCUPACION == "ocupado" & HRSOCUP >= 30 & INGOCUP > 0,
                              round(INGOCUP, 0),
                              NA),
             ING_X_HR = ifelse(OCUPACION == "ocupado" & HRSOCUP >= 30 & INGOCUP > 0,
                               ING_X_HRS,
                               NA),
             ING_CERO = ifelse(OCUPACION == "ocupado" & INGOCUP == 0,
                               "ingreso cero",
                               NA))
   
# Ajustar codigos de carreras
## Si el trimestre es posterior al 321, ajustar la clasificacion de carreras
   if(periodo_enoe %in% c("321", "421")){
     
      enoe$C_CAR <- substring(enoe$C_CAR, 1, 4)
      enoe$C_CAR <- RealizarEquivalenciasCMPE(enoe$C_CAR, "cmpe2011")
      
      enoe <- enoe %>% 
         mutate(AREA = substring(C_CAR, 1, 1),
                SUBAREA = substring(C_CAR, 1, 2),
                CARRERA = substring(C_CAR, 1, 3))
   }  else {
      
      enoe <- enoe %>% 
         mutate(AREA = substring(C_CAR, 2, 2),
                SUBAREA = substring(C_CAR, 2, 3),
                CARRERA = substring(C_CAR, 2, 4))
      
   }


   return(enoe)
}