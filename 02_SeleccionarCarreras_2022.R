# Este script selecciona las carreras que son incluidas en los analisis 

SeleccionarCarreras <- function(enoe, clas_car, periodo_enoe){
   
   # Establecer el diseño de encuesta de la enoe
   enoesvy <- as_survey_design(enoe, ids = UPM, strata = EST_D, nest = TRUE, weights = FAC)
   options(survey.lonely.psu="adjust")
   
   # El criterio de seleccion para calcular estimadores de poblacion es un coeficiente de variación menor a 15% en la estimacion del total de individuos de cada grupo
   # El criterio de seleccion para calcular estimadores de salario es un coeficiente de variación menor a 15% en la estimacion del salario promedio
   
   # Seleccionar carreras que cumplen criterios para calculos de poblacion
   ## Seleccionar subareas de licenciatura
   ### Estimar el total de individuos por subarea de licenciatura y coeficiente variacion
   tot.subarea <- enoesvy %>% 
      filter(PROF == 1) %>% 
      group_by(SUBAREA) %>% 
      summarise(TOTAL = survey_total(na.rm = TRUE,
                                     vartype = "cv"))
   
   ### Separar los registros segun si cumplen o no con el criterio preestablecido, y guardar las claves de cada grupo
   suba.si <- tot.subarea %>% 
      filter(TOTAL_cv <= .15) %>% 
      select(SUBAREA)
   
   ## Seleccionar licenciatura
   ### estimar el total de individuos por licenciatura y el error estandar de la estimacion
   tot.car <- enoesvy %>% 
      filter(PROF == 1) %>% 
      group_by(CARRERA) %>% 
      summarise(TOTAL = survey_total(na.rm = TRUE,
                                     vartype = "cv"))
   
   ### Separar los registros segun si cumplen o no con el criterio preestablecido, y guardar las claves de cada grupo
   car.si <- tot.car %>% 
      filter(TOTAL_cv <= .15) %>%
      select(CARRERA)
   
   ## Seleccionar subareas de tsu
   ### estimar el total de individuos por subarea de tsu
   tot.sub.tsu <- enoesvy %>% 
      filter(ESC == "Carrera tecnica") %>% 
      group_by(SUBAREA) %>% 
      summarise(TOTAL = survey_total(na.rm = TRUE,
                                     vartype = "cv"))
   
   ### Separar los registros segun si cumplen o no con el criterio preestablecido, y guardar las claves de cada grupo
   sub.tsu.si <- tot.sub.tsu %>% 
      filter(TOTAL_cv <= .15) %>%
      select(SUBAREA)
   
   ## Selecciona tsu
   ### estimar el total de individuos por carrera de tsu y el error estandar de la estimacion
   tot.car.tsu <- enoesvy %>% 
      filter(ESC == "Carrera tecnica") %>% 
      group_by(CARRERA) %>% 
      summarise(TOTAL = survey_total(na.rm = TRUE,
                                     vartype = "cv"))
   
   ### Separar los registros segun si cumplen o no con el criterio preestablecido, y guardar las claves de cada grupo
   car.tsu.si <- tot.car.tsu %>% 
      filter(TOTAL_cv <= .15) %>%
      select(CARRERA)
   
   ## Selecionar grupo de carreras de compara carreras, de subareas y tsu
   ### Marcar en la base las subareas que aparezcan en los registros seleccionados
   enoe <- enoe %>% 
      mutate(GCC_SUBA = ifelse(PROF == 1 & SUBAREA %in% suba.si$SUBAREA,
                               1,
                               0),
             GCC = ifelse(PROF == 1 & CARRERA %in% car.si$CARRERA,
                          1,
                          0),
             GCC_TSU = ifelse(ESC == "Carrera tecnica" & CARRERA %in% car.tsu.si$CARRERA,
                              1,
                              0))
   
   # Seleccionar carreras que cumplen criterios para calculos de ingreso
   ## asignar nuevo diseño de encuesta con la enoe modificada
   enoesvy <- as_survey_design(enoe, ids = UPM, strata = EST_D, nest = TRUE, weights = FAC)
   options(survey.lonely.psu="adjust")
   
   ## Seleccionar por licenciatura
   ### estimar el salario promedio por licenciatura 
   sal.car <- enoesvy %>% 
      filter(PROF == 1 & GCC == 1 & CARRERA != "999" & is.na(INGRESO) == FALSE) %>% 
      group_by(CARRERA) %>% 
      summarise(INGRESO = survey_mean(INGRESO,
                                      vartype = "cv"))
   
   ### Separar los registros segun si cumplen o no con el criterio preestablecido, y guardar las claves de cada grupo
   sal.car.si <- sal.car %>% filter(INGRESO_cv <= .15) %>% select(CARRERA)
   
   ## Seleccionar por tsu
   ### estimar el salario promedio por tsu 
   sal.tsu <- enoesvy %>% 
      filter(ESC == "Carrera tecnica" & GCC_TSU == 1 & CARRERA != "999" & is.na(INGRESO) == FALSE) %>% 
      group_by(CARRERA) %>% 
      summarise(INGRESO = survey_mean(INGRESO,
                                      vartype = "cv"))
   
   ### Separar los registros segun si cumplen o no con el criterio preestablecido, y guardar las claves de cada grupo
   sal.tsu.si <- sal.tsu %>% filter(INGRESO_cv <= .15) %>% select(CARRERA)
   
   ###marcar grupos de carreras de licenciatura (salarial)
   enoe <- enoe %>% 
      mutate(GCC_SAL = ifelse(PROF == 1 & CARRERA %in% sal.car.si$CARRERA,
                              1,
                              0),
             GCC_SAL_TSU = ifelse(ESC == "Carrera tecnica" & CARRERA %in% sal.tsu.si$CARRERA,
                                  1,
                                  0))
   
   # asignar nuevo diseño de encuesta con la enoe modificada  por selecciones por poblacion y salario
   enoesvy <- as_survey_design(enoe, ids = UPM, strata = EST_D, nest = TRUE, weights = FAC)
   
   res <- list(enoe, enoesvy)
   names(res) <- c("enoe", "enoesvy")
   
   
   cat("Seleccion de carreras realizada\n")
   return(res)
   
}

