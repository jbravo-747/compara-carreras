CalcularXTsu <- function(enoesvy, enoe, clas_car){

   # Calcular estimadores por tsu
   tsu.tot <- enoesvy %>% 
      filter(ESC == "Carrera tecnica" & CARRERA != "999" & GCC_TSU == 1) %>% 
      group_by(CARRERA) %>% 
      summarise(TOTAL = survey_total())
   
   tsu.sexo <- enoesvy %>% 
      filter(ESC == "Carrera tecnica" & CARRERA != "999" & GCC_TSU == 1) %>% 
      group_by(CARRERA, SEXO) %>% 
      summarise(TOTAL = survey_total())
   
   tsu.actividad <- enoesvy %>% 
      filter(ESC == "Carrera tecnica" & CARRERA != "999" & is.na(ACTIVIDAD) == FALSE & GCC_TSU == 1) %>% 
      group_by(CARRERA, ACTIVIDAD) %>% 
      summarise(TOTAL = survey_total())
   
   tsu.ocupacion <- enoesvy %>% 
      filter(ESC == "Carrera tecnica" & CARRERA != "999" & is.na(OCUPACION) == FALSE & GCC_TSU == 1) %>% 
      group_by(CARRERA, OCUPACION) %>% 
      summarise(TOTAL = survey_total())
   
   tsu.formalidad <- enoesvy %>% 
      filter(ESC == "Carrera tecnica" & CARRERA != "999" & is.na(FORMALIDAD) == FALSE & GCC_TSU == 1) %>% 
      group_by(CARRERA, FORMALIDAD) %>% 
      summarise(TOTAL = survey_total())
   
   tsu.desa <- enoesvy %>% 
      filter(ESC == "Carrera tecnica" & CARRERA != "999" & is.na(DESANIMADOS) == FALSE & GCC_TSU == 1) %>% 
      group_by(CARRERA, DESANIMADOS) %>% 
      summarise(TOTAL = survey_total())
   
   tsu.edad <- enoesvy %>% 
      filter(ESC == "Carrera tecnica" & CARRERA != "999" & is.na(GRUPO_EDAD) == FALSE & GCC_TSU == 1) %>% 
      group_by(CARRERA, GRUPO_EDAD) %>% 
      summarise(TOTAL = survey_total())
   
   tsu.sin.ingreso <- enoesvy %>% 
      filter(ESC == "Carrera tecnica" & CARRERA != "999" & is.na(ING_CERO) == FALSE & GCC_TSU == 1) %>% 
      group_by(CARRERA, ING_CERO) %>% 
      summarise(TOTAL = survey_total())
   
   tsu.posicion <- enoesvy %>% 
      filter(ESC == "Carrera tecnica" & CARRERA != "999" & is.na(POSICION) == FALSE & GCC_TSU == 1) %>% 
      group_by(CARRERA, POSICION) %>% 
      summarise(TOTAL = survey_total())
   
   tsu.sector <- enoesvy %>% 
      filter(ESC == "Carrera tecnica" & CARRERA != "999" & is.na(SECTOR) == FALSE & GCC_TSU == 1) %>% 
      group_by(CARRERA, SECTOR) %>% 
      summarise(TOTAL = survey_total())
   
   ## Ajustar formato
   tsu.tot <- select(tsu.tot, -TOTAL_se)
   
   tsu.sexo <- tsu.sexo %>% 
      select(-TOTAL_se) %>% 
      pivot_wider(names_from = SEXO,
                  values_from = TOTAL)
   
   tsu.actividad <- tsu.actividad %>% 
      select(-TOTAL_se) %>% 
      pivot_wider(names_from = ACTIVIDAD,
                  values_from = TOTAL)
   
   tsu.ocupacion <- tsu.ocupacion %>% 
      select(-TOTAL_se) %>% 
      pivot_wider(names_from = OCUPACION,
                  values_from = TOTAL)
   
   tsu.formalidad <- tsu.formalidad %>% 
      select(-TOTAL_se) %>% 
      pivot_wider(names_from = FORMALIDAD,
                  values_from = TOTAL)
   
   tsu.desa <- tsu.desa %>% 
      select(-TOTAL_se) %>% 
      pivot_wider(names_from = DESANIMADOS,
                  values_from = TOTAL)
   
   tsu.edad <- tsu.edad %>% 
      select(-TOTAL_se) %>% 
      pivot_wider(names_from = GRUPO_EDAD,
                  values_from = TOTAL)
   
   tsu.sin.ingreso <- tsu.sin.ingreso %>% 
      select(-TOTAL_se) %>% 
      pivot_wider(names_from = ING_CERO,
                  values_from = TOTAL)
   
   tsu.posicion <- tsu.posicion %>% 
      select(-TOTAL_se) %>% 
      pivot_wider(names_from = POSICION,
                  values_from = TOTAL)
   
   tsu.sector <- tsu.sector %>% 
      select(-TOTAL_se) %>% 
      pivot_wider(names_from = SECTOR,
                  values_from = TOTAL)
   
   totales <- list(tsu.tot, tsu.sexo, tsu.actividad, tsu.ocupacion, tsu.formalidad, 
                   tsu.desa, tsu.edad, tsu.sin.ingreso, tsu.posicion, tsu.sector)
   
   ## unir 
   UnirDfs <- function(x, y){
      merge(x, y, by = "CARRERA", all = TRUE)
   }
   
   totales <- Reduce(UnirDfs, totales)
   
   # Detectar y marcar Salarios atipicos por tsu
   ## Calcular por tsu los percentiles 80,85,90:99 y el salario promedio
   
   quart <- enoesvy %>% 
      filter(ESC == "Carrera tecnica" & GCC_SAL_TSU == 1 & is.na(INGRESO) == FALSE) %>% 
      group_by(CARRERA) %>% 
      summarise(INGRESO = survey_quantile(INGRESO, 
                                          c(0.80, 0.85, 0.90, 0.91, 0.92, 0.93, 0.94, 0.95, 0.96, 0.97, 0.98, 0.99, 1)))
   
   media <- enoesvy %>% 
      filter(ESC == "Carrera tecnica" & GCC_SAL_TSU == 1 & is.na(INGRESO) == FALSE) %>% 
      group_by(CARRERA) %>% 
      summarise(PROMEDIO = survey_mean(INGRESO))
   
   quart <- left_join(select(quart, -matches("se$")),
                      select(media, -PROMEDIO_se),
                      by = "CARRERA")
   
   ## Para cada percentil, calcular el ratio entre ingreso del percentil y el promedio, 
   ## luego calcular el promedio y la desviacion estandar del ratio de cada percentil, 
   ## y marcar como valores atipicos en el data.frame p.out cuando el valor del percentil es mas de dos desviaciones mas alto que el promedio del percentil
   valores_atipicos <- rep.int(NA, length(quart$CARRERA))
   
   DetectarAtipicos <- function(percentil, media, lista_atipicos){
      ratio <- percentil / media
      ratio_media <- mean(ratio, na.rm = TRUE)
      
      ratio_sd <- (sd(ratio) * (length(ratio) - 1)) / length(ratio)
      
      ratio_outlier <- ifelse(ratio > (ratio_media + (ratio_sd * 2)),
                              "si",
                              "no")
      outlier_valor <- ifelse(ratio_outlier == "si",
                              percentil,
                              NA)
      valores_atipicos <- ifelse(is.na(valores_atipicos),
                                 outlier_valor,
                                 valores_atipicos)
      
      return(valores_atipicos)
   }
   
   quart <- as.data.frame(quart)
   
   for(c in 2:14){
      valores_atipicos <- DetectarAtipicos(percentil = quart[, c], 
                                           media = quart$PROMEDIO, 
                                           lista_atipicos = valores_atipicos)   
   }
   
   valores_atipicos <- ifelse(is.na(valores_atipicos),
                              quart$INGRESO_q100 + 1,
                              valores_atipicos)
   
   ## Marcar como atipicos en la enoe a los valores iguales o mayores al valor del percentil outlier de cada tsu
   atipicos <- data.frame(CARRERA = quart$CARRERA,
                          ESC = "Carrera tecnica",
                          VALOR_ING_ATIP_TSU = valores_atipicos)
   enoe <- left_join(enoe,
                     atipicos,
                     by = c("ESC", "CARRERA"))
   
   enoe <- enoe %>% 
      mutate(INGRESO = ifelse((is.na(INGRESO) == FALSE & is.na(VALOR_ING_ATIP_TSU)) | INGRESO <= VALOR_ING_ATIP_TSU,
                              INGRESO,
                              NA))
   
   # Calcular salarios por tsu
   
   ## Asignar nuevo diseno de la encuesta a la enoe con etiquetas de salarios atipicos
   
   enoesvy <- as_survey_design(enoe, ids = UPM, strata = EST_D, nest = TRUE, weights = FAC)
   
   ## Calcular salario promedio por grupo, excluyendo salarios atipicos
   # Calcular salarios por tsu
   tsu.sal <- enoesvy %>% 
      filter(ESC == "Carrera tecnica" & is.na(CARRERA) == FALSE & CARRERA != "999" & is.na(INGRESO) == FALSE & GCC_SAL_TSU == 1) %>% 
      group_by(CARRERA) %>% 
      summarise(INGRESO = survey_mean(INGRESO))
   
   tsu.sal.sexo <- enoesvy %>% 
      filter(ESC == "Carrera tecnica" & is.na(CARRERA) == FALSE & CARRERA != "999" & is.na(INGRESO) == FALSE & GCC_SAL_TSU == 1) %>% 
      group_by(CARRERA, SEXO) %>% 
      summarise(INGRESO = survey_mean(INGRESO))
   
   tsu.sal.form <- enoesvy %>% 
      filter(ESC == "Carrera tecnica" & is.na(CARRERA) == FALSE & CARRERA != "999" & is.na(INGRESO) == FALSE & is.na(FORMALIDAD) == FALSE  & GCC_SAL_TSU == 1) %>% 
      group_by(CARRERA, FORMALIDAD) %>% 
      summarise(INGRESO = survey_mean(INGRESO))
   
   tsu.sal.edad <- enoesvy %>% 
      filter(ESC == "Carrera tecnica" & is.na(CARRERA) == FALSE & CARRERA != "999" & is.na(INGRESO) == FALSE & is.na(GRUPO_EDAD) == FALSE  & GCC_SAL_TSU == 1) %>% 
      group_by(CARRERA, GRUPO_EDAD) %>% 
      summarise(INGRESO = survey_mean(INGRESO))
   
   tsu.sal.quin <- enoesvy %>% 
      filter(ESC == "Carrera tecnica" & is.na(CARRERA) == FALSE & CARRERA != "999" & is.na(INGRESO) == FALSE  & GCC_SAL_TSU == 1) %>% 
      group_by(CARRERA) %>% 
      summarise(INGRESO = survey_quantile(INGRESO, 
                                          c(0.25, 0.5, 0.75, 1)))
   
   ## dar formato 
   tsu.sal <- tsu.sal %>% 
      select(-INGRESO_se)
   
   tsu.sal.sexo <- tsu.sal.sexo %>%
      select(-INGRESO_se) %>% 
      pivot_wider(names_from = SEXO,
                  names_prefix = "INGRESO_",
                  values_from = INGRESO)
   
   tsu.sal.form <- tsu.sal.form %>% 
      select(-INGRESO_se) %>%
      pivot_wider(names_from = FORMALIDAD,
                  names_prefix = "INGRESO_",
                  values_from = INGRESO)
   
   tsu.sal.edad <- tsu.sal.edad %>% 
      select(-INGRESO_se) %>%
      pivot_wider(names_from = GRUPO_EDAD,
                  names_prefix = "INGRESO_",
                  values_from = INGRESO)
   
   tsu.sal.quin <- tsu.sal.quin %>% 
      select(-matches("se$")) 
   
   salarios <- list(tsu.sal, tsu.sal.sexo, tsu.sal.form, tsu.sal.edad, tsu.sal.quin)

   ## unir en una sola dataframe
   salarios <- Reduce(UnirDfs, salarios)
   salarios <- salarios %>% mutate_if(is.numeric, round, digits = 0)
   
   totales.tsu <- left_join(totales,
                                salarios,
                                by = "CARRERA")
   
   ### Dar formato 
   colnames(totales.tsu) <- toupper(colnames(totales.tsu))
   
   totales.tsu <- totales.tsu %>% 
      mutate(TASA_DESOCUPACION = round((DESOCUPADO / ACTIVA), 3), 
             TASA_INFORMALIDAD = round((INFORMAL / OCUPADO) , 3),
             TASA_RIESGO = round((DESOCUPADO + INFORMAL + DESANIMADO) / TOTAL, 3)) 
   
   totales.tsu <- left_join(totales.tsu,
                                select(clas_car, c.det.cve, c.det.nom),
                                by = c("CARRERA" = "c.det.cve"))
   
   totales.tsu <- totales.tsu %>% 
      rename(CARRERA_NOMBRE = c.det.nom,
             CARRERA_CLAVE = CARRERA) %>% 
      mutate(NIVEL = "Carrera tecnica",
             CARRERA_NOMBRE = str_to_sentence(CARRERA_NOMBRE)) %>% 
      relocate(where(is.numeric), .after = where(is.character))
   
   cat("Calculos de tsu completados\n")
   
   return(totales.tsu)
   
}
