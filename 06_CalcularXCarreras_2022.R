# La funcion CalcularXCarreras realiza los calculos salariales y de indicadores del mercado laboral

CalcularXCarreras <- function(enoe, clas_car, enoesvy){
   
# Calcular estimadores por carrera
   carrera.tot <- enoesvy %>% 
      filter(PROF == 1 & CARRERA != "999" & GCC == 1) %>% 
      group_by(CARRERA) %>% 
      summarise(TOTAL = survey_total())
   
   carrera.sexo <- enoesvy %>% 
      filter(PROF == 1 & CARRERA != "999" & GCC == 1) %>% 
      group_by(CARRERA, SEXO) %>% 
      summarise(TOTAL = survey_total())
   
   carrera.actividad <- enoesvy %>% 
      filter(PROF == 1 & CARRERA != "999" & is.na(ACTIVIDAD) == FALSE & GCC == 1) %>% 
      group_by(CARRERA, ACTIVIDAD) %>% 
      summarise(TOTAL = survey_total())
   
   carrera.ocupacion <- enoesvy %>% 
      filter(PROF == 1 & CARRERA != "999" & is.na(OCUPACION) == FALSE & GCC == 1) %>% 
      group_by(CARRERA, OCUPACION) %>% 
      summarise(TOTAL = survey_total())
   
   carrera.formalidad <- enoesvy %>% 
      filter(PROF == 1 & CARRERA != "999" & is.na(FORMALIDAD) == FALSE & GCC == 1) %>% 
      group_by(CARRERA, FORMALIDAD) %>% 
      summarise(TOTAL = survey_total())
   
   carrera.desa <- enoesvy %>% 
      filter(PROF == 1 & CARRERA != "999" & is.na(DESANIMADOS) == FALSE & GCC == 1) %>% 
      group_by(CARRERA, DESANIMADOS) %>% 
      summarise(TOTAL = survey_total())
   
   carrera.edad <- enoesvy %>% 
      filter(PROF == 1 & CARRERA != "999" & is.na(GRUPO_EDAD) == FALSE & GCC == 1) %>% 
      group_by(CARRERA, GRUPO_EDAD) %>% 
      summarise(TOTAL = survey_total())
   
   carrera.niv <- enoesvy %>% 
      filter(PROF == 1 & CARRERA != "999" & ESC %in% c("Carrera profesional", "Posgrado") & GCC == 1) %>% 
      group_by(CARRERA, ESC) %>% 
      summarise(TOTAL = survey_total())
   
   carrera.sin.ingreso <- enoesvy %>% 
      filter(PROF == 1 & CARRERA != "999" & is.na(ING_CERO) == FALSE & GCC == 1) %>% 
      group_by(CARRERA, ING_CERO) %>% 
      summarise(TOTAL = survey_total())
   
   carrera.posicion <- enoesvy %>% 
      filter(PROF == 1 & CARRERA != "999" & is.na(POSICION) == FALSE & GCC == 1) %>% 
      group_by(CARRERA, POSICION) %>% 
      summarise(TOTAL = survey_total())
   
   carrera.sector <- enoesvy %>% 
      filter(PROF == 1 & CARRERA != "999" & is.na(SECTOR) == FALSE & GCC == 1) %>% 
      group_by(CARRERA, SECTOR) %>% 
      summarise(TOTAL = survey_total())
   
   ## Ajustar formato
   carrera.tot <- select(carrera.tot, -TOTAL_se)
   
   carrera.sexo <- carrera.sexo %>% 
      select(-TOTAL_se) %>% 
      pivot_wider(names_from = SEXO,
                  values_from = TOTAL)
   
   carrera.actividad <- carrera.actividad %>% 
      select(-TOTAL_se) %>% 
      pivot_wider(names_from = ACTIVIDAD,
                  values_from = TOTAL)
   
   carrera.ocupacion <- carrera.ocupacion %>% 
      select(-TOTAL_se) %>% 
      pivot_wider(names_from = OCUPACION,
                  values_from = TOTAL)
   
   carrera.formalidad <- carrera.formalidad %>% 
      select(-TOTAL_se) %>% 
      pivot_wider(names_from = FORMALIDAD,
                  values_from = TOTAL)
   
   carrera.desa <- carrera.desa %>% 
      select(-TOTAL_se) %>% 
      pivot_wider(names_from = DESANIMADOS,
                  values_from = TOTAL)
   
   carrera.edad <- carrera.edad %>% 
      select(-TOTAL_se) %>% 
      pivot_wider(names_from = GRUPO_EDAD,
                  values_from = TOTAL)
   
   carrera.niv <- carrera.niv %>% 
      select(-TOTAL_se) %>% 
      pivot_wider(names_from = ESC,
                  values_from = TOTAL)
   
   carrera.sin.ingreso <- carrera.sin.ingreso %>% 
      select(-TOTAL_se) %>% 
      pivot_wider(names_from = ING_CERO,
                  values_from = TOTAL)
   
   carrera.posicion <- carrera.posicion %>% 
      select(-TOTAL_se) %>% 
      pivot_wider(names_from = POSICION,
                  values_from = TOTAL)
   
   carrera.sector <- carrera.sector %>% 
      select(-TOTAL_se) %>% 
      pivot_wider(names_from = SECTOR,
                  values_from = TOTAL)
   
   totales <- list(carrera.tot, carrera.sexo, carrera.actividad, carrera.ocupacion, carrera.formalidad, 
                   carrera.desa, carrera.edad, carrera.niv, carrera.sin.ingreso, carrera.posicion, carrera.sector)
   
   ## unir 
   UnirDfs <- function(x, y){
      merge(x, y, by = "CARRERA", all = TRUE)
   }
   
   totales <- Reduce(UnirDfs, totales)

# Detectar y marcar Salarios atipicos por carrera
   
## Calcular por carrera los percentiles 80,85,90:99 y el salario promedio
  
   quart <- enoesvy %>% 
      filter(PROF == 1 & GCC_SAL == 1 & is.na(INGRESO) == FALSE) %>% 
      group_by(CARRERA) %>% 
      summarise(INGRESO = survey_quantile(INGRESO, 
                                          c(0.80, 0.85, 0.90, 0.91, 0.92, 0.93, 0.94, 0.95, 0.96, 0.97, 0.98, 0.99, 1)))
   
   media <- enoesvy %>% 
      filter(PROF == 1 & GCC_SAL == 1 & is.na(INGRESO) == FALSE) %>% 
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
      
   ## Marcar como atipicos en la enoe a los valores iguales o mayores al valor del percentil outlier de cada carrera
   atipicos <- data.frame(CARRERA = quart$CARRERA,
                          PROF = 1,
                          VALOR_ING_ATIP = valores_atipicos)
   enoe <- left_join(enoe,
                     atipicos,
                     by = c("PROF", "CARRERA"))
   
   enoe <- enoe %>% 
      mutate(INGRESO = ifelse((is.na(INGRESO) == FALSE & is.na(VALOR_ING_ATIP)) | INGRESO <= VALOR_ING_ATIP,
                                INGRESO,
                                NA))
   
   # Calcular salarios por carrera
   
   ## Asignar nuevo diseno de la encuesta a la enoe con etiquetas de salarios atipicos
   
   enoesvy <- as_survey_design(enoe, ids = UPM, strata = EST_D, nest = TRUE, weights = FAC)
   
   ## Calcular salario promedio por grupo, excluyendo salarios atipicos
   # Calcular salarios por carrera
   carrera.sal <- enoesvy %>% 
      filter(PROF == 1 & is.na(CARRERA) == FALSE & CARRERA != "999" & is.na(INGRESO) == FALSE & GCC_SAL == 1) %>% 
      group_by(CARRERA) %>% 
      summarise(INGRESO = survey_mean(INGRESO))
   
   carrera.sal.sexo <- enoesvy %>% 
      filter(PROF == 1 & is.na(CARRERA) == FALSE & CARRERA != "999" & is.na(INGRESO) == FALSE & GCC_SAL == 1) %>% 
      group_by(CARRERA, SEXO) %>% 
      summarise(INGRESO = survey_mean(INGRESO))
   
   carrera.sal.form <- enoesvy %>% 
      filter(PROF == 1 & is.na(CARRERA) == FALSE & CARRERA != "999" & is.na(INGRESO) == FALSE & is.na(FORMALIDAD) == FALSE  & GCC_SAL == 1) %>% 
      group_by(CARRERA, FORMALIDAD) %>% 
      summarise(INGRESO = survey_mean(INGRESO))
   
   carrera.sal.niv <- enoesvy %>% 
      filter(PROF == 1 & is.na(CARRERA) == FALSE & CARRERA != "999" & is.na(INGRESO) == FALSE & ESC %in% c("Carrera profesional", "Posgrado")  & GCC_SAL == 1) %>% 
      group_by(CARRERA, ESC) %>% 
      summarise(INGRESO = survey_mean(INGRESO))
   
   carrera.sal.edad <- enoesvy %>% 
      filter(PROF == 1 & is.na(CARRERA) == FALSE & CARRERA != "999" & is.na(INGRESO) == FALSE & is.na(GRUPO_EDAD) == FALSE  & GCC_SAL == 1) %>% 
      group_by(CARRERA, GRUPO_EDAD) %>% 
      summarise(INGRESO = survey_mean(INGRESO))
   
   carrera.sal.quin <- enoesvy %>% 
      filter(PROF == 1 & is.na(CARRERA) == FALSE & CARRERA != "999" & is.na(INGRESO) == FALSE  & GCC_SAL == 1) %>% 
      group_by(CARRERA) %>% 
      summarise(INGRESO = survey_quantile(INGRESO, 
                                          c(0.25, 0.5, 0.75, 1)))
   
   ## dar formato 
   carrera.sal <- carrera.sal %>% 
      select(-INGRESO_se)
   
   carrera.sal.sexo <- carrera.sal.sexo %>%
      select(-INGRESO_se) %>% 
      pivot_wider(names_from = SEXO,
                  names_prefix = "INGRESO_",
                  values_from = INGRESO)
   
   carrera.sal.form <- carrera.sal.form %>% 
      select(-INGRESO_se) %>%
      pivot_wider(names_from = FORMALIDAD,
                  names_prefix = "INGRESO_",
                  values_from = INGRESO)
   
   carrera.sal.niv <- carrera.sal.niv %>% 
      select(-INGRESO_se) %>%
      pivot_wider(names_from = ESC,
                  names_prefix = "INGRESO_",
                  values_from = INGRESO)
   
   carrera.sal.edad <- carrera.sal.edad %>% 
      select(-INGRESO_se) %>%
      pivot_wider(names_from = GRUPO_EDAD,
                  names_prefix = "INGRESO_",
                  values_from = INGRESO)
   
   carrera.sal.quin <- carrera.sal.quin %>% 
      select(-matches("se$")) 
   
   salarios <- list(carrera.sal, carrera.sal.sexo, carrera.sal.form, carrera.sal.niv, carrera.sal.edad, carrera.sal.quin)

   ## unir en una sola dataframe
   salarios <- Reduce(UnirDfs, salarios)
   salarios <- salarios %>% mutate_if(is.numeric, round, digits = 0)
   
   totales.carrera <- left_join(totales,
                                salarios,
                                by = "CARRERA")
   
   ### Dar formato 
   colnames(totales.carrera) <- toupper(colnames(totales.carrera))
   
   totales.carrera <- totales.carrera %>% 
      mutate(TASA_DESOCUPACION = round((DESOCUPADO / ACTIVA), 3), 
             TASA_INFORMALIDAD = round((INFORMAL / OCUPADO) , 3),
             TASA_RIESGO = round((DESOCUPADO + INFORMAL + DESANIMADO) / TOTAL, 3)) 
   
   totales.carrera <- left_join(totales.carrera,
                                select(clas_car, c.det.cve, c.det.nom),
                                by = c("CARRERA" = "c.det.cve"))
   
   totales.carrera <- totales.carrera %>% 
      rename(CARRERA_NOMBRE = c.det.nom,
             CARRERA_CLAVE = CARRERA) %>% 
      mutate(NIVEL = "Profesional",
             CARRERA_NOMBRE = str_to_sentence(CARRERA_NOMBRE)) %>% 
      relocate(where(is.numeric), .after = where(is.character)) 
      
   
   cat("Calculos por carrera completados\n")

   return(totales.carrera)
}
