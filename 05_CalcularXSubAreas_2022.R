# La funcion CreateSalariosAreas realiza los calculos salariales y de indicadores del mercado laboral, por subarea 

CalcularXSubareas <- function(enoesvy, clas_car){
  
# Calcular estimadores por subarea
  subarea.tot <- enoesvy %>% 
    filter(PROF == 1 & SUBAREA != "99") %>% 
    group_by(SUBAREA) %>% 
    summarise(TOTAL = survey_total())
  
  subarea.sexo <- enoesvy %>% 
    filter(PROF == 1 & SUBAREA != "99") %>% 
    group_by(SUBAREA, SEXO) %>% 
    summarise(TOTAL = survey_total())
  
  subarea.actividad <- enoesvy %>% 
    filter(PROF == 1 & SUBAREA != "99" & is.na(ACTIVIDAD) == FALSE) %>% 
    group_by(SUBAREA, ACTIVIDAD) %>% 
    summarise(TOTAL = survey_total())
  
  subarea.ocupacion <- enoesvy %>% 
    filter(PROF == 1 & SUBAREA != "99" & is.na(OCUPACION) == FALSE) %>% 
    group_by(SUBAREA, OCUPACION) %>% 
    summarise(TOTAL = survey_total())
  
  subarea.formalidad <- enoesvy %>% 
    filter(PROF == 1 & SUBAREA != "99" & is.na(FORMALIDAD) == FALSE) %>% 
    group_by(SUBAREA, FORMALIDAD) %>% 
    summarise(TOTAL = survey_total())
  
  subarea.desa <- enoesvy %>% 
    filter(PROF == 1 & SUBAREA != "99" & is.na(DESANIMADOS) == FALSE) %>% 
    group_by(SUBAREA, DESANIMADOS) %>% 
    summarise(TOTAL = survey_total())
  
  subarea.edad <- enoesvy %>% 
    filter(PROF == 1 & SUBAREA != "99" & is.na(GRUPO_EDAD) == FALSE) %>% 
    group_by(SUBAREA, GRUPO_EDAD) %>% 
    summarise(TOTAL = survey_total())
  
  subarea.niv <- enoesvy %>% 
    filter(PROF == 1 & SUBAREA != "99" & ESC %in% c("Carrera profesional", "Posgrado")) %>% 
    group_by(SUBAREA, ESC) %>% 
    summarise(TOTAL = survey_total())
  
  subarea.sin.ingreso <- enoesvy %>% 
    filter(PROF == 1 & SUBAREA != "99" & is.na(ING_CERO) == FALSE) %>% 
    group_by(SUBAREA, ING_CERO) %>% 
    summarise(TOTAL = survey_total())
  
  subarea.posicion <- enoesvy %>% 
    filter(PROF == 1 & SUBAREA != "99" & is.na(POSICION) == FALSE) %>% 
    group_by(SUBAREA, POSICION) %>% 
    summarise(TOTAL = survey_total())
  
  subarea.sector <- enoesvy %>% 
    filter(PROF == 1 & SUBAREA != "99" & is.na(SECTOR) == FALSE) %>% 
    group_by(SUBAREA, SECTOR) %>% 
    summarise(TOTAL = survey_total())
  
## Ajustar formato
  subarea.tot <- select(subarea.tot, -TOTAL_se)
  
  subarea.sexo <- subarea.sexo %>% 
    select(-TOTAL_se) %>% 
    pivot_wider(names_from = SEXO,
                values_from = TOTAL)
  
  subarea.actividad <- subarea.actividad %>% 
    select(-TOTAL_se) %>% 
    pivot_wider(names_from = ACTIVIDAD,
                values_from = TOTAL)
  
  subarea.ocupacion <- subarea.ocupacion %>% 
    select(-TOTAL_se) %>% 
    pivot_wider(names_from = OCUPACION,
                values_from = TOTAL)
  
  subarea.formalidad <- subarea.formalidad %>% 
    select(-TOTAL_se) %>% 
    pivot_wider(names_from = FORMALIDAD,
                values_from = TOTAL)
  
  subarea.desa <- subarea.desa %>% 
    select(-TOTAL_se) %>% 
    pivot_wider(names_from = DESANIMADOS,
                values_from = TOTAL)
  
  subarea.edad <- subarea.edad %>% 
    select(-TOTAL_se) %>% 
    pivot_wider(names_from = GRUPO_EDAD,
                values_from = TOTAL)
  
  subarea.niv <- subarea.niv %>% 
    select(-TOTAL_se) %>% 
    pivot_wider(names_from = ESC,
                values_from = TOTAL)
  
  subarea.sin.ingreso <- subarea.sin.ingreso %>% 
    select(-TOTAL_se) %>% 
    pivot_wider(names_from = ING_CERO,
                values_from = TOTAL)
  
  subarea.posicion <- subarea.posicion %>% 
    select(-TOTAL_se) %>% 
    pivot_wider(names_from = POSICION,
                values_from = TOTAL)
  
  subarea.sector <- subarea.sector %>% 
    select(-TOTAL_se) %>% 
    pivot_wider(names_from = SECTOR,
                values_from = TOTAL)
  
  totales <- list(subarea.tot, subarea.sexo, subarea.actividad, subarea.ocupacion, subarea.formalidad, 
                  subarea.desa, subarea.edad, subarea.niv, subarea.sin.ingreso, subarea.posicion, subarea.sector)
  
## unir 
  UnirDfs <- function(x, y){
    merge(x, y, by = "SUBAREA", all = TRUE)
  }
  
  totales <- Reduce(UnirDfs, totales)
  
# Calcular salarios por subarea
  subarea.sal <- enoesvy %>% 
    filter(PROF == 1 & is.na(SUBAREA) == FALSE & SUBAREA != "99" & is.na(INGRESO) == FALSE) %>% 
    group_by(SUBAREA) %>% 
    summarise(INGRESO = survey_mean(INGRESO))
  
  subarea.sal.sexo <- enoesvy %>% 
    filter(PROF == 1 & is.na(SUBAREA) == FALSE & SUBAREA != "99" & is.na(INGRESO) == FALSE) %>% 
    group_by(SUBAREA, SEXO) %>% 
    summarise(INGRESO = survey_mean(INGRESO))
  
  subarea.sal.form <- enoesvy %>% 
    filter(PROF == 1 & is.na(SUBAREA) == FALSE & SUBAREA != "99" & is.na(INGRESO) == FALSE & is.na(FORMALIDAD) == FALSE) %>% 
    group_by(SUBAREA, FORMALIDAD) %>% 
    summarise(INGRESO = survey_mean(INGRESO))
  
  subarea.sal.niv <- enoesvy %>% 
    filter(PROF == 1 & is.na(SUBAREA) == FALSE & SUBAREA != "99" & is.na(INGRESO) == FALSE & ESC %in% c("Carrera profesional", "Posgrado")) %>% 
    group_by(SUBAREA, ESC) %>% 
    summarise(INGRESO = survey_mean(INGRESO))
  
  subarea.sal.edad <- enoesvy %>% 
    filter(PROF == 1 & is.na(SUBAREA) == FALSE & SUBAREA != "99" & is.na(INGRESO) == FALSE & is.na(GRUPO_EDAD) == FALSE) %>% 
    group_by(SUBAREA, GRUPO_EDAD) %>% 
    summarise(INGRESO = survey_mean(INGRESO))
  
  subarea.sal.quin <- enoesvy %>% 
    filter(PROF == 1 & is.na(SUBAREA) == FALSE & SUBAREA != "99" & is.na(INGRESO) == FALSE) %>% 
    group_by(SUBAREA) %>% 
    summarise(INGRESO = survey_quantile(INGRESO, 
                                        c(0.25, 0.5, 0.75, 1)))
  
## dar formato 
  subarea.sal <- subarea.sal %>% 
    select(-INGRESO_se)
  
  subarea.sal.sexo <- subarea.sal.sexo %>%
    select(-INGRESO_se) %>% 
    pivot_wider(names_from = SEXO,
                names_prefix = "INGRESO_",
                values_from = INGRESO)
  
  subarea.sal.form <- subarea.sal.form %>% 
    select(-INGRESO_se) %>%
    pivot_wider(names_from = FORMALIDAD,
                names_prefix = "INGRESO_",
                values_from = INGRESO)
  
  subarea.sal.niv <- subarea.sal.niv %>% 
    select(-INGRESO_se) %>%
    pivot_wider(names_from = ESC,
                names_prefix = "INGRESO_",
                values_from = INGRESO)
  
  subarea.sal.edad <- subarea.sal.edad %>% 
    select(-INGRESO_se) %>%
    pivot_wider(names_from = GRUPO_EDAD,
                names_prefix = "INGRESO_",
                values_from = INGRESO)
  
  subarea.sal.quin <- subarea.sal.quin %>% 
    select(-matches("se$")) 
  
  salarios <- list(subarea.sal, subarea.sal.sexo, subarea.sal.form, subarea.sal.niv, subarea.sal.edad, subarea.sal.quin)
  
  ## unir en una sola dataframe
  salarios <- Reduce(UnirDfs, salarios)
  
  totales.subarea <- left_join(totales,
                            salarios,
                            by = "SUBAREA")
  
  ### Dar formato 
  colnames(totales.subarea) <- toupper(colnames(totales.subarea))
  
  totales.subarea <- totales.subarea %>% 
    mutate(TASA_DESOCUPACION = round((DESOCUPADO / ACTIVA), 4), 
           TASA_INFORMALIDAD = round((INFORMAL / OCUPADO) , 4),
           TASA_RIESGO = round((DESOCUPADO + INFORMAL + DESANIMADO) / TOTAL, 4)) 
  
  ### Redondar de resultados a dos digitos en columnas de ind
  totales.subarea <- totales.subarea %>% 
    mutate_at(vars(matches("^INGRESO")), 
              round, digits = 0)
  
  ### Unir con nombres de las subareas
  totales.subarea <- left_join(totales.subarea,
                               unique(select(clas_car, c.esp.cve, c.esp.nom)),
                               by = c("SUBAREA" = "c.esp.cve"))
  
  totales.subarea <- totales.subarea %>% 
    rename(SUBAREA_CLAVE = SUBAREA,
           SUBAREA_NOMBRE = c.esp.nom) %>% 
    relocate(where(is.numeric), .after = where(is.character))
  
  cat("Calculos por subarea completados\n")
  
  return(totales.subarea)
}
