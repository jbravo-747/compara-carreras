# La funcion CreateSalariosAreas realiza los calculos salariales y de indicadores del mercado laboral, por area 

CalcularXAreas <- function(enoesvy){
  
# Calcular estimadores por area
  area.tot <- enoesvy %>% 
    filter(PROF == 1 & AREA != "9") %>% 
    group_by(AREA) %>% 
    summarise(TOTAL = survey_total())
  
  area.sexo <- enoesvy %>% 
    filter(PROF == 1 & AREA != "9") %>% 
    group_by(AREA, SEXO) %>% 
    summarise(TOTAL = survey_total())
  
  area.actividad <- enoesvy %>% 
    filter(PROF == 1 & AREA != "9" & is.na(ACTIVIDAD) == FALSE) %>% 
    group_by(AREA, ACTIVIDAD) %>% 
    summarise(TOTAL = survey_total())
  
  area.ocupacion <- enoesvy %>% 
    filter(PROF == 1 & AREA != "9" & is.na(OCUPACION) == FALSE) %>% 
    group_by(AREA, OCUPACION) %>% 
    summarise(TOTAL = survey_total())
  
  area.formalidad <- enoesvy %>% 
    filter(PROF == 1 & AREA != "9" & is.na(FORMALIDAD) == FALSE) %>% 
    group_by(AREA, FORMALIDAD) %>% 
    summarise(TOTAL = survey_total())
  
  area.desa <- enoesvy %>% 
    filter(PROF == 1 & AREA != "9" & is.na(DESANIMADOS) == FALSE) %>% 
    group_by(AREA, DESANIMADOS) %>% 
    summarise(TOTAL = survey_total())
    
  area.edad <- enoesvy %>% 
    filter(PROF == 1 & AREA != "9" & is.na(GRUPO_EDAD) == FALSE) %>% 
    group_by(AREA, GRUPO_EDAD) %>% 
    summarise(TOTAL = survey_total())
  
  area.niv <- enoesvy %>% 
    filter(PROF == 1 & AREA != "9" & ESC %in% c("Carrera profesional", "Posgrado")) %>% 
    group_by(AREA, ESC) %>% 
    summarise(TOTAL = survey_total())
  
  area.sin.ingreso <- enoesvy %>% 
    filter(PROF == 1 & AREA != "9" & is.na(ING_CERO) == FALSE) %>% 
    group_by(AREA, ING_CERO) %>% 
    summarise(TOTAL = survey_total())
  
  area.posicion <- enoesvy %>% 
    filter(PROF == 1 & AREA != "9" & is.na(POSICION) == FALSE) %>% 
    group_by(AREA, POSICION) %>% 
    summarise(TOTAL = survey_total())
  
  area.sector <- enoesvy %>% 
    filter(PROF == 1 & AREA != "9" & is.na(SECTOR) == FALSE) %>% 
    group_by(AREA, SECTOR) %>% 
    summarise(TOTAL = survey_total())
  
## Ajustar formato
  area.tot <- select(area.tot, -TOTAL_se)
  
  area.sexo <- area.sexo %>% 
    select(-TOTAL_se) %>% 
    pivot_wider(names_from = SEXO,
                values_from = TOTAL)
  
  area.actividad <- area.actividad %>% 
    select(-TOTAL_se) %>% 
    pivot_wider(names_from = ACTIVIDAD,
                values_from = TOTAL)
  
  area.ocupacion <- area.ocupacion %>% 
    select(-TOTAL_se) %>% 
    pivot_wider(names_from = OCUPACION,
                values_from = TOTAL)
  
  area.formalidad <- area.formalidad %>% 
    select(-TOTAL_se) %>% 
    pivot_wider(names_from = FORMALIDAD,
                values_from = TOTAL)
  
  area.desa <- area.desa %>% 
    select(-TOTAL_se) %>% 
    pivot_wider(names_from = DESANIMADOS,
                values_from = TOTAL)
  
  area.edad <- area.edad %>% 
    select(-TOTAL_se) %>% 
    pivot_wider(names_from = GRUPO_EDAD,
                values_from = TOTAL)
  
  area.niv <- area.niv %>% 
    select(-TOTAL_se) %>% 
    pivot_wider(names_from = ESC,
                values_from = TOTAL)
  
  area.sin.ingreso <- area.sin.ingreso %>% 
    select(-TOTAL_se) %>% 
    pivot_wider(names_from = ING_CERO,
                values_from = TOTAL)
  
  area.posicion <- area.posicion %>% 
    select(-TOTAL_se) %>% 
    pivot_wider(names_from = POSICION,
                values_from = TOTAL)
  
  area.sector <- area.sector %>% 
    select(-TOTAL_se) %>% 
    pivot_wider(names_from = SECTOR,
                values_from = TOTAL)
  
  totales <- list(area.tot, area.sexo, area.actividad, area.ocupacion, area.formalidad, 
                  area.desa, area.edad, area.niv, area.sin.ingreso, area.posicion, area.sector)
  
## unir 
  UnirDfs <- function(x, y){
     merge(x, y, by = "AREA", all = TRUE)
  }
  
  totales <- Reduce(UnirDfs, totales)
  
# Calcular salarios por area
  area.sal <- enoesvy %>% 
    filter(PROF == 1 & is.na(AREA) == FALSE & AREA != "9" & is.na(INGRESO) == FALSE) %>% 
    group_by(AREA) %>% 
    summarise(INGRESO = survey_mean(INGRESO))
    
  area.sal.sexo <- enoesvy %>% 
    filter(PROF == 1 & is.na(AREA) == FALSE & AREA != "9" & is.na(INGRESO) == FALSE) %>% 
    group_by(AREA, SEXO) %>% 
    summarise(INGRESO = survey_mean(INGRESO))
  
  area.sal.form <- enoesvy %>% 
    filter(PROF == 1 & is.na(AREA) == FALSE & AREA != "9" & is.na(INGRESO) == FALSE & is.na(FORMALIDAD) == FALSE) %>% 
    group_by(AREA, FORMALIDAD) %>% 
    summarise(INGRESO = survey_mean(INGRESO))
  
  area.sal.niv <- enoesvy %>% 
    filter(PROF == 1 & is.na(AREA) == FALSE & AREA != "9" & is.na(INGRESO) == FALSE & ESC %in% c("Carrera profesional", "Posgrado")) %>% 
    group_by(AREA, ESC) %>% 
    summarise(INGRESO = survey_mean(INGRESO))
  
  area.sal.edad <- enoesvy %>% 
    filter(PROF == 1 & is.na(AREA) == FALSE & AREA != "9" & is.na(INGRESO) == FALSE & is.na(GRUPO_EDAD) == FALSE) %>% 
    group_by(AREA, GRUPO_EDAD) %>% 
    summarise(INGRESO = survey_mean(INGRESO))
  
  area.sal.quin <- enoesvy %>% 
    filter(PROF == 1 & is.na(AREA) == FALSE & AREA != "9" & is.na(INGRESO) == FALSE) %>% 
    group_by(AREA) %>% 
    summarise(INGRESO = survey_quantile(INGRESO, 
                                        c(0.25, 0.5, 0.75, 1)))

## dar formato 
  area.sal <- area.sal %>% 
    select(-INGRESO_se)
  
  area.sal.sexo <- area.sal.sexo %>%
    select(-INGRESO_se) %>% 
    pivot_wider(names_from = SEXO,
                names_prefix = "INGRESO_",
                values_from = INGRESO)
  
  area.sal.form <- area.sal.form %>% 
    select(-INGRESO_se) %>%
    pivot_wider(names_from = FORMALIDAD,
                names_prefix = "INGRESO_",
                values_from = INGRESO)
  
  area.sal.niv <- area.sal.niv %>% 
    select(-INGRESO_se) %>%
    pivot_wider(names_from = ESC,
                names_prefix = "INGRESO_",
                values_from = INGRESO)
  
  area.sal.edad <- area.sal.edad %>% 
    select(-INGRESO_se) %>%
    pivot_wider(names_from = GRUPO_EDAD,
                names_prefix = "INGRESO_",
                values_from = INGRESO)
  
  area.sal.quin <- area.sal.quin %>% 
    select(-matches("se$")) 
    
  salarios <- list(area.sal, area.sal.sexo, area.sal.form, area.sal.niv, area.sal.edad, area.sal.quin)
  
## unir en una sola dataframe
 salarios <- Reduce(UnirDfs, salarios)

 totales.area <- left_join(totales,
                           salarios,
                           by = "AREA")
 
### Dar formato 
 colnames(totales.area) <- toupper(colnames(totales.area))
 
 totales.area <- totales.area %>% 
   mutate(TASA_DESOCUPACION = round((DESOCUPADO / ACTIVA), 4), 
          TASA_INFORMALIDAD = round((INFORMAL / OCUPADO) , 4),
          TASA_RIESGO = round((DESOCUPADO + INFORMAL + DESANIMADO) / TOTAL, 4)) 
 
### Redondar de resultados a dos digitos en columnas de ind
 totales.area <- totales.area %>% 
   mutate_at(vars(matches("^INGRESO")), 
             round, digits = 0)
 
# Recodificar los nombres de las areas
 totales.area$AREA_NOMBRE <- recode(totales.area$AREA,
                             "1" = "Educacion",
                             "2" = "Artes y humanidades",
                             "3" = "Ciencias sociales, administracion y derecho",
                             "4" = "Ciencias naturales, exactas y de la computacion",
                             "5" = "Ingenieria, manufactura y construccion",
                             "6" = "Agronomia y veterinaria",
                             "7" = "Salud",
                             "8" = "Servicios")
 
 totales.area <- rename(totales.area,
                        AREA_CLAVE = AREA)
  
  cat("Calculos por area completados\n")
  
  return(totales.area)
}
