#  La funcion CrearCalculosNiveles realiza los calculos poblacionales y de salario por nivel de estudio

CalcularXNiveles <- function(enoesvy){
   
# Calcular salarios por nivel de estudio, total y por sexo
   niv.sal <- enoesvy %>% 
      filter(is.na(ESC) == FALSE & is.na(INGRESO) == FALSE) %>% 
      group_by(ESC) %>% 
      summarise(INGRESO = survey_mean(INGRESO))
   
   niv.sal.sexo <- enoesvy %>% 
      filter(is.na(ESC) == FALSE & is.na(INGRESO) == FALSE) %>% 
      group_by(ESC, SEXO) %>% 
      summarise(INGRESO = survey_mean(INGRESO))

## Ajustar formato
   niv.sal <- select(niv.sal, -INGRESO_se)
   
   niv.sal.sexo <- niv.sal.sexo %>% 
      select(-INGRESO_se) %>% 
      pivot_wider(names_from = SEXO,
                  names_prefix = "INGRESO_",
                  values_from = INGRESO)
   
## Unir los resultados en un solo dataframe
   niv.sal <- list(niv.sal, niv.sal.sexo)
   
   UnirDfs <- function(x, y){
      merge(x, y, by = "ESC")
   }
   
   niv.sal <-  Reduce(UnirDfs, niv.sal)

# Calcular estimaciones de total de total de personas, formalidad, ocupacion, etc
   niv_tot <- enoesvy %>% 
      filter(is.na(ESC) == FALSE) %>% 
      group_by(ESC) %>% 
      summarise(TOTAL = survey_total())
   
   niv_sexo <- enoesvy %>% 
      filter(is.na(ESC) == FALSE) %>% 
      group_by(ESC, SEXO) %>% 
      summarise(TOTAL = survey_total())
   
   niv_acti <- enoesvy %>% 
      filter(is.na(ESC) == FALSE & is.na(ACTIVIDAD) == FALSE) %>% 
      group_by(ESC, ACTIVIDAD) %>% 
      summarise(TOTAL = survey_total())
   
   niv_ocup <- enoesvy %>% 
      filter(is.na(ESC) == FALSE & is.na(OCUPACION) == FALSE) %>% 
      group_by(ESC, OCUPACION) %>% 
      summarise(TOTAL = survey_total())
   
   niv_form <- enoesvy %>% 
      filter(is.na(ESC) == FALSE & is.na(FORMALIDAD) == FALSE) %>% 
      group_by(ESC, FORMALIDAD) %>% 
      summarise(TOTAL = survey_total())
   
## Dar formato
   niv_tot <- select(niv_tot, -TOTAL_se)
   
   niv_sexo <- pivot_wider(select(niv_sexo, -TOTAL_se),
                           names_from = SEXO,
                           values_from = TOTAL)
   
   niv_acti <- pivot_wider(select(niv_acti, -TOTAL_se),
                           names_from = ACTIVIDAD,
                           values_from = TOTAL)
   
   niv_ocup <- pivot_wider(select(niv_ocup, -TOTAL_se),
                           names_from = OCUPACION,
                           values_from = TOTAL)
   
   niv_form <- pivot_wider(select(niv_form, -TOTAL_se),
                           names_from = FORMALIDAD,
                           values_from = TOTAL)
   
   niv <- list(niv_tot, niv_sexo, niv_acti, niv_ocup, niv_form)
   
## Unir en una sola dataframe
   UnirDfs <- function(x, y){
      merge(x, y, by = "ESC")
   }
   
   niv <- Reduce(UnirDfs, niv)
   
   niv <- left_join(niv, niv.sal, by = "ESC")
   
# Calcular estimaciones para el total de la poblacion
   todos.sal <- enoesvy %>% 
      filter(is.na(ESC) == FALSE & is.na(INGRESO) == FALSE) %>% 
      summarise(INGRESO = survey_mean(INGRESO))
   
   todos.sal.sexo <- enoesvy %>% 
      filter(is.na(ESC) == FALSE & is.na(INGRESO) == FALSE) %>%
      group_by(SEXO) %>% 
      summarise(INGRESO = survey_mean(INGRESO))
   
   todos.tot <- enoesvy %>% 
      filter(is.na(ESC) == FALSE) %>%
      summarise(TOTAL = survey_total())
   
   todos.sexo <- enoesvy %>% 
      filter(is.na(ESC) == FALSE) %>%
      group_by(SEXO) %>% 
      summarise(TOTAL = survey_total())
   
   todos.acti <- enoesvy %>% 
      filter(is.na(ESC) == FALSE & is.na(ACTIVIDAD) == FALSE) %>%
      group_by(ACTIVIDAD) %>% 
      summarise(TOTAL = survey_total())
   
   todos.form <- enoesvy %>% 
      filter(is.na(ESC) == FALSE & is.na(FORMALIDAD) == FALSE) %>%
      group_by(FORMALIDAD) %>% 
      summarise(TOTAL = survey_total())
   
   todos.ocup <- enoesvy %>% 
      filter(is.na(ESC) == FALSE & is.na(OCUPACION) == FALSE) %>%
      group_by(OCUPACION) %>% 
      summarise(TOTAL = survey_total())
   
## dar formato
   todos.sal <- todos.sal %>% select(-INGRESO_se)
   todos.tot <- todos.tot %>% select(-TOTAL_se)
   
   todos.sal.sexo <- todos.sal.sexo %>% 
      select(-INGRESO_se) %>% 
      pivot_wider(names_from = SEXO,
                  names_prefix = "INGRESO_",
                  values_from = INGRESO)
   
   todos.sexo <- todos.sexo %>% 
      select(-TOTAL_se) %>% 
      pivot_wider(names_from = SEXO,
                  values_from = TOTAL)
   
   todos.acti <- todos.acti %>% 
      select(-TOTAL_se) %>% 
      pivot_wider(names_from = ACTIVIDAD,
                  values_from = TOTAL)
   
   todos.form <- todos.form %>% 
      select(-TOTAL_se) %>% 
      pivot_wider(names_from = FORMALIDAD,
                  values_from = TOTAL)
   
   todos.ocup <- todos.ocup %>% 
      select(-TOTAL_se) %>% 
      pivot_wider(names_from = OCUPACION,
                  values_from = TOTAL)
   
## unir
   todos <- data.frame(ESC = c("Todos"))
   todos <- bind_cols(todos, todos.tot, todos.sexo, todos.sal, todos.sal.sexo, todos.acti, todos.ocup, todos.form)

# Calcular estimaciones para el total de profesionistas
   
   prof.sal <- enoesvy %>% 
      filter(PROF == 1 & is.na(INGRESO) == FALSE) %>% 
      summarise(INGRESO = survey_mean(INGRESO))
   
   prof.sal.sexo <- enoesvy %>% 
      filter(PROF == 1 & is.na(INGRESO) == FALSE) %>%
      group_by(SEXO) %>% 
      summarise(INGRESO = survey_mean(INGRESO))
   
   prof.tot <- enoesvy %>% 
      filter(PROF == 1) %>%
      summarise(TOTAL = survey_total())
   
   prof.sexo <- enoesvy %>% 
      filter(PROF == 1) %>%
      group_by(SEXO) %>% 
      summarise(TOTAL = survey_total())
   
   prof.acti <- enoesvy %>% 
      filter(PROF == 1 & is.na(ACTIVIDAD) == FALSE) %>%
      group_by(ACTIVIDAD) %>% 
      summarise(TOTAL = survey_total())
   
   prof.form <- enoesvy %>% 
      filter(PROF == 1 & is.na(FORMALIDAD) == FALSE) %>%
      group_by(FORMALIDAD) %>% 
      summarise(TOTAL = survey_total())
   
   prof.ocup <- enoesvy %>% 
      filter(PROF == 1 & is.na(OCUPACION) == FALSE) %>%
      group_by(OCUPACION) %>% 
      summarise(TOTAL = survey_total())
   
## dar formato
   prof.sal <-  prof.sal %>% select(-INGRESO_se)
   prof.tot <- prof.tot %>% select(-TOTAL_se)
   
   prof.sal.sexo <- prof.sal.sexo %>% 
      select(-INGRESO_se) %>% 
      pivot_wider(names_from = SEXO,
                  names_prefix = "INGRESO_",
                  values_from = INGRESO)
   
   prof.sexo <- prof.sexo %>% 
      select(-TOTAL_se) %>% 
      pivot_wider(names_from = SEXO,
                  values_from = TOTAL)
   
   prof.acti <- prof.acti %>% 
      select(-TOTAL_se) %>% 
      pivot_wider(names_from = ACTIVIDAD,
                  values_from = TOTAL)
   
   prof.form <- prof.form %>% 
      select(-TOTAL_se) %>% 
      pivot_wider(names_from = FORMALIDAD,
                  values_from = TOTAL)
   
   prof.ocup <- prof.ocup %>% 
      select(-TOTAL_se) %>% 
      pivot_wider(names_from = OCUPACION,
                  values_from = TOTAL)
   
## unir
   prof <- data.frame(ESC = c("Profesional"))
   prof <- bind_cols(prof, prof.tot, prof.sexo, prof.sal, prof.sal.sexo, prof.acti, prof.ocup, prof.form) 
   
## unir con el resto de los resultados
   niv <- bind_rows(niv, prof, todos)
   
# Dar formato
   colnames(niv) <- toupper(colnames(niv))
   
   niv <- niv %>% mutate(TASA_DESOCUPACION = round(DESOCUPADO / (OCUPADO + DESOCUPADO), 4),
                         TASA_INFORMALIDAD = round(INFORMAL / (FORMAL + INFORMAL), 4))
   
   niv <- niv %>% mutate_at(c("INGRESO", "INGRESO_MUJER", "INGRESO_HOMBRE"), round, digits = 0)
   
   orden.nivel.estudio <- c("Posgrado", "Carrera profesional", "Carrera tecnica",  "Bachillerato", 
                            "Secundaria", "Primaria", "Ninguno", "Profesional", "Todos")
   niv <- niv %>% slice(match(orden.nivel.estudio, ESC))
   
   cat("Calculos por nivel completados\n")
   
   return(niv)
}
