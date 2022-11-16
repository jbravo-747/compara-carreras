PromediarPeriodos <- function(list_res, periodo_enoe, periodo_costos, factor_inflacion, guardar){
   
   # unir los resultados de todos los periodos
   
   ## crear dataframes para guardar los resultados de cada tipo de analisis
   niveles <- data.frame()
   areas <- data.frame()
   subareas <- data.frame()
   carreras <- data.frame()
   tsu <- data.frame()
   
   ## unir los resultados de todos los peridos
   for(r in 1:length(list_res)){
      
      niveles <- bind_rows(niveles, list_res[[r]][[1]])
      
      areas <- bind_rows(areas, list_res[[r]][[2]])
      
      subareas <- bind_rows(subareas, list_res[[r]][[3]])
      
      carreras <- bind_rows(carreras, list_res[[r]][[4]])
      
      tsu <- bind_rows(tsu, list_res[[r]][[5]])
      
   }
   
   # ajustar el formato de cada tipo de resultados
   ## niveles
   orden.nivel.estudio <- c("Posgrado", "Carrera profesional", "Carrera tecnica",  "Bachillerato", 
                            "Secundaria", "Primaria", "Ninguno", "Profesional", "Todos")
   
   niveles <- niveles %>%
      select(-c("RSI_PUB", "RSI_PRI", "TASA_DESOCUPACION", "TASA_INFORMALIDAD")) %>% 
      group_by(ESC) %>% 
      summarise_all(mean, na.rm = FALSE) %>% 
      mutate(across(where(is.numeric),
                    round, digits = 0)) %>% 
      mutate(TASA_DESOCUPACION = round(DESOCUPADO / (OCUPADO + DESOCUPADO), 4),
             TASA_INFORMALIDAD = round(INFORMAL / (FORMAL + INFORMAL), 4)) %>%
      slice(match(orden.nivel.estudio, ESC))
   
   ## areas 
   areas <- areas %>%
      select(-c("RSI_PUB", "RSI_PRI", "TASA_DESOCUPACION", "TASA_INFORMALIDAD", "TASA_RIESGO", "COSTO_TOTAL_PRIVADA",  
                "COSTO_TOTAL_PUBLICA", "CI_PRI", "CI_PUB")) %>% 
      group_by(AREA_CLAVE, AREA_NOMBRE) %>% 
      summarise_all(mean, na.rm = FALSE) %>% 
      mutate(across(where(is.numeric),
                    round, digits = 0)) %>% 
      mutate(TASA_DESOCUPACION = round(DESOCUPADO / (OCUPADO + DESOCUPADO), 4),
             TASA_INFORMALIDAD = round(INFORMAL / (FORMAL + INFORMAL), 4),
             TASA_RIESGO = round((DESOCUPADO + INFORMAL + DESANIMADO) / TOTAL, 4))
  
   ## subareas
   subareas <- subareas %>%
      select(-c("RSI_PUB", "RSI_PRI", "TASA_DESOCUPACION", "TASA_INFORMALIDAD", "TASA_RIESGO", "COSTO_TOTAL_PRIVADA",  
                "COSTO_TOTAL_PUBLICA", "CI_PRI", "CI_PUB")) %>% 
      group_by(SUBAREA_CLAVE, SUBAREA_NOMBRE) %>% 
      summarise_all(mean, na.rm = FALSE) %>% 
      mutate(across(where(is.numeric),
                    round, digits = 0)) %>% 
      mutate(TASA_DESOCUPACION = round(DESOCUPADO / (OCUPADO + DESOCUPADO), 4),
             TASA_INFORMALIDAD = round(INFORMAL / (FORMAL + INFORMAL), 4),
             TASA_RIESGO = round((DESOCUPADO + INFORMAL + DESANIMADO) / TOTAL, 4))
   
   ## carreras
   carreras <- carreras %>%
      select(-c("RSI_PUB", "RSI_PRI", "TASA_DESOCUPACION", "TASA_INFORMALIDAD", "TASA_RIESGO", "COSTO_TOTAL_PRIVADA",  
                "COSTO_TOTAL_PUBLICA", "CI_PRI", "CI_PUB")) %>% 
      group_by(CARRERA_CLAVE, CARRERA_NOMBRE, NIVEL) %>% 
      summarise_all(mean, na.rm = FALSE) %>% 
      mutate(across(where(is.numeric),
                    round, digits = 0)) %>% 
      mutate(TASA_DESOCUPACION = round(DESOCUPADO / (OCUPADO + DESOCUPADO), 4),
             TASA_INFORMALIDAD = round(INFORMAL / (FORMAL + INFORMAL), 4),
             TASA_RIESGO = round((DESOCUPADO + INFORMAL + DESANIMADO) / TOTAL, 4))
   
   ## tsu
   tsu <- tsu %>%
      select(-c("RSI_PUB", "RSI_PRI", "TASA_DESOCUPACION", "TASA_INFORMALIDAD", "TASA_RIESGO", "COSTO_TOTAL_PRIVADA",  
                "COSTO_TOTAL_PUBLICA", "CI_PRI", "CI_PUB")) %>% 
      group_by(CARRERA_CLAVE, CARRERA_NOMBRE, NIVEL) %>% 
      summarise_all(mean, na.rm = FALSE) %>% 
      mutate(across(where(is.numeric),
                    round, digits = 0)) %>% 
      mutate(TASA_DESOCUPACION = round(DESOCUPADO / (OCUPADO + DESOCUPADO), 4),
             TASA_INFORMALIDAD = round(INFORMAL / (FORMAL + INFORMAL), 4),
             TASA_RIESGO = round((DESOCUPADO + INFORMAL + DESANIMADO) / TOTAL, 4))
   
   # guardar resultados
   
   resultados <- list(niveles, areas, subareas, carreras, tsu)
   names(resultados) <- c("niveles", "areas", "subareas", "carreras", "tsu")
   
   ##realizar calculos de RSI
   if(is.na(periodo_costos) == FALSE){
      
      res <- CalcularRSIeIC(periodo_costos, resultados, factor_inflacion, guardar)

      resultados[[1]] <- res$niveles
      resultados[[2]] <- res$areas
      resultados[[3]] <- res$subareas
      resultados[[4]] <- res$carreras
      resultados[[5]] <- res$tsu
      
   }
   
   cat("Resultados promediados \n")
   
    return(resultados)
   
}