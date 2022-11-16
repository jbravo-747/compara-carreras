#  La funcion CalcularRSIeIC calcula el retorno sobre la inversion y el indice de calidad de inversion por grupo de sostenimiento de la universidad
#  Toma como argumento los resultados de las funciones anterior, el csv de costos_universidades, el costo de utiles para universidades privadas y publicas que proviene de la enigh, 
#  y un valor para ajustar ese costo por inflacion (ej. 1.141 para periodo 116)

CalcularRSIeIC <- function(periodo_costos, 
                           resultados,  
                           factor_inflacion,
                           guardar){
   
   options(dplyr.summarise.inform = FALSE)
   
# importar archivo con costos por carrera y universidad, 
   
   costos_universidades <- read_excel(paste0("01_fuentes/costos/base_costos_por_universidad_", periodo_costos,".xlsx"), 
                                      col_types = c("text", "text", "text", 
                                                    "text", "text", "text", "text", "text", 
                                                    "text", "text", "numeric", "numeric", 
                                                    "numeric", "numeric", "numeric", 
                                                    "numeric"))
   
   uni <- costos_universidades
   
# procesar archivo
   colnames(uni) <- str_replace_all(colnames(uni), " ", "_")
   
   costos <- uni %>%
      rename(Clave = Clave_de_area_de_la_carrera) %>% 
      mutate(Duracion = NA,
             Duracion = ifelse(Tipo_de_periodo_de_estudios == "Anual",
                               Numero_de_periodos_en_la_carrera,
                               Duracion),
             Duracion = ifelse(Tipo_de_periodo_de_estudios == "Semestre",
                               Numero_de_periodos_en_la_carrera / 2,
                               Duracion),
             Duracion = ifelse(Tipo_de_periodo_de_estudios == "Tetramestre",
                               Numero_de_periodos_en_la_carrera / 2,
                               Duracion),
             Duracion = ifelse(Tipo_de_periodo_de_estudios == "Cuatrimestre",
                               Numero_de_periodos_en_la_carrera / 3,
                               Duracion),
             Duracion = ifelse(Tipo_de_periodo_de_estudios == "Trimestre",
                               Numero_de_periodos_en_la_carrera / 4,
                               Duracion),
             Duracion = ifelse(Tipo_de_periodo_de_estudios == "Bimestre",
                               Numero_de_periodos_en_la_carrera / 6,
                               Duracion),
             Duracion = round(Duracion, 1)) %>% 
      group_by(Nivel, Sostenimiento, Clave) %>% 
      summarise(Duracion_anios = round(mean(Duracion, na.rm = TRUE), 2),
                Costo = round(mean(Total_de_la_carrera, na.rm = TRUE), 1)) %>% 
      ungroup()
   
   colnames(costos) <- toupper(colnames(costos))
   
# escribir archivo con costos promedio por carrera y sostenimiento
    
   if(guardar == "Si"){
   write.csv(costos, 
             paste0("03_resultados//costos_universidades_", periodo_costos, ".csv"))
   }
   
# calcular costos de utiles de enight
   
   CalcularCostosUtiles <- function(){
      
      # importar microdatos de la enigh
      gas_per <- readRDS("01_fuentes\\enigh\\gastospersona.RDS")
      pob <-     readRDS("01_fuentes\\enigh\\poblacion.RDS")
      viv <-     readRDS("01_fuentes\\enigh\\viviendas.RDS")
      
      # Crear ids para unir tablas
      gas_per$IDp <- paste(gas_per$folioviv, gas_per$foliohog, gas_per$numren, sep = "/")
      pob$IDp <- paste(pob$folioviv, pob$foliohog, pob$numren, sep = "/")
      
      viv <- viv %>% select(folioviv, est_dis, upm, factor)
      pob <- filter(pob, nivel == 10 | nivel == 11 | nivel == 12) %>% select(folioviv, IDp, nivel, tipoesc)
      gas_per <- gas_per %>% filter(is.na(material) == FALSE & material > 0) %>% select(IDp, material)
      
      pob <- left_join(pob, viv, by = "folioviv")
      pob <- left_join(pob, gas_per, by = "IDp")
      
      pob$niv <- NA
      pob$sos <- NA
      
      pob$niv <- replace(pob$niv, pob$nivel == 10, "Carrera tecnica")
      pob$niv <- replace(pob$niv, pob$nivel == 11 | pob$nivel == 12, "Carrera profesional")
      
      pob$sos <- replace(pob$sos, pob$tipoesc == 1, "pub")
      pob$sos <- replace(pob$sos, pob$tipoesc == 2, "pri")
      
      pob <- pob %>% filter(is.na(material) == FALSE)
      
      # establcer diseño de encuesta
      enighsvy <- as_survey_design(pob, ids = IDp, weights = factor)
      
      costos_lice <- enighsvy %>% 
         filter(niv == "Carrera profesional" & is.na(sos) == FALSE & is.na(material) == FALSE) %>% 
         group_by(sos) %>% 
         summarise(COSTO = survey_mean(material))
      
      costos_tsu <- enighsvy %>% 
         filter(niv == "Carrera tecnica" & is.na(sos) == FALSE & is.na(material) == FALSE) %>% 
         group_by(sos) %>% 
         summarise(COSTO = survey_mean(material))
      
      costos <- list(costos_lice, costos_tsu)
      names(costos) <- c("costos_lice", "costos_tsu")
      
      return(costos)
      
   }
   
costos_utiles <- CalcularCostosUtiles()   
      
# Calcular promedio de costos por nivel, area, subarea
   
   costos$AREA <- substring(costos$CLAVE, 1, 1)
   costos$SUBAREA <- substring(costos$CLAVE, 1, 2)
   
## por nivel
   
   costos_nivel <- costos %>% 
      group_by(NIVEL, SOSTENIMIENTO) %>% 
      summarise(DURACION_ANIOS = round(mean(DURACION_ANIOS, 
                                            na.rm = TRUE), 
                                       1),
                COSTO = round(mean(COSTO, 
                                   na.rm = TRUE), 
                              0))  %>% 
      ungroup()
   
## por area

   costos_area <- costos %>% 
      group_by(AREA, SOSTENIMIENTO) %>% 
      summarise(DURACION_ANIOS = round(mean(DURACION_ANIOS, 
                                            na.rm = TRUE), 
                                       1),
                COSTO = round(mean(COSTO, 
                                   na.rm = TRUE), 
                              0))  %>% 
      ungroup()
   
## por subarea
   
   costos_subarea <- costos %>% 
      group_by(SUBAREA, SOSTENIMIENTO) %>% 
      summarise(DURACION_ANIOS = round(mean(DURACION_ANIOS, 
                                            na.rm = TRUE), 
                                       1),
                COSTO = round(mean(COSTO, 
                                   na.rm = TRUE), 
                              0))  %>% 
      ungroup()
   
## por licenciatura
   
   costos_lic <- costos %>% 
      filter(NIVEL == "Licenciatura") %>% 
      select(-c(NIVEL, AREA, SUBAREA)) %>% 
      ungroup()
   
## por tsu
   
   costos_tsu <- costos %>% 
      filter(NIVEL == "Tecnico Universitario") %>% 
      select(-c(NIVEL, AREA, SUBAREA)) %>% 
      ungroup()

#  Calcular meses efectivos de escuela, costo materiales y costo total, por separado para publicas y privadas   
## Ajustar por inflacion los costos de utiles 
   
   cos_uti_pri_lic <- costos_utiles$costos_lice %>% 
      filter(sos == "pri") %>% 
      pull(COSTO) * factor_inflacion
   
   cos_uti_pub_lic <- costos_utiles$costos_lice %>% 
      filter(sos == "pub") %>% 
      pull(COSTO) * factor_inflacion
   
   cos_uti_pri_tsu <- costos_utiles$costos_tsu %>% 
      filter(sos == "pri") %>% 
      pull(COSTO) * factor_inflacion
   
   cos_uti_pub_tsu <- costos_utiles$costos_tsu %>% 
      filter(sos == "pub") %>% 
      pull(COSTO) * factor_inflacion
   
## por NIVEL
   nivel <- costos_nivel %>% 
      mutate(MESES_ESTUDIO = DURACION_ANIOS * 9,
             COSTO_LIBROS = ifelse(NIVEL == "Licenciatura", 
                                   ifelse(SOSTENIMIENTO == "Privado", 
                                          MESES_ESTUDIO * cos_uti_pri_lic, 
                                          MESES_ESTUDIO * cos_uti_pub_lic),
                                   ifelse(SOSTENIMIENTO == "Privado", 
                                          MESES_ESTUDIO * cos_uti_pri_tsu, 
                                          MESES_ESTUDIO * cos_uti_pub_tsu)),
             COSTO_TOTAL = COSTO + COSTO_LIBROS)
   
## por area
   uni_pri_area <- costos_area %>% 
      filter(SOSTENIMIENTO == "Privado") %>% 
      mutate(MESES_ESTUDIO = DURACION_ANIOS * 9,
             COSTO_LIBROS = MESES_ESTUDIO * cos_uti_pri_lic,
             COSTO_TOTAL = COSTO + COSTO_LIBROS)
   
   uni_pub_area <- costos_area %>% 
      filter(SOSTENIMIENTO == "Publico") %>% 
      mutate(MESES_ESTUDIO = DURACION_ANIOS * 9,
             COSTO_LIBROS = MESES_ESTUDIO * cos_uti_pub_lic,
             COSTO_TOTAL = COSTO + COSTO_LIBROS)
   
## por subarea
   uni_pri_subarea <- costos_subarea %>% 
      filter(SOSTENIMIENTO == "Privado") %>% 
      mutate(MESES_ESTUDIO = DURACION_ANIOS * 9,
             COSTO_LIBROS = MESES_ESTUDIO * cos_uti_pri_lic,
             COSTO_TOTAL = COSTO + COSTO_LIBROS)
   
   uni_pub_subarea <- costos_subarea %>% 
      filter(SOSTENIMIENTO == "Publico") %>% 
      mutate(MESES_ESTUDIO = DURACION_ANIOS * 9,
             COSTO_LIBROS = MESES_ESTUDIO * cos_uti_pub_lic,
             COSTO_TOTAL = COSTO + COSTO_LIBROS)
   
## por licenciatura
   uni_pri_lic <- costos_lic %>% 
      filter(SOSTENIMIENTO == "Privado") %>% 
      mutate(MESES_ESTUDIO = DURACION_ANIOS * 9,
             COSTO_LIBROS = MESES_ESTUDIO * cos_uti_pri_lic,
             COSTO_TOTAL = COSTO + COSTO_LIBROS)
   
   uni_pub_lic <- costos_lic %>% 
      filter(SOSTENIMIENTO == "Publico") %>% 
      mutate(MESES_ESTUDIO = DURACION_ANIOS * 9,
             COSTO_LIBROS = MESES_ESTUDIO * cos_uti_pub_lic,
             COSTO_TOTAL = COSTO + COSTO_LIBROS)
   
### por tsu
   uni_pri_tsu <- costos_tsu %>% 
      filter(SOSTENIMIENTO == "Privado") %>% 
      mutate(MESES_ESTUDIO = DURACION_ANIOS * 9,
             COSTO_LIBROS = MESES_ESTUDIO * cos_uti_pri_tsu,
             COSTO_TOTAL = COSTO + COSTO_LIBROS)
   
   uni_pub_tsu <- costos_tsu %>% 
      filter(SOSTENIMIENTO == "Publico") %>% 
      mutate(MESES_ESTUDIO = DURACION_ANIOS * 9,
             COSTO_LIBROS = MESES_ESTUDIO * cos_uti_pub_tsu,
             COSTO_TOTAL = COSTO + COSTO_LIBROS)
   
#  Calcular RSI y CI
   
   niveles <- resultados$niveles
   areas <- resultados$areas
   subareas <- resultados$subareas
   carreras <- resultados$carreras
   tsu <- resultados$tsu
   
## Detectar si los resultados ya tienen los resultados promediados de RSI (ocurre al usar la funcion compara carreras con mas de un periodo de la enoe)
## en ese caso eliminar esos resultados para volverlos a realizar con los resultados promediados 
   
   if(sum(grep("RSI", colnames(niveles))) > 0){
      
      niveles <- niveles %>% 
         select(-c(RSI_PRI, RSI_PUB))
      
      areas <- areas %>% 
         select(-c(COSTO_TOTAL_PRIVADA, RSI_PRI, COSTO_TOTAL_PUBLICA, RSI_PUB))
      
      subareas <- subareas %>% 
         select(-c(COSTO_TOTAL_PRIVADA, RSI_PRI, COSTO_TOTAL_PUBLICA, RSI_PUB))
      
      carreras <- carreras %>% 
         select(-c(COSTO_TOTAL_PRIVADA, RSI_PRI, COSTO_TOTAL_PUBLICA, RSI_PUB))
      
      tsu <- tsu %>% 
         select(-c(COSTO_TOTAL_PRIVADA, RSI_PRI, COSTO_TOTAL_PUBLICA, RSI_PUB))
      
   }
   
## obtener ingreso de bachillerato
   ing_bach <- niveles %>% 
      filter(ESC == "Bachillerato") %>% 
      pull(INGRESO)
      
   
## crear matriz de categorias de calidad de inversion

   ci <- data.frame(x = c("NN", "NP", "PN", "PP"),
                    y = c("Muy Insegura", "Insegura", "Buena", "Excelente"),
                    stringsAsFactors = FALSE)
   
## Por Niveles
### unir para obtener salario promedio de cada
   
   nivel$ESC <- recode(nivel$NIVEL,
                       "Licenciatura" = "Carrera profesional",
                       "Tecnico Universitario" = "Carrera tecnica")
   
   nivel <- left_join(nivel, 
                      select(niveles, ESC, INGRESO), 
                      by = "ESC")
   
### calcular
   nivel <- nivel %>% 
      mutate(ING_BACH_VIDA = ing_bach * 12 * 45,
             ING_NIVEL_VIDA = INGRESO * 12 * (45 - DURACION_ANIOS),
             DIF_INGRESO = ING_NIVEL_VIDA - ING_BACH_VIDA,
             DIF_COSTO = DIF_INGRESO / COSTO_TOTAL,
             EXP = (1) / (45 - DURACION_ANIOS),
             RSI_PRE = DIF_COSTO ^ EXP,
             RSI = round((RSI_PRE - 1) * 100, 1))
   
### crear matriz para guardar los resultados de RSI
   nivel_mat <- data.frame(ESC = c("Carrera profesional", "Carrera tecnica"), 
                        RSI_PUB = NA, 
                        RSI_PRI = NA)
   
   nivel_mat[nivel_mat$ESC == "Carrera profesional", "RSI_PUB"] <- nivel[nivel$ESC == "Carrera profesional" & nivel$SOSTENIMIENTO == "Publico", "RSI"] 
   nivel_mat[nivel_mat$ESC == "Carrera profesional", "RSI_PRI"] <- nivel[nivel$ESC == "Carrera profesional" & nivel$SOSTENIMIENTO == "Privado", "RSI"] 
   nivel_mat[nivel_mat$ESC == "Carrera tecnica", "RSI_PUB"] <- nivel[nivel$ESC == "Carrera tecnica" & nivel$SOSTENIMIENTO == "Publico", "RSI"] 
   nivel_mat[nivel_mat$ESC == "Carrera tecnica", "RSI_PRI"] <- nivel[nivel$ESC == "Carrera tecnica" & nivel$SOSTENIMIENTO == "Privado", "RSI"] 
   
### unir los resultados
   niveles <- left_join(niveles, 
                        select(nivel_mat, ESC, RSI_PUB, RSI_PRI), 
                        by = "ESC")
   
## Por area
### unir con ingresos promedio y tasa de riesgo

   uni_pub_area <- left_join(uni_pub_area, 
                             select(areas, AREA_CLAVE, INGRESO, TASA_RIESGO), 
                             by = c("AREA" = "AREA_CLAVE"))
   
   uni_pri_area <- left_join(uni_pri_area, 
                             select(areas, AREA_CLAVE, INGRESO, TASA_RIESGO), 
                             by = c("AREA" = "AREA_CLAVE"))
   
### Para publicas, 
#### Calcular rsi y distancia desde la linea (D.f.l, valores negativos representan puntos sobre la linea y valores positivos puntos debajo de la linea)
   
   uni_pub_area <- filter(uni_pub_area, is.na(INGRESO) == FALSE)
   
#### Calculos
   
   uni_pub_area <- uni_pub_area %>% 
      mutate(ING_BACH_VIDA = ing_bach * 12 * 45,
             ING_PROF_VIDA = INGRESO * 12 * (45 - DURACION_ANIOS),
             DIF_INGRESO = ING_PROF_VIDA - ING_BACH_VIDA,
             DIF_COSTO = DIF_INGRESO / COSTO_TOTAL,
             EXP = (1) / (45 - DURACION_ANIOS))
   
#### si el diferencia de salario o costo es menor a 0, se asigna 0 para poder realizar calculos
   
   uni_pub_area$DIF_INGRESO <- replace(uni_pub_area$DIF_INGRESO, 
                                       uni_pub_area$DIF_INGRESO < 0,
                                       0)
   
   uni_pub_area$DIF_COSTO <- replace(uni_pub_area$DIF_COSTO,
                                     uni_pub_area$DIF_COSTO < 0, 
                                     0)
   
#### Mas calculos
   uni_pub_area <- uni_pub_area %>% 
      mutate(RSI_PRE = DIF_COSTO ^ EXP,
             RSI_PUB = round((RSI_PRE - 1) * 100, 1),
             p.R = mean(TASA_RIESGO, na.rm = TRUE),
             p.RSI = mean(RSI_PUB, na.rm = TRUE),
             D.f.l = (((p.RSI)*(TASA_RIESGO)) - ((p.R)*(RSI_PUB)) + ((p.R)*(0)) - ((p.RSI)*(0))) /  sqrt((p.RSI^2) + (p.R^2)),
             G1 = if_else(D.f.l < 0, "P", "N"))
   
   uni_pub_area <- uni_pub_area %>% 
      group_by(G1) %>% 
      mutate(p.D.f.l = mean(D.f.l, na.rm = TRUE),
             G2 = if_else(D.f.l < p.D.f.l, "P", "N"),
             G3 = paste(G1, G2, sep = ""),
             CI_PUB = NA) %>% rename(COSTO_TOTAL_PUBLICA = COSTO_TOTAL) %>% 
      ungroup()
   
#####  Usar la matriz de CI para reemplazar codigos por nombre de calidad de inversion
   
   for(i in c(1:nrow(ci))){
      uni_pub_area$CI_PUB <- replace(uni_pub_area$CI_PUB, 
                                     uni_pub_area$G3 == ci[i, 1], ci[i, 2])
   }
   
### Calcular percentiles para D.f.l 
   
   uni_pub_area$PERCENTIL_PUB <- 1 - round(trunc(rank(uni_pub_area$D.f.l))/length(uni_pub_area$D.f.l), 4)
   
### Para privadas: 
### Calcular rsi y distancia desde la linea (D.f.l, valores negativos representan puntos sobre la linea y valores positivos puntos debajo de la linea)
   
   uni_pri_area <- filter(uni_pri_area, is.na(INGRESO) == FALSE)
   
   ### Calcular
   uni_pri_area <- uni_pri_area %>% 
      mutate(ING_BACH_VIDA = ing_bach * 12 * 45,
             ING_PROF_VIDA = INGRESO * 12 * (45 - DURACION_ANIOS),
             DIF_INGRESO = ING_PROF_VIDA - ING_BACH_VIDA,
             DIF_COSTO = DIF_INGRESO / COSTO_TOTAL,
             EXP = (1) / (45 - DURACION_ANIOS))
   
#### si el diferencia de salario o costo es menor a 0, se asigna 0 para poder realizar calculos
   uni_pri_area$DIF_INGRESO <- replace(uni_pri_area$DIF_INGRESO, 
                                       uni_pri_area$DIF_INGRESO < 0, 
                                       0)
   
   uni_pri_area$DIF_COSTO <- replace(uni_pri_area$DIF_COSTO, 
                                     uni_pri_area$DIF_COSTO < 0, 
                                     0)
   
#### Mas calculos
   uni_pri_area <- uni_pri_area %>% 
      mutate(RSI_PRE = DIF_COSTO ^ EXP,
             RSI_PRI = round((RSI_PRE - 1) * 100, 1),
             p.R = mean(TASA_RIESGO, na.rm = TRUE),
             p.RSI = mean(RSI_PRI, na.rm = TRUE),
             D.f.l = (((p.RSI)*(TASA_RIESGO)) - ((p.R)*(RSI_PRI)) + ((p.R)*(0)) - ((p.RSI)*(0))) /  sqrt((p.RSI^2) + (p.R^2)),
             G1 = if_else(D.f.l < 0, "P", "N"))
   
   uni_pri_area <- uni_pri_area %>% 
      group_by(G1) %>% 
      mutate(p.D.f.l = mean(D.f.l, na.rm = TRUE),
             G2 = if_else(D.f.l < p.D.f.l, "P", "N"),
             G3 = paste(G1, G2, sep = ""),
             CI_PRI = NA) %>% rename(COSTO_TOTAL_PRIVADA = COSTO_TOTAL) %>% 
      ungroup()
   
##### Usar la matriz de CI para reemplazar codigos por nombre de calidad de inversion
   
   for(i in c(1:nrow(ci))){
      uni_pri_area$CI_PRI <- replace(uni_pri_area$CI_PRI, uni_pri_area$G3 == ci[i, 1], ci[i, 2])
   }
   
#### Calcular percentiles para D.f.l 
   
   uni_pri_area$PERCENTIL_PRI <- 1 - round(trunc(rank(uni_pri_area$D.f.l))/length(uni_pri_area$D.f.l), 4)
   
####  Dar formato a estos resultados y unir con resultados por area
   
   uni_pub_area <- uni_pub_area %>% 
      select(AREA, COSTO_TOTAL_PUBLICA, RSI_PUB, CI_PUB)
   
   uni_pri_area <- uni_pri_area %>% 
      select(AREA, COSTO_TOTAL_PRIVADA, RSI_PRI, CI_PRI)
   
   area.ambas <- full_join(uni_pri_area, 
                           uni_pub_area, 
                           by = "AREA")
   
   areas <- left_join(areas, 
                      area.ambas, 
                      by = c("AREA_CLAVE" = "AREA"))
   
## Por SubAreas
   
   uni_pub_subarea <- left_join(uni_pub_subarea, 
                                select(subareas, SUBAREA_CLAVE, INGRESO, TASA_RIESGO), 
                                by = c("SUBAREA" = "SUBAREA_CLAVE"))
   
   uni_pri_subarea <- left_join(uni_pri_subarea, 
                                select(subareas, SUBAREA_CLAVE, INGRESO, TASA_RIESGO), 
                                by = c("SUBAREA" = "SUBAREA_CLAVE"))
   
### Publicas: 
### Calcular rsi y distancia desde la linea (D.f.l, valores negativos representan puntos sobre la linea y valores positivos puntos debajo de la linea)
   
   uni_pub_subarea <- filter(uni_pub_subarea, is.na(INGRESO) == FALSE)
   
   uni_pub_subarea <- uni_pub_subarea %>% 
      mutate(ING_BACH_VIDA = ing_bach * 12 * 45,
             ING_PROF_VIDA = INGRESO * 12 * (45 - DURACION_ANIOS),
             DIF_INGRESO = ING_PROF_VIDA - ING_BACH_VIDA,
             DIF_COSTO = DIF_INGRESO / COSTO_TOTAL,
             EXP = (1) / (45 - DURACION_ANIOS))
   
   uni_pub_subarea$DIF_INGRESO <- replace(uni_pub_subarea$DIF_INGRESO, 
                                          uni_pub_subarea$DIF_INGRESO < 0, 
                                          0)
   
   uni_pub_subarea$DIF_COSTO <- replace(uni_pub_subarea$DIF_COSTO, 
                                        uni_pub_subarea$DIF_COSTO < 0, 
                                        0)
   
   uni_pub_subarea <- uni_pub_subarea %>% 
      mutate(RSI_PRE = DIF_COSTO ^ EXP,
             RSI_PUB = round((RSI_PRE - 1) * 100, 1),
             p.R = mean(TASA_RIESGO, na.rm = TRUE),
             p.RSI = mean(RSI_PUB, na.rm = TRUE),
             D.f.l = (((p.RSI)*(TASA_RIESGO)) - ((p.R)*(RSI_PUB)) + ((p.R)*(0)) - ((p.RSI)*(0))) /  sqrt((p.RSI^2) + (p.R^2)),
             G1 = if_else(D.f.l < 0, "P", "N"))
   
   uni_pub_subarea <- uni_pub_subarea %>% 
      group_by(G1) %>% 
      mutate(p.D.f.l = mean(D.f.l, na.rm = TRUE),
             G2 = if_else(D.f.l < p.D.f.l, "P", "N"),
             G3 = paste(G1, G2, sep = ""),
             CI_PUB = NA)%>% rename(COSTO_TOTAL_PUBLICA = COSTO_TOTAL) %>% 
      ungroup()
   
####  Reemplazar codigos por nombre de calidad de inversion
   
   for(i in c(1:nrow(ci))){
      uni_pub_subarea$CI_PUB <- replace(uni_pub_subarea$CI_PUB, uni_pub_subarea$G3 == ci[i, 1], ci[i, 2])
   }
   
#### Calcular percentiles para D.f.l 
   
   uni_pub_subarea$PERCENTIL_PUB <- 1 - round(trunc(rank(uni_pub_subarea$D.f.l))/length(uni_pub_subarea$D.f.l), 4)
   
###  Privadas: 
### Calcular rsi y distancia desde la linea (D.f.l, valores negativos representan puntos sobre la linea y valores positivos puntos debajo de la linea)
   
   uni_pri_subarea <- filter(uni_pri_subarea, is.na(INGRESO) == FALSE)
   
   uni_pri_subarea <- uni_pri_subarea %>% 
      mutate(ING_BACH_VIDA = ing_bach * 12 * 45,
             ING_PROF_VIDA = INGRESO * 12 * (45 - DURACION_ANIOS),
             DIF_INGRESO = ING_PROF_VIDA - ING_BACH_VIDA,
             DIF_COSTO = DIF_INGRESO / COSTO_TOTAL,
             EXP = (1) / (45 - DURACION_ANIOS))
   
   uni_pri_subarea$DIF_INGRESO <- replace(uni_pri_subarea$DIF_INGRESO, 
                                          uni_pri_subarea$DIF_INGRESO < 0, 
                                          0)
   
   uni_pri_subarea$DIF_COSTO <- replace(uni_pri_subarea$DIF_COSTO, 
                                        uni_pri_subarea$DIF_COSTO < 0, 
                                        0)
   
   uni_pri_subarea <- uni_pri_subarea %>% 
      mutate(RSI_PRE = DIF_COSTO ^ EXP,
             RSI_PRI = round((RSI_PRE - 1) * 100, 1),
             p.R = mean(TASA_RIESGO, na.rm = TRUE),
             p.RSI = mean(RSI_PRI, na.rm = TRUE),
             D.f.l = (((p.RSI)*(TASA_RIESGO)) - ((p.R)*(RSI_PRI)) + ((p.R)*(0)) - ((p.RSI)*(0))) /  sqrt((p.RSI^2) + (p.R^2)),
             G1 = if_else(D.f.l < 0, "P", "N"))
   
   uni_pri_subarea <- uni_pri_subarea %>% 
      group_by(G1) %>% 
      mutate(p.D.f.l = mean(D.f.l, na.rm = TRUE),
             G2 = if_else(D.f.l < p.D.f.l, "P", "N"),
             G3 = paste(G1, G2, sep = ""),
             CI_PRI = NA) %>% rename(COSTO_TOTAL_PRIVADA = COSTO_TOTAL) %>%
      ungroup()
   
####  Reemplazar codigos por nombre de calidad de inversion
   
   for(i in c(1:nrow(ci))){
      uni_pri_subarea$CI_PRI <- replace(uni_pri_subarea$CI_PRI, uni_pri_subarea$G3 == ci[i, 1], ci[i, 2])
   }
   
### Calcular percentiles para D.f.l 
   
   uni_pri_subarea$PERCENTIL_PRI <- 1 - round(trunc(rank(uni_pri_subarea$D.f.l))/length(uni_pri_subarea$D.f.l), 4)
   
###  Dar formato y unir con resultados por subarea
   
   uni_pub_subarea <- uni_pub_subarea %>% 
      select(SUBAREA, COSTO_TOTAL_PUBLICA, RSI_PUB, CI_PUB)
   
   uni_pri_subarea <- uni_pri_subarea %>% 
      select(SUBAREA, COSTO_TOTAL_PRIVADA, RSI_PRI, CI_PRI)
   
   subarea_ambas <- full_join(uni_pri_subarea, 
                              uni_pub_subarea,
                              by = "SUBAREA")
   
   subareas <- left_join(subareas, 
                         subarea_ambas, 
                         by = c("SUBAREA_CLAVE" = "SUBAREA"))
  
## Por carrera profesional
   
   suppressMessages(
   uni_pub_lic <- left_join(uni_pub_lic, 
                            select(carreras, CARRERA_CLAVE, INGRESO, TASA_RIESGO), 
                            by = c("CLAVE" = "CARRERA_CLAVE"))
   )
   
   suppressMessages(
   uni_pri_lic <- left_join(uni_pri_lic, 
                            select(carreras, CARRERA_CLAVE, INGRESO, TASA_RIESGO), 
                            by = c("CLAVE" = "CARRERA_CLAVE"))
   )
   
### Publicas: Calcular rsi y distancia desde la linea (D.f.l, valores negativos representan puntos sobre la linea y valores positivos puntos debajo de la linea)
### Para resolver problemas con carreras cuya DIF_INGRESO es < 0 y que arrojan error al elevarlo al EXP, se reemplaza con 0 los valores del Dif.salarial y el RSI resultante
   
   uni_pub_lic <- filter(uni_pub_lic, is.na(INGRESO) == FALSE)
   
   uni_pub_lic <- uni_pub_lic %>% mutate(ING_BACH_VIDA = ing_bach * 12 * 45,
                                         ING_PROF_VIDA = INGRESO * 12 * (45 - DURACION_ANIOS),
                                         DIF_INGRESO = ING_PROF_VIDA - ING_BACH_VIDA,
                                         DIF_COSTO = DIF_INGRESO / COSTO_TOTAL,
                                         EXP = (1) / (45 - DURACION_ANIOS))
   
   uni_pub_lic$DIF_INGRESO <- replace(uni_pub_lic$DIF_INGRESO, 
                                      uni_pub_lic$DIF_INGRESO < 0, 
                                      0)
   
   uni_pub_lic$DIF_COSTO <- replace(uni_pub_lic$DIF_COSTO,
                                    uni_pub_lic$DIF_COSTO < 0, 
                                    0)
   
   uni_pub_lic <- uni_pub_lic %>% 
      mutate(RSI_PRE = DIF_COSTO ^ EXP,
             RSI_PUB = (RSI_PRE - 1) * 100)
   
   uni_pub_lic$RSI_PUB <- replace(uni_pub_lic$RSI_PUB,
                                  uni_pub_lic$RSI_PUB < 0, 
                                  0)
   
   uni_pub_lic <- uni_pub_lic %>% 
      mutate(p.R = mean(TASA_RIESGO, na.rm = TRUE),
             p.RSI = mean(RSI_PUB, na.rm = TRUE),
             D.f.l = (((p.RSI)*(TASA_RIESGO)) - ((p.R)*(RSI_PUB)) + ((p.R)*(0)) - ((p.RSI)*(0))) /  sqrt((p.RSI^2) + (p.R^2)),
             G1 = if_else(D.f.l < 0, "P", "N"))
   
   uni_pub_lic <- uni_pub_lic %>% 
      group_by(G1) %>% 
      mutate(p.D.f.l = mean(D.f.l, na.rm = TRUE),
             G2 = if_else(D.f.l < p.D.f.l, "P", "N"),
             G3 = paste(G1, G2, sep = ""),
             CI_PUB = NA)%>% rename(COSTO_TOTAL_PUBLICA = COSTO_TOTAL) %>%
      ungroup()
   
#### Reemplazar codigos por nombre de calidad de inversion
   
   for(i in c(1:nrow(ci))){
      uni_pub_lic$CI_PUB <- replace(uni_pub_lic$CI_PUB, uni_pub_lic$G3 == ci[i, 1], ci[i, 2])
   }
   
### Calcular percentiles para D.f.l 
   
   uni_pub_lic$PERCENTIL_PUB <- 1 - round(trunc(rank(uni_pub_lic$D.f.l))/length(uni_pub_lic$D.f.l), 4)
   
### Privadas: Calcular rsi y distancia desde la linea (D.f.l, valores negativos representan puntos sobre la linea y valores positivos puntos debajo de la linea)
   ### Para resolver problemas con carreras cuya DIF_INGRESO es < 0 y que arrojan error al elevarlo al EXP, se reemplaza con 0 los valores del Dif.salarial y el RSI resultante
   
   uni_pri_lic <- filter(uni_pri_lic, is.na(INGRESO) == FALSE)
   
   uni_pri_lic <- uni_pri_lic %>% 
      mutate(ING_BACH_VIDA = ing_bach * 12 * 45,
             ING_PROF_VIDA = INGRESO * 12 * (45 - DURACION_ANIOS),
             DIF_INGRESO = ING_PROF_VIDA - ING_BACH_VIDA,
             DIF_COSTO = DIF_INGRESO / COSTO_TOTAL,
             EXP = (1) / (45 - DURACION_ANIOS))
   
   uni_pri_lic$DIF_INGRESO <- replace(uni_pri_lic$DIF_INGRESO, 
                                      uni_pri_lic$DIF_INGRESO < 0, 
                                      0)
   
   uni_pri_lic$DIF_COSTO <- replace(uni_pri_lic$DIF_COSTO,
                                    uni_pri_lic$DIF_COSTO < 0,
                                    0)
   
   uni_pri_lic <- uni_pri_lic %>% 
      mutate(RSI_PRE = DIF_COSTO ^ EXP,
             RSI_PRI = (RSI_PRE - 1) * 100)
   
   uni_pri_lic$RSI_PRI <- replace(uni_pri_lic$RSI_PRI,
                                  uni_pri_lic$RSI_PRI < 0, 
                                  0)
   
   uni_pri_lic <- uni_pri_lic %>% 
      mutate(p.R = mean(TASA_RIESGO, na.rm = TRUE),
             p.RSI = mean(RSI_PRI, na.rm = TRUE),
             D.f.l = (((p.RSI)*(TASA_RIESGO)) - ((p.R)*(RSI_PRI)) + ((p.R)*(0)) - ((p.RSI)*(0))) /  sqrt((p.RSI^2) + (p.R^2)),
             G1 = if_else(D.f.l < 0, "P", "N"))
   
   uni_pri_lic <- uni_pri_lic %>% 
      group_by(G1) %>% 
      mutate(p.D.f.l = mean(D.f.l, na.rm = TRUE),
             G2 = if_else(D.f.l < p.D.f.l, "P", "N"),
             G3 = paste(G1, G2, sep = ""),
             CI_PRI = NA) %>% rename(COSTO_TOTAL_PRIVADA = COSTO_TOTAL) %>%
      ungroup()
   
####  Reemplazar codigos por nombre de calidad de inversion
   
   for(i in c(1:nrow(ci))){
      uni_pri_lic$CI_PRI <- replace(uni_pri_lic$CI_PRI, uni_pri_lic$G3 == ci[i, 1], ci[i, 2])
   }
   
### Calcular percentiles para D.f.l 
   
   uni_pri_lic$PERCENTIL_PRI <- 1 - round(trunc(rank(uni_pri_lic$D.f.l))/length(uni_pri_lic$D.f.l), 4)
   
##  Dar formato y unir con resultados por carrera
   
   uni_pub_lic_ <- uni_pub_lic
   uni_pri_lic_ <- uni_pri_lic
   
   uni_pub_lic <- uni_pub_lic %>% 
      select(CLAVE, COSTO_TOTAL_PUBLICA, RSI_PUB, CI_PUB)
   
   uni_pri_lic <- uni_pri_lic %>% 
      select(CLAVE, COSTO_TOTAL_PRIVADA, RSI_PRI, CI_PRI)
   
   uni.ambas <- full_join(uni_pri_lic, 
                          uni_pub_lic, 
                          by = "CLAVE")
   
   carreras <- left_join(carreras, 
                         uni.ambas, 
                         by = c("CARRERA_CLAVE" = "CLAVE"))
   
## Por carrera de Tsu
   
   suppressMessages(
   uni_pub_tsu <- left_join(uni_pub_tsu, 
                            select(tsu, CARRERA_CLAVE, INGRESO, TASA_RIESGO), 
                            by = c("CLAVE" = "CARRERA_CLAVE"))
   )
   
   suppressMessages(
   uni_pri_tsu <- left_join(uni_pri_tsu, 
                            select(tsu, CARRERA_CLAVE, INGRESO, TASA_RIESGO), 
                            by = c("CLAVE" = "CARRERA_CLAVE"))
   )
   
### Publicas: Calcular rsi y distancia desde la linea (D.f.l, valores negativos representan puntos sobre la linea y valores positivos puntos debajo de la linea)
#### Para resolver problemas con carreras cuya DIF_INGRESO es < 0 y que arrojan error al elevarlo al EXP, se reemplaza con 0 los valores del Dif.salarial y el RSI resultante
   
   uni_pub_tsu <- filter(uni_pub_tsu, is.na(INGRESO) == FALSE)
   
   uni_pub_tsu <- uni_pub_tsu %>% 
      mutate(ING_BACH_VIDA = ing_bach * 12 * 45,
             ING_PROF_VIDA = INGRESO * 12 * (45 - DURACION_ANIOS),
             DIF_INGRESO = ING_PROF_VIDA - ING_BACH_VIDA,
             DIF_COSTO = DIF_INGRESO / COSTO_TOTAL,
             EXP = (1) / (45 - DURACION_ANIOS))
   
   uni_pub_tsu$DIF_INGRESO <- replace(uni_pub_tsu$DIF_INGRESO, 
                                      uni_pub_tsu$DIF_INGRESO < 0, 
                                      0)
   
   uni_pub_tsu$DIF_COSTO <- replace(uni_pub_tsu$DIF_COSTO, 
                                    uni_pub_tsu$DIF_COSTO < 0, 
                                    0)
   
   uni_pub_tsu <- uni_pub_tsu %>% 
      mutate(RSI_PRE = DIF_COSTO ^ EXP,
             RSI_PUB = (RSI_PRE - 1) * 100)
   
   uni_pub_tsu$RSI_PUB <- replace(uni_pub_tsu$RSI_PUB, uni_pub_tsu$RSI_PUB < 0, 0)
   
   uni_pub_tsu <- uni_pub_tsu %>% 
      mutate(p.R = mean(TASA_RIESGO, na.rm = TRUE),
             p.RSI = mean(RSI_PUB, na.rm = TRUE),
             D.f.l = (((p.RSI)*(TASA_RIESGO)) - ((p.R)*(RSI_PUB)) + ((p.R)*(0)) - ((p.RSI)*(0))) /  sqrt((p.RSI^2) + (p.R^2)),
             G1 = if_else(D.f.l < 0, "P", "N"))
   
   uni_pub_tsu <- uni_pub_tsu %>% 
      group_by(G1) %>% 
      mutate(p.D.f.l = mean(D.f.l, na.rm = TRUE),
             G2 = if_else(D.f.l < p.D.f.l, "P", "N"),
             G3 = paste(G1, G2, sep = ""),
             CI_PUB = NA) %>% rename(COSTO_TOTAL_PUBLICA = COSTO_TOTAL) %>%
      ungroup()
   
####  Reemplazar codigos por nombre de calidad de inversion
   
   for(i in c(1:nrow(ci))){
      uni_pub_tsu$CI_PUB <- replace(uni_pub_tsu$CI_PUB, uni_pub_tsu$G3 == ci[i, 1], ci[i, 2])
   }
   
#### Calcular percentiles para D.f.l 
   
   uni_pub_tsu$PERCENTIL_PUB <- 1 - round(trunc(rank(uni_pub_tsu$D.f.l))/length(uni_pub_tsu$D.f.l), 4)
   
### Privadas: Calcular rsi y distancia desde la linea (D.f.l, valores negativos representan puntos sobre la linea y valores positivos puntos debajo de la linea)
#### Para resolver problemas con carreras cuya DIF_INGRESO es < 0 y que arrojan error al elevarlo al EXP, se reemplaza con 0 los valores del Dif.salarial y el RSI resultante
   
   uni_pri_tsu <- filter(uni_pri_tsu, is.na(INGRESO) == FALSE)
   
   uni_pri_tsu <- uni_pri_tsu %>% 
      mutate(ING_BACH_VIDA = ing_bach * 12 * 45,
             ING_PROF_VIDA = INGRESO * 12 * (45 - DURACION_ANIOS),
             DIF_INGRESO = ING_PROF_VIDA - ING_BACH_VIDA,
             DIF_COSTO = DIF_INGRESO / COSTO_TOTAL,
             EXP = (1) / (45 - DURACION_ANIOS))
   
   uni_pri_tsu$DIF_INGRESO <- replace(uni_pri_tsu$DIF_INGRESO, 
                                      uni_pri_tsu$DIF_INGRESO < 0, 
                                      0)
   
   uni_pri_tsu$DIF_COSTO <- replace(uni_pri_tsu$DIF_COSTO, 
                                    uni_pri_tsu$DIF_COSTO < 0,
                                    0)
   
   uni_pri_tsu <- uni_pri_tsu %>% 
      mutate(RSI_PRE = DIF_COSTO ^ EXP,
             RSI_PRI = (RSI_PRE - 1) * 100)
   
   uni_pri_tsu$RSI_PRI <- replace(uni_pri_tsu$RSI_PRI, 
                                  uni_pri_tsu$RSI_PRI < 0,
                                  0)
   
   uni_pri_tsu <- uni_pri_tsu %>% 
      mutate(p.R = mean(TASA_RIESGO, na.rm = TRUE),
             p.RSI = mean(RSI_PRI, na.rm = TRUE),
             D.f.l = (((p.RSI)*(TASA_RIESGO)) - ((p.R)*(RSI_PRI)) + ((p.R)*(0)) - ((p.RSI)*(0))) /  sqrt((p.RSI^2) + (p.R^2)),
             G1 = if_else(D.f.l < 0, "P", "N"))
   
   uni_pri_tsu <- uni_pri_tsu %>% 
      group_by(G1) %>% 
      mutate(p.D.f.l = mean(D.f.l, na.rm = TRUE),
             G2 = if_else(D.f.l < p.D.f.l, "P", "N"),
             G3 = paste(G1, G2, sep = ""),
             CI_PRI = NA) %>% rename(COSTO_TOTAL_PRIVADA = COSTO_TOTAL) %>%
      ungroup()
   
####  Reemplazar codigos por nombre de calidad de inversion
   
   for(i in c(1:nrow(ci))){
      uni_pri_tsu$CI_PRI <- replace(uni_pri_tsu$CI_PRI, uni_pri_tsu$G3 == ci[i, 1], ci[i, 2])
   }
   
### Calcular percentiles para D.f.l 
   
   uni_pri_tsu$PERCENTIL_PRI <- 1 - round(trunc(rank(uni_pri_tsu$D.f.l))/length(uni_pri_tsu$D.f.l), 4)
   
##  Dar formato y unir con resultados por carrera
   uni_pub_tsu_ <- uni_pub_tsu
   uni_pri_tsu_ <- uni_pri_tsu
   
   uni_pub_tsu <- uni_pub_tsu %>% 
      select(CLAVE, COSTO_TOTAL_PUBLICA, RSI_PUB, CI_PUB)
   
   uni_pri_tsu <- uni_pri_tsu %>% 
      select(CLAVE, COSTO_TOTAL_PRIVADA, RSI_PRI, CI_PRI)
   
   tsu.ambas <- full_join(uni_pri_tsu, 
                          uni_pub_tsu, 
                          by = "CLAVE")
   
   tsu <- left_join(tsu,
                    tsu.ambas, 
                    by = c("CARRERA_CLAVE" = "CLAVE"))
     
# guardar resultados
   areas <- areas %>% 
      mutate_at(vars(COSTO_TOTAL_PRIVADA, COSTO_TOTAL_PUBLICA),
                round, digits = 0) %>% 
      mutate_at(vars(RSI_PRI, RSI_PUB),
                round, digits = 1)
   
   subareas <- subareas %>% 
      mutate_at(vars(COSTO_TOTAL_PRIVADA, COSTO_TOTAL_PUBLICA),
                round, digits = 0) %>% 
      mutate_at(vars(RSI_PRI, RSI_PUB),
                round, digits = 1)
   
   carreras <- carreras %>% 
      mutate_at(vars(COSTO_TOTAL_PRIVADA, COSTO_TOTAL_PUBLICA),
                round, digits = 0) %>% 
      mutate_at(vars(RSI_PRI, RSI_PUB),
                round, digits = 1)
   
   tsu <- tsu %>% 
      mutate_at(vars(COSTO_TOTAL_PRIVADA, COSTO_TOTAL_PUBLICA),
                round, digits = 0) %>% 
      mutate_at(vars(RSI_PRI, RSI_PUB),
                round, digits = 1)
   
   res <- list(niveles, areas, subareas, carreras, tsu)
   names(res) <- c("niveles", "areas", "subareas", "carreras", "tsu")

   if(guardar == "Si"){
      
      #crear data frame de costos por carrera para tabla de la base de datos del sitio
      c_lic <- costos_lic
      c_tsu <- costos_tsu
      
      #licenciatura publica
      c_lic_pub <- c_lic %>% 
         filter(SOSTENIMIENTO == "Publico") %>% 
         select(-COSTO) %>% 
         rename(duracion_universidad = DURACION_ANIOS, 
                regimen = SOSTENIMIENTO) %>% 
         mutate(nivel = "Licenciatura", 
                año = periodo_costos, 
                trimestre = "1", 
                CLAVE = as.character(CLAVE), 
                vida_laboral = 45)
      
      uni_pub_lic_ <- uni_pub_lic_ %>% 
         mutate(RSI_PUB = (RSI_PRE - 1))
      
      c_lic_pub <- left_join(c_lic_pub, 
                             select(uni_pub_lic_, CLAVE, COSTO_TOTAL_PUBLICA, DURACION_ANIOS, RSI_PUB, DIF_INGRESO, INGRESO, RSI_PUB), 
                             by = c("CLAVE"))
      
      c_lic_pub <- c_lic_pub %>% 
         rename(id_campo_detallado = CLAVE, 
                costo_total_universidad = COSTO_TOTAL_PUBLICA, 
                diferencial_ingreso_preparatoria = DIF_INGRESO, 
                roi_anual = RSI_PUB) %>% 
         mutate(subsidiado = TRUE, 
                recuperacion_de_inversion = (costo_total_universidad / INGRESO)) %>% 
         select(año, 
                trimestre,
                id_campo_detallado,
                nivel,
                regimen,
                vida_laboral,
                duracion_universidad,
                costo_total_universidad,
                recuperacion_de_inversion,
                diferencial_ingreso_preparatoria,
                roi_anual,
                subsidiado)
      
      #licenciatura privada
      c_lic_pri <- c_lic %>% 
         filter(SOSTENIMIENTO == "Privado") %>% 
         select(-COSTO) %>% 
         rename(duracion_universidad = DURACION_ANIOS, 
                regimen = SOSTENIMIENTO) %>% 
         mutate(nivel = "Licenciatura", 
                año = periodo_costos, 
                trimestre = "1", 
                CLAVE = as.character(CLAVE), 
                vida_laboral = 45)
      
      uni_pri_lic_ <- uni_pri_lic_ %>% 
         mutate(RSI_PRI = (RSI_PRE - 1))
      
      c_lic_pri <- left_join(c_lic_pri, 
                             select(uni_pri_lic_, CLAVE, COSTO_TOTAL_PRIVADA, DURACION_ANIOS, RSI_PRI, DIF_INGRESO, INGRESO), 
                             by = c("CLAVE"))
      
      c_lic_pri <- c_lic_pri %>% 
         rename(id_campo_detallado = CLAVE, 
                costo_total_universidad = COSTO_TOTAL_PRIVADA, 
                diferencial_ingreso_preparatoria = DIF_INGRESO, 
                roi_anual = RSI_PRI) %>% 
         mutate(subsidiado = FALSE,
                recuperacion_de_inversion = (costo_total_universidad / INGRESO)) %>% 
         select(año, 
                trimestre,
                id_campo_detallado,
                nivel,
                regimen,
                vida_laboral,
                duracion_universidad,
                costo_total_universidad,
                recuperacion_de_inversion,
                diferencial_ingreso_preparatoria,
                roi_anual,
                subsidiado)
      
      #Tsu pub
      c_tsu_pub <- c_tsu %>% 
         filter(SOSTENIMIENTO == "Publico") %>% 
         select(-COSTO) %>% 
         rename(duracion_universidad = DURACION_ANIOS, 
                regimen = SOSTENIMIENTO) %>% 
         mutate(nivel = "Tsu", 
                año = periodo_costos, 
                trimestre = "1", 
                CLAVE = as.character(CLAVE), 
                vida_laboral = 45)
      
      uni_pub_tsu_ <- uni_pub_tsu_ %>% 
         mutate(RSI_PUB = (RSI_PRE - 1))
      
      c_tsu_pub <- left_join(c_tsu_pub, 
                             select(uni_pub_tsu_, CLAVE, COSTO_TOTAL_PUBLICA, DURACION_ANIOS, RSI_PUB, DIF_INGRESO, INGRESO), 
                             by = c("CLAVE"))
      
      c_tsu_pub <- c_tsu_pub %>% 
         rename(id_campo_detallado = CLAVE, 
                costo_total_universidad = COSTO_TOTAL_PUBLICA, 
                diferencial_ingreso_preparatoria = DIF_INGRESO, 
                roi_anual = RSI_PUB) %>% 
         mutate(subsidiado = TRUE, 
                recuperacion_de_inversion = (costo_total_universidad / INGRESO)) %>% 
         select(año, 
                trimestre,
                id_campo_detallado,
                nivel,
                regimen,
                vida_laboral,
                duracion_universidad,
                costo_total_universidad,
                recuperacion_de_inversion,
                diferencial_ingreso_preparatoria,
                roi_anual,
                subsidiado)
      
      
      #Tsu pri
      c_tsu_pri <- c_tsu %>% 
         filter(SOSTENIMIENTO == "Privado") %>% 
         select(-COSTO) %>% 
         rename(duracion_universidad = DURACION_ANIOS, 
                regimen = SOSTENIMIENTO) %>% 
         mutate(nivel = "Tsu", 
                año = periodo_costos, 
                trimestre = "1", 
                CLAVE = as.character(CLAVE), 
                vida_laboral = 45)
      
      uni_pri_tsu_ <- uni_pri_tsu_ %>% 
         mutate(RSI_PRI = (RSI_PRE - 1))
      
      c_tsu_pri <- left_join(c_tsu_pri, 
                             select(uni_pri_tsu_, CLAVE, COSTO_TOTAL_PRIVADA, DURACION_ANIOS, RSI_PRI, DIF_INGRESO, INGRESO), 
                             by = c("CLAVE"))
      
      c_tsu_pri <- c_tsu_pri %>% 
         rename(id_campo_detallado = CLAVE, 
                costo_total_universidad = COSTO_TOTAL_PRIVADA, 
                diferencial_ingreso_preparatoria = DIF_INGRESO, 
                roi_anual = RSI_PRI) %>% mutate(subsidiado = FALSE, 
                                                recuperacion_de_inversion = (costo_total_universidad / INGRESO)) %>% 
         select(año, 
                trimestre,
                id_campo_detallado,
                nivel,
                regimen,
                vida_laboral,
                duracion_universidad,
                costo_total_universidad,
                recuperacion_de_inversion,
                diferencial_ingreso_preparatoria,
                roi_anual,
                subsidiado)
      
      #unir
      
      roi_bd <- bind_rows(c_lic_pri, c_lic_pub, c_tsu_pri, c_tsu_pub)
      roi_bd <- roi_bd %>% 
         mutate(regimen = recode(regimen,
                                 "Publico" = "Pública"))
         
      
      saveRDS(roi_bd, "03_resultados//temporales//roi_bd.RDS")
      write.csv(roi_bd, "03_resultados//temporales//roi_bd.csv")
   }
   
   cat("Calculos de RSI completados\n")
   return(res)
   
}