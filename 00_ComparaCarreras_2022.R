#  La funcion ComparaCarreras vincula las diferentes funciones usadas para los calculos de Compara Carreras 

ComparaCarreras <- function(periodo_enoe,                # especifica uno o mas periodos de la enoe a ser analizados
                            periodo_costos = NA,         # especifica el periodo de costos de colegiaturas para calculos de RSI
                            guardar_res = c("No", "Si"), # especifica si los resultados deben escribirse o no a un archivo de excel
                            factor_inflacion = 1.0613){  # ajusta los costos de utiles por un factor 

#  Crear un argumento a pasar para guardar o no los resultados en un archivo de excel
   guardar <- match.arg(guardar_res, c("No", "Si"))
   
#  Cargar paquetes 
   library(dplyr)
   library(tidyr)
   library(srvyr)
   library(xlsx)
   library(readxl)
   library(here)
   library(stringr)
   
#  Importar scripts  
   source("02_scripts\\01_CrearEnoe_2022.R")
   source("02_scripts\\02_SeleccionarCarreras_2022.R")
   source("02_scripts\\03_CalcularXNiveles_2022.R")
   source("02_scripts\\04_CalcularXAreas_2022.R")
   source("02_scripts\\05_CalcularXSubAreas_2022.R")
   source("02_scripts\\06_CalcularXCarreras_2022.R")
   source("02_scripts\\07_CalcularXTsu_2022.R")
   source("02_scripts\\08_CalcularRSIeIC_2022.R", encoding = 'UTF-8')
   source("02_scripts\\09_PromediarPeriodos.R")
   
#  Importar clasificacion de carreras
   clas_car <- read.csv("01_fuentes\\cmpe 2011.csv", encoding = "UTF-7")
   clas_car <- clas_car %>% mutate_all(as.character)
   
#  Crear funcion con los calculos a realizar para cada periodo
   
   Calculos <- function(periodo_enoe, periodo_costos, factor_inflacion){
      
      cat(paste("Iniciar calculos para periodo", periodo_enoe, "\n", sep = " "))
      
## Importar tabla "sdem" de microdatos de la enoe
      sdem <- readRDS(paste("01_fuentes\\enoe\\", 
                            "sdemt", 
                            periodo_enoe, 
                            ".RDS", 
                            sep = ""))
      
## Crear una lista para guardar los resultados y correr los scripts
      resultados <- NULL   
      
      enoe <- CrearEnoe(sdem, periodo_enoe)
      
      res <- SeleccionarCarreras(enoe, clas_car, periodo_enoe)
      
      resultados$niveles <- CalcularXNiveles(res$enoesvy)
      
      resultados$areas <- CalcularXAreas(res$enoesvy)
      
      resultados$subareas <- CalcularXSubareas(res$enoesvy, clas_car)
      
      resultados$carreras <- CalcularXCarreras(res$enoe, clas_car, res$enoesvy)
      
      resultados$tsu <- CalcularXTsu(res$enoesvy, res$enoe, clas_car)

## Realizar los calculos de retorno de inversion solo si se especifico un archivo de costos de universidades
## Si se esta analizando un solo periodo, realizar estos calculos solo en este punto. Si se trata de varios periodos, volver a realizar calculos despues de promediar resultados
      
      if(is.na(periodo_costos) == FALSE){
         
         res <- CalcularRSIeIC(periodo_costos, resultados, factor_inflacion, guardar)
         
         resultados[[1]] <- res$niveles
         resultados[[2]] <- res$areas
         resultados[[3]] <- res$subareas
         resultados[[4]] <- res$carreras
         resultados[[5]] <- res$tsu
         
         names(resultados) <- c("niveles", "areas", "subareas", "carreras", "tsu")
         
      }
      
      if(guardar == "Si"){
         
         saveRDS(resultados,
                 paste0("03_resultados//temporales//res_", periodo_enoe, ".RDS"))
      }
      
      cat(paste("Calculos realizados para periodo", periodo_enoe, "\n", sep = " "))
      
      return(resultados)
   }
   
# Correr scripts de calculos para compara carreras y guardar los resultados en una lista 
   
   if(length(periodo_enoe) == 1){
      
      resultados <- Calculos(periodo_enoe, periodo_costos, factor_inflacion)
      
   } else {
      
## Si el numero de periodo a calcular es mayor a  uno, realizar los calculos para cada periodo primero
## luego calcular, para cada dataframe de resultados, el promedio de cada dato en todos los periodos y guardar las dfs con los datos promediados en un lista
      
      if(length(periodo_enoe) > 1){
         
         lista.res <- NULL
         
         lista.res <- lapply(periodo_enoe, Calculos, periodo_costos, factor_inflacion)

         resultados <- PromediarPeriodos(lista.res, periodo_enoe, periodo_costos, factor_inflacion, guardar)
         
      } 
   }
   
   # Escribir archivos para la base de datos del sitio
   
   
   # Guardar resultados en excel y RDS
   if(guardar == "Si"){
      
      periodo_enoe <- paste(periodo_enoe, collapse = "_")
      
      saveRDS(resultados,
              paste0("03_resultados//temporales//resultados_", periodo_enoe, ".RDS"))
      
      write.xlsx(as.data.frame(resultados$niveles), 
                 file = paste("03_resultados\\ComparaCarreras",
                              periodo_enoe,
                              ".xlsx",
                              sep = ""),
                 sheetName = "Niveles de estudio")
      
      write.xlsx(as.data.frame(resultados$areas),
                 file = paste("03_resultados\\ComparaCarreras", 
                              periodo_enoe, 
                              ".xlsx",
                              sep = ""), 
                 sheetName = "Areas", 
                 append = TRUE)
      
      write.xlsx(as.data.frame(resultados$subareas),
                 file = paste("03_resultados\\ComparaCarreras", 
                              periodo_enoe, 
                              ".xlsx",
                              sep = ""), 
                 sheetName = "Subareas", append = TRUE)
      
      write.xlsx(as.data.frame(resultados$carreras), 
                 file = paste("03_resultados\\ComparaCarreras", 
                              periodo_enoe,
                              ".xlsx",
                              sep = ""),
                 sheetName = "Carreras",
                 append = TRUE)
      
      write.xlsx(as.data.frame(resultados$tsu), 
                 file = paste("03_resultados\\ComparaCarreras",
                              periodo_enoe,
                              ".xlsx",
                              sep = ""), 
                 sheetName = "Carreras_tsu", 
                 append = TRUE)
      
      cat("resultados guardados para periodo", periodo_enoe, "\n")
      
   }
   
   cat("calculos de compara carreras finalizados\n")
   return(resultados)
   
} 

