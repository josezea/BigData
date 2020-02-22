library(readr)
library(dplyr)
setwd("C:/Users/Home/Documents/Laboral2020/Konrad Lorenz/BigData/Clase 2")
hogares <- read_delim("variables_adicionales_hogar_v3.txt", 
                      delim = ";")
vivienda <- read_delim("viviendas_2017_v2_03092018.txt", delim = ",")
# Select: Eliminar o conservar variables
# Filter: filtrar
# Summarise: Crear estadísticas descriptivas (un número)
# Mutate: Crear nuevas variables
# Arrange: Ordenar por una o varias columnas
# Slice: Filtros por fila
# Group_by: Agregaciones
# left_join, right_join, inner_join, bind_rows


# Promedio del ingreso por estrato
consulta1 <-  hogares %>% group_by(ESTRATO_VIV) %>% 
              summarise(prom_ing = mean(INGRESOS_HOG), 
                        mediana_ing = median(INGRESOS_HOG))

class(group_by(hogares, ESTRATO_VIV))
class(hogares %>% group_by(ESTRATO_VIV))



hogares %>% 
  summarise(prom_ing = mean(INGRESOS_HOG), 
            mediana_ing = median(INGRESOS_HOG), 
    cv_ingPC = 100 * sd(INGRESOS_PER_CAPITA) / 
      mean(INGRESOS_PER_CAPITA))



######## Calcular ingreso promedio de los hogares para los hogares
####### de Bogotá #############

cons2  <- vivienda %>% select(DIRECTORIO, DPTOMPIO, LOCALIDAD_TEX) %>% 
             right_join(select(hogares, DIRECTORIO, DIRECTORIO_HOG,
                        ESTRATO_VIV, INGRESOS_HOG), 
                        by = "DIRECTORIO") %>%
             filter(DPTOMPIO == 11001) %>%
             group_by(ESTRATO_VIV) %>%
             summarise(Prom_ing = mean(INGRESOS_HOG))

hogares %>% mutate(INGRESOS_PER_CAPITA_R =
                       ifelse(INGRESOS_PER_CAPITA == 0, 1, 
                              INGRESOS_PER_CAPITA),
          NumPerHog = INGRESOS_HOG / INGRESOS_PER_CAPITA_R) %>%
          group_by(ESTRATO_VIV) %>%
          summarise(Prom_NP = mean(NumPerHog))
hogares


# Calcule un ranking de las localidades según su ingreso percapita
# mediano

consulta4 <- vivienda %>% select(DIRECTORIO, DPTOMPIO, LOCALIDAD_TEX) %>% 
  right_join(select(hogares, DIRECTORIO, DIRECTORIO_HOG,
                    ESTRATO_VIV, INGRESOS_HOG), 
             by = "DIRECTORIO") %>% group_by(LOCALIDAD_TEX) %>%
   summarise(mediana_ingreso = median(INGRESOS_HOG))  %>%
  arrange(-mediana_ingreso) %>% mutate(ranking = 1:nrow(.))


consulta5 <- vivienda %>% select(DIRECTORIO, DPTOMPIO, LOCALIDAD_TEX) %>% 
  right_join(select(hogares, DIRECTORIO, DIRECTORIO_HOG,
                    ESTRATO_VIV, INGRESOS_PER_CAPITA), 
             by = "DIRECTORIO") %>% 
  arrange(LOCALIDAD_TEX, -INGRESOS_PER_CAPITA) %>%
  group_by(LOCALIDAD_TEX) %>% mutate(ranking = 1:length(DIRECTORIO)) %>%
  filter(ranking <= 5)
consulta5


