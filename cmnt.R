# --- PG
# + ¿En qué idioma(s)?
# + ¿Paleta de colores? ¿Tema highcharts?
# + ¿Datos? ¿Son esos? Si sí entonces:
#   - (Ver m0) la columna "compromiso" q "id_com" deberían 
#   tener el mismo n_distinct -- no lo tienen: 25 - 24; esto se 
#   puede deber a que hay una actividad repetida, es decir, debería
#   haber una actividad por registro pero hay una repetida
#   - ¿Calcular la columna completitud? ¿Cómo? Si entiendo bien, queremos contrastar
#   esto que sería la "Expectativa" y la "Realidad" que lo reporta cada entidad: ¿de dónde
#   se saca estos datos?
# + Para la primera gráfica hay varias actividades por entidad,
# es decir, varios valores de "completitud" ¿cuál tomar? ¿el promedio?
# + ¿En cuál panel ponerl el filtro de los compromisos (burbújas y gntt)?
# + ¿Las actividades por compromiso (gntt) se colorean? El filtro
# de esto podría también tener una opción de "Todos" y mostrar
# todos los compromisos al tiempo ¿sí?
# + ¿Descargar visualización?

# PPED
# + ¿Nombre repositorio? ¿Privado? app-aga-indicadores público
# + Puede haber entidades que tengan varios compromisos, y, a su vez
# éstos tener varias actividades con varios periodos de tiempo distintos.
# Si se quiere hacer una agregación representativa de estos datos, digamos, 
# de el "proceso" de un compromiso en particular, o mejor, de una entidad,
# para la completitud tomaríamos el promedio de las completitudes pero el 
# intervalo de tiempo ¿cuál sería? ¿Fecha más antigua y fecha más avanzada?
# + Tema, paleta de colores. ds



# --  MAE
# + Halar datos google sheetss



# ----- DAT
library(tidyverse)
library(homodatum)

a0 <- read_csv("data/Base de datos indicadores AGA Plan 4 Test - aga.csv")
View(a0)

# ¿Registros duplicados?
any(duplicated(a0)) # No

# Número de filas, columnas
c(nrow(a0), ncol(a0)) # 100, 10

# Generales: uniques, na's, ftype
m0 <- map_df(seq_along(a0), ~data.frame(name = names(a0)[.x],
                                        ctype = guess_hdType(a0[[.x]]),
                                        "na's" = sum(is.na(a0[[.x]])),
                                        uniques = n_distinct(a0[[.x]]),
                                        prop_uniques = paste((n_distinct(a0[[.x]]) * 100) / nrow(a0), "%")))
View(m0)

## Limpieza
a1 <- a0
# Quitando porcentajes
a1$completitud <- as.numeric(gsub("%", "", a1$completitud))
write_csv(a1, "data/data.csv")


# Datos actualizados...
b0 <- read_csv("data/Plantilla seguimiento compromisos IV Plan - Hoja 1.csv")
View(b0)

# ¿Registros duplicados?
any(duplicated(b0)) # No

# Número de filas, columnas
c(nrow(b0), ncol(b0)) # 5, 10

# Generales: uniques, na's, ftype
n0 <- map_df(seq_along(b0), ~data.frame(name = names(b0)[.x],
                                        ctype = guess_hdType(b0[[.x]]),
                                        "na's" = sum(is.na(b0[[.x]])),
                                        uniques = n_distinct(b0[[.x]]),
                                        prop_uniques = paste((n_distinct(b0[[.x]]) * 100) / nrow(b0), "%")))
View(n0)

## Limpeza (esto iría en el app)
b1 <- b0
# Quitando porcentajes
b1$completitud <- as.numeric(gsub("%", "", b1$completitud))

# Calculando columna "expectativa" (porcentaje de días con respescto a la
# fecha final que han transcurrido)
b1 <- b1 %>%
  mutate(e0 = ifelse(fecha_fin < Sys.Date(), 100,  ifelse(fecha_inicio > Sys.Date(), 0, as.numeric(fecha_fin - fecha_inicio))),
         expectativa = ifelse(e0 == 100 | e0 == 0, e0, round((as.numeric(Sys.Date() - fecha_inicio) * 100) / e0, 0))) %>%
  select(-e0)
# ¿Un modelo podría reemplazar una --cualquier- serie de condicionales? Sí creo, pensar en cómo crear la columna de arriba
# "expectativa" en excel.
# Creando registros por cada entidad
b2 <- map_df(1:nrow(b1), function (s) {
  g0 <- strsplit(b1$`entidades responsables`[s], ",")[[1]]
  g1 <- b1[rep(s, length(g0)), ]
  g1$`entidades responsables` <- g0
  g1
}) 
b3 <- b2 %>% 
  pivot_longer(c(completitud, expectativa), "percentage_of", values_to = "percentage")


write_csv(b2, "data/data_.csv")
