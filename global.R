clean_data <- function (d) {
  
  d0 <- d
  # Quitando porcentajes
  d0$completitud <- as.numeric(gsub("%", "", d0$completitud))
  d0$completitud <- d0$completitud * 100
  d0$fecha_inicio <- as.Date(d0$fecha_inicio)
  d0$fecha_fin <- as.Date(d0$fecha_fin)
  
  # Calculando columna "expectativa" (porcentaje de días con respescto a la
  # fecha final que han transcurrido)
  d0 <- d0 %>%
    mutate(e0 = ifelse(fecha_fin < Sys.Date(), 100,  ifelse(fecha_inicio > Sys.Date(), 0, as.numeric(fecha_fin - fecha_inicio))),
           expectativa = ifelse(e0 == 100 | e0 == 0, e0, round((as.numeric(Sys.Date() - fecha_inicio) * 100) / e0, 0))) %>%
    select(-e0)
  
  # Creando registros por cada entidad
  d1 <- map_df(1:nrow(d0), function (s) {
    g0 <- strsplit(d0$`entidades responsables`[s], ",")[[1]]
    g1 <- d0[rep(s, length(g0)), ]
    g1$`entidades responsables` <- g0
    g1
  }) 
  d1
  
}