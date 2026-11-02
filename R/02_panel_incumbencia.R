# =============================================================================
# 02_panel_incumbencia.R
# -----------------------------------------------------------------------------
# Construye el panel comuna-eleccion: ganador, margen, incumbente, si el
# incumbente compite o esta inhabilitado por la Ley 21.238, y si su partido o
# pacto retiene la alcaldia. Proyecta los inhabilitados para 2028.
#
# Entradas  data/processed/alcaldes_comuna.rds, participacion_comuna.rds
#           data/homologacion/comunas.csv (opcional: nombre_servel;comuna)
# Salidas   data/processed/panel_comuna.rds
#           output/tablas/02_panel_comuna.csv
#           output/tablas/02_inhabilitados_2024.csv, 02_inhabilitados_2028.csv
#           output/tablas/02_retencion_por_tipo.csv
#           output/tablas/02_revisar_nombres.csv, 02_comunas_sin_par.csv
# =============================================================================

source(file.path("R", "00_funciones.R"))
DIR_PROC <- file.path("data", "processed"); DIR_TAB <- file.path("output", "tablas")
votos <- readRDS(file.path(DIR_PROC, "alcaldes_comuna.rds"))
part  <- if (file.exists(file.path(DIR_PROC, "participacion_comuna.rds")))
  readRDS(file.path(DIR_PROC, "participacion_comuna.rds")) else NULL

# Homologacion opcional de nombres de comuna que cambian entre anos.
hom <- file.path("data", "homologacion", "comunas.csv")
if (file.exists(hom)) {
  h <- read.csv2(hom, fileEncoding = "UTF-8") |> mutate(across(everything(), clave))
  votos <- votos |> left_join(h, by = c("comuna" = "nombre_servel"), suffix = c("", ".h")) |>
    mutate(comuna = coalesce(comuna.h, comuna)) |> select(-comuna.h)
  if (!is.null(part)) part <- part |> left_join(h, by = c("comuna" = "nombre_servel"), suffix = c("", ".h")) |>
    mutate(comuna = coalesce(comuna.h, comuna)) |> select(-comuna.h)
}

# Antartica no tiene municipio propio: la administra Cabo de Hornos y el SERVEL
# repite la misma eleccion. Se excluye para no duplicar observaciones.
votos <- votos |> filter(comuna != "ANTARTICA")
anios <- sort(unique(votos$anio))
SIN_SECTOR <- c("Independiente", "Regional y otros", "Populismo")

# 1. Resultado por comuna y eleccion ------------------------------------------
cand <- votos |>
  filter(tipo_voto == "valido") |>
  mutate(partido = coalesce(partido, "SIN PARTIDO"),
         pacto   = coalesce(pacto, partido)) |>
  asignar_persona() |>
  asignar_familias() |>
  group_by(anio, comuna) |>
  mutate(validos = sum(votos),
         pct = votos / validos,
         electo_marca = !is.na(marca) & str_detect(clave(marca), "ALCALDE|ELECTO"),
         rank = rank(-votos, ties.method = "first")) |>
  ungroup()

# Ganador: el marcado por el SERVEL; si no hay marca, el mas votado.
ganador <- cand |>
  group_by(anio, comuna) |>
  arrange(desc(electo_marca), rank, .by_group = TRUE) |>
  slice(1) |>
  ungroup() |>
  select(anio, comuna, region, ganador = candidato, ganador_persona = persona, ganador_apellidos = apellidos,
         ganador_partido = partido, ganador_pacto = pacto, ganador_familia = familia,
         ganador_fuente = fuente_familia, ganador_eje = eje, ganador_pct = pct)

segundo <- cand |>
  left_join(select(ganador, anio, comuna, ganador_persona), by = c("anio", "comuna")) |>
  filter(persona != ganador_persona) |>
  group_by(anio, comuna) |> slice_max(votos, n = 1, with_ties = FALSE) |> ungroup() |>
  select(anio, comuna, segundo = candidato, segundo_persona = persona,
         segundo_partido = partido, segundo_pct = pct)

nb <- votos |>
  group_by(anio, comuna) |>
  summarise(emitidos = sum(votos), nulo_blanco = sum(votos[tipo_voto != "valido"]),
            n_candidatos = sum(tipo_voto == "valido"), .groups = "drop") |>
  mutate(pct_nulo_blanco = nulo_blanco / emitidos)

res <- ganador |>
  left_join(segundo, by = c("anio", "comuna")) |>
  left_join(nb, by = c("anio", "comuna")) |>
  mutate(margen = ganador_pct - coalesce(segundo_pct, 0))
if (!is.null(part)) res <- res |> left_join(part, by = c("anio", "comuna")) |>
  mutate(participacion = votantes / inscritos)

# 2. Incumbencia ----------------------------------------------------------------
# Para cada eleccion t, el incumbente es el ganador de la eleccion anterior.
prev <- function(a) { i <- match(a, anios); if (is.na(i) || i == 1) NA_integer_ else anios[i - 1] }
res <- res |> mutate(anio_prev = map_int(anio, prev))

inc <- res |>
  select(anio_prev = anio, comuna, inc = ganador, inc_persona = ganador_persona, inc_apellidos = ganador_apellidos,
         inc_partido = ganador_partido, inc_pacto = ganador_pacto, inc_familia = ganador_familia,
         inc_fuente = ganador_fuente, inc_eje = ganador_eje, inc_margen_prev = margen)

# Alcaldes en ejercicio distintos del ultimo electo (fallecimiento, destitucion,
# suspension o renuncia durante el periodo): el incumbente real en la eleccion t
# es quien ejercia el cargo. Se declaran en data/homologacion/reemplazos.csv
# (anio;comuna;alcalde_en_ejercicio;familia;motivo;fuente). Si el nombre del
# reemplazante aun no se conoce, alcalde_en_ejercicio queda vacio y la eleccion
# se clasifica como "Reemplazo sin identificar" (fuera de las comparaciones).
f_reemp <- file.path("data", "homologacion", "reemplazos.csv")
reemp <- if (file.exists(f_reemp)) read.csv2(f_reemp, fileEncoding = "UTF-8", colClasses = "character") else NULL
if (!is.null(reemp)) {
  if (!"cuenta_periodo" %in% names(reemp)) reemp$cuenta_periodo <- "FALSE"
  reemp <- reemp |>
    mutate(anio = as.integer(anio), comuna = clave(comuna),
           alcalde_en_ejercicio = na_if(clave(alcalde_en_ejercicio), ""),
           familia = na_if(str_squish(familia), ""),
           cuenta_periodo = toupper(trimws(cuenta_periodo)) %in% c("TRUE", "SI", "1")) |>
    select(anio, comuna, alcalde_en_ejercicio, familia, motivo, cuenta_periodo)
}
if (!is.null(reemp) && nrow(reemp) > 0) {
  # Busca al reemplazante entre los candidatos de la comuna (en cualquier anio).
  # Acepta nombres incompletos de la prensa ("LEONEL CADIZ"): basta un nombre de
  # pila comun y que los apellidos del candidato comiencen con los declarados.
  busca_reemp <- function(com, nombre) {
    pn <- partes_nombre(nombre)
    c2 <- cand |> filter(comuna == com) |> distinct(candidato, persona, apellidos)
    hit <- c2 |> filter(str_detect(apellidos, paste0("^", pn$apellidos, "( |$)")),
                        map_lgl(candidato, ~ length(intersect(partes_nombre(.x)$pila, pn$pila)) > 0))
    if (nrow(hit) > 0) c(hit$persona[1], hit$apellidos[1]) else c(paste(com, nombre, sep = "|"), pn$apellidos)
  }
  reemp <- reemp |> mutate(
    pendiente   = is.na(alcalde_en_ejercicio),
    .hit        = map2(comuna, coalesce(alcalde_en_ejercicio, "X X X"), busca_reemp),
    r_persona   = if_else(pendiente, paste(comuna, "PENDIENTE", sep = "|"), map_chr(.hit, 1)),
    r_apellidos = if_else(pendiente, NA_character_, map_chr(.hit, 2))) |>
    select(-.hit)
  inc <- inc |>
    mutate(anio = map_int(anio_prev, function(a) { i <- match(a, anios); if (is.na(i) || i == length(anios)) NA_integer_ else anios[i + 1] })) |>
    left_join(reemp, by = c("anio", "comuna")) |>
    mutate(reemplazante = !is.na(r_persona),
           reemplazo_pendiente = coalesce(pendiente, FALSE),
           inc = if_else(reemplazante, coalesce(alcalde_en_ejercicio, "(reemplazante sin identificar)"), inc),
           inc_persona = if_else(reemplazante, r_persona, inc_persona),
           inc_apellidos = if_else(reemplazante, r_apellidos, inc_apellidos),
           inc_partido = if_else(reemplazante, NA_character_, inc_partido),
           inc_familia = if_else(reemplazante, familia, inc_familia),
           inc_fuente = if_else(reemplazante, "reemplazos.csv", inc_fuente),
           inc_eje = if_else(reemplazante, eje_de(familia), inc_eje),
           motivo_vacancia = motivo) |>
    select(-anio, -alcalde_en_ejercicio, -familia, -motivo, -pendiente, -cuenta_periodo, -r_persona, -r_apellidos)
  message(sum(inc$reemplazante), " elecciones con alcalde en ejercicio distinto del electo (reemplazos.csv)")
} else inc <- inc |> mutate(reemplazante = FALSE, reemplazo_pendiente = FALSE, motivo_vacancia = NA_character_)

personas_t <- cand |> group_by(anio, comuna) |>
  summarise(personas = list(persona), apellidos_t = list(apellidos), .groups = "drop")

panel <- res |>
  left_join(inc, by = c("anio_prev", "comuna")) |>
  left_join(personas_t, by = c("anio", "comuna")) |>
  mutate(reemplazante = coalesce(reemplazante, FALSE),
         reemplazo_pendiente = coalesce(reemplazo_pendiente, FALSE),
         inc_compite = map2_lgl(inc_persona, personas, ~ !is.na(.x) && .x %in% .y),
         # Otra persona con los mismos dos apellidos que el incumbente (posible familiar).
         familiar_compite = pmap_lgl(list(inc_persona, inc_apellidos, personas, apellidos_t), function(ip, ia, ps, as) {
           !is.na(ip) && any(as == ia & ps != ip) }),
         familiar_gana = !is.na(inc_apellidos) & ganador_apellidos == inc_apellidos & ganador_persona != inc_persona,
         inc_gana    = !is.na(inc_persona) & inc_persona == ganador_persona,
         escano_abierto = !is.na(inc_persona) & !inc_compite,
         retiene_partido = !is.na(inc_partido) & inc_partido == ganador_partido,
         retiene_pacto   = !is.na(inc_pacto) & inc_pacto == ganador_pacto,
         # Los nombres de pactos cambian entre elecciones: la retencion comparable
         # es por familia politica y por eje (ver asignar_familias en 00_funciones.R).
         retiene_familia = !is.na(inc_familia) & !inc_familia %in% SIN_SECTOR & inc_familia == ganador_familia,
         retiene_eje     = !is.na(inc_eje) & inc_eje != "Otros e independientes" & inc_eje == ganador_eje,
         # Version estricta: excluye familias imputadas por el pacto (independientes
         # sin cupo identificado), tanto del incumbente como del ganador.
         familia_estricta = !inc_fuente %in% "pacto" & !ganador_fuente %in% "pacto") |>
  select(-personas, -apellidos_t)

# Etiqueta con que compite el incumbente en t (puede cambiar de partido entre
# elecciones: un alcalde que gana con otro partido no "retiene" su familia).
etiqueta_t <- cand |> distinct(anio, comuna, inc_persona = persona, .keep_all = TRUE) |>
  select(anio, comuna, inc_persona, inc_partido_t = partido, inc_familia_t = familia)
panel <- panel |> left_join(etiqueta_t, by = c("anio", "comuna", "inc_persona"))

# 3. Limite de reeleccion (Ley 21.238) -----------------------------------------
# Inhabilitado en t quien ejercio el cargo los tres periodos anteriores.
# Criterio del SERVEL (acuerdo O547, 3-7-2024, que recoge el O541): un periodo
# cuenta si se ejercio mas de la mitad del mandato, sea como electo o como
# reemplazante. Mitades: 6-12-2014 (2012-2016), 18-3-2019 (2016-2021, mandato
# extendido al 28-6-2021) y 18-3-2023 (2021-2024). Si el reemplazante asumio
# antes de la mitad (cuenta_periodo = TRUE en reemplazos.csv), el periodo es
# suyo y no del electo.
titular <- res |> select(anio, comuna, ganador_persona)
if (!is.null(reemp) && nrow(reemp) > 0 && "cuenta_periodo" %in% names(reemp)) {
  r_tit <- reemp |> filter(cuenta_periodo, !pendiente) |>
    transmute(anio = map_int(anio, prev), comuna, r_persona)
  titular <- titular |> left_join(r_tit, by = c("anio", "comuna")) |>
    mutate(ganador_persona = coalesce(r_persona, ganador_persona)) |> select(-r_persona)
  message(nrow(r_tit), " periodos asignados al reemplazante por haber ejercido mas de la mitad del mandato")
}
gan_w <- titular |>
  pivot_wider(names_from = anio, values_from = ganador_persona, names_prefix = "g")

# La Ley 21.238 (Diario Oficial, 8-7-2020) rige desde la eleccion de 2021 y
# cuenta los periodos anteriores a su publicacion: en 2021 quedaron inhabilitados
# quienes ejercieron 2008-2012, 2012-2016 y 2016-2021.
LIMITE_DESDE <- 2021L
inhab <- function(t) {
  i <- match(t, anios); if (is.na(i) || i < 4 || t < LIMITE_DESDE) return(NULL)
  a <- paste0("g", anios[(i - 3):(i - 1)])
  gan_w |> filter(!is.na(.data[[a[1]]]), .data[[a[1]]] == .data[[a[2]]], .data[[a[2]]] == .data[[a[3]]]) |>
    transmute(anio = t, comuna, inhabilitado = TRUE)
}
inh <- bind_rows(map(anios, inhab))

# Inhabilitados segun listas publicas (data/homologacion/inhabilitados_prensa.csv:
# anio;comuna;alcalde;fuente). Cubre 2021 cuando falta el archivo de 2008. Si el
# panel tambien permite calcularlos, se contrastan ambas listas.
f_inh <- file.path("data", "homologacion", "inhabilitados_prensa.csv")
if (file.exists(f_inh)) {
  prensa <- read.csv2(f_inh, fileEncoding = "UTF-8", colClasses = "character") |>
    transmute(anio = as.integer(anio), comuna = clave(comuna), alcalde)
  h_inh <- file.path("data", "homologacion", "comunas.csv")
  if (file.exists(h_inh)) prensa <- prensa |>
    left_join(read.csv2(h_inh, fileEncoding = "UTF-8") |> mutate(across(everything(), clave)),
              by = c("comuna" = "nombre_servel")) |> mutate(comuna = coalesce(comuna.y, comuna)) |> select(-any_of("comuna.y"))
  sin_par <- setdiff(prensa$comuna, unique(res$comuna))
  if (length(sin_par)) message("Comunas de la lista de prensa sin par en el panel: ", paste(sin_par, collapse = ", "))
  calc_anios <- unique(inh$anio)
  contraste <- full_join(prensa |> mutate(prensa = TRUE),
                         inh |> filter(anio %in% unique(prensa$anio)) |> select(anio, comuna) |> mutate(datos = TRUE),
                         by = c("anio", "comuna")) |>
    mutate(across(c(prensa, datos), ~ coalesce(.x, FALSE))) |> filter(prensa != datos | !anio %in% calc_anios)
  write.csv2(contraste, file.path(DIR_TAB, "02_inhabilitados_contraste.csv"), row.names = FALSE)
  inh <- bind_rows(inh, prensa |> select(anio, comuna) |> mutate(inhabilitado = TRUE)) |>
    distinct(anio, comuna, .keep_all = TRUE)
  message(nrow(prensa), " inhabilitados segun prensa incorporados; ",
          sum(contraste$anio %in% calc_anios), " discrepancias con los datos (ver 02_inhabilitados_contraste.csv)")
}
panel <- panel |>
  left_join(inh, by = c("anio", "comuna")) |>
  # Si el electo inhabilitado fue reemplazado durante el periodo, el incumbente
  # real es el reemplazante, que si puede postular: la eleccion no es abierta
  # por limite legal (El Monte y Florida en 2024).
  mutate(inhabilitado = coalesce(inhabilitado, FALSE) & !reemplazante,
         tipo_eleccion = case_when(
           is.na(inc_persona)  ~ NA_character_,
           reemplazo_pendiente ~ "Reemplazo sin identificar",
           inhabilitado        ~ "Abierta por limite legal",
           escano_abierto      ~ "Abierta por decision propia",
           TRUE                ~ "Incumbente compite"))

# Proyeccion 2028: gano las tres ultimas elecciones del panel.
if (length(anios) >= 3) {
  a <- paste0("g", tail(anios, 3))
  inh28 <- gan_w |> filter(!is.na(.data[[a[1]]]), .data[[a[1]]] == .data[[a[2]]], .data[[a[2]]] == .data[[a[3]]]) |>
    left_join(res |> filter(anio == max(anios)) |> select(comuna, region, ganador, ganador_partido),
              by = "comuna") |>
    select(region, comuna, alcalde = ganador, partido = ganador_partido)
  write.csv2(inh28, file.path(DIR_TAB, "02_inhabilitados_2028.csv"), row.names = FALSE)
  message("Alcaldes inhabilitados para 2028 (ejercio los periodos iniciados en ", paste(tail(anios, 3), collapse = ", "), "): ", nrow(inh28))
}

# 4. Salidas y controles -------------------------------------------------------
saveRDS(panel, file.path(DIR_PROC, "panel_comuna.rds"))
write.csv2(panel, file.path(DIR_TAB, "02_panel_comuna.csv"), row.names = FALSE)
write.csv2(panel |> filter(inhabilitado) |> select(anio, region, comuna, inc, inc_partido, ganador, ganador_partido, retiene_partido),
           file.path(DIR_TAB, "02_inhabilitados_2024.csv"), row.names = FALSE)

ret <- panel |> filter(!is.na(tipo_eleccion)) |>
  group_by(anio, tipo_eleccion) |>
  summarise(comunas = n(),
            inc_gana = mean(inc_gana),
            # Retencion sobre incumbentes con partido: un alcalde independiente no
            # tiene familia que retener (se informa su numero aparte).
            inc_sin_sector = sum(inc_familia %in% SIN_SECTOR),
            con_sector = sum(!is.na(inc_familia) & !inc_familia %in% SIN_SECTOR),
            # (las columnas estrictas van antes: summarise sobrescribe retiene_familia)
            con_sector_estricto = sum(familia_estricta & !is.na(inc_familia) & !inc_familia %in% SIN_SECTOR),
            retiene_familia_estricta = mean(retiene_familia[familia_estricta & !is.na(inc_familia) & !inc_familia %in% SIN_SECTOR]),
            retiene_familia = mean(retiene_familia[!is.na(inc_familia) & !inc_familia %in% SIN_SECTOR]),
            retiene_eje = mean(retiene_eje[!is.na(inc_familia) & !inc_familia %in% SIN_SECTOR]),
            inc_sin_clasificar = sum(is.na(inc_familia)),
            n_candidatos = mean(n_candidatos), pct_nulo_blanco = mean(pct_nulo_blanco), .groups = "drop")
print(as.data.frame(ret))
write.csv2(ret, file.path(DIR_TAB, "02_retencion_por_tipo.csv"), row.names = FALSE)

# Independientes con familia imputada por el pacto (sin cupo identificado):
# ganadores e incumbentes, para completar data/homologacion/cupos.csv con el
# partido en cuyo cupo fueron (declaracion de candidaturas del SERVEL o prensa).
indep <- cand |> filter(fuente_familia == "pacto") |>
  semi_join(bind_rows(ganador |> transmute(anio, comuna, candidato = ganador),
                      cand |> filter(rank <= 2) |> select(anio, comuna, candidato)),
            by = c("anio", "comuna", "candidato")) |>
  distinct(anio, region, comuna, candidato, partido, pacto, familia_imputada = familia) |>
  mutate(partido_cupo = "") |> arrange(anio, region, comuna)
write.csv2(indep, file.path(DIR_TAB, "02_independientes_en_pacto.csv"), row.names = FALSE)
message(nrow(indep), " independientes (ganadores o segundos) con familia imputada por pacto: ver 02_independientes_en_pacto.csv")

# Posibles familiares: otra persona con los mismos apellidos que el incumbente
# compite en t. Verificar cada caso (pueden ser homonimos o parientes).
familiares <- panel |> filter(familiar_compite) |>
  left_join(cand |> select(anio, comuna, candidato, persona, apellidos), by = c("anio", "comuna"), relationship = "many-to-many") |>
  filter(apellidos == inc_apellidos, persona != inc_persona) |>
  select(anio, comuna, incumbente = inc, candidato_familiar = candidato, gana = familiar_gana, tipo_eleccion)
write.csv2(familiares, file.path(DIR_TAB, "02_posibles_familiares.csv"), row.names = FALSE)
message(nrow(familiares), " candidaturas de posibles familiares del incumbente: revisar 02_posibles_familiares.csv")

sin_fam <- cand |> filter(is.na(familia)) |> count(anio, partido, sort = TRUE)
write.csv2(sin_fam, file.path(DIR_TAB, "02_partidos_sin_familia.csv"), row.names = FALSE)
if (nrow(sin_fam) > 0) message(nrow(sin_fam), " partidos sin familia asignada: revisar 02_partidos_sin_familia.csv")

sinpar <- res |> group_by(comuna) |> summarise(n = n(), anios_presentes = paste(sort(anio), collapse = ","), region = first(region)) |>
  filter(n < length(anios))
write.csv2(sinpar, file.path(DIR_TAB, "02_comunas_sin_par.csv"), row.names = FALSE)
if (nrow(sinpar) > 0) message(nrow(sinpar), " comunas no aparecen en todos los anos: homologar en data/homologacion/comunas.csv")
