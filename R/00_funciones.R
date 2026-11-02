# =============================================================================
# 00_funciones.R - Funciones compartidas para leer los archivos del SERVEL.
# Adaptadas del proyecto "Analisis-Electoral-La-Reina-2016-2028" (script 01),
# sin filtro de comuna: aqui se leen las 345 comunas.
# =============================================================================

suppressPackageStartupMessages({
  library(readxl); library(readr); library(dplyr); library(tidyr)
  library(stringr); library(purrr)
})

# Homologa los encabezados del SERVEL, que cambian de un archivo a otro.
normalizar_nombres <- function(x) {
  x <- stringi::stri_trans_general(x, "Latin-ASCII") |>
    tolower() |>
    str_replace_all("[^a-z0-9]+", "_") |>
    str_replace_all("^_|_$", "")
  case_when(
    x %in% c("nro", "nro_region")             ~ "nro_region",
    str_detect(x, "senatorial")               ~ "circ_senatorial",
    str_detect(x, "provincial")               ~ "circ_provincial",
    str_detect(x, "^circ")                    ~ "circ_electoral",
    str_detect(x, "^nro_?voto")               ~ "nro_voto",
    str_detect(x, "^opcion")                  ~ "opcion",
    x %in% c("cargo", "carrgo", "electo", "nominado", "resultado", "seleccion") ~ "marca",
    x %in% c("votacion", "votantes")          ~ "votantes",
    TRUE                                      ~ x
  )
}

# Texto comparable: mayusculas, sin tildes, espacios simples.
clave <- function(x) {
  stringi::stri_trans_general(x, "Latin-ASCII") |> toupper() |> str_squish()
}

col_o_na <- function(df, nombre) {
  if (nombre %in% names(df)) df[[nombre]] else rep(NA_character_, nrow(df))
}

# Lee una hoja de un .xlsx del SERVEL: busca la fila de encabezado (la que
# contiene "Comuna") y descarta columnas vacias. Sin filtro de comuna.
leer_hoja_servel <- function(ruta, patron_hoja) {
  hojas <- excel_sheets(ruta)
  hoja  <- hojas[str_detect(hojas, patron_hoja) & !str_detect(hojas, "(?i)extranjero")][1]
  if (is.na(hoja)) stop("No se encontro la hoja '", patron_hoja, "' en ", basename(ruta),
                        ". Hojas disponibles: ", paste(hojas, collapse = " | "))
  df <- suppressMessages(
    read_excel(ruta, sheet = hoja, col_names = FALSE, col_types = "text",
               .name_repair = "minimal"))
  fila_hdr <- which(apply(df, 1, function(r) any(str_trim(r) == "Comuna", na.rm = TRUE)))[1]
  if (is.na(fila_hdr)) stop("Sin encabezado 'Comuna' en ", basename(ruta), " / ", hoja)
  hdr <- as.character(unlist(df[fila_hdr, ]))
  df  <- df[-(1:fila_hdr), !is.na(hdr) & str_trim(hdr) != "", drop = FALSE]
  names(df) <- normalizar_nombres(hdr[!is.na(hdr) & str_trim(hdr) != ""])
  df |> filter(!is.na(comuna), str_squish(comuna) != "")
}

clasificar_voto <- function(candidato) {
  case_when(
    is.na(candidato) | candidato == ""                      ~ "nulo_blanco",
    str_detect(clave(candidato), "^VOTOS? (EN )?BLANCOS?$") ~ "blanco",
    str_detect(clave(candidato), "^VOTOS? NULOS?$")         ~ "nulo",
    TRUE                                                    ~ "valido")
}

# Votacion comunal de alcalde en formato comun (una fila por comuna y candidato).
estandarizar_alcaldes <- function(df, anio) {
  nombre <- if ("candidato" %in% names(df)) {
    col_o_na(df, "candidato")
  } else if ("opcion" %in% names(df)) {
    col_o_na(df, "opcion")
  } else {
    str_c(coalesce(col_o_na(df, "nombres"), ""), " ",
          coalesce(col_o_na(df, "primer_apellido"), ""), " ",
          coalesce(col_o_na(df, "segundo_apellido"), ""))
  }
  tibble(
    anio      = anio,
    region    = str_squish(coalesce(col_o_na(df, "region"), col_o_na(df, "nro_region"))),
    comuna    = clave(col_o_na(df, "comuna")),
    pacto     = col_o_na(df, "pacto"),
    partido   = col_o_na(df, "partido"),
    # Algunos archivos traen el subpacto ("RENOVACION NACIONAL E INDEPENDIENTES"):
    # identifica el partido en cuyo cupo va un independiente.
    subpacto  = col_o_na(df, "subpacto"),
    candidato = str_squish(str_remove_all(nombre, "\"")),
    votos     = as.integer(round(as.numeric(col_o_na(df, "votos")))),
    marca     = col_o_na(df, "marca")
  ) |>
    mutate(across(c(pacto, partido, subpacto, marca),
                  ~ na_if(na_if(str_squish(str_remove_all(.x, "\"")), ""), "---")),
           tipo_voto = clasificar_voto(candidato),
           votos = coalesce(votos, 0L))
}

# Clave de persona para seguir candidatos entre elecciones: primer nombre +
# los dos ultimos tokens (apellidos). Tolera segundos nombres que aparecen o
# desaparecen ("JOSE MANUEL PALACIOS PARRA" = "JOSE MANUEL AUGUSTO PALACIOS PARRA").
# Los apellidos compuestos pueden fallar: revisar output/tablas/02_revisar_nombres.csv
clave_persona <- function(nombre) {
  t <- str_split(clave(str_replace_all(nombre, "-", " ")), " ")
  map_chr(t, function(x) {
    x <- x[x != ""]
    if (length(x) < 3) return(paste(x, collapse = "|"))
    paste(x[1], paste(tail(x, 2), collapse = " "), sep = "|")
  })
}

# Familia politica de un partido, a partir de su nombre oficial en el SERVEL.
# Clasificacion del autor, la misma del proyecto de La Reina, ampliada a los
# partidos con candidaturas a alcalde en el pais. Lo que no calza queda NA y se
# exporta para revision (output/tablas/02_partidos_sin_familia.csv).
dic_familias <- tribble(
  ~patron,                                                        ~familia,
  "^UNION DEMOCRATA INDEPENDIENTE",                               "Derecha",
  "^RENOVACION NACIONAL",                                         "Derecha",
  "^EVOLUCION POLITICA|^EVOPOLI",                                 "Derecha",
  "^PARTIDO REGIONALISTA (DE LOS )?INDEPENDIENTE",                "Derecha",
  "^CHILEPRIMERO|^CHILE PRIMERO",                                 "Derecha",
  "^PARTIDO REPUBLICANO",                                         "Ultraderecha",
  "^PARTIDO SOCIAL CRISTIANO|^PARTIDO CONSERVADOR CRISTIANO",     "Ultraderecha",
  "^PARTIDO NACIONAL LIBERTARIO",                                 "Ultraderecha",
  "^PARTIDO DEMOCRATA CRISTIANO",                                 "Centro",
  "^PARTIDO DEMOCRATAS|^DEMOCRATAS",                              "Centroderecha",
  "^(MOVIMIENTO )?AMARILLOS",                                     "Centroderecha",
  "^AMPLITUD",                                                    "Derecha",
  "^CENTRO UNIDO",                                                "Centro",
  "^PARTIDO SOCIALISTA",                                          "Centroizquierda",
  "^PARTIDO POR LA DEMOCRACIA",                                   "Centroizquierda",
  "^PARTIDO RADICAL",                                             "Centroizquierda",
  "^PARTIDO LIBERAL",                                             "Centroizquierda",
  "^PARTIDO PROGRESISTA",                                         "Centroizquierda",
  "^FEDERACION REGIONALISTA VERDE SOCIAL",                        "Centroizquierda",
  "^MOVIMIENTO AMPLIO SOCIAL|^MAS REGION",                        "Centroizquierda",
  "^IZQUIERDA CIUDADANA",                                         "Centroizquierda",
  "^CIUDADANOS",                                                  "Centroizquierda",
  "^PARTIDO COMUNISTA",                                           "Izquierda",
  "^FRENTE AMPLIO|^REVOLUCION DEMOCRATICA|^CONVERGENCIA SOCIAL|^COMUNES|^PARTIDO COMUNES", "Izquierda",
  "^PODER|^PARTIDO PODER",                                        "Izquierda",
  "^PARTIDO HUMANISTA|^PARTIDO ACCION HUMANISTA|^ACCION HUMANISTA", "Izquierda",
  "^(PARTIDO )?IGUALDAD",                                         "Izquierda",
  "^PARTIDO ECOLOGISTA VERDE|^PARTIDO ALIANZA VERDE POPULAR",     "Izquierda",
  "^UNION PATRIOTICA|^PARTIDO DE TRABAJADORES REVOLUCIONARIOS",   "Izquierda",
  "^PARTIDO DE LA GENTE",                                         "Populismo",
  # Partido Popular (2024): populismo de izquierda, cercano a la izquierda
  # radical (clasificacion del autor); se agrupa en la familia Izquierda.
  "^POPULAR$|^PARTIDO POPULAR",                                   "Izquierda",
  # Partidos regionales y minoritarios (clasificacion del autor).
  "REGIONALISTA AGRARIO",                                         "Centro",        # MIRAS: regionalista
  "REGIONAL PATAGONICA",                                          "Centro",        # regionalista, tercera via
  "REGIONALISTA DE MAGALLANES",                                   "Centro",        # regionalista y federalista
  "REGIONAL NORTE VERDE",                                         "Izquierda",     # regionalista
  "FRENTE REGIONAL Y POPULAR",                                    "Izquierda",     # regionalista
  "^WALLMAPUWEN",                                                 "Izquierda",     # indigenismo, laicismo, autonomismo
  "^UNIDOS RESULTA EN DEMOCRACIA",                                "Centroderecha", # antiestatismo
  "^NUEVO TIEMPO",                                                "Ultraderecha",
  "^PARTIDO NACIONAL CIUDADANO",                                  "Ultraderecha",
  # Sin orientacion asignable: fuera de la retencion.
  "^SOMOS AYSEN",                                                 "Regional y otros", # transversal
  "^PARTIDO FRENTE POPULAR",                                      "Regional y otros"  # sin registro de orientacion
)

familia_partido <- function(partido) {
  k <- clave(partido); out <- rep(NA_character_, length(k))
  for (i in seq_len(nrow(dic_familias))) {
    hit <- is.na(out) & !is.na(k) & str_detect(k, dic_familias$patron[i])
    out[hit] <- dic_familias$familia[i]
  }
  out
}

es_independiente <- function(partido) is.na(partido) | str_detect(clave(partido), "^INDEPENDIENTE|^SIN PARTIDO")

# Eje amplio para medir retencion entre sectores.
# Centroderecha (Democratas, Amarillos, Amplitud) va con la derecha: compiten en
# sus pactos. El PDC (Centro) va con centro e izquierda, como en el proyecto de La Reina.
eje_de <- function(familia) case_when(
  familia %in% c("Izquierda", "Centroizquierda", "Centro") ~ "Centro e izquierda",
  familia %in% c("Derecha", "Ultraderecha", "Centroderecha") ~ "Derecha",
  TRUE                                                     ~ "Otros e independientes")

# Asigna familia a cada candidatura: por su partido; si es independiente, por
# la familia predominante de los candidatos con partido de su pacto ese ano
# (pactos sin partidos o fuera de pacto = "Independiente").
asignar_familias <- function(cand, archivo_cupos = file.path("data", "homologacion", "cupos.csv")) {
  # Familia de cada candidatura, en este orden de prioridad:
  #  1. militancia: el partido del candidato;
  #  2. cupo: independiente que va en el cupo de un partido (afinidad con ese
  #     partido aunque no milite). Sale de data/homologacion/cupos.csv
  #     (anio;comuna;candidato;partido_cupo) o, si el archivo del SERVEL lo trae,
  #     del subpacto ("PARTIDO X E INDEPENDIENTES");
  #  3. pacto: independiente dentro de un pacto sin cupo identificado; recibe la
  #     familia modal de los militantes del pacto (aproximacion);
  #  4. sin pacto: independiente fuera de pacto, sin familia ("Independiente").
  # La columna fuente_familia guarda el criterio usado, para pruebas de robustez.
  if (!"subpacto" %in% names(cand)) cand$subpacto <- NA_character_
  cupos <- if (file.exists(archivo_cupos)) read.csv2(archivo_cupos, fileEncoding = "UTF-8") |>
    transmute(anio = as.integer(anio), comuna = clave(comuna), nombre = clave(candidato), partido_cupo) else
    tibble(anio = integer(), comuna = character(), nombre = character(), partido_cupo = character())
  ind <- es_independiente(cand$partido) |
    (!is.na(cand$partido) & str_detect(clave(cand$partido), "^INDEPENDIENTES? LISTA|^INDEPENDIENTES$"))
  cand <- cand |>
    mutate(.ind = ind, .nombre = clave(candidato),
           fam_partido = if_else(.ind, NA_character_, familia_partido(partido)),
           fam_subpacto = if_else(.ind, familia_partido(str_remove(clave(subpacto), " E INDEPENDIENTES.*$")), NA_character_)) |>
    left_join(cupos, by = c("anio", "comuna", ".nombre" = "nombre")) |>
    mutate(fam_cupo = coalesce(familia_partido(partido_cupo), fam_subpacto))
  modal_pacto <- cand |>
    filter(!is.na(fam_partido), !is.na(pacto)) |>
    count(anio, pacto, fam_partido) |>
    group_by(anio, pacto) |> slice_max(n, n = 1, with_ties = FALSE) |> ungroup() |>
    select(anio, pacto, fam_pacto = fam_partido)
  cand |>
    left_join(modal_pacto, by = c("anio", "pacto")) |>
    mutate(familia = case_when(
             !is.na(fam_partido)        ~ fam_partido,
             .ind & !is.na(fam_cupo)    ~ fam_cupo,
             .ind & !is.na(fam_pacto)   ~ fam_pacto,
             .ind                       ~ "Independiente",
             TRUE                       ~ NA_character_),
           fuente_familia = case_when(
             !is.na(fam_partido)        ~ "militancia",
             .ind & !is.na(fam_cupo)    ~ "cupo",
             .ind & !is.na(fam_pacto)   ~ "pacto",
             .ind                       ~ "sin pacto",
             TRUE                       ~ NA_character_),
           eje = eje_de(familia)) |>
    select(-.ind, -.nombre, -fam_partido, -fam_subpacto, -fam_cupo, -fam_pacto, -partido_cupo)
}

# Identidad de las personas entre elecciones, dentro de cada comuna. Dos nombres
# son la misma persona si comparten los dos apellidos y al menos un nombre de
# pila ("LUIS FELIPE DELPIN AGUILAR" = "FELIPE DELPIN AGUILAR"). Las
# equivalencias que no cumplen esa regla (apodos: "CATHY" por "CATHERINE") se
# declaran en data/homologacion/personas.csv (comuna;nombre_a;nombre_b).
# Personas distintas con los mismos dos apellidos se marcan como posible
# familiar (hermanos o parientes que suceden al alcalde).
partes_nombre <- function(nombre) {
  t <- str_split(clave(str_replace_all(nombre, "-", " ")), " ")[[1]]
  t <- t[t != ""]
  if (length(t) < 3) return(list(pila = t[1], apellidos = paste(t[-1], collapse = " ")))
  list(pila = head(t, -2), apellidos = paste(tail(t, 2), collapse = " "))
}

asignar_persona <- function(cand, archivo_equiv = file.path("data", "homologacion", "personas.csv")) {
  equiv <- if (file.exists(archivo_equiv)) read.csv2(archivo_equiv, fileEncoding = "UTF-8") |>
    mutate(across(everything(), clave)) else tibble(comuna = character(), nombre_a = character(), nombre_b = character())
  nombres <- cand |> distinct(comuna, nombre = clave(candidato))
  ids <- nombres |> group_by(comuna) |> group_modify(function(d, g) {
    n <- nrow(d); id <- seq_len(n); p <- lapply(d$nombre, partes_nombre)
    raiz <- function(i) { while (id[i] != i) i <- id[i]; i }
    unir <- function(i, j) { a <- raiz(i); b <- raiz(j); if (a != b) id[max(a, b)] <<- min(a, b) }
    if (n > 1) for (i in 1:(n - 1)) for (j in (i + 1):n) {
      if (p[[i]]$apellidos == p[[j]]$apellidos && length(intersect(p[[i]]$pila, p[[j]]$pila)) > 0) unir(i, j)
    }
    e <- equiv |> filter(comuna == g$comuna)
    for (k in seq_len(nrow(e))) {
      i <- match(e$nombre_a[k], d$nombre); j <- match(e$nombre_b[k], d$nombre)
      if (!is.na(i) && !is.na(j)) unir(i, j)
    }
    r <- vapply(seq_len(n), raiz, integer(1))
    tibble(nombre = d$nombre, persona = paste(g$comuna, d$nombre[r], sep = "|"),
           apellidos = vapply(p, `[[`, character(1), "apellidos"))
  }) |> ungroup()
  cand |> mutate(nombre = clave(candidato)) |>
    left_join(ids, by = c("comuna", "nombre")) |>
    select(-nombre)
}

# -----------------------------------------------------------------------------
# Lineal con errores agrupados (CR1, como Stata) sin dependencias externas.
mco_cluster <- function(formula, datos, cluster) {
  # Quita factores con un solo nivel en la muestra (p. ej., anio en "solo 2024").
  vars <- all.vars(formula); y <- vars[1]
  x <- Filter(function(v) !(is.character(datos[[v]]) || is.factor(datos[[v]])) ||
                length(unique(na.omit(datos[[v]]))) > 1, vars[-1])
  formula <- as.formula(paste(y, "~", paste(x, collapse = " + ")))
  datos <- datos[complete.cases(datos[, c(y, x)]), , drop = FALSE]
  m <- lm(formula, data = datos)
  X <- model.matrix(m); e <- residuals(m); g <- datos[[cluster]]
  X <- X[, !is.na(coef(m)), drop = FALSE]
  n <- nrow(X); k <- ncol(X); G <- length(unique(g))
  bread <- solve(crossprod(X))
  meat <- Reduce(`+`, lapply(split(seq_len(n), g), function(i) {
    s <- crossprod(X[i, , drop = FALSE], e[i]); s %*% t(s) }))
  V <- (G / (G - 1)) * ((n - 1) / (n - k)) * bread %*% meat %*% bread
  b <- coef(m)[!is.na(coef(m))]; se <- sqrt(diag(V))
  tibble(termino = names(b), coef = b, ee = se,
         p = 2 * pt(abs(b / se), df = G - 1, lower.tail = FALSE),
         n = n, comunas = G)
}

