# =============================================================================
# 04_regresion_escanos_abiertos.R
# -----------------------------------------------------------------------------
# Pregunta: ¿el escaño abierto por el límite de reelección (Ley 21.238) se
# comporta distinto del escaño que el alcalde deja por decisión propia?
#
# A. Retención (familia y eje) en escaños abiertos: modelo lineal de
#    probabilidad con efectos fijos de año. El efecto del límite se identifica
#    solo dentro de 2024 (único año con ambos tipos); los años 2016 y 2021
#    aportan a estimar los controles y el nivel de base.
# B. Oferta y participación (n.º de candidatos, nulo y blanco, participación)
#    en todas las elecciones con incumbente identificado, según el tipo.
# Errores estándar agrupados por comuna (la misma comuna aparece en varios años).
#
# Entrada  data/processed/panel_comuna.rds (script 02)
# Salidas  output/tablas/04_regresiones.csv, 04_descriptivos_abiertos.csv
# =============================================================================

source(file.path("R", "00_funciones.R"))
DIR_PROC <- file.path("data", "processed"); DIR_TAB <- file.path("output", "tablas")
panel <- readRDS(file.path(DIR_PROC, "panel_comuna.rds"))
SIN_SECTOR <- c("Independiente", "Regional y otros", "Populismo")

# mco_cluster(): en 00_funciones.R.
tiene <- function(v) v %in% names(panel) && any(!is.na(panel[[v]]))
panel <- panel |>
  mutate(limite = as.integer(tipo_eleccion == "Abierta por limite legal"),
         anio_f = factor(anio),
         log_inscritos = if (tiene("inscritos")) log(inscritos) else NA_real_)
controles <- c("inc_margen_prev", if (tiene("inscritos")) "log_inscritos")

# A. Retencion en escanos abiertos ---------------------------------------------
abiertos <- panel |>
  filter(tipo_eleccion %in% c("Abierta por limite legal", "Abierta por decision propia"),
         !is.na(inc_familia), !inc_familia %in% SIN_SECTOR) |>
  mutate(across(c(retiene_familia, retiene_eje), as.integer))

desc <- abiertos |> group_by(anio, tipo_eleccion) |>
  summarise(comunas = n(), retiene_familia = mean(retiene_familia), retiene_eje = mean(retiene_eje),
            margen_prev = mean(inc_margen_prev, na.rm = TRUE), n_candidatos = mean(n_candidatos), .groups = "drop")
print(as.data.frame(desc))
write.csv2(desc, file.path(DIR_TAB, "04_descriptivos_abiertos.csv"), row.names = FALSE)

f <- function(y, x) as.formula(paste(y, "~", paste(x, collapse = " + ")))
especif <- list(
  "A1 año"                  = list(datos = abiertos, x = c("limite", "anio_f")),
  "A2 año + controles"      = list(datos = abiertos, x = c("limite", "anio_f", controles)),
  "A3 año + controles + región" = list(datos = abiertos, x = c("limite", "anio_f", controles, "region")),
  "A4a solo 2021 + controles"  = list(datos = filter(abiertos, anio == 2021), x = c("limite", controles, "anio_f")),
  "A4b solo 2024 + controles"  = list(datos = filter(abiertos, anio == 2024), x = c("limite", controles, "anio_f")),
  "A5 estricta (sin familias imputadas)" = list(datos = filter(abiertos, familia_estricta), x = c("limite", "anio_f", controles)),
  "A6 sin reemplazos"       = list(datos = filter(abiertos, !reemplazante), x = c("limite", "anio_f", controles)))

resA <- imap_dfr(especif, function(s, nombre) {
  map_dfr(c("retiene_familia", "retiene_eje"), function(y)
    mco_cluster(f(y, s$x), s$datos, "comuna") |> mutate(modelo = nombre, resultado = y))
})

# B. Oferta y participacion, todas las elecciones con incumbente ----------------
todas <- panel |>
  filter(tipo_eleccion %in% c("Abierta por limite legal", "Abierta por decision propia", "Incumbente compite")) |>
  mutate(tipo = relevel(factor(tipo_eleccion), ref = "Incumbente compite"))
ys_B <- c("n_candidatos", "pct_nulo_blanco", if (tiene("participacion")) "participacion")
resB <- map_dfr(ys_B, function(y) bind_rows(
  mco_cluster(f(y, c("tipo", "anio_f")), todas, "comuna") |> mutate(modelo = "B1 año"),
  mco_cluster(f(y, c("tipo", "anio_f", controles)), todas, "comuna") |> mutate(modelo = "B2 año + controles")) |>
  mutate(resultado = y))

res <- bind_rows(resA, resB) |>
  filter(str_detect(termino, "^limite$|^tipo|inc_margen_prev|log_inscritos")) |>
  select(resultado, modelo, termino, coef, ee, p, n, comunas)
print(as.data.frame(res |> mutate(across(c(coef, ee, p), ~ round(.x, 3)))))
write.csv2(res, file.path(DIR_TAB, "04_regresiones.csv"), row.names = FALSE)

# C. Diferencia directa entre los dos tipos de escano abierto ------------------
# Misma especificacion B2 con la salida voluntaria como categoria base: el
# coeficiente del limite es la diferencia limite - voluntaria.
resC <- map_dfr(ys_B, function(y)
  mco_cluster(f(y, c("tipo", "anio_f", controles)),
              todas |> mutate(tipo = relevel(factor(tipo_eleccion), ref = "Abierta por decision propia")),
              "comuna") |>
    filter(termino == "tipoAbierta por limite legal") |>
    mutate(resultado = y, modelo = "C limite vs voluntaria (B2)"))

# D. Placebo y diferencias en diferencias por ola del limite ------------------
# El limite rige desde 2021 (Ley 21.238, con efecto retroactivo): hay dos olas,
# 2021 y 2024. Para cada ola g: tratadas = escano abierto por limite en g;
# control = el incumbente compite en g y la comuna no tuvo un escano abierto por
# limite antes de g. Placebo: en la eleccion anterior, cuando el alcalde aun
# competia, ¿ya eran distintas? Dif-en-dif: cambio entre la eleccion anterior y g.
olas <- panel |> filter(tipo_eleccion == "Abierta por limite legal") |> distinct(anio) |> pull(anio) |> sort()
anios_p <- sort(unique(panel$anio))
pila <- map_dfr(olas, function(g) {
  gp <- anios_p[match(g, anios_p) - 1]
  trat <- panel |> filter(anio == g, tipo_eleccion == "Abierta por limite legal") |> pull(comuna)
  antes <- panel |> filter(anio < g, tipo_eleccion == "Abierta por limite legal") |> pull(comuna)
  ctrl <- panel |> filter(anio == g, tipo_eleccion == "Incumbente compite", !comuna %in% antes) |> pull(comuna)
  panel |> filter(anio %in% c(gp, g), comuna %in% c(trat, ctrl)) |>
    mutate(ola = g, tratada = as.integer(comuna %in% trat), post = as.integer(anio == g),
           trat_post = tratada * post)
})
resD1 <- map_dfr(olas, function(g) map_dfr(ys_B, function(y)
  mco_cluster(f(y, c("tratada", controles)),
              filter(pila, ola == g, post == 0, tipo_eleccion == "Incumbente compite"), "comuna") |>
    filter(termino == "tratada") |> mutate(resultado = y, modelo = paste("D1 placebo eleccion previa, ola", g))))
resD2 <- map_dfr(ys_B, function(y) bind_rows(
  map_dfr(olas, function(g)
    mco_cluster(f(y, c("trat_post", "post", "comuna_f")), filter(pila, ola == g) |> mutate(comuna_f = factor(comuna)), "comuna") |>
      filter(termino == "trat_post") |> mutate(modelo = paste("D2 dif-en-dif, ola", g))),
  mco_cluster(f(y, c("trat_post", "ola_post", "ola_comuna")),
              pila |> mutate(ola_post = factor(paste(ola, post)), ola_comuna = factor(paste(ola, comuna))), "comuna") |>
    filter(termino == "trat_post") |> mutate(modelo = "D3 dif-en-dif apilado (ambas olas)")) |>
  mutate(resultado = y))

# E. Inferencia por permutacion para la retencion ----------------------------
# Se reasigna al azar la etiqueta de limite entre los escanos abiertos de cada
# ano con limite (manteniendo cuantos hay de cada tipo por ano) y se reestima.
set.seed(2024)
ab_l <- filter(abiertos, anio %in% olas)
perm <- map_dfr(c("retiene_familia", "retiene_eje"), function(y) {
  x <- c("limite", controles, if (length(olas) > 1) "anio_f")
  obs <- mco_cluster(f(y, x), ab_l, "comuna") |> filter(termino == "limite") |> pull(coef)
  cc <- complete.cases(ab_l[, c(y, "limite", controles)])
  d0 <- ab_l[cc, ]
  sim <- replicate(2000, {
    d <- d0 |> group_by(anio) |> mutate(limite = sample(limite)) |> ungroup()
    coef(lm(f(y, x), data = d))["limite"] })
  tibble(resultado = y, modelo = paste0("E permutacion dentro de ", paste(olas, collapse = " y "), " (2000)"),
         termino = "limite", coef = obs, ee = NA_real_, p = mean(abs(sim) >= abs(obs)), n = nrow(d0), comunas = n_distinct(d0$comuna))
})

extra <- bind_rows(resC, resD1, resD2, perm) |> select(resultado, modelo, termino, coef, ee, p, n, comunas)
print(as.data.frame(extra |> mutate(across(c(coef, ee, p), ~ round(.x, 3)))))
write.csv2(bind_rows(res, extra), file.path(DIR_TAB, "04_regresiones.csv"), row.names = FALSE)
