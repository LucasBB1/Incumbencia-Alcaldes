# =============================================================================
# 03_rdd_incumbencia.R
# -----------------------------------------------------------------------------
# Ventaja del incumbente con regresion discontinua en elecciones estrechas
# (diseno de Lee, 2008; aplicado a Chile por Munoz, 2021).
# Unidad: los dos primeros candidatos de cada comuna en la eleccion t-1.
# Variable de asignacion: margen en t-1 (positivo si gano, negativo si perdio).
# Resultados en t: (a) gana la alcaldia, incondicional; (b) compite.
# Nivel partido: el partido del candidato gana en t.
# Se excluyen los incumbentes inhabilitados por la Ley 21.238 (no podian competir).
#
# Paquete: rdrobust (Calonico, Cattaneo y Titiunik). Si no esta instalado, se
# usa una regresion lineal local con ancho de banda fijo como aproximacion.
# Salida: output/tablas/03_rdd_resultados.csv
# =============================================================================

source(file.path("R", "00_funciones.R"))
DIR_PROC <- file.path("data", "processed"); DIR_TAB <- file.path("output", "tablas")
votos <- readRDS(file.path(DIR_PROC, "alcaldes_comuna.rds"))
panel <- readRDS(file.path(DIR_PROC, "panel_comuna.rds"))
anios <- sort(unique(votos$anio))

cand <- votos |>
  filter(tipo_voto == "valido") |>
  filter(comuna != "ANTARTICA") |>
  mutate(partido = coalesce(partido, "SIN PARTIDO")) |>
  asignar_persona() |>
  group_by(anio, comuna) |>
  mutate(pct = votos / sum(votos), rank = rank(-votos, ties.method = "first")) |>
  ungroup()

top2 <- cand |> filter(rank <= 2) |>
  group_by(anio, comuna) |>
  mutate(margen = if (n() == 2) ifelse(rank == 1, pct - pct[rank == 2], pct - pct[rank == 1]) else NA_real_) |>
  ungroup() |> filter(!is.na(margen))

gan_t <- panel |> select(anio, comuna, ganador_persona, ganador_partido)
comp_t <- cand |> select(anio, comuna, persona) |> distinct() |> mutate(compite = TRUE)
# Excluidos: ganadores que no podian competir en t, por limite legal o porque
# dejaron el cargo durante el periodo (fallecimiento, destitucion, etc.).
inh <- panel |> filter(inhabilitado | reemplazante) |> select(anio, comuna) |> mutate(inhab = TRUE)

base <- map_dfr(anios[-1], function(t) {
  tp <- anios[match(t, anios) - 1]
  top2 |> filter(anio == tp) |>
    transmute(eleccion = t, comuna, persona, partido, margen) |>
    left_join(gan_t |> filter(anio == t) |> select(-anio), by = "comuna") |>
    left_join(comp_t |> filter(anio == t) |> select(-anio), by = c("comuna", "persona")) |>
    left_join(inh |> filter(anio == t) |> select(-anio), by = "comuna") |>
    mutate(compite = coalesce(compite, FALSE),
           gana = !is.na(ganador_persona) & persona == ganador_persona,
           partido_gana = !is.na(ganador_partido) & partido == ganador_partido & partido != "INDEPENDIENTE",
           inhab = coalesce(inhab, FALSE))
}) |>
  filter(!(inhab & margen > 0))   # el ganador inhabilitado no podia competir

saveRDS(base, file.path(DIR_PROC, "rdd_base.rds"))   # para figuras y robustez (script 05)

estimar <- function(d, y) {
  if (requireNamespace("rdrobust", quietly = TRUE)) {
    r <- rdrobust::rdrobust(as.numeric(d[[y]]), d$margen, c = 0)
    tibble(efecto = r$coef[1], ee = r$se[3], p = r$pv[3], h = r$bws[1, 1], n_izq = r$N_h[1], n_der = r$N_h[2],
           metodo = "rdrobust (robusto, sesgo corregido)")
  } else {
    h <- 0.10; dd <- d |> filter(abs(margen) <= h) |> mutate(D = margen > 0, w = 1 - abs(margen) / h)
    m <- lm(as.numeric(dd[[y]]) ~ D * margen, data = dd, weights = w)
    s <- summary(m)$coefficients["DTRUE", ]
    tibble(efecto = s[1], ee = s[2], p = s[4], h = h, n_izq = sum(!dd$D), n_der = sum(dd$D),
           metodo = "lineal local, h = 0,10 (instalar rdrobust)")
  }
}

resultados <- expand_grid(eleccion = c("todas", as.character(anios[-1])),
                          resultado = c("gana", "compite", "partido_gana")) |>
  mutate(est = map2(eleccion, resultado, function(e, y) {
    d <- if (e == "todas") base else filter(base, eleccion == as.integer(e))
    if (nrow(d) < 50) return(tibble(efecto = NA_real_))
    tryCatch(estimar(d, y), error = function(err) tibble(efecto = NA_real_, metodo = conditionMessage(err)))
  })) |>
  unnest(est)

print(as.data.frame(resultados))
write.csv2(resultados, file.path(DIR_TAB, "03_rdd_resultados.csv"), row.names = FALSE)
