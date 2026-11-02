# =============================================================================
# 05_figuras_tablas.R
# -----------------------------------------------------------------------------
# Figuras y tablas del articulo, y pruebas de robustez pendientes.
#
# 1. Estudio de eventos apilado por ola del limite (2021 y 2024).
# 2. Coeficientes de retencion por especificacion (desde 04_regresiones.csv).
# 3. Regresion discontinua: grafico por intervalos y robustez (rddensity,
#    umbrales placebo, anchos de banda), si rdrobust/rddensity estan instalados.
# 4. Incumbentes que compiten con otra etiqueta partidaria.
# 5. Tabla 1 descriptiva por tipo de eleccion y ano.
#
# Requiere ggplot2. Entradas: panel_comuna.rds (02), rdd_base.rds (03),
# 04_regresiones.csv (04). Salidas: output/figuras/*.png y output/tablas/05_*.csv
# =============================================================================

source(file.path("R", "00_funciones.R"))
library(ggplot2)
DIR_PROC <- file.path("data", "processed"); DIR_TAB <- file.path("output", "tablas")
DIR_FIG <- file.path("output", "figuras"); dir.create(DIR_FIG, showWarnings = FALSE, recursive = TRUE)
panel <- readRDS(file.path(DIR_PROC, "panel_comuna.rds"))
SIN_SECTOR <- c("Independiente", "Regional y otros", "Populismo")

ACENTO <- "#1F5A85"; GRIS <- "#8C8C8C"
tema <- theme_minimal(base_size = 11) +
  theme(panel.grid.minor = element_blank(), legend.position = "bottom",
        plot.title.position = "plot", plot.caption = element_text(color = "grey40", size = 8))
guardar <- function(g, nombre, w = 7, h = 4.5) ggsave(file.path(DIR_FIG, nombre), g, width = w, height = h, dpi = 300, bg = "white")

# 1. Estudio de eventos apilado por ola del limite ------------------------------
# El limite rige desde 2021: olas 2021 y 2024. Para cada ola g, tratadas = escano
# abierto por limite en g; control = el incumbente compite en g y la comuna no
# tuvo un escano abierto por limite antes. Se usan todas las elecciones hasta g,
# en tiempo relativo (k = elecciones antes de g; referencia k = -1), con efectos
# fijos de ola x comuna y ola x ano.
anios_p <- sort(unique(panel$anio))
olas <- panel |> filter(tipo_eleccion == "Abierta por limite legal") |> distinct(anio) |> pull(anio) |> sort()
ev <- map_dfr(olas, function(g) {
  trat <- panel |> filter(anio == g, tipo_eleccion == "Abierta por limite legal") |> pull(comuna)
  antes <- panel |> filter(anio < g, tipo_eleccion == "Abierta por limite legal") |> pull(comuna)
  ctrl <- panel |> filter(anio == g, tipo_eleccion == "Incumbente compite", !comuna %in% antes) |> pull(comuna)
  panel |> filter(anio <= g, comuna %in% c(trat, ctrl)) |>
    mutate(ola = g, tratada = as.integer(comuna %in% trat),
           k = match(anio, anios_p) - match(g, anios_p),
           grupo = if_else(tratada == 1, "Alcalde inhabilitado", "Incumbente compite"))
}) |> mutate(ola_anio = factor(paste(ola, anio)), ola_comuna = factor(paste(ola, comuna)))
ks <- setdiff(sort(unique(ev$k)), -1)
for (k in ks) ev[[paste0("k", ifelse(k < 0, paste0("m", abs(k)), k))]] <- as.integer(ev$tratada == 1 & ev$k == k)
tt <- grep("^k(m[0-9]+|[0-9]+)$", names(ev), value = TRUE)

ys_ev <- c("n_candidatos", if ("participacion" %in% names(ev) && any(!is.na(ev$participacion))) "participacion")
es <- map_dfr(ys_ev, function(y) bind_rows(
  mco_cluster(as.formula(paste(y, "~", paste(c(tt, "ola_anio", "ola_comuna"), collapse = " + "))), ev, "comuna") |>
    filter(termino %in% tt) |> mutate(muestra = "Ambas olas"),
  map_dfr(olas, function(g) {
    d <- filter(ev, ola == g); t_g <- tt[sapply(tt, function(v) any(d[[v]] == 1))]
    mco_cluster(as.formula(paste(y, "~", paste(c(t_g, "ola_anio", "ola_comuna"), collapse = " + "))), d, "comuna") |>
      filter(termino %in% t_g) |> mutate(muestra = paste("Ola", g)) })) |>
  mutate(resultado = y, k = as.integer(sub("m", "-", sub("^k", "", termino))))) |>
  bind_rows(expand_grid(resultado = ys_ev, muestra = c("Ambas olas", paste("Ola", olas))) |> mutate(k = -1L, coef = 0, ee = 0, p = NA_real_)) |>
  arrange(resultado, muestra, k)
print(as.data.frame(es |> select(resultado, muestra, k, coef, ee, p) |> mutate(across(c(coef, ee, p), ~ round(.x, 3)))))
write.csv2(es, file.path(DIR_TAB, "05_estudio_eventos.csv"), row.names = FALSE)

# Efecto sobre candidatos segun la base de comparacion (ambas olas apiladas).
ev <- ev |> mutate(trat_post = tratada * (k == 0))
dd_y <- function(datos, x, nombre) mco_cluster(as.formula(paste("n_candidatos ~", paste(x, collapse = " + "))), datos, "comuna") |>
  filter(termino == "trat_post") |> mutate(base = nombre)
rango <- bind_rows(
  dd_y(filter(ev, k %in% c(-1, 0)), c("trat_post", "ola_anio", "ola_comuna"), "Base: eleccion anterior (k = -1)"),
  dd_y(ev, c("trat_post", "ola_anio", "ola_comuna"), "Base: todas las elecciones previas"),
  dd_y(ev |> mutate(trat_tend = tratada * k), c("trat_post", "trat_tend", "ola_anio", "ola_comuna"), "Desviacion de la tendencia lineal del grupo"),
  map_dfr(olas, function(g) dd_y(filter(ev, ola == g, k %in% c(-1, 0)), c("trat_post", "ola_anio", "ola_comuna"), paste("Solo ola", g, ", base k = -1")))) |>
  select(base, coef, ee, p, n, comunas) |>
  mutate(ic_inf = coef - 1.96 * ee, ic_sup = coef + 1.96 * ee)
print(as.data.frame(rango |> mutate(across(c(coef, ee, p, ic_inf, ic_sup), ~ round(.x, 3)))))
write.csv2(rango, file.path(DIR_TAB, "05_efecto_candidatos_rango.csv"), row.names = FALSE)

medias <- ev |> group_by(ola, grupo, anio) |>
  summarise(m = mean(n_candidatos), ee = sd(n_candidatos) / sqrt(n()), n = n(), .groups = "drop") |>
  mutate(ola = paste("Ola", ola))
g1 <- ggplot(medias, aes(anio, m, color = grupo)) +
  geom_pointrange(aes(ymin = m - 1.96 * ee, ymax = m + 1.96 * ee), position = position_dodge(width = 0.8)) +
  geom_line(position = position_dodge(width = 0.8)) +
  facet_wrap(~ ola, scales = "free_x") +
  scale_color_manual(values = c("Alcalde inhabilitado" = ACENTO, "Incumbente compite" = GRIS), name = NULL) +
  scale_x_continuous(breaks = anios_p) +
  labs(title = "Candidatos a alcalde por comuna antes y en el año del límite",
       x = NULL, y = "Candidatos promedio (IC 95%)",
       caption = "Fuente: elaboración propia con datos del SERVEL. Último año de cada panel: elección con el límite vigente.") + tema
guardar(g1, "fig1_candidatos_por_grupo.png", w = 8)

g1b <- ggplot(filter(es, resultado == "n_candidatos"), aes(k, coef, color = muestra)) +
  geom_hline(yintercept = 0, color = "grey50") +
  geom_vline(xintercept = -0.5, linetype = "dashed", color = "grey60") +
  geom_pointrange(aes(ymin = coef - 1.96 * ee, ymax = coef + 1.96 * ee), position = position_dodge(width = 0.4)) +
  scale_color_manual(values = setNames(c(ACENTO, GRIS, "#9DB9D3")[seq_len(1 + length(olas))], c("Ambas olas", paste("Ola", olas))), name = NULL) +
  scale_x_continuous(breaks = sort(unique(es$k)), labels = function(x) ifelse(x == 0, "Límite", x)) +
  labs(title = "Estudio de eventos: candidatos en comunas con alcalde inhabilitado",
       x = "Elecciones respecto de la entrada en vigor del límite", y = "Diferencia respecto de k = -1 (IC 95%)",
       caption = "Efectos fijos de ola x comuna y ola x año; errores agrupados por comuna. La ola 2021 no tiene k = -3 (falta 2008).") + tema
guardar(g1b, "fig1b_estudio_eventos.png")

# 2. Coeficientes de retencion -----------------------------------------------
reg <- read.csv2(file.path(DIR_TAB, "04_regresiones.csv"), fileEncoding = "UTF-8")
coefs <- reg |> filter(termino == "limite", str_detect(modelo, "^A"), resultado %in% c("retiene_familia", "retiene_eje")) |>
  mutate(resultado = recode(resultado, retiene_familia = "Familia política", retiene_eje = "Eje"),
         modelo = factor(modelo, levels = rev(unique(modelo))))
g2 <- ggplot(coefs, aes(coef * 100, modelo, color = resultado)) +
  geom_vline(xintercept = 0, color = "grey50") +
  geom_pointrange(aes(xmin = (coef - 1.96 * ee) * 100, xmax = (coef + 1.96 * ee) * 100),
                  position = position_dodge(width = 0.5)) +
  scale_color_manual(values = c("Familia política" = ACENTO, "Eje" = GRIS), breaks = c("Familia política", "Eje"), name = NULL) +
  labs(title = "Retención del sector saliente: límite legal frente a salida voluntaria",
       x = "Diferencia en puntos porcentuales (IC 95%)", y = NULL,
       caption = "Escaños abiertos con incumbente de sector identificado. Errores agrupados por comuna.") + tema
guardar(g2, "fig2_coeficientes_retencion.png", h = 4)

# 3. Regresion discontinua ----------------------------------------------------
f_rdd <- file.path(DIR_PROC, "rdd_base.rds")
if (file.exists(f_rdd)) {
  base <- readRDS(f_rdd) |> mutate(gana = as.numeric(gana))
  bins <- base |> filter(abs(margen) <= 0.3) |>
    mutate(bin = floor(margen / 0.02) * 0.02 + 0.01) |>
    group_by(bin) |> summarise(gana = mean(gana), n = n(), .groups = "drop")
  g3 <- ggplot(filter(base, abs(margen) <= 0.3), aes(margen, gana)) +
    geom_vline(xintercept = 0, color = "grey50") +
    geom_point(data = bins, aes(bin, gana, size = n), color = GRIS, alpha = 0.8, show.legend = FALSE) +
    geom_smooth(data = ~ filter(.x, margen < 0), method = "lm", formula = y ~ x, color = ACENTO, fill = ACENTO, alpha = 0.15) +
    geom_smooth(data = ~ filter(.x, margen >= 0), method = "lm", formula = y ~ x, color = ACENTO, fill = ACENTO, alpha = 0.15) +
    scale_x_continuous(labels = function(x) paste0(x * 100)) +
    scale_y_continuous(labels = function(x) paste0(x * 100, "%")) +
    coord_cartesian(ylim = c(0, 1)) +
    labs(title = "Ventaja del incumbente en elecciones estrechas, 2016–2024",
         x = "Margen en la elección anterior (puntos)", y = "Gana la elección siguiente",
         caption = "Dos primeros candidatos de cada comuna. Intervalos de 2 puntos; ajuste lineal a cada lado.") + tema
  guardar(g3, "fig3_regresion_discontinua.png")

  rob <- list()
  if (requireNamespace("rddensity", quietly = TRUE)) {
    dens <- rddensity::rddensity(base$margen, c = 0)
    rob$densidad <- tibble(prueba = "Densidad en el umbral (rddensity)", valor = NA_real_, ee = NA_real_,
                           p = dens$test$p_jk, n = nrow(base))
  }
  if (requireNamespace("rdrobust", quietly = TRUE)) {
    rd <- function(d, c = 0, h = NULL) {
      r <- if (is.null(h)) rdrobust::rdrobust(d$gana, d$margen, c = c) else rdrobust::rdrobust(d$gana, d$margen, c = c, h = h)
      c(valor = r$coef[1], ee = r$se[3], p = r$pv[3], n = sum(r$N_h))
    }
    r0 <- rdrobust::rdrobust(base$gana, base$margen, c = 0); h0 <- r0$bws[1, 1]
    rob$anchos <- map_dfr(c(0.5, 0.75, 1, 1.5, 2), function(k) {
      x <- rd(base, h = h0 * k); tibble(prueba = sprintf("Ancho de banda %.2f x óptimo (h = %.3f)", k, h0 * k),
                                        valor = x[1], ee = x[2], p = x[3], n = x[4]) })
    # Umbrales placebo: a cada lado se usa solo la submuestra de ese lado.
    rob$placebo <- map_dfr(c(-0.10, -0.05, 0.05, 0.10), function(c) {
      d <- if (c < 0) filter(base, margen < 0) else filter(base, margen >= 0)
      x <- tryCatch(rd(d, c = c), error = function(e) c(NA, NA, NA, NA))
      tibble(prueba = sprintf("Umbral placebo %+.2f", c), valor = x[1], ee = x[2], p = x[3], n = x[4]) })
  } else message("rdrobust no instalado: se omiten anchos de banda y umbrales placebo")
  if (length(rob)) {
    rob <- bind_rows(rob); print(as.data.frame(rob |> mutate(across(c(valor, ee, p), ~ round(.x, 3)))))
    write.csv2(rob, file.path(DIR_TAB, "05_rdd_robustez.csv"), row.names = FALSE)
  }
} else message("Falta rdd_base.rds: correr de nuevo 03_rdd_incumbencia.R")

# 4. Cambio de etiqueta partidaria del incumbente -------------------------------
if ("inc_partido_t" %in% names(panel)) {
  et <- panel |> filter(tipo_eleccion == "Incumbente compite", !reemplazante, !is.na(inc_partido), !is.na(inc_partido_t)) |>
    # Las etiquetas de independientes cambian por lista ("INDEPENDIENTES LISTA H"):
    # se igualan para no contar como cambio de partido.
    mutate(across(c(inc_partido, inc_partido_t), ~ if_else(es_independiente(.x) | str_detect(clave(.x), "^INDEPENDIENTES"), "INDEPENDIENTE", clave(.x)), .names = "{.col}_n"),
           cambia_partido = inc_partido_n != inc_partido_t_n,
           cambia_familia = !is.na(inc_familia) & !is.na(inc_familia_t) & inc_familia != inc_familia_t)
  cambio <- et |> group_by(anio) |>
    summarise(incumbentes = n(),
              gana_si_cambia_familia = mean(inc_gana[cambia_familia]), gana_si_no_cambia = mean(inc_gana[!cambia_familia]),
              n_cambia_partido = sum(cambia_partido), n_cambia_familia = sum(cambia_familia),
              .groups = "drop")
  print(as.data.frame(cambio))
  write.csv2(cambio, file.path(DIR_TAB, "05_cambio_etiqueta.csv"), row.names = FALSE)
  write.csv2(et |> filter(cambia_familia) |> select(anio, region, comuna, inc, inc_partido, inc_partido_t, inc_familia, inc_familia_t, inc_gana),
             file.path(DIR_TAB, "05_cambio_etiqueta_casos.csv"), row.names = FALSE)
}

# 5. Tabla 1 -----------------------------------------------------------------
t1 <- panel |> filter(!is.na(tipo_eleccion), tipo_eleccion != "Reemplazo sin identificar") |>
  group_by(anio, tipo_eleccion) |>
  summarise(comunas = n(), candidatos = mean(n_candidatos),
            participacion = if ("participacion" %in% names(panel)) mean(participacion, na.rm = TRUE) else NA_real_,
            incumbente_gana = if_else(first(tipo_eleccion) == "Incumbente compite", mean(inc_gana), NA_real_),
            con_sector = sum(!is.na(inc_familia) & !inc_familia %in% SIN_SECTOR),
            retiene_familia = mean(retiene_familia[!is.na(inc_familia) & !inc_familia %in% SIN_SECTOR]),
            .groups = "drop")
print(as.data.frame(t1))
write.csv2(t1, file.path(DIR_TAB, "05_tabla1_descriptiva.csv"), row.names = FALSE)
message("Figuras en ", DIR_FIG)
