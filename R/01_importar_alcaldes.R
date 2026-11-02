# =============================================================================
# Incumbencia y limite de reeleccion en las elecciones de alcalde (Chile)
# 01_importar_alcaldes.R
# -----------------------------------------------------------------------------
# Lee la votacion comunal de alcalde de las 345 comunas (2012, 2016,
# 2021, 2024) y la participacion por comuna desde los archivos oficiales del SERVEL.
#
# Entradas  data/raw/  archivos .xlsx del SERVEL (cualquier subcarpeta)
# Salidas   data/processed/alcaldes_comuna.rds   votos por comuna y candidato
#           data/processed/participacion_comuna.rds
#           output/tablas/01_resumen_importacion.csv
# =============================================================================

source(file.path("R", "00_funciones.R"))
# Carpeta con los archivos del SERVEL. Para no duplicar 6,5 GB, puedes apuntar
# a la carpeta data/raw del proyecto de La Reina, por ejemplo:
# DIR_RAW <- "../Analisis-Electoral-La-Reina-2016-2028/data/raw"
DIR_RAW <- file.path("data", "raw")
DIR_PROC <- file.path("data", "processed")
DIR_TAB <- file.path("output", "tablas")
for (d in c(DIR_PROC, DIR_TAB)) dir.create(d, recursive = TRUE, showWarnings = FALSE)

# Catalogo. 2016, 2021 y 2024 usan los mismos archivos del proyecto La Reina.
# 2012: 2012_10_Alcaldes_DatosEleccion.xlsx (verificado: 346 comunas, 13,4 millones de inscritos).
catalogo <- tribble(
  ~anio, ~patron_archivo,                                   ~hoja_comuna,
  2008L, "^2008.*Alcalde.*\\.xlsx?$",                        "(?i)^Votaci[oó]n (por comuna|comunal)",
  2012L, "^2012.*Alcalde.*\\.xlsx$",                         "(?i)^Votaci[oó]n (por comuna|comunal)",
  2016L, "^2016_10_Alcaldes_DatosEleccion\\.xlsx$",          "(?i)^Votaci[oó]n (por comuna|comunal)",
  2021L, "^2021_05_Alcaldes_Datos_Eleccion\\.xlsx$",         "(?i)^Votaci[oó]n (por comuna|comunal)",
  2024L, "^2024_10_Alcaldes_Datos_Eleccion\\.xlsx$",         "(?i)^Votaci[oó]n (por comuna|comunal)"
)

archivos <- list.files(DIR_RAW, recursive = TRUE, full.names = TRUE)
catalogo$ruta <- map_chr(catalogo$patron_archivo, function(p) {
  hit <- archivos[str_detect(basename(archivos), p)]
  if (length(hit) == 0) NA_character_ else hit[1]
})
print(select(catalogo, anio, ruta))
# 2008 es opcional: el SERVEL no lo publica en formato de datos y los alcaldes
# inhabilitados en 2021 se toman de data/homologacion/inhabilitados_prensa.csv.
faltan <- catalogo$anio[is.na(catalogo$ruta) & catalogo$anio != 2008L]
if (length(faltan) > 0)
  warning("Faltan los archivos de ", paste(faltan, collapse = ", "),
          " (ver NA arriba). Se continua con los disponibles; 2012 es necesario para identificar a los inhabilitados de 2024.")
catalogo <- filter(catalogo, !is.na(ruta))

votos <- map_dfr(seq_len(nrow(catalogo)), function(i) {
  message("Leyendo alcaldes ", catalogo$anio[i], " ...")
  leer_hoja_servel(catalogo$ruta[i], catalogo$hoja_comuna[i]) |>
    estandarizar_alcaldes(catalogo$anio[i])
})

# Participacion por comuna (suma de las mesas de la hoja de participacion).
participacion <- map_dfr(seq_len(nrow(catalogo)), function(i) {
  df <- tryCatch(leer_hoja_servel(catalogo$ruta[i], "^Participaci"),
                 error = function(e) { message("  sin hoja de participacion: ", catalogo$anio[i]); NULL })
  if (is.null(df)) return(NULL)
  tibble(anio = catalogo$anio[i], comuna = clave(df$comuna),
         inscritos = as.numeric(col_o_na(df, "inscritos")),
         votantes  = as.numeric(coalesce(col_o_na(df, "votantes"), col_o_na(df, "votos")))) |>
    group_by(anio, comuna) |>
    summarise(inscritos = sum(inscritos, na.rm = TRUE), votantes = sum(votantes, na.rm = TRUE),
              .groups = "drop")
})

saveRDS(votos, file.path(DIR_PROC, "alcaldes_comuna.rds"))
saveRDS(participacion, file.path(DIR_PROC, "participacion_comuna.rds"))

# Resumen y controles: comunas por ano, un electo por comuna.
resumen <- votos |>
  group_by(anio) |>
  summarise(comunas = n_distinct(comuna),
            candidatos = sum(tipo_voto == "valido"),
            electos_marcados = sum(!is.na(marca) & str_detect(clave(marca), "ALCALDE|ELECTO")),
            votos_validos = sum(votos[tipo_voto == "valido"]),
            .groups = "drop")
print(resumen)
write.csv2(resumen, file.path(DIR_TAB, "01_resumen_importacion.csv"), row.names = FALSE)
if (any(resumen$comunas < 340)) warning("Algun ano tiene menos de 340 comunas: revisar la lectura.")
