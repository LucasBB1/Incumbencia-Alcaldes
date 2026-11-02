# Fin del atrincheramiento: el límite de reelección y la competencia municipal en Chile, 2012–2024

Código y datos del **Documento de trabajo N.° 2** de Lucas Bottinelli Baltra. El estudio evalúa qué cambia en una elección municipal cuando el alcalde deja el cargo por el límite de reelección de la Ley 21.238 y no por decisión propia, con los resultados de las 345 comunas en las elecciones de alcalde de 2012, 2016, 2021 y 2024.

**Autor:** Lucas Bottinelli Baltra · [lucasbb1.github.io](https://lucasbb1.github.io) · ORCID [0009-0000-4865-7261](https://orcid.org/0009-0000-4865-7261)

El documento está en `documento/` (Word y PDF).

## Resultados principales

- Ganar la alcaldía por un margen estrecho aumenta en 33 puntos porcentuales la probabilidad de ganar la elección siguiente (regresión discontinua).
- Las comunas con alcalde inhabilitado por el límite sumaron alrededor de 1,5 candidatos (diferencias en diferencias apiladas por ola, 2021 y 2024), más que tras una salida voluntaria.
- La familia política del alcalde saliente no retiene la alcaldía con menor frecuencia tras el límite que tras una salida voluntaria.

## Scripts

Correr en orden desde `Incumbencia-Alcaldes.Rproj`.

| Script | Qué hace |
|---|---|
| `00_funciones.R` | Funciones comunes: lectura y homologación de los archivos del SERVEL, asignación de familias políticas y errores estándar agrupados (lo cargan los demás scripts) |
| `01_importar_alcaldes.R` | Votación comunal de alcalde de las 345 comunas (2012, 2016, 2021 y 2024) y participación por comuna |
| `02_panel_incumbencia.R` | Panel comuna-elección: ganador, margen, incumbente (con reemplazos), clasificación de la elección, inhabilitados por el límite, retención de familia y eje; proyección de inhabilitados 2028 |
| `03_rdd_incumbencia.R` | Ventaja del incumbente con regresión discontinua en elecciones estrechas |
| `04_regresion_escanos_abiertos.R` | Retención en escaños abiertos, oferta electoral y participación; placebo, diferencias en diferencias por ola y apiladas, y permutación |
| `05_figuras_tablas.R` | Estudio de eventos apilado, figuras del documento, robustez de la regresión discontinua y tablas |

Paquetes: `tidyverse`, `readxl`, `rdrobust`, `rddensity`, `ggplot2`.

## Datos

- `data/raw/`: archivos originales del SERVEL (no se versionan; ver `data/raw/LEEME.md`).
- `data/homologacion/`: tablas construidas para el estudio.

| Archivo | Contenido |
|---|---|
| `reemplazos.csv` | 24 alcaldes que asumieron durante el período (fallecimiento, renuncia, destitución o suspensión del electo), con fuente y si el período cuenta para el reemplazante según el acuerdo del SERVEL de 2024 |
| `inhabilitados_prensa.csv` | Nómina pública de los 96 alcaldes inhabilitados en 2021 |
| `cupos.csv` | Independientes que compitieron en el cupo de un partido |
| `comunas.csv`, `personas.csv` | Homologación de nombres de comunas y de personas entre elecciones |

## Cómo citar

Bottinelli Baltra, L. (2026). *Fin del atrincheramiento: el límite de reelección y la competencia municipal en Chile, 2012–2024* (Documento de trabajo N.° 2).

## Licencias

Código: MIT (`LICENSE`). Datos procesados, tablas, figuras y texto: CC BY 4.0 (`LICENSE-DATOS.md`).
