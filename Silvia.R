# ==========================================
# Ejercitación Unidad - Tidyverse + ggplot
# Script: silvia.R  (cambiar por tu identificación)
# ==========================================

# 1) Paquetes
# install.packages("tidyverse")  # solo la primera vez
library(tidyverse)

# 2) Cargar el CSV (opción recomendada: desde la URL del material)
url <- "https://raw.githubusercontent.com/rominicky/materiales/main/assets/Provincias.csv"
provincias <- readr::read_csv(url)

# Si tu profe pide leerlo "local", descargalo y usá:
# provincias <- readr::read_csv("./Provincias.csv")

# 3) Revisión rápida
glimpse(provincias)

# 4) Limpieza mínima: renombrar la columna larga a "situacion_calle"
# (en el material aparece como Personas.en.situación.de.calle..vía.pública.)
# Usamos all_of() para no romper si el nombre exacto cambia.
nombre_original <- "Personas.en.situación.de.calle..vía.pública."

if (nombre_original %in% names(provincias)) {
  provincias <- provincias %>%
    rename(situacion_calle = all_of(nombre_original))
} else {
  # Por si el nombre viene ligeramente distinto, mostramos nombres para ajustar:
  message("No encontré el nombre exacto de la columna. Revisá names(provincias).")
  print(names(provincias))
}

# 5) Análisis con Tidyverse:
#    Calculamos el porcentaje de personas en situación de calle sobre la población 2022
#    y seleccionamos el Top 10 provincias (por porcentaje)
resumen <- provincias %>%
  transmute(
    provincia = Nombre.de.provincia,
    poblacion_2022 = Población..2022.,
    situacion_calle = situacion_calle,
    pct_situacion_calle = (situacion_calle / poblacion_2022) * 100
  ) %>%
  filter(!is.na(pct_situacion_calle), poblacion_2022 > 0) %>%
  arrange(desc(pct_situacion_calle)) %>%
  slice_head(n = 10)

print(resumen)

# 6) Gráfico con ggplot (barras horizontales)
g <- ggplot(resumen, aes(x = reorder(provincia, pct_situacion_calle), y = pct_situacion_calle)) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Top 10 provincias por % de personas en situación de calle (sobre población 2022)",
    x = "Provincia",
    y = "Porcentaje (%)"
  ) +
  theme_minimal()

print(g)

# 7) Guardar gráfico (opcional pero prolijo)
ggsave("grafico_situacion_calle_top10.png", g, width = 10, height = 6, dpi = 300)
