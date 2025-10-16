
# Cargar las librerías necesarias----

library(tidyverse)
library(readr)

# Leer los datos desde el archivo CSV----

CONAPRED <- read_csv("CONAPRED.csv")

# Primero: group_by y summarise----

quejas_por_entidad <- CONAPRED %>%
  group_by(entidad_federativa) %>%
  summarise(total_quejas = sum(quejas, na.rm = TRUE)) %>%
  arrange(desc(total_quejas))

# Segundo: Crear el gráfico de barras con ggplot2----
ggplot(quejas_por_entidad,
       aes(x = reorder(entidad_federativa, total_quejas),
           y = total_quejas)) +
  geom_col(fill = "steelblue", alpha = 0.8) +
  # Agregar los números encima de las columnas
  geom_text(aes(label = total_quejas),
            hjust = -0.2,
            size = 3.5,
            fontface = "bold") +
  labs(
    title = "Total de Quejas por Entidad Federativa",
    subtitle = "Análisis CONAPRED",
    x = "Entidad Federativa",
    y = "Total de Quejas",
    caption = "Fuente: CONAPRED"
  ) +
  theme_classic () +
  coord_flip(clip = "off") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    axis.title = element_text(face = "bold"),
    plot.margin = margin(10, 30, 10, 10)
  )

