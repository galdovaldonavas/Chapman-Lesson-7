library(ggplot2)
library(dplyr)
library(tibble)
library(purrr)

set.seed(123)

# Función para generar datos con correlación r
simulate_data <- function(r, n = 100) {
  x <- scale(rnorm(n)) # variable X estandarizada
  e <- rnorm(n, mean = 0, sd = sqrt(1 - r^2)) # error para lograr correlación deseada
  y <- scale(r * x + e)
  tibble(x = as.numeric(x), y = as.numeric(y), r = r)
}

# Crear datasets para cada r
r_vals <- c(0, 0.2, 0.5, 0.8, 1)
all_data <- map_dfr(r_vals, simulate_data, .id = "id")

# Convertir r en factor con etiquetas más legibles
all_data <- all_data %>%
  mutate(r = factor(r, levels = r_vals, labels = paste0("r = ", r_vals)))

# Graficar
ggplot(all_data, aes(x = x, y = y)) +
  geom_point(alpha = 0.6, color = "steelblue") +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  facet_wrap(~r, nrow = 1) +
  coord_fixed(xlim = c(-1, 1), ylim = c(-1, 1)) +
  theme_minimal() +
  labs(title = "Relación entre correlación y forma de la regresión",
       x = "Variable X (estandarizada)",
       y = "Variable Y (estandarizada)")

