install.packages(c("tidyverse", "car", "ggplot2", "rstatix", "gridExtra"))

# Cargar las bibliotecas necesarias
library(tidyverse)
library(car)
library(ggplot2)
library(rstatix)
library(gridExtra)

# Leer los datos
datos <- read.csv("data.csv")

# Limpiar y preparar los datos
datos_filtrados <- datos %>%
  filter(!str_detect(Title, "^Punto")) %>%  # Eliminar los puntos de vértices
  mutate(
    tipo_zona = case_when(
      Title == "Planta" & !is.na(interior) ~ "isla_interior",
      Title == "Planta" & !is.na(exterior) ~ "isla_exterior",
      Title == "Suelo" ~ "sin_isla",
      TRUE ~ "otros"
    )
  ) %>%
  filter(tipo_zona %in% c("isla_interior", "isla_exterior", "sin_isla")) %>%
  # Convertir NA a 0 en columnas numéricas específicas
  mutate(across(c(interior, exterior), ~replace_na(., 0)))

# 1. Análisis Descriptivo
estadisticas_descriptivas <- datos_filtrados %>%
  group_by(tipo_zona) %>%
  summarise(
    n = n(),
    media_interior = mean(interior, na.rm = TRUE),
    sd_interior = sd(interior, na.rm = TRUE),
    media_elevation = mean(Elevation, na.rm = TRUE),
    sd_elevation = sd(Elevation, na.rm = TRUE)
  )

print("Estadísticas Descriptivas:")
print(estadisticas_descriptivas)

# 2. Test de Normalidad
# Función para realizar test de normalidad de manera segura
test_normalidad <- function(x) {
  if (length(na.omit(x)) >= 3) {  # Shapiro-Wilk necesita al menos 3 observaciones
    test <- shapiro.test(na.omit(x))
    return(data.frame(
      shapiro_stat = test$statistic,
      shapiro_p = test$p.value
    ))
  } else {
    return(data.frame(
      shapiro_stat = NA,
      shapiro_p = NA
    ))
  }
}

normalidad_test <- datos_filtrados %>%
  group_by(tipo_zona) %>%
  summarise(
    test_result = list(test_normalidad(interior))
  ) %>%
  unnest(test_result)

print("\nTest de Normalidad:")
print(normalidad_test)

# 3. Visualización de la distribución
p1 <- ggplot(datos_filtrados, aes(x = tipo_zona, y = interior)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Distribución de inclinación por tipo de zona",
       x = "Tipo de zona",
       y = "Inclinación (grados)")

print(p1)
ggsave("boxplot_inclinacion.png", p1)

# 4. Análisis Estadístico
# Kruskal-Wallis para comparación general
if (length(unique(datos_filtrados$tipo_zona)) >= 2) {
  kruskal_result <- kruskal.test(interior ~ tipo_zona, data = datos_filtrados)
  print("\nResultados Kruskal-Wallis:")
  print(kruskal_result)
  
  # Pruebas post-hoc de Dunn con corrección de Bonferroni
  dunn_test <- datos_filtrados %>%
    dunn_test(interior ~ tipo_zona, p.adjust.method = "bonferroni")
  print("\nPruebas post-hoc de Dunn:")
  print(dunn_test)
}

# 5. Análisis de correlación entre elevación e inclinación
# Usar método de Spearman que es más robusto para datos no normales y con empates
datos_cor <- datos_filtrados %>%
  filter(!is.na(interior) & !is.na(Elevation))

if (nrow(datos_cor) > 0) {
  correlation_test <- cor.test(datos_cor$interior, 
                               datos_cor$Elevation, 
                               method = "spearman",
                               exact = FALSE)  # Usar aproximación para manejar empates
  
  print("\nCorrelación entre elevación e inclinación (Spearman):")
  print(correlation_test)
}

# 6. Visualización de la relación elevación-inclinación
p2 <- ggplot(datos_filtrados, aes(x = Elevation, y = interior, color = tipo_zona)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) +
  theme_minimal() +
  labs(title = "Relación entre elevación e inclinación",
       x = "Elevación",
       y = "Inclinación (grados)")

print(p2)
ggsave("scatter_elevacion_inclinacion.png", p2)

# 7. Análisis de vegetación específica
especies <- datos_filtrados %>%
  group_by(tipo_zona) %>%
  summarise(across(
    c(Pela, Mosquero, Cabecinegro, Palo_Santo, Cruceto),
    ~mean(., na.rm = TRUE)
  ))

print("\nResumen de presencia de especies por zona:")
print(especies)

# 8. Guardar resultados en un archivo
sink("resultados_analisis.txt")
cat("RESULTADOS DEL ANÁLISIS ECOLÓGICO\n\n")
cat("1. Estadísticas Descriptivas:\n")
print(estadisticas_descriptivas)
cat("\n2. Test de Normalidad:\n")
print(normalidad_test)
if (exists("kruskal_result")) {
  cat("\n3. Resultados Kruskal-Wallis:\n")
  print(kruskal_result)
  cat("\n4. Pruebas post-hoc de Dunn:\n")
  print(dunn_test)
}
if (exists("correlation_test")) {
  cat("\n5. Correlación Spearman:\n")
  print(correlation_test)
}
cat("\n6. Resumen de especies por zona:\n")
print(especies)
sink()



## Plots adicionales
# Cargar las bibliotecas necesarias
library(tidyverse)
library(ggplot2)
library(gridExtra)
library(reshape2)
library(viridis)

# 1. Preparar los datos para especies
datos_especies <- datos_filtrados %>%
  select(tipo_zona, interior, exterior, Elevation, 
         Pela, Mosquero, Cabecinegro, Palo_Santo, Cruceto) %>%
  mutate(across(c(Pela:Cruceto), ~replace_na(., 0)))

# 2. Abundancia total de cada especie
p1 <- datos_especies %>%
  select(Pela:Cruceto) %>%
  gather(key = "especie", value = "abundancia") %>%
  ggplot(aes(x = reorder(especie, abundancia), y = abundancia)) +
  geom_bar(stat = "sum", fill = "steelblue") +
  coord_flip() +
  theme_minimal() +
  labs(title = "Abundancia total por especie",
       x = "Especie",
       y = "Abundancia total") +
  theme(axis.text.y = element_text(size = 10))

# 3. Relación entre pendiente y abundancia por especie
datos_long <- datos_especies %>%
  gather(key = "especie", value = "abundancia", Pela:Cruceto) %>%
  filter(abundancia > 0)  # Solo mantener registros con presencia

p2 <- ggplot(datos_long, aes(x = abundancia, y = interior, color = especie)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = TRUE, alpha = 0.2) +
  theme_minimal() +
  labs(title = "Relación entre pendiente y abundancia por especie",
       x = "Abundancia",
       y = "Pendiente (grados)") +
  facet_wrap(~especie, scales = "free_x")

# 4. Distribución de especies por tipo de zona
p3 <- datos_long %>%
  filter(abundancia > 0) %>%
  ggplot(aes(x = tipo_zona, y = abundancia, fill = especie)) +
  geom_boxplot() +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Distribución de abundancia por especie y tipo de zona",
       x = "Tipo de zona",
       y = "Abundancia")

# 5. Mapa de calor de correlaciones
matriz_cor <- datos_especies %>%
  select(interior, Pela:Cruceto) %>%
  cor(use = "pairwise.complete.obs")

p4 <- ggplot(data = melt(matriz_cor), aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  scale_fill_viridis() +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Mapa de calor de correlaciones",
       x = "", y = "") +
  geom_text(aes(label = round(value, 2)), size = 3)

# 6. Relación entre elevación y abundancia por especie
p5 <- ggplot(datos_long, aes(x = Elevation, y = abundancia, color = especie)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) +
  theme_minimal() +
  labs(title = "Relación entre elevación y abundancia por especie",
       x = "Elevación",
       y = "Abundancia") +
  facet_wrap(~especie, scales = "free_y")

# 7. Composición de especies por tipo de zona (proporción)
p6 <- datos_long %>%
  group_by(tipo_zona, especie) %>%
  summarise(abundancia_total = sum(abundancia)) %>%
  ggplot(aes(x = tipo_zona, y = abundancia_total, fill = especie)) +
  geom_bar(position = "fill", stat = "identity") +
  theme_minimal() +
  labs(title = "Composición proporcional de especies por tipo de zona",
       x = "Tipo de zona",
       y = "Proporción") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# 8. Relación entre pendiente interior y exterior para plantas
p7 <- datos_filtrados %>%
  filter(!is.na(interior) & !is.na(exterior)) %>%
  ggplot(aes(x = interior, y = exterior)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) +
  theme_minimal() +
  labs(title = "Relación entre pendiente interior y exterior",
       x = "Pendiente interior (grados)",
       y = "Pendiente exterior (grados)")

# Guardar todos los gráficos
ggsave("abundancia_total_especies.png", p1, width = 10, height = 6)
ggsave("pendiente_vs_abundancia.png", p2, width = 12, height = 8)
ggsave("distribucion_especies_zona.png", p3, width = 10, height = 6)
ggsave("mapa_calor_correlaciones.png", p4, width = 10, height = 8)
ggsave("elevacion_vs_abundancia.png", p5, width = 12, height = 8)
ggsave("composicion_especies_zona.png", p6, width = 10, height = 6)
ggsave("pendiente_interior_vs_exterior.png", p7, width = 8, height = 6)

# Crear un resumen estadístico de las relaciones
# Correlaciones entre pendiente y abundancia por especie
correlaciones <- datos_long %>%
  group_by(especie) %>%
  summarise(
    cor_pendiente = cor(interior, abundancia, use = "complete.obs"),
    cor_elevacion = cor(Elevation, abundancia, use = "complete.obs")
  )

print("Correlaciones entre variables:")
print(correlaciones)

# Guardar resumen en archivo
sink("resumen_correlaciones.txt")
cat("RESUMEN DE CORRELACIONES\n\n")
print(correlaciones)
sink()
