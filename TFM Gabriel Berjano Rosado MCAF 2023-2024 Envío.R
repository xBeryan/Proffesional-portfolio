
### Instalamos paquetes para visualizar las variables y definimos el directorio de la base de datos

install.packages("readxl")
library(readxl)
install.packages("ggplot2")
library(ggplot2)

# dataCar contiene siniestros de valor 0 y dataCar2 los excluye

ruta <- "C:/Users/Beryan/Desktop/Universidad/Master Actuariales/Segundo año/Segundo Cuatrimestre/TFM/Bases de datos/MCAF_2023-24_02583631H_Anexo1.xlsx"

data <- read_excel(ruta)

### Hacemos las gráficas de las variables

# Veh_value: Valor del vehículo (variable continua)
ggplot(data, aes(x = veh_value)) +
  geom_histogram(binwidth = 1, fill = "blue", color = "black", alpha = 0.7) +
  labs(x = "Valor del Vehículo (en decenas de miles de dólares)", y = "Frecuencia") +
  theme_minimal()

# Exposure: Grado de exposición (variable continua)
ggplot(data, aes(x = exposure)) +
  geom_histogram(binwidth = 0.1, fill = "green", color = "black", alpha = 0.7) +
  labs(x = "Grado de Exposición", y = "Frecuencia") +
  theme_minimal()

# Clm: Indicación de siniestro (variable dicotómica)
ggplot(data, aes(x = factor(clm))) +
  geom_bar(fill = "red", color = "black", alpha = 0.7) +
  labs(x = "Siniestro (0: No, 1: Sí)", y = "Frecuencia") +
  theme_minimal()

# Veh_body: Tipología del vehículo (variable categórica)
ggplot(data, aes(x = factor(veh_body))) +
  geom_bar(fill = "brown", color = "black", alpha = 0.7) +
  labs(x = "", y = "Frecuencia") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Veh_age: Edad del vehículo (variable categórica)
ggplot(data, aes(x = factor(veh_age))) +
  geom_bar(fill = "pink", color = "black", alpha = 0.7) +
  labs(x = "Edad del Vehículo", y = "Frecuencia") +
  theme_minimal()

# Gender: Género del propietario (variable categórica)
ggplot(data, aes(x = factor(gender))) +
  geom_bar(fill = "cyan", color = "black", alpha = 0.7) +
  labs(x = "Género", y = "Frecuencia") +
  theme_minimal()

# Area: Zona geográfica (variable categórica)
ggplot(data, aes(x = factor(area))) +
  geom_bar(fill = "yellow", color = "black", alpha = 0.7) +
  labs(x = "Zona Geográfica", y = "Frecuencia") +
  theme_minimal()

# Agecat: Edad del propietario (variable categórica)
ggplot(data, aes(x = factor(agecat))) +
  geom_bar(fill = "violet", color = "black", alpha = 0.7) +
  labs(x = "Categoría de Edad", y = "Frecuencia") +
  theme_minimal()

# Número de siniestros (variable discreta)

ggplot(data, aes(x = numclaims)) + geom_histogram(binwidth = 1, fill = "blue", color = "black", alpha = 0.7) +
  labs(x = "Número de siniestros", y = "Frecuencia") + theme_minimal()

# Cuantía de los siniestros (variable)

ggplot(data, aes(x = 1:nrow(data), y = claimcst0)) + geom_point(color = "blue", alpha = 0.5) +
  labs(x = "Observaciones", y = "Cuantía de los siniestros ($)") + theme_minimal()

# Descriptoresde la variable

variable <- data$claimcst0

n <- length(variable)

media <- mean(variable)

minimo <- min(variable)

maximo <- max(variable)

varianza <- var(variable)

desviacion_estandar <- sd(variable)

mediana <- median(variable)

asimetria <- sum((variable - media)^3) / ((n-1) * desviacion_estandar^3)

percentiles <- c(0.25, 0.50, 0.75, 0.90)
valores_percentiles <- quantile(variable, percentiles)
conteos_por_encima <- sapply(valores_percentiles, function(x) sum(variable > x))

resultados <- list(
  Tamaño_de_la_muestra = n,
  Media = media,
  Mínimo = minimo,
  Máximo = maximo,
  Varianza = varianza,
  Desviación_estándar = desviacion_estandar,
  Mediana = mediana,
  Asimetría = asimetria,
  Percentiles = valores_percentiles,
  Conteo_por_encima_de_percentil = conteos_por_encima
)

print(resultados)

# Descriptores de los siniestros extremos

umbral_99 <- quantile(variable, 0.99)

valores_superan_99 <- variable[variable > umbral_99]

n <- length(valores_superan_99)

media <- mean(valores_superan_99)

minimo <- min(valores_superan_99)

maximo <- max(valores_superan_99)

varianza <- var(valores_superan_99)

desviacion_estandar <- sd(valores_superan_99)

mediana <- median(valores_superan_99)

asimetria <- sum((valores_superan_99 - media)^3) / ((n-1) * desviacion_estandar^3)

percentiles <- c(0.25, 0.50, 0.75, 0.90)
valores_percentiles <- quantile(valores_superan_99, percentiles)
conteos_por_encima <- sapply(valores_percentiles, function(x) sum(valores_superan_99 > x))

resultados <- list(
  Tamaño_de_la_muestra = n,
  Media = media,
  Mínimo = minimo,
  Máximo = maximo,
  Varianza = varianza,
  Desviación_estándar = desviacion_estandar,
  Mediana = mediana,
  Asimetría = asimetria,
  Percentiles = valores_percentiles,
  Conteo_por_encima_de_percentil = conteos_por_encima
)

print(resultados)

### Estudio de los valores extremos

install.packages("extRemes")
library(extRemes)

## Resumen de la variable

summary(data$claimcst0) # Ejecutar todo este analisis en una base de datos sin los coste 0

## Establecer un umbral para los valores extremos (dataCar2)

# 1. Estableciendo como umbral el percentil 99

u <- quantile(data$claimcst0, 0.99) # Aquí usar dataCar sin coste 0

# 2. Con el método de Tukey

claimcst0 <- data$claimcst0

Q1 <- quantile(claimcst0, 0.25, na.rm = TRUE)
Q1
Q3 <- quantile(claimcst0, 0.75, na.rm = TRUE)
Q3
IQR <- Q3 - Q1
IQR

limite_inferior <- Q1 - 1.5 * IQR
limite_superior <- Q3 + 1.5 * IQR
print(paste("Límite inferior:", limite_inferior))
print(paste("Límite superior:", limite_superior))

# 3. Con la función empírica de exceso medio

ordenados <- sort(claimcst0, decreasing = TRUE)

k <- length(ordenados)
E_kn <- numeric(k-1)
for (i in 1:(k-1)) {
  E_kn[i] <- mean(ordenados[1:i]) - ordenados[i + 1]
}

u <- ordenados[2:k]
me_plot <- data.frame(u, E_kn)

ggplot(me_plot, aes(x = u, y = E_kn)) + geom_line() + labs(x = "Umbral (u)", y = "E(k,n)") +
  theme_minimal()

# 4. Con el gráfico de Hill

Y <- sort(claimcst0, decreasing = TRUE)

hill_estimator <- function(Y, i) {
  return(mean(log(Y[1:i] / Y[i + 1])))
}

k <- length(Y)
hill_values <- sapply(2:(k-1), function(i) hill_estimator(Y, i))

hill_plot <- data.frame(i = 2:(k-1), hill_values)

ggplot(hill_plot, aes(x = i, y = hill_values)) + geom_line() + labs(x = "Número de observaciones (i)", y = "Estimador de Hill") +
  theme_minimal()

# 5. Visualmente

# Se podría optar por fijar el umbral en 20000.


## Filtrar los valores que exceden el umbral

valores_extremos <- data$claimcst0[data$claimcst0 > u] # dataCar

## Mostrar el umbral y algunos valores extremos

u <- quantile(data$claimcst0, 0.99)
print(paste("Umbral:", u))

head(valores_extremos)

## Ajuste de los valores extremos a la distribución GEV

ajuste_gev <- fevd(valores_extremos, type = "GEV")

summary(ajuste_gev)

plot(ajuste_gev)

# Usando el método de Hill para estimar los parámetros dado que el parámetro de forma va a ser superior a 0:

install.packages("evir")
library(evir)

hill_est <- mean(log(valores_extremos / u))
hill_est_inv <- 1 / hill_est

print(paste("Estimador de Hill para 1/alpha:", hill_est))
print(paste("Estimador de Hill para alpha:", hill_est_inv))

plot(hill(data), main = "Estimador de Hill", xlab = "Orden", ylab = "Estimación de 1/alpha")

### GPD

umbral <- quantile(data$claimcst0, 0.99)
umbral

valores_extremos <- data$claimcst0[data$claimcst0 > umbral]
valores_extremos

excesos <- valores_extremos - umbral # usar la base de datos modificada (sin coste 0)
excesos

plot(excesos, col = 'red', xlab = 'Índice', ylab = 'Exceso', pch = 16)

plot(valores_extremos, col = 'blue', xlab = 'Índice', ylab = 'Valor', pch = 16)
# Los excesos son iguales, trasladas los valores extremos hacia abajo por el umbral

print(paste("Umbral:", umbral))
head(excesos)

ajuste_gpd <- gpd(excesos,17000)

summary(ajuste_gpd)

plot(ajuste_gpd)


