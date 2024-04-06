library(readr)
data <- read.csv("DataSetByGender_Aggregates_Excel_AggregateDS.csv")


# Supongamos que queremos probar si la proporción de migración total de hombres de 25 años o más en USA es mayor que 0.5.
conf.lev <- 0.95
sig.lev <- 1 - conf.lev
p.prop <- 0.5

# Realizamos la prueba de hipótesis:
p.prob <- prop.test(sum(m.datos$ms_mal_tot[m.datos$Origin == 'United States']), 
                    nrow(m.datos[m.datos$Origin == 'United States',]),
                    p = p.prop,
                    alternative = "greater",
                    conf.level = conf.lev,
                    correct = TRUE)

# Mostramos los resultados de la prueba
p.prob
str(p.prob)

# Tomamos la decisión basada en el valor p
decision <- ifelse(p.prob$p.value < sig.lev, 'Decisión: Rechazar H0', 'Decisión: No Rechazar H0')

# Mostramos el resultado de la prueba
print(paste0('Con un nivel de significancia de ',
             sig.lev,
             ', la evidencia sugiere que la proporción de migración total de hombres de 25 años o más en USA',
             ifelse(p.prob$p.value < sig.lev, ' es mayor que ', ' no es mayor que o igual a '),
             p.prop))

###########################
# Varianza desconocida
###########################

library(readr)
m.datos <- read.csv("DataSetByGender_Aggregates_Excel_AggregateDS.csv")


# Seleccionamos el nivel de significancia
conf.lev <- 0.90
sig.lev <- 1 - conf.lev

# Definimos el valor de referencia para la prueba
m.prueba <- 10000 

# Filtramos los datos para un país específico (por ejemplo, 'USA') y una columna específica (por ejemplo, 'ms_mal_tot')
m.country <- 'United States'  # Reemplaza 'USA' con el país de interés
m.column <- 'MS_MAL_TOT'  # Reemplaza 'ms_mal_tot' con la columna de interés

m.sample <- m.datos[m.datos$Origin == m.country, m.column]

# Realizamos la prueba de hipótesis
p.t <- t.test(m.sample, 
              mu = m.prueba,
              alternative = 'greater',
              conf.level = conf.lev)

# Mostramos los resultados de la prueba
p.t

# Tomamos la decisión basada en el valor p
decision <- ifelse(p.t$p.value < sig.lev, 'Decisión: Rechazar H0', 'Decisión: No Rechazar H0')

# Mostramos el resultado de la prueba
print(paste0('Con un nivel de significancia de ',
             sig.lev,
             ', la evidencia sugiere que el nivel de migración total de hombres de 25 años o más en ',
             m.country,
             ifelse(p.t$p.value < sig.lev, ' es mayor que ', ' no es mayor que '),
             m.prueba))


###################
# Diferencia de medias de dos muestras con varianzas conocidas
###################

library(readr)
m.datos <- read.csv("DataSetByGender_Aggregates_Excel_AggregateDS.csv")


# Seleccionamos el nivel de significancia
conf.lev <- 0.90
sig.lev <- 1 - conf.lev

# Filtramos los datos para Colombia y Estados Unidos
m.country1 <- 'Colombia'
m.country2 <- 'United States'

m.sample1 <- m.datos[m.datos$Origin == m.country1, 'MS_ALL_TOT']
m.sample2 <- m.datos[m.datos$Origin == m.country2, 'MS_ALL_TOT']

# Realizamos la prueba t para la diferencia de medias
p.t <- t.test(x = m.sample1, 
              y = m.sample2,
              alternative = 'two.sided',
              conf.level = conf.lev)

# Mostramos los resultados de la prueba
p.t

# Tomamos la decisión basada en el valor p
decision <- ifelse(p.t$p.value < sig.lev, 'Decisión: Rechazar H0', 'Decisión: No Rechazar H0')

# Mostramos el resultado de la prueba
print(paste0('Con un nivel de significancia de ',
             sig.lev,
             ', la evidencia sugiere que hay una diferencia significativa en el nivel de migración total entre ',
             m.country1,
             ' y ',
             m.country2))
###############################################
# Varianza desconocida pero iguales
###############################################

# Seleccionamos el nivel de significancia
conf.lev <- 0.05
sig.lev <- 1 - conf.lev

# Filtramos los datos para los dos países de interés
country1 <- 'Colombia'
country2 <- 'Argentina'

sample1 <- m.datos[m.datos$Origin == country1, 'MS_ALL_TOT']
sample2 <- m.datos[m.datos$Origin == country2, 'MS_ALL_TOT']

# Realizamos la prueba t de Student para muestras independientes
p.t <- t.test(sample1, sample2, var.equal = TRUE)

# Mostramos los resultados de la prueba
print(p.t)

# Tomamos la decisión basada en el valor p
decision <- ifelse(p.t$p.value < sig.lev, 'Decisión: Rechazar H0', 'Decisión: No Rechazar H0')

# Mostramos el resultado de la prueba
print(paste0('Con un nivel de significancia de ',
             sig.lev,
             ', la evidencia sugiere que la media de migración total para ',
             country1,
             ' y ',
             country2,
             ' es ',
             ifelse(p.t$p.value < sig.lev, 'diferente.', 'igual.')))

x1_bar <- mean(sample1)
x2_bar <- mean(sample2)
n1 <- length(sample1)
n2 <- length(sample2)
s1_sq <- var(sample1)
s2_sq <- var(sample2)
sp_sq <- ((n1 - 1) * s1_sq + (n2 - 1) * s2_sq) / (n1 + n2 - 2)

###############################################
# Varianza desconocida pero desiguales
###############################################
conf.lev <- 0.05
sig.lev <- 1 - conf.lev
# Realizamos la prueba t de Welch para la diferencia de medias
p.t <- t.test(x = data[data$Origin == 'Colombia', "MS_ALL_TOT"],
              y = data[data$Origin == 'United States', "MS_ALL_TOT"],
              alternative = "two.sided", 
              var.equal = FALSE, 
              conf.level = conf.lev)

# Imprimir el resultado y la decisión
print(p.t)
decision <- ifelse(p.t$p.value < sig.lev, 'Decisión: Rechazar H0', 'Decisión: No Rechazar H0')
print(decision)
print(paste0('Con un nivel de significancia de ',
             sig.lev,
             ', la evidencia sugiere que la diferencia en el promedio de migración entre Colombia y Estados Unidos',
             ifelse(p.t$p.value < sig.lev, ' es significativa.', ' no es significativa.')))
muestra1 = data[data$Origin == 'Colombia', "MS_ALL_TOT"]
muestra2 = data[data$Origin == 'United States', "MS_ALL_TOT"]
x1_bar <- mean(muestra1)
x2_bar <- mean(muestra2)
n1 <- length(muestra1)
n2 <- length(muestra2)
s1_squared <- var(muestra1)
s2_squared <- var(muestra2)

####################################
# Supongamos que tenemos los siguientes datos en nuestro dataset
# ms_mal_low_colombia: Migración de hombres con baja escolaridad en Colombia
# ms_mal_low_usa: Migración de hombres con baja escolaridad en Estados Unidos
# ms_all_tot_colombia: Total de migración en Colombia
# ms_all_tot_usa: Total de migración en Estados Unidos

# Filtrar datos para Colombia
ms_mal_low_colombia <- datos_migration$MS_MAL_LOW[datos_migration$Origin == "Colombia"]

# Filtrar datos para Estados Unidos
ms_mal_low_usa <- datos_migration$MS_MAL_LOW[datos_migration$Origin == "United States"]

# Filtrar datos para Colombia
ms_all_tot_colombia <- datos_migration$MS_ALL_TOT[datos_migration$Origin == "Colombia"]

# Filtrar datos para Estados Unidos
ms_all_tot_usa <- datos_migration$MS_ALL_TOT[datos_migration$Origin == "United States"]


# Proporciones muestrales
p_colombia <- sum(ms_mal_low_colombia) / sum(ms_all_tot_colombia)
p_usa <- sum(ms_mal_low_usa) / sum(ms_all_tot_usa)

# Tamaños de las muestras
n_colombia <- length(ms_all_tot_colombia)
n_usa <- length(ms_all_tot_usa)

# Estadístico de prueba Z
Z <- (p_colombia - p_usa) / sqrt((p_colombia * (1 - p_colombia) / n_colombia) + (p_usa * (1 - p_usa) / n_usa))

# Valor p
p_value <- 2 * (1 - pnorm(abs(Z)))

# Nivel de significancia
alpha <- 0.1

# Regla de decisión
if (p_value < alpha) {
  decision <- "Rechazar H0"
} else {
  decision <- "No Rechazar H0"
}

# Resultado
print(paste("Valor p:", p_value))
print(paste("Decisión:", decision))

mean(ms_mal_low_colombia)
mean(ms_mal_low_usa)

if (decision == "Rechazar H0") {
  conclusion <- "Hay una diferencia significativa entre las medias de migración de Colombia y Estados Unidos."
} else {
  conclusion <- "No hay suficiente evidencia para concluir que hay una diferencia significativa entre las medias de migración de Colombia y Estados Unidos."
}

########################################
# Suponiendo que 'data' es tu conjunto de datos

# Suponiendo que tienes un dataframe llamado df que contiene tus datos
# Primero, filtras las filas para Colombia
data_colombia <- data[data$Origin == "Colombia" & data$Origin == "Colombia", ]

# Luego, selecciona las columnas de interés para hombres de baja habilidad
mr_mal_low_colombia <- data_colombia$MR_MAL_LOW

# Y para mujeres de baja habilidad
mr_fem_low_colombia <- data_colombia$MR_FEM_LOW


# Filtrar las observaciones para Colombia
data_colombia <- data[data$Origin == "Colombia", ]

# Calcular la diferencia entre la tasa de migración total de hombres y mujeres de baja habilidad
difference <- data_colombia$MR_MAL_LOW - data_colombia$MR_FEM_LOW

# Suponiendo que tienes tres vectores: mr_mal_low_colombia, mr_fem_low_colombia y difference
p.t.dep <- t.test(mr_mal_low_colombia, mr_fem_low_colombia, paired = TRUE)


# Realizar la prueba t pareada
p.t.dep <- t.test(difference, paired = TRUE)

# Imprimir el resultado
if (p.t.dep$p.value < 0.05) {
  print("Decisión: Rechazar H0")
} else {
  print("Decisión: No Rechazar H0")
}

# Imprimir la conclusión
if (p.t.dep$p.value < 0.05) {
  print("Con un nivel de significancia de 0.05, existe una diferencia significativa entre la tasa de migración total de hombres y mujeres de baja habilidad en Colombia.")
} else {
  print("Con un nivel de significancia de 0.05, no hay suficiente evidencia para afirmar que existe una diferencia significativa entre la tasa de migración total de hombres y mujeres de baja habilidad en Colombia.")
}

# Supongamos que tienes dos dataframes llamados df_colombia y df_usa que contienen los datos de migración para Colombia y Estados Unidos respectivamente.

# Paso 1: Calcular la diferencia
difference <- mr_mal_low_colombia$MR_MALL_LOW - mr_fem_low_colombia$mr_mal_low

# Paso 2: Calcular la media de las diferencias
mean_difference <- mean(difference)

# Paso 3: Calcular la desviación estándar de las diferencias
sd_difference <- sd(difference)

# Paso 4: Calcular el tamaño de la muestra
n <- nrow(data_colombia)  # Suponiendo que ambos dataframes tienen el mismo número de filas

########################################


# Realizar la prueba de bondad de ajuste
# Hipótesis nula: La distribución de la variable 'ms_mal_low' sigue una distribución teórica esperada
# Hipótesis alternativa: La distribución de la variable 'ms_mal_low' no sigue una distribución teórica esperada
observed_data <- data$MS_MAL_LOW
expected_distribution <- rep(1/length(observed_data), length(observed_data))  # Distribución uniforme como ejemplo
p_value <- chisq.test(table(observed_data), p = expected_distribution)$p.value
chisq.test(table(observed_data))
# Nivel de significancia
sig_level <- 0.05

# Regla de decisión
if (p_value < sig_level) {
  decision <- "Rechazar H0"
} else {
  decision <- "No rechazar H0"
}

# Conclusión
if (decision == "Rechazar H0") {
  conclusion <- paste("Con un nivel de significancia de", sig_level, ", se rechaza la hipótesis nula.")
} else {
  conclusion <- paste("Con un nivel de significancia de", sig_level, ", no se rechaza la hipótesis nula.")
}

# Imprimir la conclusión
print(conclusion)

###################################
levels_origin <- length(unique(data$Origin))

# Contar los niveles únicos de 'destination'
levels_destination <- length(unique(data$Destination))

n <- nrow(data)  # Número de filas en el conjunto de datos
destinations <- c("New York","Los angeles","London","Paris","Tokyo","Sydney","Dubai","Madrid")  # Nuevos destinos
data_updated <- data
data_updated$Destination <- destinations[1:n %% length(destinations) + 1]

# Verificar los nuevos valores en "destination"
table(data_updated$Destination)

# Realizar la prueba de independencia
prueba_independencia <- chisq.test(data_updated$Origin, data_updated$Destination)

# Imprimir el resultado
print(prueba_independencia)

# Extraer el valor p
valor_p <- prueba_independencia$p.value

# Nivel de significancia
alpha <- 0.05

# Decisión basada en el valor p
if (valor_p < alpha) {
  cat("Con un nivel de significancia de", alpha, ", rechazamos la hipótesis nula.\n")
  cat("Hay una relación significativa entre el país de origen y el país de destino de las migraciones.\n")
} else {
  cat("Con un nivel de significancia de", alpha, ", no podemos rechazar la hipótesis nula.\n")
  cat("No hay suficiente evidencia para concluir que hay una relación significativa entre las variables.\n")
}

# Calcular k para la variable Origin
k_origin <- length(unique(data$Origin))

# Calcular k para la variable Destination
k_destination <- length(unique(data$Destination))
# Calcular n
n <- nrow(data)
# Calcular r
r <- k_origin * k_destination

##################################
# Seleccionar las columnas necesarias para la prueba (por ejemplo, ms_mal_low y ms_fem_low)
sample1 <- data$MS_MAL_LOW
sample2 <- data$MS_FEM_LOW

# Calcular las diferencias entre las dos muestras
differences <- sample1 - sample2

# Contar el número de diferencias no nulas (signos diferentes)
n <- sum(differences != 0)

# Calcular el estadístico de prueba
statistic <- n

# Imprimir el estadístico de prueba
print(paste("Estadístico de prueba:", statistic))

# Definir la regla de decisión
alpha <- 0.1  # Nivel de significancia
critical_value <- qbinom(alpha/2, n, 0.5)  # Calcular el valor crítico usando la distribución binomial

# Imprimir el valor crítico
print(paste("Valor crítico:", critical_value))

# Conclusion
print(paste0('Con un nivel de significancia de ',
             alpha,
             ', la evidencia sugiere que la diferencia entre las dos muestras',
             ifelse(abs(statistic) > critical_value, ' es significativa.', ' no es significativa.')))

t.test(sample1, sample2)

# 1. Calcula el número total de observaciones (n)
n <- nrow(data)

# 2. Calcula el número de observaciones positivas (W+)
W_plus <- sum(sample1 > 0)  # Reemplaza 'variable' con la columna que estés evaluando

# 3. Calcula el número de observaciones negativas (W-)
W_minus <- sum(sample2 < 0)  # Reemplaza 'variable' con la columna que estés evaluando

# 4. Calcula el estadístico de la Prueba de Signos (W)
W <- min(W_plus, W_minus)

# Imprime los resultados
print(paste("Número total de observaciones (n):", n))
print(paste("Número de observaciones positivas (W+):", W_plus))
print(paste("Número de observaciones negativas (W-):", W_minus))
print(paste("Estadístico de la Prueba de Signos (W):", W))

