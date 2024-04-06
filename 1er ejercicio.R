library(readr)
m.datos <- read.csv("DataSetByGender_Aggregates_Excel_AggregateDS.csv")


# Supongamos que queremos probar si el promedio de migración total de hombres de 25 años o más en USA es mayor que 410.
install.packages("TeachingDemos")
library(TeachingDemos)

# Extraemos los datos para USA
m.usa <- m.datos[m.datos$Origin == 'Afghanistan', 'MS_MAL_TOT']

# Definimos el valor dado y el nivel de confianza
m.prueba <- 410
conf.lev <- 0.95
sig.lev <- 1 - conf.lev

sum(is.na(m.datos$ms_mal_tot))

# Realizamos la prueba z
p.zeta <- z.test(m.usa,
                 sd = sd(m.usa),
                 mu = m.prueba,
                 alternative = 'greater',
                 conf.level = conf.lev)

# Mostramos el valor p y su estructura
p.zeta
str(p.zeta)

# Tomamos la decisión basada en el valor p
decision <- ifelse(p.zeta$p.value < sig.lev, 'Decisión: Rechazar H0', 'Decisión: No Rechazar H0')

# Mostramos el resultado de la prueba
print(paste0('Con un nivel de significancia de ',
             sig.lev,
             ', la evidencia sugiere que el promedio de migración total de hombres de 25 años o más en USA',
             ifelse(p.zeta$p.value < sig.lev, ' es mayor que ', ' no es mayor que o igual a '),
             m.prueba))

