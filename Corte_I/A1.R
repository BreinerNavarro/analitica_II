####################################################
# Analítica Descriptiva II
# Taller 1: Análisis empírico de una variable continua
# Estudiante: Breiner Navarro Vergara
####################################################

# SIMULACIÓN DE LOS DATOS

# Fijar semilla usando fecha de nacimiento
set.seed(19970822)

# Generar 225 observaciones
altura <- rnorm(
  n = 225,
  mean = 168,
  sd = 7
)

# Crear dataframe
datos <- data.frame(altura)

# Mostrar primeras filas
head(datos)

# ¿Por qué es importante fijar una semilla?

# La semilla garantiza la reproducibilidad del experimento.
# Esto significa que cualquier persona que ejecute exactamente el   
# mismo código con la misma semilla obtendrá los mismos resultados



# ¿Qué significa simular datos bajo una distribución normal?

# Significa generar observaciones que siguen el comportamiento de una   
# distribución gaussiana, donde la mayoría de los datos se concentran alrededor  
# de la media y la frecuencia disminuye progresivamente hacia los extremos.


# EXPLORACIÓN ESTADÍSTICA INICIAL

media <- mean(datos$altura)
mediana <- median(datos$altura)
desviacion <- sd(datos$altura)
minimo <- min(datos$altura)
maximo <- max(datos$altura)

media
mediana
desviacion
minimo
maximo

# Media: Representa el valor promedio de las alturas.
# Mediana: Es el valor central de los datos ordenados y es menos sensible a valores extremos.
# Desviación estándar: Mide la dispersión respecto a la media. Mientras mayor sea, más dispersos estarán los datos.
# Mínimo: Menor altura observada.
# Máximo: Mayor altura observada.


# VISUALIZACIÓN DE LA DISTRIBUCIÓN

# Histograma

hist(
  datos$altura,
  probability = TRUE,
  col = "purple",
  main = "Distribución de Alturas",
  xlab = "Altura (cm)"
)
# Histograma: Se observa muy bien la clásica forma de campana. La gran mayoría de las 
# personas mide entre 160 cm y 175 cm (con el centro alrededor de los 168 cm). Conforme te alejas 
# hacia estaturas más altas o bajas, la cantidad de casos cae de forma suave y pareja, sin saltos raros.


# Boxplot

boxplot(
  datos$altura,
  col = "green",
  main = "Boxplot de Alturas",
  ylab = "Altura (cm)"
)
# Boxplot: Se confirma que todo está muy bien balanceado. El 50% central de los datos se 
# ubica entre los 163 cm y 172 cm. Solo aparece un dato atípico abajo (alguien de unos 
# 142 cm), pero es algo totalmente esperable por puro azar dentro de una muestra.


# AJUSTE DE DISTRIBUCIÓN NORMAL

hist(
  datos$altura,
  probability = TRUE,
  col = "blue",
  main = "Histograma con Curva Normal",
  xlab = "Altura (cm)"
)

curve(
  dnorm(
    x,
    mean = media,
    sd = desviacion
  ),
  col = "red",
  lwd = 3,
  add = TRUE
)

# Se considera que la distribución normal es un modelo adecuado para representar los datos simulados:
  
# Forma y simetría: Los datos muestran una clara forma de campana y se ajustan muy 
# bien a la curva teórica normal.

# Sin sesgos: La media y la mediana son prácticamente iguales, lo que confirma un comportamiento balanceado.

# Utilidad: Al alinearse con el modelo normal, la variable queda lista y validada para realizar 
# análisis probabilísticos posteriores.



# REGLA EMPÍRICA


# Intervalos

inf1 <- media - desviacion
sup1 <- media + desviacion

inf2 <- media - 2*desviacion
sup2 <- media + 2*desviacion

inf3 <- media - 3*desviacion
sup3 <- media + 3*desviacion

# Porcentajes observados

porc1 <- mean(datos$altura >= inf1 &
                datos$altura <= sup1) * 100

porc2 <- mean(datos$altura >= inf2 &
                datos$altura <= sup2) * 100

porc3 <- mean(datos$altura >= inf3 &
                datos$altura <= sup3) * 100

porc1
porc2
porc3


# TABLA RESUMEN


resultado <- data.frame(
  Intervalo = c("μ ± 1σ",
                "μ ± 2σ",
                "μ ± 3σ"),
  Observado = c(porc1,
                porc2,
                porc3),
  Teorico = c(68,
              95,
              99.7)
)

resultado

# Al analizar los resultados obtenidos en la muestra simulada se observa que:

# El 67.11% de las alturas se encuentra dentro del intervalo μ ± 1σ, valor muy cercano al 68% esperado.
# El 96.89% de las observaciones se encuentra dentro del intervalo μ ± 2σ, ligeramente superior al 95% teórico.
# El 99.56% de los datos se encuentra dentro del intervalo μ ± 3σ, resultado muy próximo al 99.7% establecido por la teoría.