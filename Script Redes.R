#####################################################################################
##                                                                                 ##
##    LOS TEXTOS POÉTICOS DE FERNANDO DE HERRERA.                                  ##
##    APROXIMACIONES DESDE LA ESTILÍSTICA DE CORPUS Y LA ESTILOMETRÍA              ##
##    Tesis doctoral escrita por Laura Hernández Lorenzo                           ##
##    Capítulo 7. Análisis con métodos estilométricos                              ##
##    Apartado 7.8. Un mapa de la evolución de la poesía áurea: análisis de redes  ##
##    Script 4 de la Tesis                                                         ##
##                                                                                 ##
#####################################################################################

# Establecemos el directorio de trabajo donde está la tabla exportada de Gephi
setwd("Desktop")

# Importamos la tabla en R y guardamos los datos de las fechas de nacimiento
my.data = read.csv("ADSO table3.csv")
Birth = my.data[,15]

# Creamos un histograma para las fechas de nacimiento
hist(Birth)

# Creación del boxplot para evaluar la relación entre cronología y modularidad
Modularity = my.data[,20]
boxplot(Birth~Modularity, main = "Modularity class", ylab = "Birth date", col = c("green", "blue", "brown"))

# Para ver si existe una correlación entre fecha de nacimiento y centralidad en el grafo

# Calculamos la diferencia de cada fecha de nacimiento a la media y convertimos a valores absolutos
difference_to_mean_year = Birth - mean(Birth)
abs_difference_to_mean_year = abs(difference_to_mean_year)

# Guardamos los datos de closeness y harmonic closeness centrality
Closeness = my.data[,12]
Harmonic_closeness_centrality = my.data[,13]

# Creamos dos variables que incluyan los dos conjuntos de datos a testar (fecha de nacimiento + medida de centralidad)
x = abs_difference_to_mean_year
y = Closeness

# Generamos la regresión lineal
regression <- lm(x~y)

# Pedimos el resumen de resultados para ver el valor p obtenido
summary(regression)

# Excluimos a Joseph de Litala como outlier
Birth2 = my.data[-(4),15]
my.data2 = my.data[-(4),]

# Repetimos los procesos anteriores del análisis sin Joseph de Litala
difference_to_mean_year2 = Birth2 - mean(Birth2)
abs_difference_to_mean_year2 = abs(difference_to_mean_year2)
Closeness2 = my.data2[,12]
Harmonic_closeness_centrality2 = my.data2[,13]
x = abs_difference_to_mean_year2
y = Harmonic_closeness_centrality2
regression <- lm(x~y)
summary(regression)
