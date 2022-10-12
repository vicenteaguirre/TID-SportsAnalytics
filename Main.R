## Influencia de la altura de los jugadores de beach volley en los ataques
#-------------------------------------------------------------------------------
# Importar librerias y setear los seeds.
pacman::p_load(tidymodels, tidyverse, readr, car, ggplot2, ggExtra)
set.seed(42)

# Leemos la data y la guardamos
data <- 
  read_delim(file = "/Users/vaguirre/Desktop/UAI/2022/2do/TID-SportsAnalytics/full_archive.csv", delim = ",")%>%
  # Filtramos solo hombres.
  filter(gender=="M")%>%
  glimpse()%>%
  # Seleccionamos las variables que vamos a uitilizar.
  select(year,score,w_p1_hgt,w_p1_tot_kills,l_p1_hgt,l_p1_tot_kills)%>%
  mutate(
    # Convertimos la altura de pulgadas a metros
    w_p1_hgt = round(w_p1_hgt*0.0254,2),
    l_p1_hgt = round(l_p1_hgt*0.0254,2)
  )%>%
  # ***** Sacamos los na's, pero esta pendiente este tema
  na.omit()

# Revisamos los tipos de datos.
glimpse(data)

# Revisamos las estadisticas para saber el comportamiento.
summary(data)

#-------------------------------------------------------------------------------
### Graficamos la frecuencia de las diferentes alturas ###

plot(factor(data$w_p1_hgt), main = "Frecuencia de la altura de los jugadores masculinos", col= rainbow(16))

#-------------------------------------------------------------------------------
### Gráfico de disperción con histogramas ###
# Altura vs Puntos Remate

# Guarda el gráfico de dispersión en una variable
p <- ggplot(data, aes(x = w_p1_hgt, y = w_p1_tot_kills)) +
  geom_point()

# Crea el gráfico con histogramas marginales
ggMarginal(p, type = "histogram")

#-------------------------------------------------------------------------------
### Gráfico de disperción con box plots ###
# Altura vs Puntos Remate
scatterplot(x= data$w_p1_hgt,
            y= data$w_p1_tot_kills,
            pch=19,
            smooth=FALSE,
            regLine=FALSE)

#-------------------------------------------------------------------------------
ggplot(data, aes(x = w_p1_hgt, y = w_p1_tot_kills)) +
  geom_density2d_filled()

