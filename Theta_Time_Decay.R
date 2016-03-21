library(dplyr)
library(ggplot2)

#Parametros para filtrar opcion
Strike_option <- 1500
Type_option <- 'PUT'

#Cargando dataset con la informacion de vencimientos y primas
tmp <- read.csv(file = 'data/SPX-BID-ASK-2015-08-24.csv', stringsAsFactors = F)
tmp <- filter(tmp, Strike == Strike_option, Type == Type_option)

#Cogemos el mid_price para hacer el estudio del decaimiento
tmp$mid_price <- (tmp$Ask + tmp$Bid) / 2
tmp$Bid <- NULL
tmp$Ask <- NULL

#Ordenamos de mayor a menor
tmp <- arrange(tmp, desc(Days_expiration))

#Añadimos la diferencia de numero de dias y de prima
tmp$Diff_days <- c(diff(tmp$Days_expiration) * -1, NA)
tmp$Diff_price <- c(diff(tmp$mid_price) * -1, NA)

#Calculamos lo que baja la prima con respecto a la diferencia de dias
tmp$Diff_price_per_days <- (tmp$Diff_price / tmp$Diff_days) * 100

#Plot
ggplot(data = tmp, aes(x = reorder(Option, -Days_expiration), y = Diff_price_per_days)) + 
  geom_point()

