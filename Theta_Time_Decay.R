library(dplyr)
library(ggplot2)

#Parametros para filtrar opcion
Strike_option <- 225
Type_option <- 'PUT'

#Cargando dataset con la informacion de vencimientos y primas
tmp <- read.csv(file = '../Time-and-Sales_TOS/data/SPY_BID_ASK_Prices/SPY-BID-ASK-2017-01-17.csv', stringsAsFactors = F)
tmp <- filter(tmp, Strike == Strike_option, Type == Type_option)

#COMPROBAR QUE NO HAY VENCIMIENTOS MINI O COSAS ASI QUE MODIFICAN LAS PRIMAS SIN SENTIDO

#Cogemos el mid_price para hacer el estudio del decaimiento
tmp$mid_price <- (tmp$Ask + tmp$Bid) / 2
tmp$Bid <- NULL
tmp$Ask <- NULL

#Ordenamos de mayor a menor
tmp <- arrange(tmp, desc(Days_expiration))

#Añadimos la diferencia de numero de dias y de prima
tmp$Diff_days_next_expiration <- c(diff(tmp$Days_expiration) * -1, NA)
tmp$Diff_price_next_expiration <- c(diff(tmp$mid_price) * -1, NA)

#Calculamos lo que baja la prima con respecto a la diferencia de dias
tmp$Diff_price_per_days <- (tmp$Diff_price_next_expiration / tmp$Diff_days_next_expiration) * 100

#Funcion x ^ (1/ log(y)) para calcular la mayor eficiencia de decaimiento con respecto a los días dentro del mercado
tmp$rank_most_efficicent_less_days <-  tmp$Diff_price_per_days ^ (1 / log(tmp$Diff_days_next_expiration))

#########

#Plot - Por la proporción de pérdida entre vencimientos
ggplot(data = tmp, aes(x = reorder(Option, -Days_expiration), y = Diff_price_per_days)) + 
  theme(axis.text.x=element_text(angle=45, hjust=1)) +
  ggtitle(paste(unique(tmp$Today), '-', unique(tmp$Type), unique(tmp$Strike))) +
  geom_point() 
  
#Plot - Por ratio eficiencia de máxima prima en menos dias
ggplot(data = tmp, aes(x = reorder(Option, -Days_expiration), y = rank_most_efficicent_less_days)) + 
  theme(axis.text.x=element_text(angle=45, hjust=1)) +
  ggtitle(paste(unique(tmp$Today), '-', unique(tmp$Type), unique(tmp$Strike))) +
  geom_point() 


View(tmp %>% arrange(desc(rank_most_efficicent_less_days)))


