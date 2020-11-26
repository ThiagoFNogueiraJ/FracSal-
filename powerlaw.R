library(gridExtra)
install.packages("poweRlaw")
require(scales)
library(ggplot2)
library(tidyr)
library(tibble)
library(dplyr)
library(lubridate)
library(plyr)
library(poweRlaw)

#Lê arquivo 
soc <- read.csv("socddt2.txt",  sep = "\t", header = FALSE )


L1 <- lm(log10(V1) ~log10(V2), data = soc)
L1
#Plota o gráfico log-log 
ggplot(soc, aes(x = V1, y = V2)) + geom_point(color = "steelblue", size =3) +
  scale_x_log10()+
  scale_y_log10()+
  labs(title = "Accumulated distribution of landslides records in Salvador-Ba",
       subtitle = "From 2005-01-01 to 2019-12-31",
       y = "",
       x ="") 

soc_pl <- conpl$new(soc[1:87,]$V2)
est = estimate_xmin(soc_pl)
est
soc_pl$setXmin(est)
plot(soc_pl)
lines(soc_pl, col = 4, lwd =1)



