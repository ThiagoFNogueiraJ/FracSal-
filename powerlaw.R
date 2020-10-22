require(scales)
library(ggplot2)
library(tidyr)
library(tibble)
library(dplyr)
library(lubridate)
library(plyr)

#Lê arquivo 

soc <- read.csv("socaddtxt.txt",  sep = "\t", header = FALSE )

#Plota o gráfico log-log 
ggplot(soc, aes(x = V1, y = V2)) + geom_point() +
  coord_trans(x = "log10", y = "log10") 
