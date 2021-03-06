install.packages("dplyr")
install.packages("tidyr")
install.packages("tibbles")
install.packages("lubridate")
install.packages("plyr")
require(scales)
library(ggplot2)
library(tidyr)
library(tibble)
library(dplyr)
library(lubridate)
library(plyr)

# Carrega o dataset em .csv

sol1418 <- read.csv("solicitações14a18.csv")
str(sol1418)

sol1418 <- readr::read_csv("solicitações14a18.csv")
str(sol1418)

# Exclui as colunas sujas do dataframe
names(sol2018)[7]  <- ("n_casa")
sol2018 <- sol2018[ -c(14:27)]
str(sol2018)
sol2018

#Gera a time series a partir da contagem de amostras 

ts2018 <- data.frame(table(factor(format(sol2018$dt_abertura,"%D"))))
#renomeia as colunas
names(ts2018)[1] <- ("Date")
names(ts2018)[2] <- ("obs")

ts1418 <- data.frame(table(factor(format(sol1418$`Dt Abertura`,"%D"))))
#renomeia as colunas
names(ts1418)[1] <- ("Date")
names(ts1418)[2] <- ("obs")

#reformata a variável para date
ts1418$Date <- as.Date(ts1418$Date, format = "%m/%d/%y")
class(ts1418$Date)

#preenche valores vazios da série temporal
#completa a série com as datas faltantes
ts1418 <- ts1418 %>% 
  complete(Date = seq(ymd("2014-01-01"), ymd("2018-12-31"), by = "1 day"))
#preenche os valores NA com 0
ts1418[is.na(ts1418)] <- 0
hist(ts1418$obs, breaks = 500)

vam <- count(ts1418, 'obs')
vam
str(vam)

p2 <- ggplot(vam, aes(x = obs, y = freq)) + geom_point() +
  scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  theme_bw() 
p2 + annotation_logticks(sides="trbl")  

###########################################################################
<<<<<<< HEAD
s2014 = filter(sol1418, year(`Dt Abertura`) == 2014)

series = list()
for(i in 1:5){
  temp = filter(sol1418, year(`Dt Abertura`) == (2013 +i))
  print(temp)
  series[[i]] <- as.data.frame(count(temp$`Dt Abertura`)) %>% 
                complete(Date =seq(ymd(paste(as.character(2013+i), "-01-01", sep="")), ymd(paste(as.character(2013+i), "-12-31", sep="")), by = "1 day"))
  
}
=======
s2014 = filter(sol1418, year(Dt.Abertura) == 2014)

series = list()
for(i in 1:5){
  temp = filter(sol1418, year(Dt.Abertura) == (2013 +i))
  print(temp)
  series[[i]] <- as.data.frame(count(temp$Dt.Abertura)) %>% 
                complete(Date =seq(ymd(paste(as.character(2013+i), "-01-01", sep="")), ymd(paste(as.character(2013+i), "-12-31", sep="")), by = "1 day"))
  
}



dataset <- read.csv("sgdc_tiago.txt", sep = "\t")
unique(dataset$desc_status)
dataset$desc_status == 'PENDENTE'
str(dataset)

filter(sgdc, desc_status == "PENDENTE")
filter(sgdc, cod_proc == "15208" )
filter(sgdc, sgdc$cod_proc == 9801  )

sgdc_clr <- sgdc
sgdc_clr

count(sgdc[sgdc$ocorr_solic == "DESLIZAMENTO DE TERRA",], "ocorr_solic")
#identificação de processos duplicados 
cont_process <- count(sgdc$cod_proc)
ggplot(cont_process, aes(x = x, y = freq)) + geom_point()
#Seleciona linhas repetidas na coluna 
sgdc[duplicated(sgdc),]
#Seleciona linha de acordo com o valor na coluna
cont_process[cont_process$freq >= 15,]

