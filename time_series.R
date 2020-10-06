# Carrega o dataset em .csv
sol1418 <- read.csv("solicitações14a18.csv")
sol1418$Dt.Abertura <- as_date(sol1418$Dt.Abertura)


#Filtra o dataset por ano
s2014 = filter(sol1418, year(Dt.Abertura) == 2014)
#Converte  as solicitações em uma série temporal de solicitações por dia
ts2014 <- as.data.frame(count(s2014$Dt.Abertura))
#completa a série com as datas faltantes
ts2014 <- ts2014 %>% 
  complete(x = seq(ymd("2014-01-01"), ymd("2014-12-31"), by = "1 day")) 
  ts2014[is.na(ts2014)] <- 0
                              
hist(ts2014$freq, breaks = 500)                
  
