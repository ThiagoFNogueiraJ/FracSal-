pdata<- read.csv("sgdc_anom.csv", sep = ",")

#cRIA O DATASET CAPENAS COM SOLICITAÇÕES QUE FORAM ATENDIDAS E EXISTEM VISTORIAS
pdata <- pdata[!(pdata$desc_status == "CANCELADO"),]
pdata <- pdata[!(pdata$desc_status == "BLOQUEADO"),]
pdata <- pdata[!(pdata$desc_status == "PENDENTE"),]
pdata <- pdata[!(pdata$desc_status == "NÃO VISUALIZADA"),]
pdata <- pdata[!(pdata$desc_status == "VISTORIA PROGRAMADA"),]


sdata <- pdata %>%  mutate(coerencia = ifelse(ocorr_solic == ocorr_vist, TRUE, FALSE))
proporcao <- sdata %>%  group_by(ocorr_solic) %>% 
            summarise(coerencia = mean(coerencia)*100)
proporcao <- as.data.frame(proporcao)
names(proporcao)[2] <- ("ano_solic")
proporcao[proporcao$ocorr_solic == "AVALIAÇÃO DE IMÓVEL ALAGADO",] %>%  ggplot(aes(x=ano_solic, y = coerencia)) + geom_line()

proporcao <- proporcao[order(-proporcao$coerencia),]
proporcao <- proporcao %>% mutate(mais_freq = NA)

#CALCULA A OCORRÊCNIA MAIS FREQUENTE NA VISTORIA, QUANDO NÃO É A MESMA QUE FOI ESPECIFICADA NA SOLICITAÇÃO 
for (i in proporcao$ocorr_solic) {
  palaga <- sdata[sdata$ocorr_solic == i,]
  freq <- palaga %>% group_by(ocorr_vist) %>% 
          summarise(nv = n()) %>% 
          arrange(-nv)
           mais_comum <- ifelse(freq[1,1] == i, freq[2,1], freq[1,1])
          proporcao[proporcao$ocorr_solic == i,3] <- mais_comum
}

proporcao %>% ggplot(aes(x = ocorr_solic, y = ano_solic)) +geom_bar(stat = "Identity")
