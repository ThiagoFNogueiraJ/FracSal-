install.packages("ggrepel")
install.packages("esquisse")
install.packages("zoo")
detach(package:plyr)
library(RColorBrewer)
library(ggrepel)
library(esquisse)
library(zoo)




pdata<- read.csv("sgdc_anom.csv", sep = ",")

#cRIA O DATASET CAPENAS COM SOLICITAÇÕES QUE FORAM ATENDIDAS E EXISTEM VISTORIAS
pdata <- pdata[!(pdata$desc_status == "CANCELADO"),]
pdata <- pdata[!(pdata$desc_status == "BLOQUEADO"),]
pdata <- pdata[!(pdata$desc_status == "PENDENTE"),]
pdata <- pdata[!(pdata$desc_status == "NÃO VISUALIZADA"),]
pdata <- pdata[!(pdata$desc_status == "VISTORIA PROGRAMADA"),]
pdata <- pdata[!(pdata$desc_orig_solic == "ABERTA EM CAMPO"),]


unique(pdata$desc_orig_solic)

alagamentos <- c("AVALIAÇÃO DE IMÓVEL ALAGADO", "ALAGAMENTO DE IMÓVEL", "ALAGAMENTO DE ÁREA")
pdata$ocorr_solic <- ifelse(pdata$ocorr_solic %in% alagamentos, "ALAGAMENTO", pdata$ocorr_solic)
pdata$ocorr_vist <- ifelse(pdata$ocorr_vist %in% alagamentos, "ALAGAMENTO", pdata$ocorr_vist)
dim(pdata)

sdata <- pdata %>%  mutate(coerencia = ifelse(ocorr_solic == ocorr_vist, TRUE, FALSE))
proporcao <- sdata %>%  group_by(ocorr_solic) %>% 
            summarise(coerencia = mean(coerencia)*100)
proporcao <- as.data.frame(proporcao)
names(proporcao)[2] <- ("Corresp")

proporcao <- proporcao[order(-proporcao$Corresp),]
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

ocorr_emerg = c("DESLIZAMENTO DE TERRA", "DESABAMENTO DE IMOVEL", "DESABAMENTO DE MURO", "EXPLOSAO",
                 "PISTA ROMPIDA", "ARVORE CAIDA", "DESABAMENTO PARCIAL")
delete <- c("VAZAMENTO DE GÁS", "ORIENTAÇÃO TÉCNICA", "AVALIACAO DA AREA", "ARMAZENAMENTO DE MATERIAIS PERIGOSOS", "DESABAMENTO DE BLOCOS DE UMA PEDREIRA" )

prop2 <- filter(proporcao, !(ocorr_solic %in% delete))%>%  
  mutate(name = fct_reorder(ocorr_solic, Corresp)) %>%  
  mutate(resp = ifelse(ocorr_solic %in% ocorr_emerg, "contingência", "prevenção")) 
prop2 <- prop2[order(-prop2$Corresp ),]

##Gráfico para observar indicador de correspondência por ocorrência, de forma geraal
general_grph <- prop2 %>%      ggplot() + geom_bar(aes(x = name, y= Corresp, fill = resp), stat = "identity") +
                               geom_text(aes(x=ocorr_solic, y = Corresp, hjust = -0.1, label = paste0(round(Corresp,1), "%")))+
                               scale_fill_manual(values = c("#3568ab", '#d9d9d9')) +
                               geom_hline(yintercept = 50,  color = "red", linetype = "dashed", size=0.9)+
                               theme_grey() +
                               coord_flip() +
                               theme(legend.position = "none")+
                               labs(title = "Indicador de correspondência por tipo de ocorrência",
                                    subtitle = "Correspondência entre a ocorrência informada na abertura da solicitação e a observada na vistoria!",
                                    caption = "Período analisado: 07/05/2020 a 31/12/2020. 
                                               Total de observações: 96051", 
                                    x = "Ocorrência",
                                    y = "Índice de correspondência (%)") 
#ánálise por ano, para as 5 ocorrências mais frequentes 

prop_ano <- sdata %>%  group_by(year(data_solic), month(data_solic), ocorr_solic) %>% 
                      summarise(coerencia = mean(coerencia)*100) 

prop_ano %>% mutate(mes_ano = year)
names(prop_ano)[1] <- ('ano')
prop_ano
prop_ano[prop_ano$ocorr_solic == 'DESLIZAMENTO DE TERRA',] %>%  ggplot(aes(x=ano, y = coerencia)) +geom_line(aes(color = ocorr_solic))
