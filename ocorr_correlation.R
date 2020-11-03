detach(package:plyr)


pdata<- read.csv("sgdc_anom.csv", sep = ",")

#cRIA O DATASET CAPENAS COM SOLICITAÇÕES QUE FORAM ATENDIDAS E EXISTEM VISTORIAS
pdata <- pdata[!(pdata$desc_status == "CANCELADO"),]
pdata <- pdata[!(pdata$desc_status == "BLOQUEADO"),]
pdata <- pdata[!(pdata$desc_status == "PENDENTE"),]
pdata <- pdata[!(pdata$desc_status == "NÃO VISUALIZADA"),]
pdata <- pdata[!(pdata$desc_status == "VISTORIA PROGRAMADA"),]

pdata$ocorr_solic <- ifelse(pdata$ocorr_solic %in% alagamentos, "ALAGAMENTO", pdata$ocorr_solic)
pdata$ocorr_vist <- ifelse(pdata$ocorr_vist %in% alagamentos, "ALAGAMENTO", pdata$ocorr_vist)


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



tipos_ocorr = unique(sdata$ocorr_solic)
matriz_corresp2 <- matrix(nrow = length(tipos_ocorr), ncol = length(tipos_ocorr), dimnames = list(tipos_ocorr, tipos_ocorr))
for (i in tipos_ocorr) {
  for (j in tipos_ocorr) {
    n = count(sdata[sdata$ocorr_solic == i & sdata$ocorr_vist == j,] )
    m = count(sdata[sdata$ocorr_solic == i,])
    v = (n[1,1]/m[1,1])*100
    matriz_corresp2[i,j] = v
  }
}

matriz_corresp2 %>% 
  as.data.frame() %>%
  rownames_to_column("f_id") %>%
  pivot_longer(-c(f_id), names_to = "samples", values_to = "counts") %>% 
  ggplot(aes(x=samples, y=f_id, fill=counts)) + 
  geom_raster() +
  scale_fill_viridis_c()

matriz_corresp2 %>% 
  as.data.frame() %>%
  rownames_to_column("f_id") %>%
  pivot_longer(-c(f_id), names_to = "samples", values_to = "corresp") %>% 
  ggplot(aes(x=samples, y=f_id)) + 
  geom_tile(fill = corresp) +
  geom_text(aes(label = round(corresp, 1))) +
  scale_fill_gradient(low = "white", high = "red") 

matriz <- matriz_corresp2 %>% 
  as.data.frame() %>%
  rownames_to_column("f_id") %>%
  pivot_longer(-c(f_id), names_to = "samples", values_to = "corresp") 

matriz %>%   ggplot(aes(x=samples, y=f_id)) + 
  geom_tile(aes(fill = corresp)) +
  geom_text(aes(label = round(corresp, 1))) +
  scale_fill_gradient(low = "white", high = "red") 