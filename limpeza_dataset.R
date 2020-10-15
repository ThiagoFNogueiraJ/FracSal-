filter(sgdc, desc_status == "PENDENTE")
filter(sgdc, cod_proc == "15208" )
filter(sgdc, sgdc$cod_proc == 9801  )

#Seleciona linhas repetidas na coluna 
sgdc[duplicated(sgdc),]


#carrega o dataset
sgdc <- read.csv("sgdc_dois.txt", sep = "\t", encoding = "UTF-8")
str(sgdc)
dim(sgdc)
head(filter(sgdc, desc_status =="PENDENTE"))


#converte datas da solicitação e de vistoria em formato de datahora
sgdc$data_proc <- ymd_hms(sgdc$data_proc)
sgdc$data_vist <- ymd_hms(sgdc$data_vist)

#cria fatores com os tipos de ocorrência e solicitação
tipos_ocorr <- as.factor(unique(sgdc$ocorr_solic))
tipos_origem <- as.factor(unique(sgdc$desc_orig_solic)

#identificação de processos duplicados 
cont_process <- count(sgdc$cod_proc)
ggplot(cont_process, aes(x = x, y = freq)) + geom_point()

#remove linhas duplicadas
sgdc <- distinct(sgdc)
