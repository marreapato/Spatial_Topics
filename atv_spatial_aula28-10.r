require(geobr)
require(tidyverse)
require(rio)
require(readr)
require(sf)
require(dplyr)
require(readxl)
library(ggthemes)
library(sp)

#geobr https://cran.r-project.org/web/packages/geobr/vignettes/intro_to_geobr.html

#geobr
datasets <- list_geobr()
###################################
#analisarei a crime_rate
#os dados vieram do kaggle
#disponíveis no link: https://www.kaggle.com/datasets/dbwaller/official-crime-data-sao-paulo-statebrazil-ssp
#Foi utilizado apenas o período de 2020

sp <- read.csv2("https://raw.githubusercontent.com/marreapato/Crimes-No-Estado-de-Sao-Paulo/main/ds_SSP_CrimeRate_SP-BR_utf8_2001-2020_.csv",sep = ";")

head(sp)
names(sp)

#Vírgula em ponto
for(i in 2:8){
  sp[,i] <- as.numeric(gsub(",", ".", gsub("\\.", "", sp[,i])))
}

nrow(as.data.frame(table(sp$Cidade)))#645 municipios

sao_p <- sp[!sp$Ano=="None",]

nrow(as.data.frame(table(sao_p$Cidade)))#645 municipios porém n há dados para
#todos

sp19 <- sao_p %>% filter(Ano==2020)

nrow(as.data.frame(table(sp19$Cidade)))#645 municipios

as=ggplot(data = sp19, mapping = aes(x = as.character(sp19$Cidade), y =as.numeric(as.character(sp19$Homicídio.Doloso.por.100.mil.habitantes)))) +
  geom_col()+coord_flip()+
  labs(title="Homicídio Doloso por 100 mil habitantes no estado de SP em 2020 (por cidade).",x="Região",y="Homicídios")+theme_few()+ylim(min = 0, max = max(as.numeric(as.character(sp19$Homicídio.Doloso.por.100.mil.habitantes))))
as


########################################
mesoscity <- read_municipality("SP",year = 2020)
mesocity=mesos
#Mesorregioes#geo_br
mesos <- read_intermediate_region(code_intermediate = "SP",year=2020)
mesos$name_intermediate
table(sp19$Regiao)

# plot dos municípios de sp
ggplot() +
  geom_sf(data=mesos,aes(fill=mesos$name_intermediate), size=.15) +
  labs(subtitle="Mesorregiões de SP, 2020", size=8,fill="Regiões") +
  theme_minimal() + scale_fill_stata()

#mesorregioes
#https://www.saopaulo.sp.gov.br/spnoticias/governo-do-estado-atualiza-classificacao-do-plano-sp-sem-regressao-de-regioes/
#https://pt.wikipedia.org/wiki/Lista_de_mesorregi%C3%B5es_e_microrregi%C3%B5es_de_S%C3%A3o_Paulo#Mesorregi%C3%A3o_de_Bauru

marilia <- c("Arco-Íris",
             "Bastos",
             "Herculândia",
             "Iacri",
             "Queiroz",
             "Quintana",
             "Tupã","Álvaro de Carvalho",
             "Alvinlândia",
             "Echaporã",
             "Fernão",
             "Gália",
             "Garça",
             "Lupércio",
             "Marília",
             "Ocauçu",
             "Oriente",
             "Oscar Bressane",
             "Pompeia",
             "Vera Cruz","Manduri",
             "Óleo",
             "Ourinhos",
             "Piraju",
             "Ribeirão do Sul",
             "Salto Grande",
             "Santa Cruz do Rio Pardo",
             "São Pedro do Turvo",
             "Sarutaiá",
             "Taguaí",
             "Tejupá",
             "Timburi")



#mudando regiao de marilia
where <- match(sp19$Cidade,marilia)
sp19$Cidade[!is.na(where)]
sp19$Regiao[!is.na(where)] <-"Marília"

sp19$Regiao[sp19$Cidade=="Araraquara"]="Araraquara"

sp19$Regiao[sp19$Cidade=="São Carlos"]="Araraquara"

sp19$Regiao[sp19$Regiao=="Capital"]="São Paulo"  

sp19$Regiao[sp19$Regiao=="Santos"]="São Paulo"  

sp19$Regiao[sp19$Regiao=="Piracicaba"]="Campinas"#mapa wiki

sp19$Regiao[sp19$Regiao=="Grande São Paulo (exclui a Capital)"]="São Paulo" 

df=as.data.frame(table(sp19$Regiao))#igual
df$Var1

soma_homi_100mil <- aggregate(as.numeric(sp19$Homicídio.Doloso.por.100.mil.habitantes),by=list(sp19$Regiao),mean)
df$soma_homi_100mil <- soma_homi_100mil$x
furto <- aggregate(as.numeric(sp19$Furto.por.100.mil.habitantes),by=list(sp19$Regiao),mean)
df$furto <- furto$x
furto <- aggregate(as.numeric(sp19$Roubo.por.100.mil.habitantes),by=list(sp19$Regiao),mean)
df$roubo <- furto$x
furto <- aggregate(as.numeric(sp19$Furto.e.Roubo.de.Veículo.por.100.mil.habitantes),by=list(sp19$Regiao),mean)
df$furto_roubo_veic_habi <- furto$x
furto <- aggregate(as.numeric(sp19$Furto.por.100.mil.veículos),by=list(sp19$Regiao),mean)
df$furto_100mil_veic <- furto$x
furto <- aggregate(as.numeric(sp19$Roubo.por.100.mil.veículos),by=list(sp19$Regiao),mean)
df$roubo_100mil_veic <- furto$x
furto <- aggregate(as.numeric(sp19$Furto.e.Roubo.de.Veículo.por.100.mil.veículos),by=list(sp19$Regiao),mean)
df$furto_roubo_100mil_veic <- furto$x

mesos_sp <- dplyr::left_join(mesos, df, by = c("name_intermediate" = "Var1"))

#sf para o sp (para pegar o centroide)
mesos_sp_sp <- as(mesos_sp,Class = "Spatial")
centroids.df <- as.data.frame(coordinates(mesos_sp_sp))

ggplot() +
  geom_sf(data=mesos_sp,aes(fill=as.numeric(as.character(mesos_sp$soma_homi_100mil))), color="Black", size=.15) +
  scale_fill_continuous()+
  labs(subtitle="Mesorregiões de SP", size=8,fill="Mortes por 100 mil") +
  theme_minimal()+theme(legend.position = "right")  +
  geom_text(aes(label = mesos_sp$name_intermediate, x = centroids.df$V1, y = centroids.df$V2))+ scale_fill_viridis_c()



ggplot() +
  geom_sf(data=mesos_sp,aes(fill=as.numeric(as.character(mesos_sp$furto))), color="Black", size=.15) +
  scale_fill_continuous()+
  labs(subtitle="Mesorregiões de SP", size=8,fill="Furtos por 100 mil") +
  theme_minimal()+theme(legend.position = "right")  +
  geom_text(aes(label = mesos_sp$name_intermediate, x = centroids.df$V1, y = centroids.df$V2))+ scale_fill_viridis_c()+theme(legend.position = "right",axis.title.x=element_blank(),
                                                                                                                             axis.text.x=element_blank(),
                                                                                                                             axis.ticks.x=element_blank(),axis.title.y=element_blank(),
                                                                                                                             axis.text.y=element_blank(),
                                                                                                                             axis.ticks.y=element_blank())

ggplot() +
  geom_sf(data=mesos_sp,aes(fill=as.numeric(as.character(mesos_sp$roubo))), color="Black", size=.15) +
  scale_fill_continuous()+
  labs(subtitle="Mesorregiões de SP", size=8,fill="Roubos por 100 mil") +
  theme_minimal()+theme(legend.position = "right")  +
  geom_text(aes(label = mesos_sp$name_intermediate, x = centroids.df$V1, y = centroids.df$V2))+ scale_fill_viridis_c()+theme(legend.position = "right",axis.title.x=element_blank(),
                                                                                                                             axis.text.x=element_blank(),
                                                                                                                             axis.ticks.x=element_blank(),axis.title.y=element_blank(),
                                                                                                                             axis.text.y=element_blank(),
                                                                                                                             axis.ticks.y=element_blank())




ggplot() +
  geom_sf(data=mesos_sp,aes(fill=as.numeric(as.character(mesos_sp$furto_roubo_veic_habi))), color="Black", size=.15) +
  scale_fill_continuous()+
  labs(subtitle="Mesorregiões de SP", size=8,fill="Furto e Roubos de Veículos por 100 mil") +
  theme_minimal()+theme(legend.position = "right")  +
  geom_text(aes(label = mesos_sp$name_intermediate, x = centroids.df$V1, y = centroids.df$V2))+ scale_fill_viridis_c()+theme(legend.position = "right",axis.title.x=element_blank(),
                                                                                                                             axis.text.x=element_blank(),
                                                                                                                             axis.ticks.x=element_blank(),axis.title.y=element_blank(),
                                                                                                                             axis.text.y=element_blank(),
                                                                                                                             axis.ticks.y=element_blank())





ggplot() +
  geom_sf(data=mesos_sp,aes(fill=as.numeric(as.character(mesos_sp$furto_100mil_veic))), color="Black", size=.15) +
  scale_fill_continuous()+
  labs(subtitle="Mesorregiões de SP", size=8,fill="Furto por 100 mil Veículos") +
  theme_minimal()+theme(legend.position = "right")  +
  geom_text(aes(label = mesos_sp$name_intermediate, x = centroids.df$V1, y = centroids.df$V2))+ scale_fill_viridis_c()+theme(legend.position = "right",axis.title.x=element_blank(),
                                                                                                                             axis.text.x=element_blank(),
                                                                                                                             axis.ticks.x=element_blank(),axis.title.y=element_blank(),
                                                                                                                             axis.text.y=element_blank(),
                                                                                                                             axis.ticks.y=element_blank())




ggplot() +
  geom_sf(data=mesos_sp,aes(fill=as.numeric(as.character(mesos_sp$roubo_100mil_veic))), color="Black", size=.15) +
  scale_fill_continuous()+
  labs(subtitle="Mesorregiões de SP", size=8,fill="Roubo por 100 mil Veículos") +
  theme_minimal()+theme(legend.position = "right")  +
  geom_text(aes(label = mesos_sp$name_intermediate, x = centroids.df$V1, y = centroids.df$V2))+ scale_fill_viridis_c()+theme(legend.position = "right",axis.title.x=element_blank(),
                                                                                                                             axis.text.x=element_blank(),
                                                                                                                             axis.ticks.x=element_blank(),axis.title.y=element_blank(),
                                                                                                                             axis.text.y=element_blank(),
                                                                                                                             axis.ticks.y=element_blank())




ggplot() +
  geom_sf(data=mesos_sp,aes(fill=as.numeric(as.character(mesos_sp$furto_roubo_100mil_veic))), color="Black", size=.15) +
  scale_fill_continuous()+
  labs(subtitle="Mesorregiões de SP", size=8,fill="Furto e Roubo por 100 mil Veículos") +
  theme_minimal()+theme(legend.position = "right")  + scale_fill_viridis_c()+
  geom_text(aes(label = mesos_sp$name_intermediate, x = centroids.df$V1, y = centroids.df$V2))+theme(legend.position = "right",axis.title.x=element_blank(),
                                                                                                     axis.text.x=element_blank(),
                                                                                                     axis.ticks.x=element_blank(),axis.title.y=element_blank(),
                                                                                                     axis.text.y=element_blank(),
                                                                                                     axis.ticks.y=element_blank())





####


# Criar e utilizar legenda:

# Descobrindo os quantis
quantile(mesos_sp$soma_homi_100mil,c(0.25,0.50,0.75,1.00))

# Intervalos para a Legenda
classes = quantile(mesos_sp$soma_homi_100mil,c(0.25,0.50,0.75)); classes
classes_plot = findInterval(mesos_sp$soma_homi_100mil,classes); classes_plot

# Criar legenda
legenda =  c('até 453', '>453 até 531 ', '>531 até 541','>541'); legenda

palette()

cores = c("green","blue",
          "yellow",
          "red")


dados_mapa <- mesos_sp %>% mutate(classes_plot = factor(classes_plot)); dados_mapa

# Mapa
dados_mapa %>%
  ggplot() +
  geom_sf(aes(fill = classes_plot), color = "black") +
  labs(title = "Mapa Média de Homicídios por 100 mil habitantes São paulo", fill = "Frequ�ncia Criada")+
  scale_fill_manual(labels = legenda,
                    values = cores)+ 
  theme_minimal()+
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank())

