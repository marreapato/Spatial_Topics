#install.packages("WDI")
library(tidyverse)
library(Metrics)
library(MLmetrics)
library(forecast)
library(zoo)
library(nnfor)
#install.packages("sidrar")
library(sidrar)
library(tsfgrnn)
library(car)
library(stringr)
library(dplyr)
library(stringr)
library(viridisLite)
library(zoo)
library(bslib)
library(tidyverse)
library(ggthemes)
library(zoo)
library(tsfgrnn)
library(car)
library(lubridate)
library(chron)
library(data.table)
library(treemapify)
library(plotly)
library(DT)
library(rgdal)
library(geobr)
library(devtools)
#install.packages("devtools")
#devtools::install_github("ipeaGIT/geobr", subdir = "r-package")
library(geobr)
library(sf)
library(tmap)
library(sp)
library(raster)
library(reshape2)
#options("repos")
library(leaflet)
library(spData)
library(WDI)     # for World Bank goodness
library(GetBCBData)
library(ipeadatar)
#install.packages("ipeadatar")
#install.packages("GetBCBData")
library(OECD)
##############################33
#install.packages("sidrar")
# Código de consulta com filtros na tabela 7060 (Sidra/IBGE)
cod_sidra <- "/t/1093/n1/all/n3/all/v/all/p/all/c12716/115236/c12529/118225/d/v1000151%202,v1000284%202,v1000285%202"
# Coleta dos dados com o código
dados_sidra <- sidrar::get_sidra(api = cod_sidra)

###################
#Mesorregioes#geo_br#apenas rode se necessario
mesos <- read_state(year=2017,simplified = T)

mesos$name_state <- toupper(iconv(mesos$name_state,from="UTF-8",to="ASCII//TRANSLIT"))
View(table(dados_sidra$Variável))
mesos <- mesos[,c(3,6)]

#Peso total das carcaças
estados <- dados_sidra %>% filter(Variável=="Animais abatidos"&`Brasil e Unidade da Federação`!="Brasil")

estados$`Brasil e Unidade da Federação` <- toupper(iconv(estados$`Brasil e Unidade da Federação`,from="UTF-8",to="ASCII//TRANSLIT"))
#mesos$name_state <- toupper(mesos$name_state)

estados <- as_tibble(estados)

mesos_sp <- left_join(mesos,estados, by = c("name_state" = "Brasil e Unidade da Federação"))
#mesos_sp_sp <- as(mesos_sp,Class = "Spatial")
#centroids.df <- as.data.frame(coordinates(mesos_sp_sp))
################################################
###########################

# Ano de 2021

estados <- mesos_sp %>% filter(Trimestre=="1º trimestre 2021"|Trimestre=="2º trimestre 2021"|Trimestre=="3º trimestre 2021"|Trimestre=="4º trimestre 2021") %>% group_by(name_state) %>% summarize(suinos_no_ano=sum(Valor))
###################################

estados$ano <- rep(2021,nrow(estados))

tmap_mode(mode = "plot")
tmap_style("natural")
#interactive plot
tm_basemap(leaflet::providers$Stamen.TonerLite) +
  tm_shape(estados) + tm_polygons("suinos_no_ano",textNA = "Sem Registro",labels = c("0mln to 5mln", "5mln to 10mln", "10mln to 15mln", "15mln to 20mln"),colorNA = "grey",title='Suínos Abatidos',)+
  tm_tiles('https://stamen-tiles-{s}.a.ssl.fastly.net/toner-lite/{z}/{x}/{y}{r}.{ext}') +
  tm_layout(legend.position = c("left", "bottom"),
            title.position = c('left', 'bottom'),legend.bg.alpha = 0.3,title.size = 2,title = 'DO')+
  tm_compass(type = "4star", size = 2, position = c("right", "top"))#+
#tm_text("name", size = "AREA")+,
#tm_facets(by='date')

######################################################3

estados2 <- mesos_sp %>% filter(Trimestre=="1º trimestre 2020"|Trimestre=="2º trimestre 2020"|Trimestre=="3º trimestre 2020"|Trimestre=="4º trimestre 2020") %>% group_by(name_state) %>% summarize(suinos_no_ano=sum(Valor))
###################################

estados2$ano <- rep(2020,nrow(estados))
estados <- rbind(estados,estados2)
tmap_mode(mode = "plot")
tmap_style("natural")
#interactive plot
tm <- tm_basemap(leaflet::providers$Stamen.TonerLite) +
  tm_shape(estados) + tm_polygons("suinos_no_ano",colorNA = "grey",title='Suínos Abatidos',)+
  tm_tiles('https://stamen-tiles-{s}.a.ssl.fastly.net/toner-lite/{z}/{x}/{y}{r}.{ext}') +
  tm_layout(legend.position = c("left", "bottom"),
            title.position = c('left', 'bottom'),legend.bg.alpha = 0.3)+
  tm_compass(type = "4star", size = 2, position = c("right", "top"))+
  #tm_text("name", size = "AREA")+,
  tm_facets(by='ano')

#tmap_save(tm, "my_map.png", width = 1000, height = 750, dpi = 300)

#####################################################



######################################################3

estados2 <- mesos_sp %>% filter(Trimestre=="1º trimestre 2019"|Trimestre=="2º trimestre 2019"|Trimestre=="3º trimestre 2019"|Trimestre=="4º trimestre 2019") %>% group_by(name_state) %>% summarize(suinos_no_ano=sum(Valor))
###################################

estados2$ano <- rep(2019,nrow(estados2))
estados <- rbind(estados,estados2)
tmap_mode(mode = "plot")
tmap_style("natural")
#interactive plot
(tm <- tm_basemap(leaflet::providers$Stamen.TonerLite) +
    tm_shape(estados) + tm_polygons("suinos_no_ano",colorNA = "grey",title='Suínos Abatidos',)+
    tm_tiles('https://stamen-tiles-{s}.a.ssl.fastly.net/toner-lite/{z}/{x}/{y}{r}.{ext}') +
    tm_layout(legend.position = c("left", "bottom"),
              title.position = c('left', 'bottom'),legend.bg.alpha = 0.3)+
    tm_compass(type = "4star", size = 2, position = c("right", "top"))+
    #tm_text("name", size = "AREA")+,
    tm_facets(by='ano'))

#tmap_save(tm, "my_map.png", width = 1000, height = 750, dpi = 300)

#####################################################

######################################################3

estados2 <- mesos_sp %>% filter(Trimestre=="1º trimestre 2018"|Trimestre=="2º trimestre 2018"|Trimestre=="3º trimestre 2018"|Trimestre=="4º trimestre 2018") %>% group_by(name_state) %>% summarize(suinos_no_ano=sum(Valor))
###################################

estados2$ano <- rep(2018,nrow(estados2))
estados <- rbind(estados,estados2)
tmap_mode(mode = "plot")
tmap_style("natural")
#interactive plot
(tm <- tm_basemap(leaflet::providers$Stamen.TonerLite) +
    tm_shape(estados) + tm_polygons("suinos_no_ano",textNA = "Sem Registro",colorNA = "grey",title='Suínos Abatidos',)+
    tm_tiles('https://stamen-tiles-{s}.a.ssl.fastly.net/toner-lite/{z}/{x}/{y}{r}.{ext}') +
    tm_layout(legend.position = c("left", "bottom"),main.title = "Distribuição espacial do abatimento de Suínos no Brasil",main.title.size = 2,main.title.position = "center",
              title.position = c('left', 'bottom'),legend.bg.alpha = 0.3)+
    tm_compass(type = "4star", size = 2, position = c("right", "top"))+
    #tm_text("name", size = "AREA")+,
    tm_facets(by='ano'))

#tmap_save(tm, "my_map.png", width = 1000, height = 750, dpi = 300)

#####################################################

table(dados_sidra$`Brasil e Unidade da Federação`[is.na(dados_sidra$Valor)])
#nao tem como fazer para esses estados

##########################################################33
#Peso total das carcaças
estados <- dados_sidra %>% filter(Variável=="Peso total das carcaças"&`Brasil e Unidade da Federação`!="Brasil")

estados$`Brasil e Unidade da Federação` <- toupper(iconv(estados$`Brasil e Unidade da Federação`,from="UTF-8",to="ASCII//TRANSLIT"))
#mesos$name_state <- toupper(mesos$name_state)

estados <- as_tibble(estados)

mesos_sp <- left_join(mesos,estados, by = c("name_state" = "Brasil e Unidade da Federação"))
#mesos_sp_sp <- as(mesos_sp,Class = "Spatial")
#centroids.df <- as.data.frame(coordinates(mesos_sp_sp))
################################################
###########################

# Ano de 2021

estados <- mesos_sp %>% filter(Trimestre=="1º trimestre 2021"|Trimestre=="2º trimestre 2021"|Trimestre=="3º trimestre 2021"|Trimestre=="4º trimestre 2021") %>% group_by(name_state) %>% summarize(suinos_no_ano=sum(Valor))
###################################

estados$ano <- rep(2021,nrow(estados))

tmap_mode(mode = "plot")
tmap_style("natural")
#interactive plot
tm_basemap(leaflet::providers$Stamen.TonerLite) +
  tm_shape(estados) + tm_polygons("suinos_no_ano",textNA = "Sem Registro",labels = c("0mln to 5mln", "5mln to 10mln", "10mln to 15mln", "15mln to 20mln"),colorNA = "grey",title='Suínos Peso Carcaças',)+
  tm_tiles('https://stamen-tiles-{s}.a.ssl.fastly.net/toner-lite/{z}/{x}/{y}{r}.{ext}') +
  tm_layout(legend.position = c("left", "bottom"),
            title.position = c('left', 'bottom'),legend.bg.alpha = 0.3,title.size = 2,title = 'DO')+
  tm_compass(type = "4star", size = 2, position = c("right", "top"))#+
#tm_text("name", size = "AREA")+,
#tm_facets(by='date')

######################################################3

estados2 <- mesos_sp %>% filter(Trimestre=="1º trimestre 2020"|Trimestre=="2º trimestre 2020"|Trimestre=="3º trimestre 2020"|Trimestre=="4º trimestre 2020") %>% group_by(name_state) %>% summarize(suinos_no_ano=sum(Valor))
###################################

estados2$ano <- rep(2020,nrow(estados))
estados <- rbind(estados,estados2)
tmap_mode(mode = "plot")
tmap_style("natural")
#interactive plot
tm <- tm_basemap(leaflet::providers$Stamen.TonerLite) +
  tm_shape(estados) + tm_polygons("suinos_no_ano",colorNA = "grey",title='Suínos Abatidos',)+
  tm_tiles('https://stamen-tiles-{s}.a.ssl.fastly.net/toner-lite/{z}/{x}/{y}{r}.{ext}') +
  tm_layout(legend.position = c("left", "bottom"),
            title.position = c('left', 'bottom'),legend.bg.alpha = 0.3)+
  tm_compass(type = "4star", size = 2, position = c("right", "top"))+
  #tm_text("name", size = "AREA")+,
  tm_facets(by='ano')

#tmap_save(tm, "my_map.png", width = 1000, height = 750, dpi = 300)

#####################################################



######################################################3

estados2 <- mesos_sp %>% filter(Trimestre=="1º trimestre 2019"|Trimestre=="2º trimestre 2019"|Trimestre=="3º trimestre 2019"|Trimestre=="4º trimestre 2019") %>% group_by(name_state) %>% summarize(suinos_no_ano=sum(Valor))
###################################

estados2$ano <- rep(2019,nrow(estados2))
estados <- rbind(estados,estados2)
tmap_mode(mode = "plot")
tmap_style("natural")
#interactive plot
(tm <- tm_basemap(leaflet::providers$Stamen.TonerLite) +
    tm_shape(estados) + tm_polygons("suinos_no_ano",colorNA = "grey",title='Suínos Abatidos',)+
    tm_tiles('https://stamen-tiles-{s}.a.ssl.fastly.net/toner-lite/{z}/{x}/{y}{r}.{ext}') +
    tm_layout(legend.position = c("left", "bottom"),
              title.position = c('left', 'bottom'),legend.bg.alpha = 0.3)+
    tm_compass(type = "4star", size = 2, position = c("right", "top"))+
    #tm_text("name", size = "AREA")+,
    tm_facets(by='ano'))

#tmap_save(tm, "my_map.png", width = 1000, height = 750, dpi = 300)

#####################################################

######################################################3

estados2 <- mesos_sp %>% filter(Trimestre=="1º trimestre 2018"|Trimestre=="2º trimestre 2018"|Trimestre=="3º trimestre 2018"|Trimestre=="4º trimestre 2018") %>% group_by(name_state) %>% summarize(suinos_no_ano=sum(Valor))
###################################

estados2$ano <- rep(2018,nrow(estados2))
estados <- rbind(estados,estados2)
tmap_mode(mode = "plot")
tmap_style("natural")
#interactive plot
(tm <- tm_basemap(leaflet::providers$Stamen.TonerLite) +
    tm_shape(estados) + tm_polygons("suinos_no_ano",textNA = "Sem Registro",colorNA = "grey",title='Peso Total das Carcaças de Suínos',)+
    tm_tiles('https://stamen-tiles-{s}.a.ssl.fastly.net/toner-lite/{z}/{x}/{y}{r}.{ext}') +
    tm_layout(legend.position = c("left", "bottom"),main.title = "Distribuição espacial do peso de carcaça dos Suínos no Brasil",main.title.size = 2,main.title.position = "center",
              title.position = c('left', 'bottom'),legend.bg.alpha = 0.3)+
    tm_compass(type = "4star", size = 2, position = c("right", "top"))+
    #tm_text("name", size = "AREA")+,
    tm_facets(by='ano'))

#tmap_save(tm, "my_map.png", width = 1000, height = 750, dpi = 300)
###################################
library(rgdal)
library(spdep)

#automodelo para peso de carcaça suinos
estados21 <- estados %>% filter(ano==2021) 
estados21 <- na.omit(estados21)
mesos_sp_sp <- as(estados21,Class = "Spatial")
centroids.df <- as.data.frame(coordinates(mesos_sp_sp))

us.nb4<-knearneigh(centroids.df, k=2)
us.nb4<-knn2nb(us.nb4)
us.nb4<-make.sym.nb(us.nb4)


coor <- coordinates(mesos_sp_sp)
cartePPV3.knn <- knearneigh(coor, k=4) #2 neighbours
cartePPV3.nb <- knn2nb(cartePPV3.knn,row.names = mesos_sp_sp$name_state)
PPV3.w <- nb2listw(cartePPV3.nb, style = "W", zero.policy = TRUE)#norm by row
us.wt4<-nb2listw(cartePPV3.nb, style = "W", zero.policy = TRUE)#norm by row
hist(estados21$suinos_no_ano)
#Creating an auto- model
#this involves creating an "auto-covariate"z
estados21$lag_rate<-lag.listw(x=us.wt4, var=(mesos_sp_sp$suinos_no_ano))
#binomial response
fit.bn.auto<-glm(suinos_no_ano~lag_rate+ano, family=poisson(link="log"), data=estados21)
summary(fit.bn.auto)

plot(mesos_sp_sp, col='gray', border='blue', lwd=2,main= "Vizinhos")
plot(PPV3.w, coordinates(mesos_sp_sp), col='red', lwd=2, add=TRUE)#links
#lots of variables missing

#monthly
#death_pop
moran.plot(mesos_sp_sp$suinos_no_ano, PPV3.w, zero.policy=TRUE)
moran.test(mesos_sp_sp$suinos_no_ano,PPV3.w,zero.policy = TRUE,na.action = na.omit)
moran.mc(nsim=100,mesos_sp_sp$suinos_no_ano,PPV3.w,zero.policy = TRUE,na.action = na.omit)
#validated


#death pop local
local.mi.prod<-localmoran(mesos_sp_sp$suinos_no_ano, PPV3.w)

mesos_sp_sp$lmi<-local.mi.prod[,1]

mesos_sp_sp$lmi.p<-local.mi.prod[,5]

mesos_sp_sp$lmi.p.sig<-as.factor(ifelse(local.mi.prod[,5]<.001,"Sig p<.001",
                                         ifelse(local.mi.prod[,5]<.05,"Sig p<.05", "NS" )))




spplot(mesos_sp_sp, "lmi.p.sig", col.regions=c("white", "#E6550D","#FDAE6B"), main = "Peso dos Suinos auto Corr",colorkey=FALSE)
#boxmap
quadrant <- vector(mode="numeric",length=nrow(local.mi.prod))

# centers the variable of interest around its mean
m.qualification <- mesos_sp_sp$suinos_no_ano - mean(mesos_sp_sp$suinos_no_ano)     

# centers the local Moran's around the mean
m.local <- local.mi.prod[,1] - mean(local.mi.prod[,1])    

# significance threshold
signif <- 0.05 

# builds a data quadrant
#positions
quadrant[m.qualification >0 & m.local>0] <- 4#AA  
quadrant[m.qualification <0 & m.local<0] <- 1#BB      
quadrant[m.qualification <0 & m.local>0] <- 2#BA
quadrant[m.qualification >0 & m.local<0] <- 3#AB
#quadrant[local.mi.prod[,5]>signif] <- 0#you can choose not to run it
# plot in r
?dev.size
brks <- c(0,1,2,3,4)
colors <- c("white","blue",rgb(0,0,1,alpha=0.4),rgb(1,0,0,alpha=0.4),"red")
plot(mesos_sp_sp,border="lightgray",col=colors[findInterval(quadrant,brks,all.inside=FALSE)],main="Janeiro")
box()  
legend("bottomleft", legend = c("Nenhum","BB","BA","AB","AA"),
       fill=colors,bty="n")



