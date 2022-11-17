require(geoR)

###################################
#Gerando Pontos
#install.packages("geoR")
#?s100
set.seed(5)
x=sample(1:300,replace = F)
set.seed(156)
y=sample(1:300,replace = F)
#?sample
plot(x,y)  #pontos
dados <- data.frame(x=x,y=y)

###########################################

dados$grupo=NULL
dados$grupo[1:50] <- "selecionado"

dados$grupo[51:300] <- "n_selecionado"

plot(x=dados$x,y=dados$y,pch="o",col=as.factor(dados$grupo),xlab="lat",ylab="long")

dados$x[1:50]#latitude
dados$y[1:50]#longitude     



dados_nosso <- data.frame(lat=dados$x[1:50],long=dados$y[1:50])
dados_nosso$folhas <- c(26,4,6,1,6,22,15,6,6,15,12,12,5,6,33,64,2,2,9,6,6,7,8,9,7,17,4,6,8,1,10,6,9,9,4,4,3,2,5,5,4,6,2,NA,12,8,4,5,18,7)
dados_nosso$petalas <- c(0,0,0,0,0,0,4,16,5,0,0,0,0,4,0,0,0,0,16,3,0,0,0,0,0,0,0,0,0,0,5,0,16,0,0,0,5,0,0,0,0,0,0,NA,12,8,4,5,18,7)
dados_nosso$flor <- c(0,0,0,0,0,0,1,1,1,0,0,0,0,1,0,0,0,0,1,1,0,0,0,0,0,0,0,0,0,0,1,0,1,0,0,0,1,0,0,0,0,0,0,NA,0,1,1,0,0,0)
dados_nosso$altura <- c(21,0,2,1,0,8,25,15,17,5,4,10,2,13.5,35.5,28,2,2,18.75,12.5,7,6.5,2,2.5,9.5,5,2,1,22.5,0,22,5.5,21.5,5,2,14.5,10,2,5,4,0,1,2,NA,16,16,13,2.5,22.5,12)

dados_nosso <- na.omit(dados_nosso)
summary(dados_nosso)
boxplot(dados_nosso$folhas~dados_nosso$flor)

boxplot(dados_nosso$altura~dados_nosso$flor)#altura parece maior

shapiro.test(sqrt(dados_nosso$altura))#alturaa é normal

boxplot(sqrt(dados_nosso$altura)~dados_nosso$flor)

t.test(sqrt(dados_nosso$altura)~as.factor(dados_nosso$flor),pairwise=T)

ver <- t.test(sqrt(dados_nosso$altura)~as.factor(dados_nosso$flor),pairwise=T)

ver$estimate^2
ver$conf.int^2

#######################################################################
require(geoR)
dados <- dados_nosso
plot(dados)
dados_nosso$raiz_altura <- sqrt(dados$altura)
dadosgeo=as.geodata(dados_nosso, coords.col = 1:2, data.col = 7)

plot(dadosgeo)

###################################


