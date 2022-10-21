require(geoR)
#install.packages("geoR")
?s100
set.seed(5)
x=sample(1:300,replace = F)
set.seed(156)
y=sample(1:300,replace = F)
?sample
plot(x,y)  #pontos
dados <- data.frame(x=x,y=y)


dados$grupo=NULL
dados$grupo[1:50] <- "selecionado"

dados$grupo[51:300] <- "n_selecionado"

plot(x=dados$x,y=dados$y,pch="o",col=as.factor(dados$grupo),xlab="lat",ylab="long")

dados$x[1:50]#latitude
dados$y[1:50]#longitude     

dados_nosso <- data.frame(lat=dados$x[1:50],long=dados$y[1:50])
dados_nosso$folhas <- c(26,4,6,1,6,22,15,6,6,15,12,12,5,6,33,64,2,2,9,6,6,7,8,9,7,17,4,6,8,1,10,6,9,9,4,4,3,2,5,5,4,6,2,NA,12,8,4,5,18,7)
