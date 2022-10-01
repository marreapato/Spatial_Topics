require(geoR)
?s100
set.seed(5)
x=sample(1:300,replace = F)
set.seed(156)
y=sample(1:300,replace = F)
?sample
plot(x,y)  #pontos
dados <- data.frame(x=x,y=y)


dados$grupo=NULL
dados$grupo[1:60] <- "selecionado"

dados$grupo[61:300] <- "n_selecionado"

plot(x=dados$x,y=dados$y,pch="o",col=as.factor(dados$grupo),xlab="lat",ylab="long")

dados$x[1:60]#latitude
dados$y[1:60]#longitude     

cbind(lat=dados$x[1:60],long=dados$y[1:60])
