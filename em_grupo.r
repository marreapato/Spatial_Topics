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
#plot(x,y)  #pontos
dados <- data.frame(x=x,y=y)

###########################################
#interessante colocar foto da nossa coleta
dados$grupo=NULL
dados$grupo[1:50] <- "selecionado"

dados$grupo[51:300] <- "n_selecionado"

plot(x=dados$x,y=dados$y,pch="o",col=as.factor(dados$grupo),xlab="latitude",ylab="longitude",main="Pontos selecionados em Vermelho")

dados$x[1:50]#latitude
dados$y[1:50]#longitude     


#base de dados
dados_nosso <- data.frame(lat=dados$x[1:50],long=dados$y[1:50])
dados_nosso$folhas <- c(26,4,6,1,6,22,15,6,6,15,12,12,5,6,33,64,2,2,9,6,6,7,8,9,7,17,4,6,8,1,10,6,9,9,4,4,3,2,5,5,4,6,2,NA,12,8,4,5,18,7)
dados_nosso$petalas <- c(0,0,0,0,0,0,4,16,5,0,0,0,0,4,0,0,0,0,16,3,0,0,0,0,0,0,0,0,0,0,5,0,16,0,0,0,5,0,0,0,0,0,0,NA,12,8,4,5,18,7)
dados_nosso$flor <- c(0,0,0,0,0,0,1,1,1,0,0,0,0,1,0,0,0,0,1,1,0,0,0,0,0,0,0,0,0,0,1,0,1,0,0,0,1,0,0,0,0,0,0,NA,0,1,1,0,0,0)
dados_nosso$altura <- c(21,0,2,1,0,8,25,15,17,5,4,10,2,13.5,35.5,28,2,2,18.75,12.5,7,6.5,2,2.5,9.5,5,2,1,22.5,0,22,5.5,21.5,5,2,14.5,10,2,5,4,0,1,2,NA,16,16,13,2.5,22.5,12)

dados_nosso <- na.omit(dados_nosso)
summary(dados_nosso)

####################################################333
#correlção entre altura e folhas, correlação entre altura e petalas
cor.test(dados_nosso$altura,dados_nosso$folhas,method = "spearman")

plot(dados_nosso$altura,dados_nosso$folhas,main="Dispersão Altura e Folhas",xlab="Altura",ylab="Folhas")


plot(dados_nosso$altura,dados_nosso$petalas,main="Dispersão Altura e Pétalas",xlab="Altura",ylab="Pétalas")

cor.test(dados_nosso$altura,dados_nosso$petalas,method = "spearman")

shapiro.test(dados_nosso$altura)
shapiro.test(dados_nosso$folhas)
shapiro.test(dados_nosso$petalas)

boxplot(dados_nosso$altura~dados_nosso$flor,main="Boxplot da Altura (Flor, Não Flor)",xlab="flor?",ylab="altura (cm)")
#######################################################################
require(geoR)
dados <- dados_nosso
plot(dados)
dadosgeo=as.geodata(dados, coords.col = 1:2, data.col = 6)
#Avaliando Altura
#plot(dadosgeo)

require(MASS)
bor <- dadosgeo$coords[chull(dadosgeo$coords),]
dadosgeo$borders <- bor
plot(dadosgeo)
#anisotropia em x, porem o mesmo não é identificado em y
#parece n haver padrão


points(dadosgeo,main="Distribuição dos Pontos",xlab="Latitude",ylab="Longitude")
shapiro.test(dados$altura)# não sao normais
#n podemos variograma

#teste de mantel
#h0: padrão é aleatório
#h1: padrão não é aleatório

x=dados$lat; x
y=dados$long; y
z=dados$altura; z
n=length(x); n

sim=dadosgeo
plot(sim)

# Matriz L: matriz dos pontos (xi,yi)
L=matrix(c(x,y), n, 2)
L

# Calcular n (n�mero de pares (xi,yi)): ordem da matriz sim�trica A (matriz
# das dist�ncias das localiza��es)

n=nrow(L)
n

# Antes de encontrar a matriz A, cria-se uma matriz nula A

A=matrix(c(rep(0)),n,n)
# dependendo da vers�o do R, pode ser A=matrix(c(0),n,n)

for (i in 1:n) {
  for (j in 1:n) {
    if (i==j) {
      A[i,j]=0 }
    else {
      A[i,j]=sqrt((L[i,1]-L[j,1])^2+(L[i,2]-L[j,2])^2) }
  } }

A

# Matriz B: matriz das dist�ncias entre as repostas

# Matriz Z: cont�m as respostas

Z=matrix(c(z),n,1)
Z

# Calcula-se m (n�mero de observa��es de Z): ordem da matriz sim�trica B

m=nrow(Z)
m

# Como em A, antes de encontrar a matriz B, cria-se uma matriz nula B

B=matrix(c(rep(0)),m,m)
# dependendo da vers�o do R, pode ser B=matrix(c(0), m,m)

for (i in 1:m) {
  for (j in 1:m) {
    if (i==j) {
      B[i,j]=0 }
    else {
      B[i,j]=sqrt((Z[i,1]-Z[j,1])^2) }
  } }

B

A1=matrix(c(A),n,1)
B1=matrix(c(B),m,1)
A1
B1

cor(A1,B1,method='pearson')

# Teste de Mantel

proc=function(q){
  # Lendo os dados
  
  aux=NULL
  # Teste de aleatoriza��o
  for (i in 1:q){
    A1.al=sample(A1)
    correlAl=cor(A1.al,B1, method="pearson")  
    if (abs(correlAl)>=abs(cor(A1,B1, method="pearson")))
      aux[i]=1
    else aux[i]=0
  }
  pvalor=mean(aux)
  pvalor
  
}

proc(10000)
#o padrao é aleatorio
#logo n há padrão na altura das plantas

############################################


#######################################################################
require(geoR)
dados <- dados_nosso
plot(dados)
dadosgeo=as.geodata(dados, coords.col = 1:2, data.col = 4)
#Avaliando Pétalas
#plot(dadosgeo)

require(MASS)
bor <- dadosgeo$coords[chull(dadosgeo$coords),]
dadosgeo$borders <- bor
plot(dadosgeo)
#anisotropia em x, porem o mesmo não é identificado em y
#parece n haver padrão


points(dadosgeo,main="Distribuição dos Pontos",xlab="Latitude",ylab="Longitude")
shapiro.test(dados$altura)# não sao normais
#n podemos variograma

#teste de mantel
#h0: padrão é aleatório
#h1: padrão não é aleatório

x=dados$lat; x
y=dados$long; y
z=dados$petalas; z
n=length(x); n

sim=dadosgeo
plot(sim)

# Matriz L: matriz dos pontos (xi,yi)
L=matrix(c(x,y), n, 2)
L

# Calcular n (n�mero de pares (xi,yi)): ordem da matriz sim�trica A (matriz
# das dist�ncias das localiza��es)

n=nrow(L)
n

# Antes de encontrar a matriz A, cria-se uma matriz nula A

A=matrix(c(rep(0)),n,n)
# dependendo da vers�o do R, pode ser A=matrix(c(0),n,n)

for (i in 1:n) {
  for (j in 1:n) {
    if (i==j) {
      A[i,j]=0 }
    else {
      A[i,j]=sqrt((L[i,1]-L[j,1])^2+(L[i,2]-L[j,2])^2) }
  } }

A

# Matriz B: matriz das dist�ncias entre as repostas

# Matriz Z: cont�m as respostas

Z=matrix(c(z),n,1)
Z

# Calcula-se m (n�mero de observa��es de Z): ordem da matriz sim�trica B

m=nrow(Z)
m

# Como em A, antes de encontrar a matriz B, cria-se uma matriz nula B

B=matrix(c(rep(0)),m,m)
# dependendo da vers�o do R, pode ser B=matrix(c(0), m,m)

for (i in 1:m) {
  for (j in 1:m) {
    if (i==j) {
      B[i,j]=0 }
    else {
      B[i,j]=sqrt((Z[i,1]-Z[j,1])^2) }
  } }

B

A1=matrix(c(A),n,1)
B1=matrix(c(B),m,1)
A1
B1

cor(A1,B1,method='pearson')

# Teste de Mantel

proc=function(q){
  # Lendo os dados
  
  aux=NULL
  # Teste de aleatoriza��o
  for (i in 1:q){
    A1.al=sample(A1)
    correlAl=cor(A1.al,B1, method="pearson")  
    if (abs(correlAl)>=abs(cor(A1,B1, method="pearson")))
      aux[i]=1
    else aux[i]=0
  }
  pvalor=mean(aux)
  pvalor
  
}

proc(10000)
#o padrao é aleatorio
#logo n há padrão na quantidade de pétalas das plantas

##############


#######################################################################
require(geoR)
dados <- dados_nosso
plot(dados)
dadosgeo=as.geodata(dados, coords.col = 1:2, data.col = 3)
#Avaliando Folhas
#plot(dadosgeo)

require(MASS)
bor <- dadosgeo$coords[chull(dadosgeo$coords),]
dadosgeo$borders <- bor
plot(dadosgeo)
#anisotropia em x, porem o mesmo não é identificado em y
#parece n haver padrão


points(dadosgeo,main="Distribuição dos Pontos",xlab="Latitude",ylab="Longitude")
shapiro.test(dados$altura)# não sao normais
#n podemos variograma

#teste de mantel
#h0: padrão é aleatório
#h1: padrão não é aleatório

x=dados$lat; x
y=dados$long; y
z=dados$folhas; z
n=length(x); n

sim=dadosgeo
plot(sim)

# Matriz L: matriz dos pontos (xi,yi)
L=matrix(c(x,y), n, 2)
L

# Calcular n (n�mero de pares (xi,yi)): ordem da matriz sim�trica A (matriz
# das dist�ncias das localiza��es)

n=nrow(L)
n

# Antes de encontrar a matriz A, cria-se uma matriz nula A

A=matrix(c(rep(0)),n,n)
# dependendo da vers�o do R, pode ser A=matrix(c(0),n,n)

for (i in 1:n) {
  for (j in 1:n) {
    if (i==j) {
      A[i,j]=0 }
    else {
      A[i,j]=sqrt((L[i,1]-L[j,1])^2+(L[i,2]-L[j,2])^2) }
  } }

A

# Matriz B: matriz das dist�ncias entre as repostas

# Matriz Z: cont�m as respostas

Z=matrix(c(z),n,1)
Z

# Calcula-se m (n�mero de observa��es de Z): ordem da matriz sim�trica B

m=nrow(Z)
m

# Como em A, antes de encontrar a matriz B, cria-se uma matriz nula B

B=matrix(c(rep(0)),m,m)
# dependendo da vers�o do R, pode ser B=matrix(c(0), m,m)

for (i in 1:m) {
  for (j in 1:m) {
    if (i==j) {
      B[i,j]=0 }
    else {
      B[i,j]=sqrt((Z[i,1]-Z[j,1])^2) }
  } }

B

A1=matrix(c(A),n,1)
B1=matrix(c(B),m,1)
A1
B1

cor(A1,B1,method='pearson')

# Teste de Mantel

proc=function(q){
  # Lendo os dados
  
  aux=NULL
  # Teste de aleatoriza��o
  for (i in 1:q){
    A1.al=sample(A1)
    correlAl=cor(A1.al,B1, method="pearson")  
    if (abs(correlAl)>=abs(cor(A1,B1, method="pearson")))
      aux[i]=1
    else aux[i]=0
  }
  pvalor=mean(aux)
  pvalor
  
}

proc(10000)
#o padrao é aleatorio
#logo n há padrão na quantidade de folhas das plantas

