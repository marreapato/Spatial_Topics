Altura=c(1.745, 1.75, 1.70); Altura
Peso=c(79,72,60)
plot(Altura, Peso)
cor(Altura, Peso, method="spearman")
cor(sample(Altura), Peso, method="spearman")
cor(sample(Altura), Peso, method="spearman")
cor(sample(Altura), Peso, method="spearman")

cor.test(Altura,Peso)

###########


# Matriz A: dist�ncias entre as localiza��es

sim1 <- grf(50, mean=20, cov.pars=c(0.5, 0.8))
x=sim1$coords[,1]; x
y=sim1$coords[,2]; y
z=sim1$data; z
sim1
n=length(x); n

sim=as.geodata(sim1)
plot(sim)

plot(sim1)

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



