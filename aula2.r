require(geoR)
borda=read.table("contorno_bahia1.txt", head=T)
plot(borda)
summary(borda)

dados=read.table("Bahia_G.txt", head=T, dec=",")
dados
plot(dados)
dadosgeo=as.geodata(dados, coords.col = 1:2, data.col = 4)
dadosgeo$borders=borda
plot(dadosgeo)
borders=borda
dadosgeo=as.geodata(dados, coords.col = 1:2, data.col = 5, borders=borda)
plot(dadosgeo)

###################################


require(MASS)


s100.bin <- variog(dadosgeo)
plot(s100.bin)

s100.bin <- variog(s100, max.dist=1)
s100.vario.env <- variog.mc.env(s100, obj=s100.bin)
plot(s100.bin, env=s100.vario.env)


s100.fit.exp <- variofit(s100.bin, ini=c(0.9,.35), cov.model="exp",nug=FALSE, fix.kappa=FALSE)
summary(s100.fit.exp)
s100.fit.exp
plot(s100.bin)
lines(s100.fit.exp,col="green")
s100.fit.exp
