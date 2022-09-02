#install.packages("geoR")
#install.packages("tidyverse")
library(geoR)
library(tidyverse)

data(s100)
s100
plot(s100)
points(s100)

require(MASS)

BOR <- s100$coords[chull(s100$coords),]#bordadura


names(s100)
s100.bin <- variog(s100)
plot(s100.bin)
s100.bin <- variog(s100, max.dist=1)
s100.vario.env <- variog.mc.env(s100, obj=s100.bin)
plot(s100.bin, env=s100.vario.env)
