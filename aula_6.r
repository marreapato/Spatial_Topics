require(ape)
#install.packages("ape")

tr <- rtree(30); tr

x <- rnorm(30); x#
  
w <- 1/cophenetic(tr); w

## set the diagonal w[i,i] = 0 (instead of Inf...):

diag(w) <- 0
Moran.I(x, w)
Moran.I(x, w, alt = "l")

Moran.I(x, w, alt = "g")
