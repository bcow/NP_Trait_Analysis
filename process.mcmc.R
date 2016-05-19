library(rjags)
library(coda)
library(mvtnorm)
library(data.table)

source("load.try.data.R")

source("load.try.mcmc.R")
names <- colnames(try)
uni <- load.try.mcmc("uni",names)
multi <- load.try.mcmc("multi",names)
pft <- load.try.mcmc("pft",names)

t <- paste("save(", paste(c(uni,multi,pft), collapse = ","), ", file = 'output/try.outs.Rdata')")

eval(parse(text = t))

