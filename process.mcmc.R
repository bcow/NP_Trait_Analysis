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

out.names <- c(uni,multi,pft)
traits <- colnames(try)

mus <- as.data.frame(matrix(NA, length(out.names)+1, length(traits)))
mus[1,] <- colMeans(try)
for(i in 1:length(out.names)){
  for(j in 1:length(traits)){
    mus[i+1,j] <- mean(get(out.names[i])[traits[j]][,1])
  }
}
colnames(mus) <- traits
rownames(mus) <- c("Data",out.names)
print(mus)

outs <- list()
for(n in out.names){
  outs[[n]] = get(n)
}

iter <- dim(outs[[1]])[1]

save(outs, mus, traits, iter, file = 'output/try.outs.Rdata')

