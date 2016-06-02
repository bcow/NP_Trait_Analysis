library(rjags)
library(coda)
library(mvtnorm)
library(data.table)
library(mcmc)
library(mcmcplots)

source("load.try.data.R")
source("load.try.mcmc.R")

## MU_TRAIT #################################################################### 

v = "mu_trait"

uni <- load.try.mcmc("uni.trait",traits,v)
multi <- load.try.mcmc("multi.trait",traits,v)
hier.tp <- load.try.mcmc("hier.trait.pft",traits,v)
#hier.tps <- load.try.mcmc("hier.trait.pft.spec",traits,v)

out.names <- c(uni,multi,hier.tp,hier.tps)
outs <- list()
for(n in out.names){
  outs[[n]] = get(n)
}
paste(out.names)
mus <- as.data.frame(matrix(NA, length(out.names)+1, length(traits)))
mus[1,] <- colMeans(try)
for(i in 1:length(out.names)){
  for(j in 1:length(traits)){
    mus[i+1,j] <- mean(get(out.names[i])[j][,1])
  }
}
colnames(mus) <- traits
rownames(mus) <- c("Data",out.names)
print(mus)

iter <- dim(outs[[1]])[1]

save(outs, mus, traits, iter, file = paste0('output/',v,'.outs.Rdata'))

## MU_PFT #################################################################### 

v = "mu_pft"

hier.tp <- load.try.mcmc("hier.trait.pft",traits,v)
hier.tps <- load.try.mcmc("hier.trait.pft.spec",traits,v)

out.names <- c(hier.tp,hier.tps)
outs <- list()
for(n in out.names){
  outs[[n]] = get(n)
}

iter <- dim(outs[[1]])[1]

save(outs, traits, iter, file = paste0('output/',v,'.outs.Rdata'))


eval(parse(text = paste0("remove(",paste(out.names,collapse = ","),")")))

## MU_SPEC #################################################################### 

v = "mu_spec"

hier.tps <- load.try.mcmc("hier.trait.pft.spec",traits,v)

out.names <- c(hier.tps)
outs <- list()
for(n in out.names){
  outs[[n]] = get(n)
}

iter <- dim(outs[[1]])[1]

save(outs, traits, iter, file = paste0('output/',v,'.outs.Rdata'))
