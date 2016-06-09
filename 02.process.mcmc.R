library(rjags)
library(coda)
library(mvtnorm)
library(data.table)
library(mcmc)
library(mcmcplots)

source("load.try.data.R")
source("load.try.mcmc.R")

## TRAIT MEANS - global and by pft #############################################


models = dir("output/")
models = models[grep("na",models)]

v = "mu_trait"
out.names <- load.try.mcmc(models, v)

outs <- list()
for(n in out.names){
  outs[[n]] = get(n)
  names(outs[[n]]) <- traits
}
paste(out.names)

out.trait.means <- outs

save(out.trait.means, traits, file = 'output/out.trait.means.Rdata')
eval(parse(text = paste0("remove(",paste(out.names,collapse = ","),")")))

### SIGMA trait ################################################################

models = dir("output/")
models = models[grep("na",models)]

v = "Sigma_trait"
out.names <- load.try.mcmc(models, v)

outs <- list()
for(n in out.names){
  outs[[n]] = get(n)
}
names(outs)
out.Sigma.trait <- outs

save(out.Sigma.trait, traits, file = paste0('output/',v,'.outs.Rdata'))
eval(parse(text = paste0("remove(",paste(out.names,collapse = ","),")")))

### SIGMA pft ################################################################

v = "Sigma_pft"
out.names <- load.try.mcmc(models, v)

outs <- list()
for(n in out.names){
  outs[[n]] = get(n)
}
names(outs)
out.Sigma.pft <- outs

save(out.Sigma.pft, traits, file = paste0('output/',v,'.outs.Rdata'))
eval(parse(text = paste0("remove(",paste(out.names,collapse = ","),")")))

