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

## MU_PFT #################################################################### 

models = dir("output/")[grep(dir("output/"), pattern="hier")]
v = "mu_pft"
out.names <- load.try.mcmc(models,v)

cname <- c()
for(t in traits){
  cname <- c(cname,paste(t,"pft", 1:(dim(outs[[1]])[2]/length(traits)), sep = "."))
}

outs <- list()
for(n in out.names){
  outs[[n]] = get(n)
  cname <- c()
  for(t in traits){
    cname <- c(cname,paste(t,"pft", 1:(dim(outs[[n]])[2]/length(traits)), sep = "."))
  }
  names(outs[[n]]) <- cname
}

outs.mu_pft <- outs

save(outs.mu_pft, traits, file = paste0('output/',v,'.outs.Rdata'))
eval(parse(text = paste0("remove(",paste(out.names,collapse = ","),")")))

### SIGMAs ##################################################################### 

models = dir("output/")
v = "Sigma_trait"
out.names <- load.try.mcmc(models, v)

outs <- list()
for(n in out.names){
  outs[[n]] = get(n)
}
names(outs)
out.Sigma <- outs

save(out.Sigma, traits, file = paste0('output/',v,'.outs.Rdata'))
eval(parse(text = paste0("remove(",paste(out.names,collapse = ","),")")))

### MU_PFT_TRAIT ###############################################################

models = c("hier.trait.pft","hier.trait.pft.na")
v = "mu_pft_trait"
out.names <- load.try.mcmc(models, v)

outs <- list()
for(n in out.names){
  outs[[n]] = get(n)
}
names(outs)


save(outs, traits, file = paste0('output/',v,'.outs.Rdata'))
eval(parse(text = paste0("remove(",paste(out.names,collapse = ","),")")))
