## LOAD DATA AND PACKAGES #############################################################
library(rjags)
library(coda)
library(mvtnorm)
library(data.table)
# library(shinystan)
library(reshape)

source("load.try.data.R")
source("custom.jags.R")

args = commandArgs(trailingOnly=TRUE)
# args = c("c1", "TRUE", "TRUE", "TRUE", "TRUE") # just for dataing
print(args)

## DATA FORMATTING #############################################################

na <- args[6]
na <- ifelse(exists(na),paste0(".",na),"") 
x <- get(paste0("try.tps",na))

try_data <- melt(x, id.vars = c("pft", "AccSpeciesName"))
colnames(try_data) <- c("pft","species","trait","observed")

try_data$pft<- as.numeric(as.factor(try_data$pft))
try_data$species <- as.numeric(as.factor(try_data$species))
try_data$trait <- as.numeric(try_data$trait)

trait_means <- aggregate(observed ~ trait, data = try_data, FUN = mean)
pft_means   <- aggregate(observed ~ trait + pft, data = try_data, FUN = mean)
spec_means  <- aggregate(observed ~ trait + pft + species, data = try_data, 
                         FUN = mean)

n_obvs=dim(try_data)[1] 
n_traits = dim(trait_means)[1]
n_pfts = dim(pft_means)[1]
n_species = dim(spec_means)[1]

n.chains <- 1
burnin <- 1000
n.update <- 20
n.iter <- 10000
thin <- NULL

## JAGS RUNS SETUP #############################################################

runs = list(
  uni.trait = as.logical(args[2]),  
  multi.trait = as.logical(args[3]),
  hier.trait.pft = as.logical(args[4]),  
  hier.trait.pft.spec = as.logical(args[5])  
)

n.chains = 1
n.iter <- 15000*40
n.update <- 20*40
burnin <- 10000
thin <- 40

## UNIVARIATE: TRAIT ###########################################################

if(runs$uni.trait){
  
  model = "models/univarite.trait.txt"
  print("-------------------------")
  print(model)
  print(Sys.time())
  
  data <- list(
    trait_obvs = try_data$trait,
    obvs = try_data$observed,
    n_traits = n_traits,
    n_obvs = n_obvs)
  
  inits = list(mu_trait = trait_means$observed)
  variable.names <- c("mu_trait")
  out <- custom.jags(model = model, data = data, inits = inits,
                     n.chains = n.chains, burnin = burnin, 
                     n.update = n.update, n.iter = n.iter, thin = thin, 
                     variable.names = variable.names)
  
  save(out, file = paste0("output/uni.trait",na,".",args[1],".Rdata"))
  remove(model,out)
  print("Done with univariate")
}

## MULTIVARIATE: TRAIT #########################################################

if(runs$multi.trait){
  
  model = "models/multivariate.trait.txt"
  print("-------------------------")
  print(model)
  print(Sys.time())
  
  data <- list(
    trait_obvs = try_data$trait,
    obvs = try_data$observed,
    n_traits = n_traits,
    n_obvs = n_obvs, 
    mu0 = rep(0,n_traits), 
    Vmu0 = diag(.001,n_traits),
    Omega = diag(n_traits))
  
  inits = list(mu_trait = trait_means$observed)
  variable.names <- c("mu_trait")
  
  out <- custom.jags(model = model, data = data, inits = inits,
                     n.chains = n.chains, burnin = burnin, 
                     n.update = n.update, n.iter = n.iter, thin = thin, 
                     variable.names = variable.names)
  
  save(out, file = paste0("output/multi.trait",na,".",args[1],".Rdata"))
  remove(model,out)
  print("Done with multivariate")
}

## HIERARCHICAL: TRAIT, PFT ####################################################

if(runs$hier.trait.pft){
  
  model = "models/hierarchical.trait.pft.txt"
  print("-------------------------")
  print(model)
  print(Sys.time())
  
  data <- list(
    trait_pft = pft_means$trait,
    pft_obvs = try_data$pft,
    obvs = try_data$observed,
    n_pfts = n_pfts,
    n_traits = n_traits,
    n_obvs = n_obvs, 
    mu0 = rep(0,n_traits), 
    Vmu0 = diag(.001,n_traits),
    Omega = diag(n_traits))
  
  inits = list(mu_trait = trait_means$observed,
               mu_pft = pft_means$observed)
  variable.names <- c("mu_trait","mu_pft")
  
  out <- custom.jags(model = model, data = data, inits = inits,
                     n.chains = n.chains, burnin = burnin, 
                     n.update = n.update, n.iter = n.iter, thin = thin, 
                     variable.names = variable.names)
  
  save(out, file = paste0("output/hier.trait.pft",na,".",args[1],".Rdata"))
  remove(model,out)
  print("Done with hierarchical: trait, pft")
}

## HIERARCHICAL: TRAIT, PFT, SPECIES ###########################################

if(runs$hier.trait.pft.spec){
  
  model = "models/hierarchical.trait.pft.spec.txt"
  print("-------------------------")
  print(model)
  print(Sys.time())
  
  data <- list(
    trait_pft = pft_means$trait, 
    # pft = pft_means$pft, # want to bring this back if I can get tau's working
    pft_spec = spec_means$pft,
    # spec = spec_means$species,
    spec_obvs = try_data$species, 
    obvs = try_data$observed,
    
    n_traits = n_traits, 
    n_pfts = n_pfts,
    n_species = n_species,
    n_obvs = n_obvs, 
    
    mu0 = rep(0,n_traits), 
    Vmu0 = diag(.001,n_traits),
    Omega = diag(n_traits))
  
  inits = list(mu_trait = trait_means$observed, 
               mu_pft = pft_means$observed, 
               mu_spec = spec_means$observed)
  
  variable.names <- c("mu_trait","mu_pft","mu_spec")
  
  out <- custom.jags(model = model, data = data, inits = inits,
                     n.chains = n.chains, burnin = burnin, 
                     n.update = n.update, n.iter = n.iter, thin = thin, 
                     variable.names = variable.names)
  
  save(out, file = paste0("output/hier.trait.pft.spec",na,".",args[1],".Rdata"))
  remove(model,out)
  print("Done with hierarchical: trait, pft, species")
}

################################################################################
print(Sys.time())
print("All Done!")
