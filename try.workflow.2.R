## LOAD DATA AND PACKAGES #############################################################
library(R2jags)
library(mvtnorm)
library(data.table)
# library(shinystan)

source("load.try.data.R")
source("custom.jags.R")

args = commandArgs(trailingOnly=TRUE)
if(length(args) == 0){
    args = c("c1", "TRUE", "TRUE", "TRUE", "TRUE", "na") # just for testing
}
print(args)

## DATA FORMATTING #############################################################

na <- args[6]
na <- ifelse(exists(na),paste0(".",na),"") 

DT <- get(paste0("try.tps",na))
obvs <- DT[,traits,with=FALSE]

trait_means <- DT[, lapply(.SD, function(x) mean(x, na.rm = T)), 
                  .SDcols = traits]
pft_means <-   DT[, lapply(.SD, function(x) mean(x, na.rm = T)),
                  by = pft, .SDcols = traits]
spec_means <-  DT[, lapply(.SD, function(x) mean(x, na.rm = T)),
                  by = list(pft,species)]

n_obvs=dim(obvs)[1] 
n_traits = length(traits)
n_pfts = dim(pft_means)[1]
n_species = dim(spec_means)[1]

## JAGS RUNS SETUP #############################################################

runs = list(
  uni.trait = as.logical(args[2]),  
  multi.trait = as.logical(args[3]),
  hier.trait.pft = as.logical(args[4]),  
  hier.trait.pft.spec = as.logical(args[5])  
)

#thin <- NULL; thin.n <- ifelse(is.null(thin),1,thin)
n.chains <- 5
n.iter <- 5000

## UNIVARIATE: TRAIT ###########################################################

if(runs$uni.trait){
  
  model = "models/univarite.trait.txt"
  print("-------------------------")
  print(model)
  print(Sys.time())
  
  data <- list(
    obvs = obvs,
    n_traits = n_traits,
    n_obvs = n_obvs)
  
  inits = function() list(mu_trait = as.numeric(trait_means))
  variable.names <- c("mu_trait", "tau_obvs")
  out <- custom.jags(model = model, data = data, inits = inits,
                     n.chains = n.chains, n.iter = n.iter,
                     variable.names = variable.names)
  
  save(out, file = paste0("output/uni.trait",na,".",args[1],".Rdata"))
  remove(model,out)
  print("Done with univariate")
}

## MULTIVARIATE: TRAIT #########################################################

if(runs$multi.trait){
  
  n.iter <- 15000
  model = "models/multivariate.trait.txt"
  print("-------------------------")
  print(model)
  print(Sys.time())
  
  data <- list(
    obvs = obvs,
    n_traits = n_traits,
    n_obvs = n_obvs,
    mu0 = rep(0,n_traits), 
    Sigma0 = diag(.001,n_traits))
  
  inits = function() list(mu_trait = as.numeric(trait_means))
  variable.names <- c("mu_trait", "Sigma_trait")
  
  out <- custom.jags(model = model, data = data, inits = inits,
                     n.chains = n.chains, n.iter = n.iter,
                     variable.names = variable.names)
  
  save(out, file = paste0("output/multi.trait",na,".",args[1],".Rdata"))
  remove(model,out)
  print("Done with multivariate")
}

## HIERARCHICAL: TRAIT, PFT ####################################################

# if(runs$hier.trait.pft){

n.iter <- 30000
model = "models/hierarchical.trait.pft.txt"
print("-------------------------")
print(model)
print(Sys.time())

data <- list(
  pft_obvs = as.numeric(as.factor(DT[,pft])),
  obvs = DT[,traits,with=FALSE],
  
  n_pfts = n_pfts,
  n_traits = n_traits,
  n_obvs = n_obvs, 
  mu0 = rep(0,n_traits), 
  Sigma0 = diag(.001,n_traits),
  Omega = diag(0.001,n_traits))

trait_means_vec <- as.numeric(trait_means)
pft_means_mat <- as.matrix(pft_means[,traits,with=F])

inits = function() list(mu_trait = as.numeric(trait_means_vec),
                        mu_pft_trait = as.matrix(pft_means_mat))
variable.names <- c("mu_trait","Sigma_trait", 
                    "mu_pft_trait", "Sigma_pfts")

out <- custom.jags(model = model, data = data, inits = inits,
                   n.chains = n.chains, n.iter = n.iter,
                   variable.names = variable.names)

save(out, file = paste0("output/hier.trait.pft",na,".",args[1],".Rdata"))
remove(model,out)
print("Done with hierarchical: trait, pft")
# }

## HIERARCHICAL: TRAIT, PFT, SPECIES ###########################################

if(runs$hier.trait.pft.spec){
  
  n.iter <- 60000
  model = "models/hierarchical.trait.pft.spec.txt"
  print("-------------------------")
  print(model)
  print(Sys.time())
  
  data <- list(
    pft_spec = as.numeric(as.factor(spec_means$pft)),
    spec_obvs = as.numeric(as.factor(DT$species)), 
    obvs =  DT[,traits,with=FALSE],
    
    n_traits = n_traits, 
    n_pfts = n_pfts,
    n_species = n_species,
    n_obvs = n_obvs, 
    
    mu0 = rep(0,n_traits), 
    Sigma0 = diag(.001,n_traits),
    Omega = diag(n_traits))
  
trait_means_vec <- as.numeric(trait_means)
pft_means_mat <- as.matrix(pft_means[,traits,with=F])
species_means_mat <- as.matrix(spec_means[,traits,with=F])

  inits = function() list(mu_trait = trait_means_vec,
                          mu_pft_trait = pft_means_mat,
                          mu_spec_pft_trait = species_means_mat)  
  
  variable.names <- c("mu_trait","mu_pft_trait","mu_spec_pft_trait",
                      "Sigma_trait", "Sigma_pfts", "Sigma_spec")
  
  out <- custom.jags(model = model, data = data, inits = inits,
                     n.chains = n.chains, n.iter = n.iter,
                     variable.names = variable.names)
  
  save(out, file = paste0("output/hier.trait.pft.spec",na,".",args[1],".Rdata"))
  remove(model,out)
  print("Done with hierarchical: trait, pft, species")
}

################################################################################
print(Sys.time())
print("All Done!")
