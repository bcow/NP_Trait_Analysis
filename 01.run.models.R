## LOAD DATA AND PACKAGES #############################################################

library(R2jags)
library(mvtnorm)
library(data.table)
# library(shinystan)

source("00.common.R")
source("load.try.data.R")
source("custom.jags.R")

# Defining Gamma & Wishart parameters here so we can experiment
# See test.wishart.R for more.
# dgamma(gamma.shape,gamma.rate)
# dwish(Wishart.rate,Wishart.df)

n_traits = 5
n = 1
Wishart.rate = diag(n, n_traits)
Wishart.df = n_traits
mean = n * Wishart.df
gamma.shape = Wishart.df/2 
gamma.rate = n/2   

out.dir <- paste0("output.runjags.n",n)
if(!dir.exists(out.dir)) dir.create(out.dir)

# uni, multi, and hier can either be 
# NULL : the model will not run
# 0 : the model will be run with all the data
# 1-35: the model will be run with pft 1-35
args = commandArgs(trailingOnly=TRUE)
if(length(args) == 0){ # for testing
  args = c("uni=NULL","multi=1", "hier=NULL", "with.na=TRUE", "n.chains=3") }
print(args)

for(i in 1:length(args)){
  eval(parse(text=args[i]))
}

global.trait.means <- as.numeric(try.na[, lapply(.SD, mean, na.rm=TRUE),
                                 .SDcols = traits])
names(global.trait.means) <- traits
na <- ifelse(with.na, ".na","") 
DT.run <- get(paste0("try", na))[,pft:=droplevels(pft)][, pft := as.numeric(pft)]

## UNIVARIATE ##################################################################

if(is.numeric(uni)){
  source("models/run.uni.R")
  print(sprintf("Start univariate model with pft input = %i", uni))
  
  dir <- sprintf("%s/uni.trait%s",out.dir,na)
  if(!dir.exists(dir)) dir.create(dir)
  if(uni == 0){
    DT <- DT.run
    outname <- sprintf("%s/uni.trait%s.global.Rdata",dir, na)
  }else if(uni >0){
    DT <- DT.run[pft == uni]
    outname <- sprintf("%s/uni.trait%s.pft.%02d.Rdata",dir, na, uni)
  }
  out <- run.uni(DT = DT, n.chains = n.chains,
                   Wishart.rate = Wishart.rate, Wishart.df = Wishart.df )
  if (!all(is.error(out))){
    save(out, file = sprintf("%s/uni.trait%s.pft.%02d.Rdata",dir, na, uni))
    remove(out)
  } else {
    warning(sprintf("Error running PFT %d. Skipping and moving on", uni))
  }
  if(length(errors) != 0){
    warning(paste("Errors in the following models:", errors, collapse=" "))
  }
  save(out, file = outname)
  remove(out,DT)
  print(paste("Done!", uni))
  remove(model)
  print("All Done!")
}

## MULTIVARIATE ################################################################

if(is.numeric(multi)){
  source("models/run.multi.R")
  print(sprintf("Start multivariate model with pft input = %i", uni))
  
  dir <- sprintf("%s/multi.trait%s",out.dir,na)
  if(!dir.exists(dir)) dir.create(dir)
  if(multi == 0){
    DT <- DT.run
    outname <- sprintf("%s/multi.trait%s.global.Rdata",dir, na)
  }else if(multi >0){
    DT <- DT.run[pft == multi]
    outname <- sprintf("%s/multi.trait%s.pft.%02d.Rdata",dir, na, multi)
  }
  out <- run.multi(DT = DT, n.chains = n.chains,
                   Wishart.rate = Wishart.rate, Wishart.df = Wishart.df )
  save(out, file = outname)
  remove(out,DT)
  print(paste("Done!", multi))
  remove(model)
  print("All Done!")
}

## HIERARCHICAL ################################################################

if(is.numerical(hier) && hier == 0){
  print("Start hierarchical model")
  dir <- sprintf("%s/hier.trait.pft%s",out.dir,na)
  if(!dir.exists(dir)) dir.create(dir)
  DT <- DT.run
  source("models/run.hier.R")
  out <- run.hier(DT = DT, n.chains = n.chains,
                  Wishart.rate = Wishart.rate, Wishart.df = Wishart.df )
  save(out, file = sprintf("%s/hier.trait.pft%s.Rdata",dir,na))
  remove(model,out,DT)
  print("Done!")
}

## PRINT WARNINGS ##############################################################

print("=================================")
print("PRINT WARNINGS")

warnings()
