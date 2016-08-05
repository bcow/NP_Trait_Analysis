## LOAD DATA AND PACKAGES #############################################################

library(mvtnorm)
library(data.table)
# library(shinystan)

source("00.common.R")
source("load.try.data.R")
source("custom.jags.R")

source("models/run.uni.R")
source("models/run.multi.R")
source("models/run.hier.R")

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

out.dir <- paste0("output.n",n)
if(!dir.exists(out.dir)) dir.create(out.dir)


args <- commandArgs(trailingOnly=TRUE)
# Possible arguments: 
#   uni -- Run univariate model (all one PFT)
#   uni_group -- Run univariate model (grouped by PFT)
#   multi -- Run multivariate model (all one PFT)
#   multi_group -- Run multivariate model (grouped by PFT)
#   hier -- Run hierarchical model (global and PFT means)
#   no_na -- Use only complete rows (i.e. apply `na.omit` filter to data)
#   n.chains=n -- Run with `n` chains, where `n` is an integer.

if(length(args) == 0){
  args <- c("uni_group", "multi_group", "hier", "n.chains=3")   # For testing
}
print(args)

# Get number of chains from arguments
default.chains <- 3
n.chains.index <- grep("n.chains", args)
n.chains <- ifelse(length(n.chains.index > 0),
                   as.numeric(gsub("n.chains=", "", args[n.chains.index])),
                   default.chains)

global.trait.means <- as.numeric(try.data[, lapply(.SD, mean, na.rm=TRUE),
                                 .SDcols = traits])
names(global.trait.means) <- traits

# Select dataset to use based on "na" argument
if("no_na" %in% args){
    DT.run <- na.omit(try.data)
    na <- "no_na"
} else {
    DT.run <- try.data
    na <- ""
}

DT.run[,pft := as.numeric(pft)]
pfts <- unique(DT.run[,pft])

## UNIVARIATE ##################################################################

if("uni" %in% args){
  print("Start univariate model without grouping")
  dir <- sprintf("%s/uni.trait%s", out.dir, na)
  if(!dir.exists(dir)) dir.create(dir)
  DT <- DT.run
  out <- run.uni(DT)
  save(out, file = sprintf("%s/uni.trait%s.Rdata", dir, na))
  remove(model,out,DT)
  print("Done!")
}

if("uni_group" %in% args){
  print("Start univariate model with grouping") 
  dir <- sprintf("%s/uni.trait%s", out.dir, na)
  if(!dir.exists(dir)) dir.create(dir)
  errors <- numeric(0)
  for(i in 1:length(pfts)){
    print(paste("Running PFT", pfts[i]))
    DT <- DT.run[pft == pfts[i]]
    out <- run.uni(DT)
    if (!all(is.error(out))){
        save(out, file = sprintf("%s/uni.trait%s.pft.%02d.Rdata",dir, na, pfts[i]))
      remove(out)
    } else {
        warning(sprintf("Error running PFT %d. Skipping and moving on", pfts[i]))
    }
    print(paste("Done!", pfts[i]))
    if(length(errors) != 0){
        warning(paste("Errors in the following models:", errors, collapse=" "))
    }
  }
  remove(model,DT)
  print("All Done!")
}

## MULTIVARIATE ################################################################

if("multi" %in% args){
  print("Start multivariate model without grouping")
  dir <- sprintf("%s/multi.trait%s", out.dir, na)
  if(!dir.exists(dir)) dir.create(dir)
  DT <- DT.run
  out <- run.multi(DT)
  save(out, file = sprintf("%s/multi.trait%s.Rdata",dir, na))
  remove(model,out,DT)
  print("Done!")
}

if("multi_group" %in% args){
  print("Start multivariate model with grouping")
  dir <- sprintf("%s/multi.trait%s", out.dir, na)
  if(!dir.exists(dir)) dir.create(dir)
  for(i in 1:length(pfts)){
    DT <- DT.run[pft == pfts[i]]
    out <- run.multi(DT)
    save(out, file = sprintf("%s/multi.trait%s.pft.%02d.Rdata",dir, na, pfts[i]))
    remove(out,DT)
    print(paste("Done!", pfts[i]))
  }
  remove(model)
  print("All Done!")
}

## HIERARCHICAL ################################################################

if("hier" %in% args){
  print("Start hierarchical model")
  dir <- sprintf("%s/hier.trait.pft%s", out.dir, na)
  if(!dir.exists(dir)) dir.create(dir)
  DT <- DT.run
  out <- run.hier(DT)
  save(out, file = sprintf("%s/hier.trait.pft%s.Rdata",dir,na))
  remove(model,out,DT)
  print("Done!")
}

## PRINT WARNINGS ##############################################################

print("=================================")
print("PRINT WARNINGS")

warnings()
