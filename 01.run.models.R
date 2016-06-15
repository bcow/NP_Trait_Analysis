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
n = 80
Wishart.rate = diag(n, n_traits)
Wishart.df = n_traits
mean = n * Wishart.df
gamma.shape = Wishart.df/2 
gamma.rate = n/2   

out.dir <- paste0("output.n",n)
if(!dir.exists(out.dir)) dir.create(out.dir)
print(out.dir)

args = commandArgs(trailingOnly=TRUE)
if(length(args) == 0){
  args = c("uni=FALSE", "uni.group=TRUE", 
           "multi=FALSE", "multi.group=TRUE", 
           "hier=TRUE", "with.na=TRUE", 
           "n.chains=3") # just for testing
}
print(args)

for(i in 1:length(args)){
  eval(parse(text=args[i]))
}

global.trait.means <- as.numeric(try.na[, lapply(.SD, mean, na.rm=TRUE),
                                 .SDcols = traits])
names(global.trait.means) <- traits

na <- ifelse(with.na, ".na","") 
DT.run <- get(paste0("try", na))[,pft:=droplevels(pft)][, pft := as.numeric(pft)]
pfts <- unique(DT.run[,pft])

## UNIVARIATE ##################################################################

if(uni){
  print("Start univariate model without grouping")
  dir <- sprintf("%s/uni.trait%s",out.dir,na)
  if(!dir.exists(dir)) dir.create(dir)
  DT <- DT.run
  source("models/run.uni.R")
  save(out, file = sprintf("%s/uni.trait%s.Rdata",dir, na))
  remove(model,out,DT)
  print("Done!")
}

if(uni.group){
  print("Start univariate model with grouping") 
  dir <- sprintf("%s/uni.trait%s",out.dir,na)
  if(!dir.exists(dir)) dir.create(dir)
  errors <- numeric(0)
  for(i in 1:length(pfts)){
    print(paste("Running PFT", pfts[i]))
    DT <- DT.run[pft == pfts[i]]
    source("models/run.uni.R")
    if (!all(is.error(out))){
        save(out, file = sprintf("%s/uni.trait%s.pft.%02.0f.Rdata",dir, na, i))
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

if(multi){
  print("Start multivariate model without grouping")
  dir <- sprintf("%s/multi.trait%s",out.dir,na)
  if(!dir.exists(dir)) dir.create(dir)
  DT <- DT.run
  source("models/run.multi.R")
  save(out, file = sprintf("%s/multi.trait%s.Rdata",dir, na, i))
  remove(model,out,DT)
  print("Done!")
}

if(multi.group){
  print("Start multivariate model with grouping")
  dir <- sprintf("%s/multi.trait%s",out.dir,na)
  if(!dir.exists(dir)) dir.create(dir)
  for(i in 1:length(pfts)){
    DT <- DT.run[pft == pfts[i]]
    source("models/run.multi.R")
    save(out, file = sprintf("%s/multi.trait%s.pft.%02.0f.Rdata",dir, na, i))
    remove(out,DT)
    print(paste("Done!", pfts[i]))
  }
  remove(model)
  print("All Done!")
}

## HIERARCHICAL ################################################################

if(hier){
  print("Start hierarchical model")
  dir <- sprintf("%s/hier.trait.pft%s",out.dir,na)
  if(!dir.exists(dir)) dir.create(dir)
  DT <- DT.run
  source("models/run.hier.R")
  save(out, file = sprintf("%s/hier.trait.pft%s.Rdata",dir,na))
  remove(model,out,DT)
  print("Done!")
}

## PRINT WARNINGS ##############################################################

print("=================================")
print("PRINT WARNINGS")

warnings()
