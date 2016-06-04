## LOAD DATA AND PACKAGES #############################################################

library(R2jags)
library(mvtnorm)
library(data.table)
# library(shinystan)

source("00.common.R")
source("load.try.data.R")
source("custom.jags.R")

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
DT.run <- get(paste0("try", na))[, pft := as.numeric(pft)]
pfts <- unique(DT.run[,pft])

## UNIVARIATE ##################################################################

if(uni){
  print("Start univariate model without grouping")
  DT <- DT.run
  source("models/run.uni.R")
  save(out, file = paste0("output/uni.trait",na,".Rdata"))
  remove(model,out)
  remove(DT)
  print("Done!")
}

if(uni.group){
  print("Start univariate model with grouping")
  errors <- numeric(0)
  for(i in 1:length(pfts)){
    print(paste("Running PFT", pfts[i]))
    DT <- DT.run[pft == pfts[i]]
    source("models/run.uni.R")
    if (!is.error(out)){
        save(out, file = sprintf("output/uni.trait%s.pft.%02.0f.Rdata", na, i))
    } else {
        warning(sprintf("Error running PFT %d. Skipping and moving on", pfts[i]))
    }
    remove(DT)
    print(paste("Done!", pfts[i]))
    if(length(errors) != 0){
        warning(paste("Errors in the following models:", errors, collapse=" "))
    }
  }
  print("All Done!")
}

## MULTIVARIATE ################################################################

if(multi){
  print("Start multivariate model without grouping")
  DT <- DT.run
  source("models/run.multi.R")
  save(out, file = paste0("output/multi.trait",na,".Rdata"))
  remove(model,out)
  remove(DT)
  print("Done!")
}

if(multi.group){
  print("Start multivariate model with grouping")
  for(i in 1:length(pfts)){
    DT <- DT.run[pft == pfts[i]]
    source("models/run.multi.R")
    save(out, file = sprintf("output/multi.trait%s.pft.%02.0f.Rdata", na, i))
    remove(DT)
    print(paste("Done!", pfts[i]))
  }
  print("All Done!")
}

## HIERARCHICAL ################################################################

if(hier){
  print("Start hierarchical model")
  DT <- DT.run
  source("models/run.hier.R")
  save(out, file = paste0("output/hier.trait.pft",na,".Rdata"))
  remove(model,out)
  remove(DT)
  print("Done!")
}

## PRINT WARNINGS ################################################################

print("=================================")
print("PRINT WARNINGS")

warnings()
