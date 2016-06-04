## LOAD DATA AND PACKAGES #############################################################

library(R2jags)
library(mvtnorm)
library(data.table)
# library(shinystan)

source("load.try.data.R")
source("custom.jags.R")

args = commandArgs(trailingOnly=TRUE)
if(length(args) == 0){
  args = c("uni=TRUE", "uni.group=TRUE", 
           "multi=TRUE", "multi.group=TRUE", 
           "hier=TRUE", "with.na=FALSE", 
           "n.chains=5") # just for testing
}
print(args)

for(i in 1:length(args)){
  eval(parse(text=args[i]))
}

na <- ifelse(with.na, ".na","") 
DT.run <- get(paste0("try",na))
pfts <- levels(droplevels(DT.run[,pft]))

## UNIVARIATE ##################################################################

if(uni){
  print("Start univariate model without grouping")
  DT <- DT.run
  source("run.uni.R")
  save(out, file = paste0("output/uni.trait",na,".Rdata"))
  remove(model,out)
  remove(DT)
  print("Done!")
}

if(uni.group){
  print("Start univariate model with grouping")
  for(i in 1:length(pfts)){
    DT <- DT.run[pft == pfts[i]]
    source("run.uni.R")
    save(out, file = paste0("output/uni.trait",na,".pft.",i,".Rdata"))
    remove(DT)
    print(paste("Done!", pfts[i]))
  }
  print("All Done!")
}

## MULTIVARIATE ################################################################

if(multi){
  print("Start multivariate model without grouping")
  DT <- DT.run
  source("run.multi.R")
  save(out, file = paste0("output/multi.trait",na,".Rdata"))
  remove(model,out)
  remove(DT)
  print("Done!")
}

if(multi.group){
  print("Start multivariate model with grouping")
  for(i in 1:length(pfts)){
    DT <- DT.run[pft == pfts[i]]
    source("run.multi.R")
    save(out, file = paste0("output/multi.trait",na,".pft.",i,".Rdata"))
    remove(DT)
    print(paste("Done!", pfts[i]))
  }
  print("All Done!")
}

## HIERARCHICAL ################################################################

if(hier){
  print("Start hierarchical model")
  DT <- DT.run
  source("run.hier.R")
  save(out, file = paste0("output/hier.trait.pft",na,".Rdata"))
  remove(model,out)
  remove(DT)
  print("Done!")
}