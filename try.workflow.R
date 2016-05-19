args = commandArgs(trailingOnly=TRUE)

#######################################################################################
## Set up packages & data 
library(rjags)
library(coda)
library(mvtnorm)
library(data.table)
source("load.try.data.R")

#######################################################################################
## Do JAGS runs

runs = list(
  uni = FALSE,
  uni.na = FALSE,
  multi = FALSE,
  multi.na = FALSE,
  multi.pft = FALSE,
  multi.pft.na = TRUE)

if(file.exists("try.uni.mult.outputs.Rdata")){
  load("try.uni.mult.outputs.Rdata")
  runs[c("uni","uni.na","multi","multi.na")] <- FALSE
}

#######################################################################################
## Univariate Run

if(uni){

model = "univarite.model.txt"

# Without na's
remove(j.data, j.model, j.out)
print(model)
print(Sys.time())

j.data <- try
N=dim(j.data)[1]; n=dim(j.data)[2]
data = list(Y=j.data, N=N, n=n)
init = NULL
j.model   <- jags.model (file = model,data = data,inits = init, n.chains = 1)
update(j.model, n.iter=1000)
j.out   <- coda.samples (model = j.model,variable.names= c("mu"), n.iter = 10000)
out.uni.try <- j.out


# With na's
remove(j.data, j.model, j.out)
print(model)
print(Sys.time())

j.data <- try.na
N=dim(j.data)[1]; n=dim(j.data)[2]
data = list(Y=j.data, N=N, n=n)
init = NULL
j.model   <- jags.model (file = model,data = data,inits = init, n.chains = 1)
update(j.model, n.iter=1000)
j.out   <- coda.samples (model = j.model,variable.names= c("mu"), n.iter = 10000)
out.uni.try.na <- j.out

#######################################################################################
## Save Data

save(out.uni.try,
     out.uni.try.na,
     file= paste0("output/try.uni.outputs.",args[1],".Rdata"))

} # end if uni
#######################################################################################
## Multivariate Run

if(multi){

model = "multivarite.model.txt"

# Without na's
remove(j.data, j.model, j.out)
print(model)
print(Sys.time())

j.data <- try
N=dim(j.data)[1]; n=dim(j.data)[2]
data = list(Y=j.data, N=N, n=n, Vsig = diag(n), mu0 = rep(0,n), Vmu = diag(.001,n))
init = list(mu = colMeans(j.data), prec.Sigma = solve(cov(j.data)))
j.model   <- jags.model (file = model ,data = data,inits = init, n.chains = 1)
update(j.model, n.iter=1000)
j.out   <- coda.samples (model = j.model,variable.names= c("mu", "Sigma"),n.iter = 10000)
out.multi.try <- j.out


# With na's
remove(j.data, j.model, j.out)
print(model)
print(Sys.time())

j.data <- try.na
N=dim(j.data)[1]; n=dim(j.data)[2]
data = list(Y=j.data, N=N, n=n, Vsig = diag(n), mu0 = rep(0,n), Vmu = diag(.001,n))
init = list(mu = colMeans(j.data), prec.Sigma = solve(cov(j.data)))
j.model   <- jags.model (file = model, data = data, inits = init, n.chains = 1)
update(j.model, n.iter=1000)
j.out   <- coda.samples (model = j.model, variable.names= c("mu"), n.iter = 10000)
out.multi.try.na <- j.out


#######################################################################################
## Save Data

save(out.multi.try,
     out.multi.try.na,
     file= paste0("output/try.multi.outputs.",args[1],".Rdata"))
}# end if uni.na

#######################################################################################
## PFT Run without na's

model = "models/multivariate.grp.model.txt"

j.data <- try.pft.na
Nvars <- dim(j.data)[2]
Nobs = dim(j.data)[1]
Nvars = dim(j.data)[2] 
GroupNo = as.numeric(as.factor(try_full$pft[!is.na(try_full$pft)]))
Ngroup = length(unique(GroupNo))
Gamma = diag(Nvars)
Omega = diag(Nvars)

n.chains = 1
n.iter <- 10000
n.update <- 20
burnin <- 1000


data = list(
  X=j.data,
  Nobs = Nobs,
  Nvars = Nvars, 
  GroupNo = GroupNo,
  Ngroup = Ngroup,
  Gamma = Gamma,
  Omega = Omega
)
init = NULL

print("Compiling JAGS model...")
j.model   <- jags.model (file = model, data = data, inits = init, n.chains = n.chains)

print("Updating JAGS model (burnin)...")
n.update <- 20
for(i in 1:n.update){
  print(sprintf("[%d%%]", i*100/n.update))
  update(j.model, n.iter = round(burnin/n.update))
}
print("Sampling JAGS model...")
j.out   <- coda.samples (model = j.model, n.iter = n.iter,
                         variable.names= c("Sigma","theta","mu"))

out.pft.try.na <- j.out
print("Done! Saving output...")
#######################################################################################
## Save Data
save(out.pft.try.na, file= paste0("output/try.pft.outputs.",args[1],".Rdata"))
print("Done!")
#######################################################################################

# Next run process.mcmc.R


