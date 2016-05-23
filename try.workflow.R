################################################################################
## Set up packages & data
library(rjags)
library(coda)
library(mvtnorm)
library(data.table)

args = commandArgs(trailingOnly=TRUE)
run_name <- args[1]

setwd(getwd())
source(file.path("load.try.data.R"))
source(file.path("custom.jags.R"))

################################################################################
## JAGS runs setup 
runs = list(
  uni = FALSE,
  uni.na = FALSE,
  multi = FALSE,
  multi.na = FALSE,
  multi.pft = TRUE,
  multi.pft.na = TRUE)

# needs to be the same for all the runs

n.chains = 1
n.iter <- 15000
n.update <- 20
burnin <- 1000
thin <- 0

################################################################################
## Univariate Run
model = "models/univarite.model.txt"
uni.save <- c()
if(runs$uni){
  # Without na's
  print(model)
  print(Sys.time())
  j.data <- try
  N=dim(j.data)[1]; n=dim(j.data)[2]
  data = list(Y=j.data, N=N, n=n)
  inits = NULL
  var.names = c("mu")
  out.uni.try <- custom.jags(model,data,inits,n.chains,burnin,n.update,var.names)
  uni.save <- c(uni.save, "out.uni.try")
}
if(runs$uni.na){
  # With na's
  print(model)
  print(Sys.time())
  j.data <- try.na
  N=dim(j.data)[1]; n=dim(j.data)[2]
  data = list(Y=j.data, N=N, n=n)
  inits = NULL
  var.names = c("mu")
  out.uni.try.na <- custom.jags(model,data,inits,n.chains,burnin,n.update,var.names)
  uni.save <- c(uni.save, "out.uni.try.na")
}
################################################################################
## Save Data
if(length(uni.save > 0)){
  t <- paste0("save(", paste(uni.save, collapse = ','),", file='",
              paste0('output/try.uni.outputs.',args[1],'.Rdata'),"')")
  eval(parse(text=t))
  print("Done with univariate")
}

################################################################################
## Multivariate Run
model = "models/multivarite.model.txt"
multi.save <- c()
if(runs$multi){
  # Without na's
  print(model)
  print(Sys.time())
  j.data <- try
  N=dim(j.data)[1]; n=dim(j.data)[2]
  data = list(Y=j.data, N=N, n=n, Vsig = diag(n), mu0 = rep(0,n), Vmu = diag(.001,n))
  inits = list(mu = colMeans(j.data), prec.Sigma = solve(cov(j.data)))
  var.names = c("mu", "Sigma")
  out.multi.try <- custom.jags(model,data,inits,n.chains,burnin,n.update,var.names)
  multi.save <- c(multi.save, "out.multi.try")
}
if(runs$multi.na){
  # With na's
  print(model)
  print(Sys.time())
  j.data <- try.na
  N=dim(j.data)[1]; n=dim(j.data)[2]
  data = list(X=j.data, N=N, n=n, Vsig = diag(n), mu0 = rep(0,n), Vmu = diag(.001,n))
  inits = list(mu = colMeans(j.data), prec.Sigma = solve(cov(j.data)))
  var.names = c("mu", "Sigma")
  out.multi.try.na <- custom.jags(model,data,inits,n.chains,burnin,n.update,var.names)
  multi.save <- c(multi.save, "out.multi.try.na")
}
################################################################################
## Save Data
if(length(multi.save > 0)){
  t <- paste0("save(", paste(multi.save, collapse = ','),", file='",
              paste0('output/try.multi.outputs.',args[1],'.Rdata'),"')")
  eval(parse(text=t))
  print("Done with multivariate")
}

################################################################################
################################################################################
## PFT Run without na's

n.chains = 1
n.iter <- 600000
n.update <- 20
burnin <- 1000
thin <- 40

model = "models/multivariate.grp.model.txt"
pft.save <- c()
if(runs$multi.pft){
  print("-------------------------")
  print(model)
  print(Sys.time())
  j.data <- try
  Nvars <- dim(j.data)[2]
  Nobs = dim(j.data)[1]
  Nvars = dim(j.data)[2]
  GroupNo = as.numeric(as.factor(try_full$pft[!is.na(try_full$pft)]))
  Ngroup = length(unique(GroupNo))
  Gamma = diag(Nvars)
  Omega = diag(Nvars)
  data = list(
    X=j.data,
    Nobs = Nobs,
    Nvars = Nvars,
    GroupNo = GroupNo,
    Ngroup = Ngroup,
    Gamma = Gamma,
    Omega = Omega
  )
  inits = list(mu = colMeans(j.data),
               theta = as.matrix(pft_means))
  var.names = c("theta","mu")
  out.pft.try <- custom.jags(model,data,inits,n.chains,burnin,n.update,thin,var.names)
  pft.save <- c(pft.save, "out.pft.try")
}


if(runs$multi.pft.na){
  print("-------------------------")
  print(model)
  print(Sys.time())
j.data <- try.na
Nvars <- dim(j.data)[2]
Nobs = dim(j.data)[1]
Nvars = dim(j.data)[2]
GroupNo = as.numeric(as.factor(try_full$pft[!is.na(try_full$pft)]))
Ngroup = length(unique(GroupNo))
Gamma = diag(Nvars)
Omega = diag(Nvars)

data = list(
  X=j.data,
  Nobs = Nobs,
  Nvars = Nvars,
  GroupNo = GroupNo,
  Ngroup = Ngroup,
  Gamma = Gamma,
  Omega = Omega
)
inits = list(mu = colMeans(j.data), 
             theta = as.matrix(pft_means))
var.names = c("theta","mu")
out.pft.try.na <- custom.jags(model,data,inits,n.chains,burnin,n.update,thin,var.names)
pft.save <- c(pft.save, "out.pft.try.na")
}
print(Sys.time())
print("Done with all the model runs! Saving output...")
#######################################################################################
## Save Data
if(length(pft.save > 0)){
  t <- paste0("save(", paste(pft.save, collapse = ','),", file='",
              paste0('output/try.pft.outputs.',args[1],'.Rdata'),"')")
  eval(parse(text=t))
  print("Done with pfts")
}
#######################################################################################
print(Sys.time())
print("All Done!")
# Next run process.mcmc.R
