args = commandArgs(trailingOnly=TRUE)

# Basic Univariate on TRY data
#######################################################################################
## Set up data
library(rjags)
library(coda)
library(mvtnorm)
library(data.table)

# load data in as data.table
try_full <- data.table(readRDS("try.data.rds"))

sum(is.nan(try_full$LMA)) +sum(is.na(try_full))

# Some na's in the table accidentally got changed to NaN, switch back so it doesn't confuse the model
nan2na <- function(x){
  x[is.nan(x)] <- NA
  return(x)
}

try_full <- try_full[,lapply(.SD, nan2na)]

# data without pfts
try.na <- try_full[,.(log.LL, log.LMA, log.Nmass, log.Pmass, log.Rdmass)]
try <- na.omit(try.na)

# data with pfts
try.pft.na <- try_full[,.(log.LL, log.LMA, log.Nmass, log.Pmass, log.Rdmass, pft)]
try.pft <- na.omit(try.na)


#######################################################################################
## Pairs plots 
pairs = FALSE

if(pairs){
  source("pairs_BC.R")
  pal <- c(palette(rainbow(32))[c(1,4,7,11,17,20,24,27,30)],"grey")
  
  #png(filename = "pairs.png", width=600, height=600)
  pairs(try[,.(log.LL, log.LMA, log.Nmass, log.Pmass, log.Rdmass)], panel=function(x,y){
    points(x,y)#,col=pal[glopnet$BIOME])
    fit <- lm(y~x)
    p <- pf(summary(fit)$fstatistic[1],summary(fit)$fstatistic[2],summary(fit)$fstatistic[3], lower.tail = F)
    if(p < .01){abline(fit, col='red',lwd=2)}
    # legend("top", legend=sprintf("R2 = %.2f",summary(fit)$r.squared), text.col="blue")
  },diag.panel=panel.hist,upper.panel=panel.r2)
  #dev.off()
}



#######################################################################################
## Do JAGS runs

runs = list(
  uni = TRUE,
  uni.na = TRUE,
  multi = TRUE,
  multi.na = TRUE,
  multi.pft = TRUE,
  multi.pft.na = TRUE)

if(file.exists("try.uni.mult.outputs.Rdata")){
  load("try.uni.mult.outputs.Rdata")
  runs[c("uni","uni.na","multi","multi.na")] <- FALSE
}

#######################################################################################
## Univariate Run


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


#######################################################################################
## Multivariate Run

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


