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
data = list(X=j.data, N=N, n=n, Vsig = diag(n), mu0 = rep(0,n), Vmu = diag(.001,n))
init = list(mu = colMeans(j.data), prec.Sigma = solve(cov(j.data)))
j.model   <- jags.model (file = model, data = data, inits = init, n.chains = 1)
update(j.model, n.iter=1000)
j.out   <- coda.samples (model = j.model, variable.names= c("mu"), n.iter = 10000)
out.multi.try.na <- j.out


#######################################################################################
## Save Data

save(out.uni.try,
     out.uni.try.na,
     out.multi.try,
     out.multi.try.na,
     file= paste0("try.uni.mult.outputs",args[1],".Rdata"))

