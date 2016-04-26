# Basic Univariate on TRY data


#######################################################################################
## Set up data
library("rjags")
library(coda)
library(mvtnorm)
library(data.table)

setwd(getwd())
try_full <- data.table(readRDS("try.data.rds"))

try.na <- try_full[,.(log.LL, log.LMA, log.Nmass, log.Pmass, log.Rdmass)]
try <- na.omit(try.na)

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
  uni.pft = TRUE,
  multi = TRUE,
  multi.pft = TRUE)

#######################################################################################
## Univariate Run


if(runs$uni){
  model = "univarite.model.txt"
  
  # Without na's
  j.data <- try
  N=dim(j.data)[1]; n=dim(j.data)[2]
  data = list(Y=j.data, N=N, n=n)
  init = NULL
  j.model   <- jags.model (file = model,data = data,inits = init, n.chains = 3)
  update(j.model, n.iter=1000)
  j.out   <- coda.samples (model = j.model,variable.names= c("mu"), n.iter = 10000)
  out.uni.try <- j.out
  
  
  # With na's
  j.data <- try.na[1:1000]
  N=dim(j.data)[1]; n=dim(j.data)[2]
  data = list(Y=j.data, N=N, n=n)
  init = NULL
  j.model   <- jags.model (file = model,data = data,inits = init, n.chains = 3)
  update(j.model, n.iter=1000)
  j.out   <- coda.samples (model = j.model,variable.names= c("mu"), n.iter = 10000)
  out.uni.try.na <- j.out
}

#######################################################################################
## Univariate Run


if(runs$uni){
  model = "multivarite.model.txt"
  
  # Without na's
  j.data <- try
  N=dim(j.data)[1]; n=dim(j.data)[2]
  data = list(Y=j.data, N=N, n=n, Vsig = diag(n), mu0 = rep(0,n), Vmu = diag(.001,n))
  init = NULL
  j.model   <- jags.model (file = textConnection(MultModel),data = data,inits = init,n.chains = 3)
  update(j.model, n.iter=1000)
  j.out   <- coda.samples (model = j.model,variable.names= c("mu"),n.iter = 10000)
  out.multi.try <- j.out
  
  
  # With na's
  j.data <- try.na[1:1000]
  N=dim(j.data)[1]; n=dim(j.data)[2]
  data = list(Y=j.data, N=N, n=n, Vsig = diag(n), mu0 = rep(0,n), Vmu = diag(.001,n))
  init = NULL
  j.model   <- jags.model (file = textConnection(MultModel),data = data,inits = init,n.chains = 3)
  update(j.model, n.iter=1000)
  j.out   <- coda.samples(model = j.model,variable.names= c("mu"),n.iter = 10000)
  out.multi.try.na <- j.out
}
