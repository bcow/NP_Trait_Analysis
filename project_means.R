#######################################################################################
## Set up data
library(rjags)
library(coda)
library(mvtnorm)

glopnet <- read.csv("glopnet_full.csv")
glopnet[c("X")] <- NULL
glopnet[c("X.1")] <- NULL
glopnet[c("X.2")] <- NULL
glopnet[c("X.3")] <- NULL

gdata.na <- as.data.frame(cbind(
  glopnet$log.LL,
  glopnet$log.LMA,
  glopnet$log.Amass,
  glopnet$log.Nmass,
  glopnet$log.Pmass,
  glopnet$log.Rdmass
))
colnames(gdata.na)<-c("Log.LL","Log.LMA","Log.Amass","Log.Nmass","Log.Pmass","Log.Rmass")
gdata <- na.omit(gdata.na)

run = TRUE
if(file.exists("jags.outputs.Rdata")){
  load("jags.outputs.Rdata")
  run = FALSE
  }

#######################################################################################
## Pairs plots 

# Including studies with missing observations 

pairs(gdata.na, panel=function(x,y){
  points(x,y)
  fit <- lm(y~x)
  p <- pf(summary(fit)$fstatistic[1],summary(fit)$fstatistic[2],summary(fit)$fstatistic[3], lower.tail = F)
  if(p < .01){abline(fit, col='red',lwd=2)}
  legend("top", legend=sprintf("R2 = %.2f",summary(fit)$r.squared), text.col="blue")
})

# Excluding studies with missing observations 
pairs(gdata, panel=function(x,y){
  points(x,y)
  fit <- lm(y~x)
  p <- pf(summary(fit)$fstatistic[1],summary(fit)$fstatistic[2],summary(fit)$fstatistic[3], lower.tail = F)
  if(p < .01){abline(fit, col='red',lwd=2)}
  legend("top", legend=sprintf("R2 = %.2f",summary(fit)$r.squared), text.col="blue")
})

#######################################################################################
## Univariate Model 

if(run){
  UnivModel = "
  model{
    prec.sigma~dgamma(.001,.001)
    sigma <- 1/prec.sigma

    for(i in 1:n){mu[i]~dnorm(0,.001)}
  
    for(i in 1:N){
      for(j in 1:n){
        Y[i,j]~dnorm(mu[j],prec.sigma)
      }
    }
  }"
  
  
  # Without na's
  remove(j.data, j.model, j.out)
  
  j.data <- gdata
  N=dim(j.data)[1]; n=dim(j.data)[2]
  data = list(Y=j.data, N=N, n=n)
  init = NULL
  j.model   <- jags.model (file = textConnection(UnivModel),data = data,inits = init, n.chains = 3)
  update(j.model, n.iter=1000)
  j.out   <- coda.samples (model = j.model,variable.names= c("mu"), n.iter = 10000)
  out1 = j.out
  
  
  # With na's
  remove(j.data, j.model, j.out)
  j.data <- gdata.na
  N=dim(j.data)[1]; n=dim(j.data)[2]
  data = list(Y=j.data, N=N, n=n)
  init = NULL
  j.model   <- jags.model (file = textConnection(UnivModel),data = data,inits = init, n.chains = 3)
  update(j.model, n.iter=1000)
  j.out   <- coda.samples (model = j.model,variable.names= c("mu"), n.iter = 10000)
  out3 = j.out
}

#######################################################################################
## Multivariate Model

if(run){
  MultModel = "
  model{
  prec.Sigma~dwish(Vsig[,],n)
  Sigma[1:n,1:n] <- inverse(prec.Sigma[,])
  
  mu[1:n]~dmnorm(mu0[],Vmu)
  
  for(i in 1:N){
  Y[i,1:n]~dmnorm(mu[],prec.Sigma[,])
  for(j in 1:n){
  X[i,j]~dnorm(Y[i,j],10000000)
  }
  
  }
  }"

  # Without na's 
  remove(j.data, j.model, j.out)
  j.data <- gdata
  N=dim(j.data)[1]; n=dim(j.data)[2]
  data = list(Y=j.data, N=N, n=n, Vsig = diag(n), mu0 = rep(0,n), Vmu = diag(.001,n))
  init = NULL
  j.model   <- jags.model (file = textConnection(MultModel),data = data,inits = init,n.chains = 3)
  update(j.model, n.iter=1000)
  j.out   <- coda.samples (model = j.model,variable.names= c("mu"),n.iter = 10000)
  out2 = j.out
  
  
  # With na's 
  
  j.data <- gdata.na
  N=dim(j.data)[1]; n=dim(j.data)[2]
  data = list(X=j.data, N=N, n=n, Vsig = diag(n), mu0 = rep(0,n), Vmu = diag(.001,n))
  init = NULL
  j.model   <- jags.model (file = textConnection(MultModel), data = data, inits = init, n.chains = 3)
  update(j.model, n.iter=1000)
  j.out   <- coda.samples (model = j.model, variable.names= c("mu"), n.iter = 10000)
  out4 = j.out
  
  
}


#######################################################################################
## Save Data

# Univariate outputs
out1.df <- as.data.frame(as.matrix(out1)) #Univ
out3.df <- as.data.frame(as.matrix(out3)) #Univ
out2.df <- as.data.frame(as.matrix(out2)) #Multi
out4.df <- as.data.frame(as.matrix(out4)) #Multi

colnames(out1.df)<-colnames(out3.df)<-colnames(out2.df)<-colnames(out4.df)<-colnames(gdata)


outs <- list(out1.df,out2.df,out3.df,out4.df)

mus <- as.data.frame(matrix(NA, 5,6))
mus[1,] <- colMeans(gdata)
for(i in 1:4){
  for(j in 1:6){
    mus[i+1,j] <- mean(outs[[i]][,j])
  }
}
rownames(mus) <- c("Data ","Univariate   without NA's ", "Multivariate without NA's ", "Univariate   with NA's",  "Multivariate with NA's" )
colnames(mus) <- colnames(gdata) 
options(digits=4)
print(mus)

if(run){
  save(out1,out2,out3,out4, outs, mus, file="jags.outputs.Rdata")
}

#######################################################################################
## Overall comparison of computed and estimated means for each of the 6 variables 

par(mfrow = c(3,2))
for(j in 1:6){
  ranges <-  apply(cbind(outs[[1]][,j],outs[[2]][,j],outs[[3]][,j],outs[[4]][,j]), 2,
                   function(x) { dens <- density(x); c(range(dens$x), range(dens$y)) })
  plot(density(outs[[1]][,j]), col="red", lty=2,
       xlim = range(ranges[1:2, ]), ylim = range(ranges[3:4, ]),
       main=paste(names(gdata)[j]), xlab = paste(names(gdata)[j]))
  if(j==1 | j ==2){position="topright"}else{position="topleft"}
  legend(position,legend = c( "Multi w/ NA's","Uni   w/ NA's", "Mult w/o NA's ", "Uni  w/o NA's " ), lty = c(1,1,2,2), col=c("blue","red","blue","red"))
  lines(density(outs[[2]][,j]), col="blue", lty=2)
  lines(density(outs[[3]][,j]), col="red")
  lines(density(outs[[4]][,j]), col="blue")
}

#######################################################################################
## Error around the mean

options(digits=4)
for(j in 1:6){
cat("","",colnames(gdata)[j],sep="\n")
summary <- as.data.frame(matrix(NA,5,4))
summary[1,] <- c(mean(gdata[,j]), sd(gdata[,j])/sqrt(length(gdata[,j])), quantile(gdata[,j], c(.025, .975)))
for(i in 1:4){ 
summary[i+1,] <- c(mean(outs[[i]][,j]), sd(outs[[i]][,j])/sqrt(length(outs[[i]][,j])), quantile(outs[[i]][,j], c(.025, .975)))
}

rownames(summary) <- c("Data ","Univariate   without NA's ", "Multivariate without NA's ", "Univariate   with NA's",  "Multivariate with NA's" )
colnames(summary) <- c("Mean", "SE", "2.5%", "97.5%")
print(summary)

cat("lowest SE:", rownames(summary)[which(summary$SE==min(summary$SE))])
}

#######################################################################################
#### Excluding Studies with Missing Observations

par(mfrow = c(3,2))
for(j in 1:6){
ranges <-  apply(cbind(outs[[1]][,j],outs[[2]][,j]), 2,
function(x) { dens <- density(x); c(range(dens$x), range(dens$y)) })
plot(density(outs[[1]][,j]), col="red",
xlim = range(ranges[1:2, ]), ylim = range(ranges[3:4, ]),
main=paste(names(gdata)[j]))
legend("topleft",legend = c("Mult w/o NA's ", "Uni  w/o NA's " ), lty = c(1,1), col=c("blue","red"))
legend("topright",legend = c("2.5%","mean","97.5%" ), lty = c(2,3,2))
lines(density(outs[[2]][,j]), col="blue")
abline(v=mean(outs[[1]][,j]), col="red", lty=3)
abline(v=quantile(outs[[1]][,j], .025), col="red", lty=2)
abline(v=quantile(outs[[1]][,j], .975), col="red", lty=2)
abline(v=mean(outs[[2]][,j]), col="blue", lty=3)
abline(v=quantile(outs[[2]][,j], .025), col="blue", lty=2)
abline(v=quantile(outs[[2]][,j], .975), col="blue", lty=2)  
}

#######################################################################################
#### Including Studies with Missing Observations
par(mfrow = c(3,2))
for(j in 1:6){
ranges <-  apply(cbind(outs[[3]][,j],outs[[4]][,j]), 2,
function(x) { dens <- density(x); c(range(dens$x), range(dens$y)) })
plot(density(outs[[3]][,j]), col="red",
xlim = range(ranges[1:2, ]), ylim = range(ranges[3:4, ]),
main=paste(names(gdata)[j]))
legend("topleft",legend = c( "Multi w/ NA's","Uni   w/ NA's"), lty = c(1,1), col=c("blue","red"))
legend("topright",legend = c("2.5%","mean","97.5%" ), lty = c(2,3,2))
lines(density(outs[[4]][,j]), col="blue")
abline(v=mean(outs[[3]][,j]), col="red", lty=3)
abline(v=quantile(outs[[3]][,j], .025), col="red", lty=2)
abline(v=quantile(outs[[3]][,j], .975), col="red", lty=2)
abline(v=mean(outs[[4]][,j]), col="blue", lty=3)
abline(v=quantile(outs[[4]][,j], .025), col="blue", lty=2)
abline(v=quantile(outs[[4]][,j], .975), col="blue", lty=2) 
}
