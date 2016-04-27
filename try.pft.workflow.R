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

# data with pfts
try.pft.na <- try_full[,.(log.LL, log.LMA, log.Nmass, log.Pmass, log.Rdmass, pft)]
#try.pft.na$pft <- as.factor(try.pft.na$pft)
try.pft <- na.omit(try.pft.na)
#try.pft$pft <- as.factor(try.pft$pft)

###################################################################
# RUN MODEL

sum(is.na(try.pft.na$pft))

model = "multivariate.grp.model.txt"

j.data <- try.pft.na[1:100]
Nvars <- dim(j.data)[2]
Nobs = dim(j.data)[1]
Nvars = dim(j.data)[2] 
GroupNo = as.numeric(try.pft.na$pft)
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
init = NULL
j.model   <- jags.model (file = textConnection(GroupModel), data = data, inits = init, n.chains = 3)
update(j.model, n.iter=1000)
j.out   <- coda.samples (model = j.model, variable.names= c("Sigma","theta"), n.iter = 10000)

out <- as.data.frame(as.matrix(j.out))
out.theta <- out[,91:120]
out.Sigma <- out[,1:90]

##

vname <- "Sigma"
for(k in 1:Ngroup){
  S <- as.data.frame(matrix(NA,p,p))
  for(i in 1:Nvars){
    for(j in 1:Nvars){
      idx <- which(colnames(Sigma)==paste0(vname,"[",k,",",i,",",j,"]"))
      S[i,j] <- mean(Sigma[,idx])
      # plot(density(abs(Sigma[,idx])),main=colnames(Sigma)[idx], xlim=c(abs(var(gdata)[i,j])-.01, max(abs(Sigma[,idx]))))
      # abline(v=abs(S[i,j]),col=3, lwd=2)
      # abline(v=abs(var(gdata)[i,j]), lty=2, col=2, lwd=2)
    }
  }
  colnames(S) <- colnames(gdata.na)
  assign(paste0("S",k),S)
}
remove(S)

Th <- as.data.frame(matrix(NA,Ngroup,Nvars))
vname <- "theta"
for(i in 1:Ngroup){
  for(j in 1:Nvars){
    idx <- which(colnames(out.theta)==paste0(vname,"[",i,",",j,"]"))
    Th[i,j] <- mean(out.theta[,idx])
    # plot(density(abs(Sigma[,idx])),main=colnames(Sigma)[idx], xlim=c(abs(var(gdata)[i,j])-.01, max(abs(Sigma[,idx]))))
    # abline(v=abs(S[i,j]),col=3, lwd=2)
    # abline(v=abs(var(gdata)[i,j]), lty=2, col=2, lwd=2)
  }
}
colnames(Th) <- colnames(gdata.na)

save(out, S1, S2, S3, S4, S5, S6, S7, S8, S9, S10, Th, file = "model2.Rdata")

j.data <- gdata.na
Nvars <- dim(j.data)[2]
Nobs = dim(j.data)[1]
Nvars = dim(j.data)[2] 
GroupNo = as.numeric(glopnet$BIOME)
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
init = NULL
j.model   <- jags.model (file = textConnection(GroupModel), data = data, inits = init, n.chains = 3)
update(j.model, n.iter=1000)
j.out.X <- coda.samples (model = j.model, variable.names= c("X"), n.iter = 1000)

X.g <- as.data.frame(as.matrix(j.out.X))
X1.g <- as.numeric(colMeans(X[,1:Nobs]))
X2.g <- as.numeric(colMeans(X[,(Nobs+1):(2*Nobs)]))
X3.g <- as.numeric(colMeans(X[,(2*Nobs+1):(3*Nobs)]))

save(X.g, file = "X_group.Rdata")

par(mfrow=c(2,2))
qqplot(gdata.na[,1],X1.g)
abline(0, 1, lty=2,  col = 'red')
qqplot(gdata.na[,2],X2.g)
abline(0, 1, lty=2,  col = 'red')
qqplot(gdata.na[,3],X3.g)
abline(0, 1, lty=2,  col = 'red')

