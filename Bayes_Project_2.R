#######################################################################################
## Set up data
library("rjags")
library(coda)
library(mvtnorm)
setwd(file.path(getwd(),"Bayes_Project"))
glopnet <- read.csv("glopnet.csv")
glopnet[c("X")] <- NULL
glopnet[c("X.1")] <- NULL
glopnet[c("X.2")] <- NULL
glopnet[c("X.3")] <- NULL

gdata.na <- as.data.frame(cbind(
  # glopnet$log.LL,
  glopnet$log.LMA,
  # glopnet$log.Amass,
  glopnet$log.Nmass,
  # glopnet$log.Pmass,
  glopnet$log.Rdmass
))
# colnames(gdata.na)<-c("Log.LL","Log.LMA","Log.Amass","Log.Nmass","Log.Pmass","Log.Rmass")
colnames(gdata.na)<-c("Log.LMA","Log.Nmass", "Log.Rmass")
gdata.na$BIOME <- glopnet$BIOME
gdata <- na.omit(gdata.na)

library(ggplot2)
ggplot(gdata.na, aes(x=Log.LMA, fill=BIOME)) +
  geom_histogram()+theme_bw()

barplot(table(gdata.na,as.numeric(gdata.na$BIOME)))


# sum(is.na(gdata.na$Log.LL))
# sum(is.na(gdata.na$Log.LMA))
# sum(is.na(gdata.na$Log.Amass))
# sum(is.na(gdata.na$Log.Nmass))
# sum(is.na(gdata.na$Log.Pmass))
# sum(is.na(gdata.na$Log.Rmass))

#######################################################################################
## Pairs plots 
pal <- c(palette(rainbow(32))[c(1,4,7,11,17,20,24,27,30)],"grey")

gdata.na <- gdata.na[,c(1,2,3)]

source("pairs_BC.R")
png(filename = "pairs.png", width=600, height=600)
pairs(gdata.na, panel=function(x,y){
  points(x,y)#,col=pal[glopnet$BIOME])
  fit <- lm(y~x)
  p <- pf(summary(fit)$fstatistic[1],summary(fit)$fstatistic[2],summary(fit)$fstatistic[3], lower.tail = F)
  if(p < .01){abline(fit, col='red',lwd=2)}
  # legend("top", legend=sprintf("R2 = %.2f",summary(fit)$r.squared), text.col="blue")
},diag.panel=panel.hist,upper.panel=panel.r2)
dev.off()


#######################################################################################
## Do JAGS runs

run = TRUE
if(file.exists("model2.Rdata")){
  load("model2.Rdata")
  load("model1.Rdata")
  run = FALSE
}

if(run){
  source("GroupModel.R")
  source("model1.Rdata")
}

#######################################################################################
## Prep and (maybe) plot the JAGS output for Sigma
# source("prep_sigma.R")

#######################################################################################
## Assessment of mutivariate normality

data.na <- as.data.frame(cbind(
  glopnet$log.LMA,
  glopnet$log.Nmass,
  glopnet$log.Rdmass,
  glopnet$BIOME
))
# colnames(gdata.na)<-c("Log.LL","Log.LMA","Log.Amass","Log.Nmass","Log.Pmass","Log.Rmass")
colnames(data.na)<-c("Log.LMA","Log.Nmass", "Log.Rmass", "BIOME")
d <- na.omit(data.na)

idx <- which(d$BIOME==8)
Y <- as.matrix(d[idx,1:3])

idx <- which(as.numeric(glopnet$BIOME)==1)
Y <- as.matrix(gdata.na[idx,])
means <- colMeans(Y, na.rm = T)


# # Mahalanobis distance 
# source("mahalanobis_abnalysis.R")
# mah <- mahalanobis_analysis(Y,means,S1,p=3)

########################################################

pal <- c(palette(rainbow(32))[c(1,4,7,11,17,20,24,27,30)],"grey")

png(filename = "LMA_post.png", width=600, height=650)
par(mfrow = c(2,1))
hist(gdata.na[,1], xlim = c(1,3), xlab = "LMA", main = "Histogram of Observed LMA (log-transformed)")
plot(density(theta[,1]), xlim = c(1,3), ylim=c(0,65), lwd=3, lty=3,
     main = "Posterior LMA Plots")
lines(density(out$`theta[1,1]`),col=pal[1],lwd=3)
lines(density(out$`theta[2,1]`),col=pal[2],lwd=3)
lines(density(out$`theta[3,1]`),col=pal[3],lwd=3)
lines(density(out$`theta[4,1]`),col=pal[4],lwd=3)
lines(density(out$`theta[5,1]`),col=pal[5],lwd=3)
lines(density(out$`theta[6,1]`),col=pal[6],lwd=3)
lines(density(out$`theta[7,1]`),col=pal[7],lwd=3)
lines(density(out$`theta[8,1]`),col=pal[8],lwd=3)
lines(density(out$`theta[9,1]`),col=pal[9],lwd=3)
lines(density(out$`theta[10,1]`),col=pal[10],lwd=3)
legend("topright", legend = c(levels(glopnet$BIOME),"Pooled"), col=c(pal,"black"), lty = c(rep(1,10),3), lwd=3)
dev.off()

png(filename = "Nmass_post.png", width=600, height=650)
par(mfrow = c(2,1))
hist(gdata.na[,2],xlim=c(-.8,.8), xlab="Nmass", main = "Histogram of Observed Nmass (log-transformed)")
plot(density(theta[,2]), xlim=c(-.8,.8),ylim=c(0,85), lwd=3, lty=3,
     main = "Posterior Nmass Plots")
lines(density(out$`theta[1,2]`),col=pal[1],lwd=3)
lines(density(out$`theta[2,2]`),col=pal[2],lwd=3)
lines(density(out$`theta[3,2]`),col=pal[3],lwd=3)
lines(density(out$`theta[4,2]`),col=pal[4],lwd=3)
lines(density(out$`theta[5,2]`),col=pal[5],lwd=3)
lines(density(out$`theta[6,2]`),col=pal[6],lwd=3)
lines(density(out$`theta[7,2]`),col=pal[7],lwd=3)
lines(density(out$`theta[8,2]`),col=pal[8],lwd=3)
lines(density(out$`theta[9,2]`),col=pal[9],lwd=3)
lines(density(out$`theta[10,2]`),col=pal[10],lwd=3)
legend("topleft", legend = c(levels(glopnet$BIOME),"Pooled"), col=c(pal,"black"), lty = c(rep(1,10),3), lwd=3)
dev.off()

png(filename = "Rmass_post.png", width=600, height=650)
par(mfrow = c(2,1))
hist(gdata.na[,3], xlim=c(0,2), xlab="Rmass", main = "Histogram of Observed Rmass (log-transformed)")
plot(density(theta[,3]), xlim=c(0,2),ylim=c(0,35), lwd=3, lty=3,
     main = "Posterior Nmass Plots")
lines(density(out$`theta[1,3]`),col=pal[1],lwd=3)
lines(density(out$`theta[2,3]`),col=pal[2],lwd=3)
lines(density(out$`theta[3,3]`),col=pal[3],lwd=3)
lines(density(out$`theta[4,3]`),col=pal[4],lwd=3)
lines(density(out$`theta[5,3]`),col=pal[5],lwd=3)
lines(density(out$`theta[6,3]`),col=pal[6],lwd=3)
lines(density(out$`theta[7,3]`),col=pal[7],lwd=3)
lines(density(out$`theta[8,3]`),col=pal[8],lwd=3)
lines(density(out$`theta[9,3]`),col=pal[9],lwd=3)
lines(density(out$`theta[10,3]`),col=pal[10],lwd=3)
legend("topright", legend = c(levels(glopnet$BIOME),"Pooled"), col=c(pal,"black"), lty = c(rep(1,10),3), lwd=3)
dev.off()


par(mfrow=c(2,2))
qqplot(gdata.na[,1],X1)
abline(0, 1, lty=2,  col = 'red')
qqplot(gdata.na[,2],X2)
abline(0, 1, lty=2,  col = 'red')
qqplot(gdata.na[,3],X3)
abline(0, 1, lty=2,  col = 'red')




