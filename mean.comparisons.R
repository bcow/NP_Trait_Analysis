load("try.uni.mult.outputs.Rdata")

#######################################################################################
## Overall comparison of computed and estimated means for each of the 6 variables 

nv <- ncol(try)

par(mfrow = c(3,2))
for(j in 1:nv){
  ranges <-  apply(cbind(outs[[1]][,j],outs[[2]][,j],outs[[3]][,j],outs[[4]][,j]), 2,
                   function(x) { dens <- density(x); c(range(dens$x), range(dens$y)) })
  plot(density(outs[[1]][,j]), col="red", lty=2,
       xlim = range(ranges[1:2, ]), ylim = range(ranges[3:4, ]),
       main=paste(names(try)[j]), xlab = paste(names(try)[j]))
  if(j==1 | j ==2){position="topright"}else{position="topleft"}
  legend(position,legend = c( "Multi w/ NA's","Uni   w/ NA's", "Mult w/o NA's ", "Uni  w/o NA's " ), lty = c(1,1,2,2), col=c("blue","red","blue","red"))
  lines(density(outs[[2]][,j]), col="blue", lty=2)
  lines(density(outs[[3]][,j]), col="red")
  lines(density(outs[[4]][,j]), col="blue")
}

#######################################################################################
## Error around the mean

options(digits=4)
for(j in 1:nv){
  cat("","",colnames(try)[j],sep="\n")
  summary <- as.data.frame(matrix(NA,5,4))
  summary[1,] <- c(mean(try[,j]), sd(try[,j])/sqrt(length(try[,j])), quantile(try[,j], c(.025, .975)))
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
for(j in 1:nv){
  ranges <-  apply(cbind(outs[[1]][,j],outs[[2]][,j]), 2,
                   function(x) { dens <- density(x); c(range(dens$x), range(dens$y)) })
  plot(density(outs[[1]][,j]), col="red",
       xlim = range(ranges[1:2, ]), ylim = range(ranges[3:4, ]),
       main=paste(names(try)[j]))
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
for(j in 1:nv){
  ranges <-  apply(cbind(outs[[3]][,j],outs[[4]][,j]), 2,
                   function(x) { dens <- density(x); c(range(dens$x), range(dens$y)) })
  plot(density(outs[[3]][,j]), col="red",
       xlim = range(ranges[1:2, ]), ylim = range(ranges[3:4, ]),
       main=paste(names(try)[j]))
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
