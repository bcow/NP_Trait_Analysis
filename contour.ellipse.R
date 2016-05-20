################################################################################
# Plots using dataEllipse() and with eigenvectors
require(ggplot2)

load("output/try.outs.Rdata")

a <- nrow(mus)-1
b <- ncol(mus)

v1 = c(); v2 = c();
for(i in 1:a){
  v1 = c(v1, rep(i, b-i))
  v2 = c(v2, (i+1):b)
}

k = iter

par(mfrow=c(1,1))
for(N in 1:length(v1)){
  
  xname <- paste(unlist(strsplit(colnames(outs[[1]])[v1[N]], "[.]"))[1:2],
                 collapse =" ")
  yname <- paste(unlist(strsplit(colnames(outs[[1]])[v2[N]], "[.]"))[1:2],
                 collapse =" ")
  
  png(filename = file.path(
    "figures",
    paste(colnames(outs[[1]])[v1[N]],colnames(outs[[1]])[v2[N]],"png", sep=".")), 
    height = 800, width = 800)
  
  # ellipses 
  
  o <- as.data.frame(cbind(
    "o1.v1" = outs[[1]][,v1[N]],
    "o1.v2" = outs[[1]][,v2[N]]
  ))
  
  for(i in 1:length(outs)){
    o[paste0("o",i,".v",1)]<- outs[[i]][,v1[N]]
    o[paste0("o",i,".v",2)]<- outs[[i]][,v2[N]]
  }
  
  p <- ggplot(data = o) +
    stat_ellipse(aes(o1.v1, o1.v2),type="norm",linetype=2,colour="red") +
    stat_ellipse(aes(o2.v1, o2.v2),type="norm",linetype=2,colour="blue") +
    stat_ellipse(aes(o3.v1, o3.v2),type="norm",linetype=3, colour="red") +
    stat_ellipse(aes(o4.v1, o4.v2),type="norm",linetype=3, colour="blue") +
    stat_ellipse(aes(o5.v1, o5.v2),type="norm",colour="green") +
    stat_ellipse(aes(o6.v1, o6.v2),type="norm",colour="green") +
    labs(title = paste(xname,"vs",yname),x = xname,y = yname)
  plot(p)
  
  dev.off()
  print(paste(xname, yname))
}