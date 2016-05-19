################################################################################
# Plots using dataEllipse() and with eigenvectors

par(mfrow=c(1,1))
for(N in 1:length(v1)){
  
  xname <- paste(unlist(strsplit(colnames(out1.df[,v1])[N], "[.]"))[1:2],
                 collapse =" ")
  yname <- paste(unlist(strsplit(colnames(out1.df[,v2])[N], "[.]"))[1:2],
                 collapse =" ")
  
  png(filename = file.path("figures",paste(colnames(out1.df[,v1])[N],colnames(out1.df[,v2])[N],"png", sep=".")), height = 800, width = 800)
  xr <- range(c(DEN.1[[N]]$x1, DEN.3[[N]]$x1, DEN.2[[N]]$x1, DEN.4[[N]]$x1))
  yr <- range(c(DEN.1[[N]]$x2, DEN.3[[N]]$x2, DEN.2[[N]]$x2, DEN.4[[N]]$x2))
  plot(xr[1]-.1,xr[2], xlim=c(xr[1]-.1,xr[2]), xlab="", ylab="",
       ylim=c(yr[1]-.1,yr[2]), lty=2, lwd=4, col="white")
  title(main = paste(xname,"vs",yname),cex.main=2, 
        xlab = xname,
        ylab = yname, cex.lab=1.5)
  
  
  # ellipses 
  
  dataEllipse(out1.df[,v1[N]][1:k], out1.df[,v2[N]][1:k], levels=c(0.95), add = T,
              plot.points = F, lty=2, lwd=4, col="red", center.pch="o")
  dataEllipse(out2.df[,v1[N]][1:k], out2.df[,v2[N]][1:k], levels=c(0.95), add = T, 
              plot.points = F, lty=2, lwd=4, col="blue", center.pch="o")
  dataEllipse(out3.df[,v1[N]][1:k], out3.df[,v2[N]][1:k], levels=c(0.95), add = T,
              plot.points = F, lty=1, lwd=4, col="red", center.pch="o")
  h <- dataEllipse(out4.df[,v1[N]][1:k], out4.df[,v2[N]][1:k], levels=c(0.95), add = T, plot.points = F, lty=1, lwd=4, col="blue", center.pch="o")
  
  # eigenvectors
  n = k
  p = 2
  M1 = cbind(out1.df[,v1[N]][1:k], out1.df[,v2[N]][1:k])
  M2 = cbind(out2.df[,v1[N]][1:k], out2.df[,v2[N]][1:k])
  M3 = cbind(out3.df[,v1[N]][1:k], out3.df[,v2[N]][1:k])
  M4 = cbind(out4.df[,v1[N]][1:k], out4.df[,v2[N]][1:k])
  
  ctr1 = colMeans(M1); S1 = cov(M1)
  ctr2 = colMeans(M2); S2 = cov(M2)
  ctr3 = colMeans(M3); S3 = cov(M3)
  ctr4 = colMeans(M4); S4 = cov(M4)
  
  color <- rep(c("red", "blue"), 2)
  for(i in 1:4){
    eval(parse(text = paste0("S = S",i)))
    eval(parse(text = paste0("ctr = ctr",i)))
    
    eigVal  <- eigen(S)$values
    eigVec  <- eigen(S)$vectors
    eigScl  <- eigVec %*% diag(sqrt(eigVal))  # scale eigenvectors to length = square-root
    xMat    <- rbind(ctr[1] + eigScl[1, ], ctr[1] - eigScl[1, ])
    yMat    <- rbind(ctr[2] + eigScl[2, ], ctr[2] - eigScl[2, ])
    matlines(xMat, yMat, lty=1, lwd=4, col=color[i])
    l1 <- 2*sqrt(eigVal[1])*sqrt((p*(n-1)/(n*(n-p)))*qf(.95,p,n-p))
    l2 <- 2*sqrt(eigVal[2])*sqrt((p*(n-1)/(n*(n-p)))*qf(.95,p,n-p))
    print(sqrt(eigVal[1])/sqrt(eigVal[2]))  
  }
  
  
  # densities 
  pdfx.1 <- density(out1.df[,v1[N]])
  pdfx.2 <- density(out2.df[,v1[N]])
  pdfx.3 <- density(out3.df[,v1[N]])
  pdfx.4 <- density(out4.df[,v1[N]])
  pdfy.1 <- density(out1.df[,v2[N]])
  pdfy.2 <- density(out2.df[,v2[N]])
  pdfy.3 <- density(out3.df[,v2[N]])
  pdfy.4 <- density(out4.df[,v2[N]])
  
  par(new=TRUE)
  plot(pdfx.1$x, pdfx.1$y, col="red", xlim=c(xr[1]-.1,xr[2]), 
       ylim=c(0,15*max(pdfx.1$y)), type="l", lty=2,axes = FALSE, 
       bty = "n", xlab = "", ylab = "",lwd=3)
  lines(pdfx.2$x, pdfx.2$y, col="blue", xlim=c(xr[1]-.1,xr[2]), 
        ylim=c(0,10*max(pdfx.2$y)), lty=2,lwd=3)
  lines(pdfx.3$x, pdfx.3$y, col="red", xlim=c(xr[1]-.1,xr[2]), 
        ylim=c(0,10*max(pdfx.3$y)),lwd=3)
  lines(pdfx.4$x, pdfx.4$y, col="blue", xlim=c(xr[1]-.1,xr[2]),
        ylim=c(0,10*max(pdfx.4$y)),lwd=3)
  #   abline(v = quantile(out1.df[,v1[N]],c(.05,.5,.95)), lty=2, lwd=1, col="red")
  #   abline(v = quantile(out2.df[,v1[N]],c(.05,.95)), lty=2, lwd=1, col="blue")
  #   abline(v = quantile(out3.df[,v1[N]],c(.05,.95)), lty=1, lwd=1, col="red")
  #   abline(v = quantile(out4.df[,v1[N]],c(.05,.95)), lty=1, lwd=1, col="blue")
  
  par(new=TRUE)
  scale <- 4
  plot(scale*pdfy.1$y, pdfy.1$x, col="red", xlim=c(0,xr[2]*300), 
       ylim=c(yr[1]-.1,yr[2]), type="l", lty=2,axes = FALSE, 
       bty = "n", xlab = "", ylab = "",lwd=3)
  lines(scale*pdfy.2$y, pdfy.2$x, col="blue",xlim=c(0,xr[2]*300), 
        ylim=c(yr[1]-.1,yr[2]), lty=2,lwd=3)
  lines(scale*pdfy.3$y, pdfy.3$x, col="red", xlim=c(0,xr[2]*300),
        ylim=c(yr[1]-.1,yr[2]),lwd=3)
  lines(scale*pdfy.4$y, pdfy.4$x, col="blue", xlim=c(0,xr[2]*300),
        ylim=c(yr[1]-.1,yr[2]),lwd=3)
  #   abline(h = quantile(out1.df[,v2[N]],c(.05,.5,.95)), lty=2, lwd=1, col="red")
  #   abline(h = quantile(out2.df[,v2[N]],c(.05,.95)), lty=2, lwd=1, col="blue")
  #   abline(h = quantile(out3.df[,v2[N]],c(.05,.95)), lty=1, lwd=1, col="red")
  #   abline(h = quantile(out4.df[,v2[N]],c(.05,.95)), lty=1, lwd=1, col="blue")
  dev.off()
}