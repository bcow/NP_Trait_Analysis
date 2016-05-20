################################################################################
# Plots using dataEllipse() and with eigenvectors

par(mfrow=c(1,1))
for(N in 1:length(v1)){
  
  xname <- paste(unlist(strsplit(colnames(out1.df[,v1])[N], "[.]"))[1:2],
                 collapse =" ")
  yname <- paste(unlist(strsplit(colnames(out1.df[,v2])[N], "[.]"))[1:2],
                 collapse =" ")
  
  png(filename = file.path("figures",paste(colnames(out1.df[,v1])[N],colnames(out1.df[,v2])[N],"png", sep=".")), height = 800, width = 800)
  
  # ellipses 
  
  o <- as.data.frame(cbind(
    "o1.v1" = out1.df[,v1[N]],
    "o1.v2" = out1.df[,v2[N]],
    "o2.v1" = out2.df[,v1[N]],
    "o2.v2" = out2.df[,v2[N]],
    "o3.v1" = out3.df[,v1[N]],
    "o3.v2" = out3.df[,v2[N]],
    "o4.v1" = out4.df[,v1[N]],
    "o4.v2" = out4.df[,v2[N]]
  ))
  
  p <- ggplot(data = o) +
    stat_ellipse(aes(o1.v1, o1.v2),type="norm",linetype=2,colour="red") +
    stat_ellipse(aes(o2.v1, o2.v2),type="norm",linetype=2,colour="blue") +
    stat_ellipse(aes(o3.v1, o3.v2),type="norm",colour="red") +
    stat_ellipse(aes(o4.v1, o4.v2),type="norm",colour="blue") +
    labs(title = paste(xname,"vs",yname),x = xname,y = yname)
  plot(p)
  
  dev.off()
  print(paste(xname, yname))
}