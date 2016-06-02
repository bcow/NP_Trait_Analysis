################################################################################
# Plots using dataEllipse() and with eigenvectors
require(ggplot2)
library(RColorBrewer)

load("output/mu_trait.outs.Rdata")
#load("output/mu_pft.outs.Rdata")
m <- max(as.numeric(lapply(outs, function(x) dim(x)[1])))


a <- b <- length(traits)

v1 = c(); v2 = c()
for(i in 1:(a-1)){
  v1 = c(v1, rep(i,a-i))
  v2 = c(v2, (i+1):(a))
}

k = iter

par(mfrow=c(1,1))
for(N in 1:length(v1)){
  
  xname <- paste(unlist(strsplit(traits[v1[N]], "[.]"))[1:2],collapse =" ")
  yname <- paste(unlist(strsplit(traits[v2[N]], "[.]"))[1:2],collapse =" ")
  
  # ellipses 
  
  o <- as.data.frame(matrix(NA,m,2*length(outs)))
  colnames(o) <- unlist(lapply(1:length(outs), function(i) c(paste0("o",i,".v",1), paste0("o",i,".v",2))))
  
  
  for(i in 1:length(outs)){
    o[1:dim(outs[[i]])[1],paste0("o",i,".v",1)]<- outs[[i]][,v1[N]]
    o[1:dim(outs[[i]])[1],paste0("o",i,".v",2)]<- outs[[i]][,v2[N]]
  }
  
  pal_col <- c()
  pal <- brewer.pal(length(outs),"Paired")    
  names(pal) <- names(outs)
  #pal<-brewer.pal(round(length(outs)/2),"hcl")
  pal <- c(palette(rainbow(32))[])
  
  p1 <- ggplot(data = o)
  p2 <-  ggplot(data = o)
  for(i in 1:length(outs)){
    x <- paste0("o",i,".v",1); y <- paste0("o",i,".v",2)
    p1 <- p1 + stat_ellipse(aes_string(x = x, y = y), type="norm",
                            # colour = pal[ceiling(i/2)], 
                            colour = pal[i],
                            linetype = i %% 2 + 1, size = 1.1, alpha=.5)
    if(i %in% grep(".na", names(outs))){
      p2 <- p2 + stat_ellipse(aes_string(x = x, y = y), type="norm",
                              # colour = pal[ceiling(i/2)], 
                              colour = pal[i],
                              linetype = i %% 2 + 1, size = 1.1, alpha = .5)
    }
  }
  
  png(filename = file.path("figures", paste(traits[v1[N]], traits[v2[N]],"all","png", sep=".")),
      height = 800, width = 800)
  plot(p1 + labs(title = paste(xname,"vs",yname),x = xname,y = yname))
  dev.off()
  
  png(filename = file.path("figures", paste(traits[v1[N]], traits[v2[N]],"na","png", sep=".")),
      height = 800, width = 800)
  plot(p2 + labs(title = paste(xname,"vs",yname),x = xname,y = yname))
  dev.off()
  
  print(paste(xname, yname))
}