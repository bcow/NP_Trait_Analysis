################################################################################
# Plots using dataEllipse() and with eigenvectors
require(ggplot2)
library(RColorBrewer)
require(grid)
require(gridExtra)
require(data.table)

v = "mu_trait"

model <- "multi.trait"
load("output/Sigma.outs.Rdata")
ind <- grep(names(outs), pattern=paste0(model,".na"))
Sigma_outs <- outs[ind]
remove(outs)

load("output/mu_trait.outs.Rdata")
ind <- grep(names(outs), pattern=paste0(model,".na"))
mu_trait_outs <- outs[ind]
remove(outs)

filename <- sprintf("figures/%s.%s.Sigma.png",v,model)

m <- max(as.numeric(lapply(mu_trait_outs, function(x) dim(x)[1])))

pft.table <- readRDS("data/try.pft.table.rds")
pft.biome <- pft.table[,pft.num := as.numeric(pft.factor)][,biome.num := as.numeric(as.factor(biome))][,list(biome.num=unique(biome.num)),pft.num][1:35]

pal <- brewer.pal(length(unique(pft.biome[,biome.num])),"Set1")

Mcol <- data.table(model = names(mu_trait_outs))[,end := lapply(model, function(x) tail(unlist(strsplit(x,"[.]")),1))][,color:= lapply(end, function(x) ifelse(is.na(as.numeric(x)),"black", pal[pft.biome[pft.num == as.numeric(x),biome.num]]))]

biomeColors <- unlist(Mcol[,color])
names(biomeColors) <- names(mu_trait_outs)


a <- b <- length(traits)
v1 = c(); v2 = c()
for(i in 1:(a-1)){
  v1 = c(v1, rep(i,a-i))
  v2 = c(v2, (i+1):(a))
}

par(mfrow=c(1,1))
for(N in 1:length(v1)){
  
  xname <- paste(unlist(strsplit(traits[v1[N]], "[.]"))[1:2],collapse =" ")
  yname <- paste(unlist(strsplit(traits[v2[N]], "[.]"))[1:2],collapse =" ")
  
  # ellipses 

  o <- as.data.frame(matrix(NA,m,2*length(mu_trait_outs)))
  colnames(o) <- unlist(lapply(1:length(mu_trait_outs), function(i) c(paste0(names(mu_trait_outs)[i],".v",1), paste0(names(mu_trait_outs)[i],".v",2))))
  
  eigs <- list()
  ctrs <- as.data.frame(matrix(NA,length(mu_trait_outs),2))
  colnames(ctrs) <- c("x","y")
  rownames(ctrs) <- names(mu_trait_outs)
  for(i in 1:length(mu_trait_outs)){
    
    # Using trait means output
    # n <- names(mu_trait_outs)[i]
    # o[1:dim(mu_trait_outs[[i]])[1],paste0(n,".v",1)]<- mu_trait_outs[[i]][,v1[N], with =F]
    # o[1:dim(mu_trait_outs[[i]])[1],paste0(n,".v",2)]<- mu_trait_outs[[i]][,v2[N],with = F]
    # M = cbind(mu_trait_outs[[i]][,v1[N], with =F],mu_trait_outs[[i]][,v2[N],with = F])
    # ctr <- colMeans(M); S = cov(M)
    
    # Using Sigma output
    M = cbind(mu_trait_outs[[i]][,v1[N], with =F],mu_trait_outs[[i]][,v2[N],with = F])
    ctrs[i,1:2] <- colMeans(M)
    Sigma_traits <- head(Sigma_outs[[i]][,grep(names(Sigma_outs[[i]]), pattern = "trait"), with=F])
    Sigma_trait <- as.data.frame(matrix(NA,5,5))
    for(j in 1:length(Sigma_traits)){
      eval(parse(text = paste(names(Sigma_traits)[j] ,"= colMeans(Sigma_outs[[i]][,j,with=F], na.rm =T)")))
    }
    colnames(Sigma_trait) <- rownames(Sigma_trait)<-traits
    S <- Sigma_trait[c(v1[N],v2[N]),c(v1[N],v2[N])]
    
    ###
    eigVal  <- eigen(S)$values
    eigVec  <- eigen(S)$vectors
    eigScl  <- eigVec %*% diag(sqrt(eigVal))
    eigUse <- eigScl
    Mat    <- as.data.frame(cbind(rbind(ctrs[i,1] + eigUse[1, ], ctrs[i,1] - eigUse[1, ]), rbind(ctrs[i,2] + eigUse[2, ], ctrs[i,2] - eigUse[2, ])))
    colnames(Mat) <- c("x1","x2","y1","y2")
    eigs <- append(eigs, list(Mat))
  }
  names(eigs) <- names(mu_trait_outs)
  
  varnames <- names(mu_trait_outs)
  
  # add_ellipses <- lapply(varnames, function(i) stat_ellipse(data = o,
  #   aes_q(x = as.name(paste0(i,".v",1)), y = as.name(paste0(i,".v",2)), colour = i),
  #   type="norm", size = 1))
  # 
  
  add_eig1 <- lapply(varnames, function(i) geom_line(data = eigs[[i]],aes_q(x=as.name("x1"),y=as.name("y1"), colour = i), size=1))
  add_eig2 <- lapply(varnames, function(i) geom_line(data = eigs[[i]],aes_q(x=as.name("x2"),y=as.name("y2"), colour = i), size=.5))

  add_centers<- lapply(varnames, function(i) geom_point(data = ctrs[i,], aes_q(x=as.name("x"),y=as.name("y"), colour = i),size=1.5))
  
  # add_scatter <- lapply(varnames, function(i) geom_point(
  #   aes_q(x = as.name(paste0(i,".v",1)), y = as.name(paste0(i,".v",2)), colour = i),
  #   alpha=.05))
  
  p <- ggplot() + 
    #add_ellipses +
    #add_scatter + 
    add_eig1 +  
    add_eig2 +
    add_centers + 
    labs(title = paste(xname,"vs",yname),x = xname,y = yname) + 
    scale_colour_manual(name = names(mu_trait_outs), values = biomeColors) + 
    theme(legend.position="none")
  
  assign(paste0("p",N), p)
  
  print(paste("p",N,"=", xname, yname))
}

################################################################################
################################################################################
# modelColors <- c("black",palette(rainbow(length(mu_trait_outs)-1)))
# names(modelColors) <- names(mu_trait_outs)
# 
# p1+colScale + scale_colour_manual(name = names(mu_trait_outs), values = modelColors)

biomes <-  unique(pft.table[,biome])
biompal <- pal; names(pal) <- biomes
biomeplot <- ggplot(data = pft.table) + geom_point(aes(x=pft.num, y=biome.num, col=biome)) + scale_colour_manual(name = biomes, values = biompal)
# biomeplot

r <- rectGrob(gp=gpar(fill="white"))
p <- arrangeGrob(p1, r, r, r, p2, p5, r, r, p3, p6, p8,r, p4, p7, p9, p10)
g.biome <- ggplotGrob(biomeplot + theme(legend.position="right"))$grobs
g.pft <- ggplotGrob(p1 + theme(legend.position="right"))$grobs
legend.biome <- g.biome[[which(sapply(g.biome, function(x) x$name) == "guide-box")]]
legend.pft <- g.pft[[which(sapply(g.pft, function(x) x$name) == "guide-box")]]


lheight <- sum(legend.pft$height)
lwidth <- sum(legend.pft$width)

legend <- arrangeGrob(legend.biome,legend.pft,ncol=1,heights = unit.c(unit(1, "npc") - lheight, lheight))

png(filename = filename, height = 900, width=1600)
grid.newpage()
grid.draw(arrangeGrob(p,
                      legend,
                      ncol = 2,
                      widths = unit.c(unit(1, "npc") - lwidth, lwidth)))
dev.off()


# 
# png(filename = file.path("figures", paste(traits[v1[N]], traits[v2[N]],"all","png", sep=".")),
#     height = 800, width = 800)
# plot(p1 + labs(title = paste(xname,"vs",yname),x = xname,y = yname))
# dev.off()
# 
# png(filename = file.path("figures", paste(traits[v1[N]], traits[v2[N]],"na","png", sep=".")),
#     height = 800, width = 800)
# plot(p2 + labs(title = paste(xname,"vs",yname),x = xname,y = yname))
# dev.off()





################################################################################
# Old version, not necessary right now

# pal_col <- c()
# pal <- brewer.pal(length(outs),"Paired")    
# names(pal) <- names(outs)
# #pal<-brewer.pal(round(length(outs)/2),"hcl")
# pal <- c(palette(rainbow(32))[])

# p1 <- ggplot(data = o)
# p2 <-  ggplot(data = o)
# for(i in 1:length(outs)){
#   x <- paste0("o",i,".v",1); y <- paste0("o",i,".v",2)
#   p1 <- p1 + stat_ellipse(aes_string(x = x, y = y), type="norm",
#                           # colour = pal[ceiling(i/2)], 
#                           colour = pal[i],
#                           linetype = i %% 2 + 1, size = 1.1, alpha=.5)
#   if(i %in% grep(".na", names(outs))){
#     p2 <- p2 + stat_ellipse(aes_string(x = x, y = y), type="norm",
#                             # colour = pal[ceiling(i/2)], 
#                             colour = pal[i],
#                             linetype = i %% 2 + 1, size = 1.1, alpha = .5)
#   }
# }
# 
