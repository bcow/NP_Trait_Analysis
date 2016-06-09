library(ggplot2)
library(grid)
library(gridExtra)

source("00.common.R")

##### Setup Data ###############################################################

source("load.try.data.R")
load("output/out.trait.means.Rdata")

combine.means <- function(n){
  dat <- out.trait.means[[n]]
  dat$Model <- rep(names(out.trait.means[n]),nrow(dat))
  return(dat)
}


n <- grep(".na", names(out.trait.means))
samples <- do.call(rbind,lapply(n, function(n) combine.means(n)))
dat.long <- samples %>% gather(Trait, Sample, 1:5)

color.dt <- generate.colors(unique(dat.long$Model))

##### Violin Plots #############################################################

## Hierarchical in one figure
plot.dat <- filter(dat.long, grepl("hier.trait.pft",Model))
color.dt <- generate.colors(unique(plot.dat$Model))
model.colors <- color.dt$Color; names(model.colors) <- color.dt$Model

plt <- ggplot(plot.dat) + aes(x=Model, y=Sample, colour=Model) + 
  geom_violin() + facet_grid(Trait~., scales="free") +
  scale_colour_manual(name = plot.dat$Model, values = model.colors)

png(filename = "figures/violin.hier.na.png", height = 900, width=1600)
plot(plt)
dev.off()

## All models by pft except right now the facets are wrong
for(trait in traits){
  plot.dat <- filter(dat.long, Trait == trait)
  color.dt <- generate.colors(unique(plot.dat$Model))
  model.colors <- color.dt$Color; names(model.colors) <- color.dt$Model
  
  plt <- ggplot(plot.dat) + aes(x=Model, y=Sample, colour=Model) + 
    geom_violin() + labs(title = trait) + facet_grid(Model~., scales="free") +
    scale_colour_manual(name = plot.dat$Model, values = model.colors)
  
  png(filename = sprintf("figures/violin.hier.%s.na.png",trait), height = 900, width=1600)
  plot(plt)
  dev.off()
}

##### Marginal Distribution Plots for each trait ###############################

model.dat <- filter(dat.long, grepl("hier.trait.pft",Model))
color.dt <- generate.colors(unique(plot.dat$Model))
model.colors <- color.dt$Color; names(model.colors) <- color.dt$Model

for(trait in traits){
  dat.trait <- filter(model.dat, Trait == trait)
  color.dt <- generate.colors(unique(dat.trait$Model))
  plot.dat <- merge(dat.trait,color.dt,by = "Model")
  global.dat <- filter(plot.dat,Biome=="global")
  p <- ggplot() +
    geom_density(data=plot.dat, aes(Sample, colour=Model))  +
    geom_density(data=global.dat, aes(Sample, col=Model), size = 1.5) +
    scale_colour_manual(values = model.colors) + 
    labs(title = trait) + theme(legend.position="none")
  assign(paste0("p.",trait),p)
}

# png(filename = "figures/stacked.marginal.means.png", height = 900, width=1600)
# ggplot() + geom_bar(data=plot.dat, aes(Sample, colour=Model))
# dev.off()

biome.colors <- unique(color.dt$Color) 
names(biome.colors) <- unique(color.dt$Biome)

# just for making biome legend
biomeplot <- ggplot(data = color.dt) + 
  geom_point(aes(x=as.numeric(as.factor(Model)), y=as.numeric(as.factor(Biome)), colour=Biome)) + 
  scale_colour_manual(name =  color.dt$Biome, values = biome.colors)

p <- arrangeGrob(p.log.LL,p.log.LMA,p.log.Nmass,p.log.Pmass,p.log.Rdmass)
g.biome <- ggplotGrob(biomeplot + theme(legend.position="right"))$grobs
g.pft <- ggplotGrob(p.log.LL + theme(legend.position="right"))$grobs
legend.biome <- g.biome[[which(sapply(g.biome, function(x) x$name) == "guide-box")]]
legend.pft <- g.pft[[which(sapply(g.pft, function(x) x$name) == "guide-box")]]

lheight <- sum(legend.pft$height)
lwidth <- sum(legend.pft$width)

legend <- arrangeGrob(legend.biome,legend.pft,ncol=1,heights = unit.c(unit(1, "npc") - lheight, lheight))

# png(filename = "figures/marginal.dist.hier.na.png", height = 900, width=1600)
grid.newpage()
grid.draw(arrangeGrob(p,
                      legend,
                      ncol = 2,
                      widths = unit.c(unit(1, "npc") - lwidth, lwidth)))
# dev.off()

##### PFT ellipses #############################################################

source("load.try.data.R")
color.dt <- generate.colors(unique(dat.long$Model))
a <- b <- length(traits)
v1 = c(); v2 = c()
for(i in 1:(a-1)){
  v1 = c(v1, rep(i,a-i))
  v2 = c(v2, (i+1):(a))
}
class(samples)

samp.dat <-  as.data.table(merge(samples,color.dt[,.(Model,pft.num)],by="Model"))

obvs.dat <- try.na[,pft.num := sprintf("%02.f", as.numeric(pft))]
obvs.dat <- obvs.dat[,pft:=NULL]
obvs.dat[,Data := rep("Observed", nrow(obvs.dat))]
names(obvs.dat)

for(P in unique(samp.dat$pft.num)){
  pft.samp.dat <- filter(samp.dat, pft.num == P)
  pft.obvs.dat <- filter(obvs.dat, pft.num == P)
  for(N in 1:length(v1)){
    xname <- paste(unlist(strsplit(traits[v1[N]], "[.]"))[1:2],collapse =" ")
    yname <- paste(unlist(strsplit(traits[v2[N]], "[.]"))[1:2],collapse =" ")
    
    plot.samp.dat <- pft.samp.dat[,c(traits[v1[N]],traits[v2[N]],"Model"), with=F]
    if(P=="global"){
      plot.obvs.dat <- try.na[,c(traits[v1[N]],traits[v2[N]]),with=F][,Data := rep("Observed", nrow(obvs.dat))]
    }else{
      plot.obvs.dat <- pft.obvs.dat[,c(traits[v1[N]],traits[v2[N]],"Data"),with=F]
    }
    
    colnames(plot.samp.dat) <- c("V1","V2","Model")
    colnames(plot.obvs.dat) <- c("V1","V2","Data") 
    
    plot.colors <- c("black","red","green","blue")
    names(plot.colors) <- c("Observed", unique(plot.samp.dat$Model))
    
    p <- ggplot() +
      geom_point(data=plot.obvs.dat, aes(x=V1,y=V2,colour=Data), alpha=.3) +
      scale_colour_manual(values = plot.colors) +
      stat_ellipse(data=plot.samp.dat, aes(x=V1,y=V2,colour=Model)) +
      theme(
        panel.background = element_rect(fill = "white"),
        #panel.border = element_rect(colour = "black"),
        axis.text = element_text(size = 14),
        legend.position = "none",
        panel.grid.major = element_line(colour = "grey90"),
        axis.line = element_line(colour = "black")) +
      labs(title = sprintf("%s vs. %s", xname,yname), x=xname, y=yname) 
    assign(sprintf("p%i",N),p)
  }
  
  t <- textGrob(P, gp = gpar(fontface = "bold", cex = 4))
  r <- rectGrob(gp=gpar(fill="white"))
  p <- arrangeGrob(p1, r, r, t, p2, p5, r, r, p3, p6, p8,r, p4, p7, p9, p10)
  g <- ggplotGrob(p1 + theme(legend.position="right"))$grobs
  legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]
  lheight <- sum(legend$height)
  lwidth <- sum(legend$width)
  
  png(filename = sprintf("figures/join.density.by.pft/joint.density.%s.png",P), height = 800, width=1100)
    grid.newpage() 
    grid.draw(arrangeGrob(p,legend,ncol = 2, 
                          widths = unit.c(unit(1, "npc") - lwidth, lwidth)))
  dev.off()
}
