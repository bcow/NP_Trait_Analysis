source("00.common.R")
source("results.compute.covdat.R")

library(dplyr)
library(tidyr)
library(ggplot2)
library(RColorBrewer)
library(grid)
library(gridExtra)

##### Facet Grid Correlation Plot ##############################################

cov.plt <- ggplot(cov.dat) + 
    aes(x=Function, y=q500, ymin=q025, ymax=q975, color=Function) +
    geom_pointrange() + 
    geom_hline(yintercept = 0) + 
    facet_grid(Trait ~ Biome, scales="free") +
    theme(axis.text.x = element_blank(), 
          axis.ticks.x = element_blank(),
          legend.position = "bottom") +
    ylab("Covariance") + xlab("PFT")

mypng <- function(filename){
    png(filename, width=1000, height=700)
}

print("Generating covariance plot...")
mypng("figures/pft.cov.plot.png")
plot(cov.plt)
dev.off()

print("Generating correlation plot...")
mypng("figures/pft.cor.plot.png")
plot(cov.plt %+% cor.dat + ylab("Correlation"))
dev.off()

##### Plot the ANOVA ###########################################################

Type.colors <- c(brewer.pal(5, "Spectral"), "grey")
names(Type.colors) <- rnames
  
cor.anova.plot <- ggplot(cor.plot.dat) + 
  aes(x = Trait, fill = Type) + 
  geom_bar(stat="identity") + 
  scale_fill_manual(values = Type.colors) 

mypng("figures/pft.cor.anova.scaled.png")
plot(cor.anova.plot + aes(y = scaledValue))
dev.off()

mypng("figures/pft.cor.anova.png")
plot(cor.anova.plot + aes(y = Value))
dev.off()

print("All done!")

##### Stacked Correlation Plot #################################################

# playing with different colors
biome.colors <- brewer.pal(length(unique(cor.dat$Biome)),"Set1")  
names(biome.colors) <- unique(cor.dat$Biome)
ps_type.colors <- brewer.pal(length(unique(cor.dat$ps_type)),"Set2")  
names(ps_type.colors) <- unique(cor.dat$ps_type)
leaf_type.colors <- brewer.pal(length(unique(cor.dat$leaf_type)),"Set3")  
names(leaf_type.colors) <- unique(cor.dat$leaf_type)
growth_form.colors <- brewer.pal(length(unique(cor.dat$growth_form)),"Dark2")  
names(growth_form.colors) <- unique(cor.dat$growth_form)
Function.colors <- brewer.pal(length(unique(cor.dat$Function)),"Accent")  
names(Function.colors) <- unique(cor.dat$Function)

for(i in 1:length(trait.pairs)){
  dat <- filter(cor.dat, Trait == trait.pairs[i])
  # ggplot(dat) + geom_density(aes(Mean, colour=Biome))
  # ggplot(dat) + geom_histogram()
  # ggplot(dat, aes(Mean, colour=Biome)) + geom_density()
  # ggplot(dat, aes(Mean, fill = Biome)) + geom_histogram(binwidth = .06)
  p <- ggplot(dat, aes(x=Mean, y=..density..)) + 
    geom_density(aes(fill=Biome), position="stack") +
    scale_fill_manual(values=biome.colors) + labs(title = trait.pairs[i]) + 
    theme(legend.position="none")
  assign(paste0("p",i), p)
}
r <- rectGrob(gp=gpar(fill="white"))
p <- arrangeGrob(p1, r, r, r, p2, p5, r, r, p3, p6, p8,r, p4, p7, p9, p10)
g <- ggplotGrob(p1 + theme(legend.position="right"))$grobs
legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]
lheight <- sum(legend$height)
lwidth <- sum(legend$width)
png(filename = sprintf("figures/stacked.cor.Biome.png"), height = 800, width=1100)
grid.newpage() 
grid.draw(arrangeGrob(p,legend,ncol = 2, 
                      widths = unit.c(unit(1, "npc") - lwidth, lwidth)))
dev.off()
