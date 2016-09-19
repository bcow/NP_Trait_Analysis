library(mvtraits)
library(RColorBrewer)
library(grid)
library(gridExtra)

##### Facet Grid Correlation Plot ##########################

h_summary <- readRDS("processed_output/summary.rds")[model_type == "hier"]
cov.dat <- h_summary[var_type == "Sigma_pft"][PFT != "global"]
cor.dat <- h_summary[var_type == "Omega_pft"][PFT != "global"]
cor.global.dat <- h_summary[var_type == "Omega_pft"][PFT == "global"]
trait.pairs <- cor.dat[, unique(trait)]
#cov.dat[, lapply(.SD, function(x) {x[is.na(x)] <- 0; return(x)})]

cov.plt <- ggplot(cov.dat) + 
    aes(x=Function, y=q500, ymin=q025, ymax=q975, color=Function) +
    geom_pointrange() + 
    geom_hline(yintercept = 0) + 
    facet_grid(trait ~ Biome, scales="free") +
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

##### Stacked Correlation Plot ################################


# playing with different colors
Biome.colors <- brewer.pal(length(unique(cor.dat$Biome))+1,"Set1")  
names(Biome.colors) <- c(unique(cor.dat$Biome), "Global")

ps_type.colors <- brewer.pal(length(unique(cor.dat$ps_type)),"Set2")  
names(ps_type.colors) <- unique(cor.dat$ps_type)
leaf_type.colors <- brewer.pal(length(unique(cor.dat$leaf_type)),"Set3")  
names(leaf_type.colors) <- unique(cor.dat$leaf_type)
growth_form.colors <- brewer.pal(length(unique(cor.dat$growth_form)),"Dark2")  
names(growth_form.colors) <- unique(cor.dat$growth_form)
Function.colors <- brewer.pal(length(unique(cor.dat$Function)),"Accent")  
names(Function.colors) <- unique(cor.dat$Function)

for(i in 1:length(trait.pairs)){
  dat <- filter(cor.dat, trait == trait.pairs[i])
  global.mean <- filter(cor.global.dat, trait == trait.pairs[i])$Mean
  biome.means <- as.data.frame(summarise(group_by(dat, Biome),
            mean=mean(Mean)))
  mean <- mean(dat$Mean)

  p <- ggplot() + geom_vline(xintercept = global.mean, size=1.5, color="magenta") + 
    scale_colour_manual(values=Biome.colors)+ 
    geom_density(data=dat,aes(x=Mean, y=..density..,fill=Biome), position="stack") +
    scale_fill_manual(values=Biome.colors) + labs(title = trait.pairs[i]) + 
    xlim(-1,1) + geom_vline(xintercept = 0, size=.5, linetype = "longdash") + 
    theme(legend.position = "none")
  assign(paste0("p",i), p)
}

png("/dev/null")
r <- rectGrob(gp=gpar(fill="white"))
p <- arrangeGrob(p1, r, r, r, p2, p5, r, r, p3, p6, p8,r, p4, p7, p9, p10)
g <- ggplotGrob(p1 + theme(legend.position="right"))$grobs

legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]
lheight <- sum(legend$height)
lwidth <- sum(legend$width)
dev.off()
png(filename = sprintf("figures/stacked.cor.biome.png"), height = 800, width=1100)
grid.newpage() 
grid.draw(arrangeGrob(p,legend,ncol = 2, 
                      widths = unit.c(unit(1, "npc") - lwidth, lwidth)))
dev.off()
