source("00.common.R")

library(dplyr)
library(tidyr)
library(ggplot2)
library(RColorBrewer)
library(grid)
library(gridExtra)

print("Loading covariance matrix...")
load("output/hier.trait.pft.na/hier.trait.pft.na.Rdata") # Object name is "out"
cov.all <- out$BUGSoutput$sims.list$Sigma_pft
#cov.all <- array3Dapply(out$BUGSoutput$sims.list$Sigma_pft, solve)
print("Calculating sample correlation matrices...")
cor.all <- cov.all
cor.all <- array3Dapply(cov.all, cov2cor)
cov.dat <- summarizeSampleMatrix(cov.all, 2:4, list(pft.names,traits,traits))
print("Summarizing correlation matrices...")
cor.dat <- summarizeSampleMatrix(cor.all, 2:4, list(pft.names,traits,traits)) %>%
    separate(Function, into = c("growth_form", "ps_type", "leaf_type", "phenology"),
             sep = "_", extra = "drop", remove=FALSE) %>%
    as.data.table()
cor.dat[is.na(ps_type), c("ps_type", "leaf_type", "phenology") := Function]

cov.global <- out$BUGSoutput$sims.list$Sigma_trait
cor.global <- array2DApply(cov.global, cov2cor)

cor.all.g <-  abind(cor.all, cor.global, along= 2)
cor.dat.names <- c(list(c(pft.names, "global"), traits, traits))
cor.dat.g <- summarizeSampleMatrix(cor.all.g, 2:4, cor.dat.names) %>%
  separate(Function, into = c("growth_form", "ps_type", "leaf_type", "phenology"),
           sep = "_", extra = "drop", remove=FALSE) %>%
  as.data.table()
cor.dat.g[is.na(ps_type), c("ps_type", "leaf_type", "phenology") := Function]
cor.dat.test <- filter(cor.dat.g, PFT != "global")
all(cor.dat.test == cor.dat)

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

print("Generating ANOVA plot...")
trait.pairs <- unique(cor.dat[, Trait])
cor.list <- list()
rnames <- c("Biome", "ps_type", "growth_form", "leaf_type", "phenology", "Residuals")
for(trait in trait.pairs){
    cor.list[[trait]] <- lm(Mean ~ Biome + ps_type + growth_form + ps_type + leaf_type + phenology,
                cor.dat, subset = Trait == trait) %>% 
                anova %>% select(2)
}

cor.anova <- do.call(cbind, cor.list)
colnames(cor.anova) <- trait.pairs
cor.plot.dat <- cor.anova %>% add_rownames(var = "Type") %>%
    gather(Trait, Value, -Type) %>%
    as.data.table()

# as per Mike's request
 cor.plot.dat[, Value := Value / sum(Value), by=Trait]

Type.colors <- c(brewer.pal(5, "Spectral"), "grey")
names(Type.colors) <- rnames
  
cor.anova.plot <- ggplot(cor.plot.dat) + 
  aes(x = Trait, y = Value, fill = Type) + 
  geom_bar(stat="identity") + 
  scale_fill_manual(values = Type.colors) 

mypng("figures/pft.cor.anova.scaled.png")
plot(cor.anova.plot)
dev.off()

print("All done!")

##### Stacked Correlation Plot #################################################

cor.dat.what<- filter(cor.dat.g, PFT != "global")

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

global.mean <- mean(cor.dat$Mean)

for(i in 1:length(trait.pairs)){
  dat <- filter(cor.dat, Trait == trait.pairs[i])
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
r <- rectGrob(gp=gpar(fill="white"))
p <- arrangeGrob(p1, r, r, r, p2, p5, r, r, p3, p6, p8,r, p4, p7, p9, p10)
g <- ggplotGrob(p1 + theme(legend.position="right"))$grobs
legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]
lheight <- sum(legend$height)
lwidth <- sum(legend$width)
png(filename = sprintf("figures/stacked.cor.growth_form.png"), height = 800, width=1100)
grid.newpage() 
grid.draw(arrangeGrob(p,legend,ncol = 2, 
                      widths = unit.c(unit(1, "npc") - lwidth, lwidth)))
dev.off()

##### Plot the ANOVA ###########################################################

print("Generating ANOVA plot...")
cor.list <- list()
rnames <- c("Biome", "ps_type", "growth_form", "leaf_type", "phenology", "Residuals")
for(trait in trait.pairs){
    cor.list[[trait]] <- lm(Mean ~ Biome + ps_type + growth_form + ps_type + leaf_type + phenology,
                cor.dat, subset = Trait == trait) %>% 
                anova %>% select(2)
}

cor.anova <- do.call(cbind, cor.list)
colnames(cor.anova) <- trait.pairs
cor.plot.dat <- cor.anova %>% add_rownames(var = "Type") %>%
    gather(Trait, Value, -Type) %>%
    as.data.table()

# as per Mike's request
# cor.plot.dat[, Value := Value / sum(Value), by=Trait]

Type.colors <- c(brewer.pal(5, "Spectral"), "grey")
names(Type.colors) <- rnames
  
cor.anova.plot <- ggplot(cor.plot.dat) + 
  aes(x = Trait, y = Value, fill = Type) + 
  geom_bar(stat="identity") + 
  scale_fill_manual(values = Type.colors) 

mypng("figures/pft.cor.anova.png")
plot(cor.anova.plot)
dev.off()

print("All done!")
