library(R2jags)
library(ggplot2)
library(tidyr)
library(dplyr)

source("convert.rownames.R")
if (!all(c("mcmc.full", "traits") %in% ls())) load("output.processed.RData")

get.samples <- function(out){
    samps <- as.data.frame(out$BUGSoutput$sims.list$mu_trait)
    colnames(samps) <- traits
    return(samps)
}

global.samples <- do.call(rbind, lapply(mcmc.full, get.samples)) %>% convert.rownames

## Violin plot
violin.dat <- global.samples %>% gather(Trait, Sample, 1:5)
plt <- ggplot(violin.dat) + aes(x=Model, y=Sample) + 
    geom_violin() + 
    facet_grid(Trait~., scales="free")
plot(plt)


## Global pairs plot
model.factor <- factor(global.samples$Model)
par(xpd=FALSE)
global.samples[,traits] %>% as.matrix %>% pairs(col = model.factor)
par(xpd=TRUE)
legend(0.02, 0.98, levels(model.factor), pch=19, col=1:3, bty="n")
