library(R2jags)
library(dplyr)
library(corrplot)
library(tidyr)
library(ggplot2)
library(stringr)

traits <- c("log.LL", "log.LMA", "log.Nmass", "log.Pmass", "log.Rdmass")
load("pfts.RData")
load("output/hier.trait.pft.na.c1-5.Rdata") # Object name is "out"

# Get the covariance matrices for each PFT
cov.inv.all <- out$BUGSoutput$mean$Sigma_pfts
cov.all <- array(NA, c(105, 5, 5))
for(i in 1:105) cov.all[i,,] <- solve(cov.inv.all[i,,])
dimnames(cov.all)[[1]] <- levels(pft.factor)
dimnames(cov.all)[[2]] <- traits
dimnames(cov.all)[[3]] <- traits

covcorPlot <- function(cov.all){
#
    lma.nmass.cov <- cov.all[, "log.LMA", "log.Nmass"]
    ll.lma.cov <- cov.all[, "log.LL", "log.LMA"]
    nmass.pmass.cov <- cov.all[, "log.Nmass", "log.Pmass"]
    lma.rdmass.cov <- cov.all[, "log.LMA", "log.Rdmass"]
#
    cov.subset <- cbind(lma.nmass.cov, ll.lma.cov, nmass.pmass.cov, lma.rdmass.cov)
    cov.dat <- cov.subset %>% 
        as.data.frame %>% 
        cbind("PFT"=factor(levels(pft.factor), levels(pft.factor))) %>%
        gather(Trait, Value, 1:4)
    pft.pattern <- "(.*)_(.*)_(.*)_(.*)_(.*)"
    cov.dat$Biome <- cov.dat$PFT %>% as.character %>% 
        str_replace(pft.pattern, "\\1") %>% factor
    cov.dat$growthform <- cov.dat$PFT %>% as.character %>%
        str_replace(pft.pattern, "\\2") %>% factor
    cov.dat$PS <- cov.dat$PFT %>% as.character %>%
        str_replace(pft.pattern, "\\3") %>% factor
    cov.dat$leaftype <- cov.dat$PFT %>% as.character %>%
        str_replace(pft.pattern, "\\4") %>% factor
    cov.dat$phenology <- cov.dat$PFT %>% as.character %>%
        str_replace(pft.pattern, "\\5") %>% factor
#
    cov.plot <- ggplot(cov.dat) + aes(x=PFT, y=Value) + 
        geom_bar(stat="identity") + 
        theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
    return(cov.plot)
}

mypng <- function(filename){
    png(filename, width=1000, height=700)
}
cov.plot <- covcorPlot(cov.all) + aes(fill=leaftype) + 
    facet_grid(Trait ~ Biome, scales="free") + 
    theme(legend.position = "bottom") + ylab("Covariance")
mypng("figures/global_means/pft.cov.plot.png")
plot(cov.plot)
dev.off()

# Calculate the correlation matrices for each PFT
cor.all <- array(NA, c(105,5,5))
for (i in 1:105){
    cor.all[i,,] <- cov2cor(cov.all[i,,])
}
dimnames(cor.all)[[1]] <- levels(pft.factor)
dimnames(cor.all)[[2]] <- dimnames(cor.all)[[3]] <- traits

cor.plot <- covcorPlot(cor.all) + aes(fill=leaftype) + 
    facet_grid(Trait ~ Biome, scales="free") + 
    theme(legend.position = "bottom") + ylab("Correlation")
mypng("figures/global_means/pft.cor.plot.png")
plot(cor.plot)
dev.off()
