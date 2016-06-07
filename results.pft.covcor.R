source("00.common.R")

library(dplyr)
library(tidyr)
library(ggplot2)

applySampleMatrix <- function(samples, func){
    out <- array(NA, dim(samples))
    for(i in 1:dim(out)[1]){
        for(j in 1:dim(out)[2]){
            out[i,j,,] <- func(samples[i,j,,])
        }
    }
    return(out)
}

summarizeSampleMatrix <- function(cov.all.samples, dims){
    # Calculate summary statistics across samples
    cov.all.list <- list(Mean = apply(cov.all.samples, dims, mean),
                        SD = apply(cov.all.samples, dims, sd),
                        q025 = apply(cov.all.samples, dims, quantile, 0.025),
                        q500 = apply(cov.all.samples, dims, quantile, 0.500),
                        q975 = apply(cov.all.samples, dims, quantile, 0.975)
                        ) %>% lapply("dimnames<-", list(pft.names, traits, traits))

    trait.combine <- combn(traits, 2)
    getcov <- function(trait, cov.all) cov.all[, trait[1], trait[2]]
    columnize <- function(mat.wide){
        cov.mat <- apply(trait.combine, 2, getcov, mat.wide) %>% as.data.frame
        cov.mat.names <- apply(trait.combine, 2, paste, collapse="_") %>%
            gsub("log.", "", .)
        colnames(cov.mat) <- cov.mat.names
        return(cov.mat)
    }

    cov.dat <- lapply(cov.all.list, columnize) %>% 
        do.call(cbind, .) %>%
        cbind(., "PFT" = rownames(.)) %>%
        gather(Stat.Trait, Value, -PFT) %>%
        separate(Stat.Trait, into=c("Stat", "Trait"), sep="\\.") %>%
        spread(Stat, Value) %>%
        separate(PFT, into=c("Biome", "Function"), sep="_", 
                 extra="merge", remove=FALSE)

    return(cov.dat)
}

load("output/hier.trait.pft.na.Rdata") # Object name is "out"
print("Inverting sample precision matrices...")
cov.all <- applySampleMatrix(out$BUGSoutput$sims.list$Sigma_pfts, solve)
print("Calculating sample correlation matrices...")
cor.all <- applySampleMatrix(cov.all, cov2cor)
print("Summarizing covariance matrices...")
cov.dat <- summarizeSampleMatrix(cov.all, 2:4)
print("Summarizing correlation matrices...")
cor.dat <- summarizeSampleMatrix(cor.all, 2:4)

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

print("All done!")

