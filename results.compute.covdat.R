source("00.common.R")

library(dplyr)
library(tidyr)
library(ggplot2)
library(RColorBrewer)
library(grid)
library(gridExtra)

getcov <- function(trait, cov.all) cov.all[, trait[1], trait[2]]

columnize <- function(mat.wide){
    trait.combine <- combn(traits, 2)
    cov.mat <- apply(trait.combine, 2, getcov, mat.wide) %>% as.data.frame
    cov.mat.names <- apply(trait.combine, 2, paste, collapse="_") %>%
        gsub("log.", "", .)
    colnames(cov.mat) <- cov.mat.names
    return(cov.mat)
}

summarizeSampleMatrix <- function(cov.all.samples, dims){
    # Calculate summary statistics across samples
    dim.names <- list(pft.names, traits, traits)
    cov.all.list <- list(Mean = apply(cov.all.samples, dims, mean),
                        SD = apply(cov.all.samples, dims, sd),
                        q025 = apply(cov.all.samples, dims, quantile, 0.025),
                        q500 = apply(cov.all.samples, dims, quantile, 0.500),
                        q975 = apply(cov.all.samples, dims, quantile, 0.975)
                        ) %>% lapply("dimnames<-", dim.names)

    cov.dat <- lapply(cov.all.list, columnize) %>% 
        do.call(cbind, .) %>%
        add_rownames(var = "PFT") %>%
        gather(Stat.Trait, Value, -PFT) %>%
        separate(Stat.Trait, into=c("Stat", "Trait"), sep="\\.") %>%
        spread(Stat, Value) %>%
        separate(PFT, into=c("Biome", "Function"), sep="_", 
                 extra="merge", remove=FALSE)

    return(cov.dat)
}

print("Loading covariance matrix...")
load("output/hier.trait.pft.na/hier.trait.pft.na.Rdata") # Object name is "out"
# Get global covariance and correlation
cov.global <- out$BUGSoutput$sims.list$Sigma_trait
cor.global <- array2DApply(cov.global, cov2cor)
traits.nolog <- traits %>% gsub("log.", "", .)
cor.mean <- apply(cor.global, 2:3, mean)
cor.global.dat <- apply(cor.global, 2:3, mean) %>%
    "dimnames<-"(list(traits.nolog, traits.nolog)) %>%
    replace(upper.tri(., diag=TRUE), NA) %>%
    as.data.frame %>%
    add_rownames(var = "trait2") %>%
    gather(trait1, Value, -trait2) %>%
    filter(!is.na(Value)) %>%
    mutate(trait = paste(trait1, trait2, sep="_")) %>%
    select(trait, Value)

# Get PFT covariance matrices
cov.all <- out$BUGSoutput$sims.list$Sigma_pft
print("Calculating sample correlation matrices...")
cor.all <- cov.all
cor.all <- array3Dapply(cov.all, cov2cor)
cov.dat <- summarizeSampleMatrix(cov.all, 2:4)
print("Summarizing correlation matrices...")
cor.dat <- summarizeSampleMatrix(cor.all, 2:4) %>%
    separate(Function, into = c("growth_form", "ps_type", "leaf_type", "phenology"),
             sep = "_", extra = "drop", remove=FALSE) %>%
    as.data.table()
cor.dat[is.na(ps_type), c("ps_type", "leaf_type", "phenology") := Function]

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

cor.plot.dat[, scaledValue := Value / sum(Value), by=Trait]

# Compute total variance by type
tot.var.table <- cor.plot.dat[, list(tot.var = sum(Value)), by=Type]
tot.var.table[, pct.var := 100 * tot.var / sum(tot.var)]
sink("figures/tot.var.table.txt")
print(tot.var.table[order(tot.var, decreasing=TRUE)], 
      digits = 3)
sink()

