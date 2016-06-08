source("00.common.R")

library(dplyr)
library(tidyr)
library(ggplot2)

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
        add_rownames(var = "PFT") %>%
        gather(Stat.Trait, Value, -PFT) %>%
        separate(Stat.Trait, into=c("Stat", "Trait"), sep="\\.") %>%
        spread(Stat, Value) %>%
        separate(PFT, into=c("Biome", "Function"), sep="_", 
                 extra="merge", remove=FALSE)

    return(cov.dat)
}

print("Loading covariance matrix...")
load("output/hier.trait.pft.na.Rdata") # Object name is "out"
cov.all <- array3Dapply(out$BUGSoutput$sims.list$Sigma_pfts, solve)
print("Calculating sample correlation matrices...")
cor.all <- array3Dapply(cov.all, cov2cor)
print("Summarizing covariance matrices...")
cov.dat <- summarizeSampleMatrix(cov.all, 2:4)
print("Summarizing correlation matrices...")
cor.dat <- summarizeSampleMatrix(cor.all, 2:4) %>%
    separate(Function, into = c("growth_form", "ps_type", "leaf_type", "phenology"),
             sep = "_", extra = "drop", remove=FALSE) %>%
    as.data.table()
cor.dat[is.na(ps_type), c("ps_type", "leaf_type", "phenology") := Function]

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

# Plot the ANOVA
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

cor.plot.dat[, Value := Value / sum(Value), by=Trait]
cor.anova.plot <- ggplot(cor.plot.dat) + aes(x = Trait, y = Value, fill = Type) + 
    geom_bar(stat="identity")

mypng("figures/pft.cor.anova.png")
plot(cor.anova.plot)
dev.off()

print("All done!")

