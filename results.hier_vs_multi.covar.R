library(corrplot)
library(R2jags)

source("00.common.R")

plot_covar <- function(fname, ...){
    load(fname)                        # Object name is "out"
    cov.global <- solve(out$BUGSoutput$mean$Sigma_trait)
    rownames(cov.global) <- colnames(cov.global) <- traits
    cor.global <- cov2cor(cov.global)
    plt <- corrplot.mixed(cor.global, lower = "ellipse", upper = "number",
                          mar=c(0,0,2,0), ...)
    return(plt)
}

png("figures/cor.global.multi.png")
plot_covar("output/multi.trait.na.Rdata", main="Multivariate")
dev.off()

png("figures/cor.global.hierarchical.png")
plot_covar("output/hier.trait.pft.na.Rdata", main="Hierarchical")
dev.off()

