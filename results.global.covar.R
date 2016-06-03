library(corrplot)
library(R2jags)

traits <- c("log.LL", "log.LMA", "log.Nmass", "log.Pmass", "log.Rdmass")

# Get the global covariance matrix
load("output/multi.trait.c1-5.Rdata") # Object name is "out"
cov.global <- solve(out$BUGSoutput$mean$Sigma_trait)
rownames(cov.global) <- colnames(cov.global) <- traits
cor.global <- cov2cor(cov.global)
png("figures/global_means/cor.global.multi.png")
corrplot.mixed(cor.global, lower = "ellipse", upper = "number",
               main="Multivariate", mar=c(0,0,2,0))
dev.off()

load("output/hier.trait.pft.na.c1-5.Rdata") # Object name is "out"
cov.global <- solve(out$BUGSoutput$mean$Sigma_trait)
rownames(cov.global) <- colnames(cov.global) <- traits
cor.global <- cov2cor(cov.global)
png("figures/global_means/cor.global.hierarchical.png")
corrplot.mixed(cor.global, lower = "ellipse", upper = "number",
               main="Hierarchical", mar=c(0,0,2,0))
dev.off()

