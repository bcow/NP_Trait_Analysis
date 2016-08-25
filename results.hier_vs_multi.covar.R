library(mvtraits)

png("figures/cor.global.multi.png")
plot_covar("processed_output/sims.multi_global.rds", main="Multivariate")
dev.off()

png("figures/cor.global.hierarchical.png")
plot_covar("processed_output/sims.hier.rds", main="Hierarchical")
dev.off()

