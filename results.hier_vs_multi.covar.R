library(mvtraits)

all_summary <- readRDS("processed_output/summary.rds")

png("figures/cor.global.multi.png")
plot_covar(all_summary[model_type == "multi"][var_type == "Omega"][PFT == "global"], 
           main="Multivariate")
dev.off()

png("figures/cor.global.hierarchical.png")
plot_covar(all_summary[model_type == "hier"][var_type == "Omega_global"], 
           main="Hierarchical")
dev.off()

