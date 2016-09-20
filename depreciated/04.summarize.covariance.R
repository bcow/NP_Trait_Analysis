library(mvtraits)

traits_combn <- combn(traits, 2)

message("Loading covariance matrix...")
hier_results <- readRDS("processed_output/sims.hier.rds")
cov.global <- hier_results$Sigma_trait
cor.global <- covToCor.global(cov.global)
message("Summarizing global covariance and correlation matrices...")
cov.global.summary <- summarizeSigma(cov.global, "global")
cor.global.summary <- summarizeSigma(cor.global, "global")

# Get PFT covariance and correlation matrices
cov.all <- hier_results$Sigma_pft

message("Summarizing PFT covariances...")
cov.dat <- summarizeSigmaPFT(cov.all) %>% 
    rbind(cov.global.summary)

message("Summarizing PFT correlations...")
cor.dat <- summarizeSigmaPFT(cov.all, Cor = TRUE) %>%
    rbind(cor.global.summary)

message("Saving results...")
saveRDS(cov.dat, file = "processed_output/summary.hier.cov.rds")
saveRDS(cor.dat, file = "processed_output/summary.hier.cor.rds")
message("Done!")

