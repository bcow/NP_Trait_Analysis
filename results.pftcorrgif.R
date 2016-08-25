library(mvtraits)

# Get the covariance matrices for each PFT
outdir <- "figures/pft.corrs"
cov.all <- readRDS("processed_output/sims.hier.rds")$Sigma_pft
dir.create(outdir, showWarnings = FALSE)

for(i in 1:npft){
    pft_name <- pft.names[i]
    print(paste(i, pft_name))
    cov.pft <- getPFTFromHier(cov.all, pft_name)
    cor.pft <- covToCor.global(cov.pft) %>% colMeans %>% vecToMat
    png(sprintf("figures/pft.corrs/%02d.%s.png", 
                i, gsub("/","-",pft_name)))
    corrplot(cor.pft, method="ellipse", main=pft_name, mar = c(0,0,2,0))
    dev.off()
}

print("Creating GIF using system command 'gm'...")
system("gm convert -delay 70 figures/pft.corrs/*.png figures/pft.corrs.gif")
print("Done!")
