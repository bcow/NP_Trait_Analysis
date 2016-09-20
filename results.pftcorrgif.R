library(mvtraits)

# Get the covariance matrices for each PFT
outdir <- "figures/pft.corrs"
all_summary <- readRDS("processed_output/summary.rds")
cov_all <- all_summary[model_type == "hier"][var_type == "Omega_pft"]
dir.create(outdir, showWarnings = FALSE)

for(i in 1:npft){
    pft_name <- pft.names[i]
    print(paste(i, pft_name))
    cor_pft <- tab2mat(cov_all[PFT == pft_name], corr = TRUE)
    png(sprintf("figures/pft.corrs/%02d.%s.png", 
                i, gsub("/","-",pft_name)))
    corrplot(cor_pft, method="ellipse", main=pft_name, mar = c(0,0,2,0))
    dev.off()
}

print("Creating GIF using system command 'gm'...")
system("gm convert -delay 70 figures/pft.corrs/*.png figures/pft.corrs.gif")
print("Done!")
