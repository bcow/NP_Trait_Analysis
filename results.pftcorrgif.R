library(R2jags)
library(corrplot)

source("00.common.R")
load("output/hier.trait.pft.na/hier.trait.pft.na.Rdata") # Object name is "out"

# Get the covariance matrices for each PFT
cov.all <- out$BUGSoutput$mean$Sigma_pft
cor.all <- array2DApply(cov.all, 1,  cov2cor)

dimnames(cor.all) <- list(pft.names, traits, traits)

for(i in 1:npft){
    pft <- dimnames(cor.all)[[1]][i]
    print(pft)
    png(paste0("figures/pft.corrs/", gsub("/","-",pft), ".png"))
    corrplot(cor.all[i,,], method="ellipse", main=pft, mar = c(0,0,2,0))
    dev.off()
}

print("Creating GIF using system command 'gm'...")
system("gm convert -delay 70 figures/pft.corrs/*.png figures/pft.corrs.gif")
print("Done!")
