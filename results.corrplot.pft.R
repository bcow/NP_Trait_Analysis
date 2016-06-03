library(R2jags)
library(corrplot)

load("output/hier.trait.pft.na.c1-5.Rdata") # Object name is "out"
load("pfts.RData")
traits <- c("log.LL", "log.LMA", "log.Nmass", "log.Pmass", "log.Rdmass")

# Get the covariance matrices for each PFT
cov.inv.all <- out$BUGSoutput$mean$Sigma_pfts
cov.all <- array(NA, c(105, 5, 5))
for(i in 1:105) cov.all[i,,] <- solve(cov.inv.all[i,,])

cor.all <- array(NA, c(105,5,5))
for (i in 1:105){
    cor.all[i,,] <- cov2cor(cov.all[i,,])
}
dimnames(cor.all)[[1]] <- levels(pft.factor)
dimnames(cor.all)[[2]] <- dimnames(cor.all)[[3]] <- traits

for(i in 1:105){
    pft <- dimnames(cor.all)[[1]][i]
    print(pft)
    png(paste0("figures/pft.corrs/", pft, ".png"))
    corrplot(cor.all[i,,], method="ellipse", main=pft, mar = c(0,0,2,0))
    dev.off()
}

system("gm convert -delay 70 figures/pft.corrs/*.png figures/pft.corrs.gif")
