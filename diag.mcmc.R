require(R2jags)
require(mcmc)
library(mcmcplots)
require(shinystan)

files_na <- dir("output/", pattern = ".na.Rdata", full.names = TRUE )

for(i in 1:length(files_na)){
  load(files_na[i])
  out.mcmc <- as.mcmc(out)
  assign(paste0(basename(files_na[i]), ".sso"), as.shinystan(out.mcmc))
  remove(out.mcmc)
  remove(out)
}

launch_shinystan(uni.trait.na.Rdata.sso)
launch_shinystan(multi.trait.na.Rdata.sso)
launch_shinystan(hier.trait.pft.na.Rdata.sso)
save(uni.trait.na.Rdata.sso,multi.trait.na.Rdata.sso, hier.trait.pft.na.Rdata.sso, file = "sso.Rdata")


# Generate MCMC object for analysis
 uni.mcmc <- as.mcmc(out)
 summary(uni.mcmc)
 
# Trace Plot:
 par(mfrow = c(2,6))
 traceplot(uni.mcmc)
# Density plot:
 par(mfrow = c(1,3))
  densplot(uni.mcmc)
# Autocorrelation
  par(mfrow = c(1,3))
  autplot1(uni.mcmc)
  