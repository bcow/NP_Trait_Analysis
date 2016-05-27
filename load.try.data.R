# load.try.data
library(data.table)
try_full <- data.table(readRDS("data/try.data.rds"))
# Some na's in the table accidentally got changed to NaN, 
# switch back so it doesn't confuse the model
nan2na <- function(x){
  x[is.nan(x)] <- NA
  return(x)
}
try_full <- try_full[,lapply(.SD, nan2na)]

# data with pfts and missing values
try.tps.na <-  try_full[,.(log.LL, log.LMA, log.Nmass, log.Pmass, log.Rdmass,pft,AccSpeciesName)]
try.na <- try_full[,.(log.LL, log.LMA, log.Nmass, log.Pmass, log.Rdmass)]

try.na <- try.na[!is.na(try_full$pft)]
try.tps.na <- try.tps.na[!is.na(try_full$pft)]
# data with pfts and no missing values
try <- na.omit(try.na)
try.more <- na.omit(try.more.na)
# setup initializations for models
traits <- colnames(try)
trait_means <- colMeans(try)
pfts <- unique(try_full$pft[!is.na(try_full$pft)])

s <- sort(try_full$pft[!is.na(try_full$pft)],index.return = T)
splt.pft <- split(try.na[s$ix,], s$x)

pft_means <- as.data.frame(matrix(NA, length(splt.pft), length(traits)))
colnames(pft_means) <- traits

for(i in 1:length(splt.pft)){
  pft_means[i,] <- colMeans(splt.pft[[i]],na.rm=T)
  for(j in 1:length(traits)){
    # If there are no values for trait[j] in pft[i], use overall mean
    pft_means[i,traits[j]] <- ifelse(is.nan(pft_means[i,traits[j]]),
                                     trait_means[j],pft_means[i,traits[j]])
  }
}

#  this returns an array with many missing values so I'm not using it for now
pft_sigmas <- array(NA,dim = c(length(pfts),length(traits),length(traits)))
for (i in 1:length(pfts)){
  pft_sigmas[i,,] <- cov(splt.pft[[i]])
}