# load.try.data

try_full <- data.table(readRDS("data/try.data.rds"))
# Some na's in the table accidentally got changed to NaN, switch back so it doesn't confuse the model
nan2na <- function(x){
  x[is.nan(x)] <- NA
  return(x)
}
try_full <- try_full[,lapply(.SD, nan2na)]

# data with pfts
try.na <- try_full[,.(log.LL, log.LMA, log.Nmass, log.Pmass, log.Rdmass)]
try.na <- try.na[!is.na(try_full$pft)]

try <- na.omit(try.na)

traits <- colnames(try)

a <- sort(try_full$pft[!is.na(try_full$pft)],index.return = T)
splits <- split(try.na[a$ix,], a$x)

trait_means <- colMeans(try)
pft_means <- as.data.frame(matrix(NA, length(splits), length(traits)))
colnames(pft_means) <- traits

for(i in 1:length(splits)){
  pft_means[i,] <- colMeans(splits[[i]],na.rm=T)
  for(j in 1:length(traits)){
    pft_means[i,traits[j]] <- ifelse(is.nan(pft_means[i,traits[j]]),trait_means[j],pft_means[i,traits[j]])
  }
}


pfts <- unique(try_full$pft[!is.na(try_full$pft)])
pft_sigmas <- array(NA,dim = c(length(pfts),length(traits),length(traits)))

for (i in 1:length(pfts)){
  pft_sigmas[i,,] <- cov(splits[[i]])
}