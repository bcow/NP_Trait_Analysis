# load.try.data

try_full <- data.table(readRDS("data/try.data.rds"))
# Some na's in the table accidentally got changed to NaN, switch back so it doesn't confuse the model
nan2na <- function(x){
  x[is.nan(x)] <- NA
  return(x)
}
try_full <- try_full[,lapply(.SD, nan2na)]

# data without pfts
try.na <- try_full[,.(log.LL, log.LMA, log.Nmass, log.Pmass, log.Rdmass)]
try <- na.omit(try.na)

# data with pfts
try.pft.na <- try.na[!is.na(try_full$pft)]
try.pft <- na.omit(try.na)