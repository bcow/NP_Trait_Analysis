# load.try.data
library(data.table)
source("00.common.R")
try_full <- readRDS("data/try.data.rds")

# Some na's in the table accidentally got changed to NaN, 
# switch back so it doesn't confuse the model
nan2na <- function(x){
  x[is.nan(x)] <- NA
  return(x)
}
try_full <- try_full[,lapply(.SD, nan2na)]

# data with pfts
try.data <-  try_full[,c(traits,"pft.factor"), with = F]
setnames(try.data, colnames(try.data), c(traits, "pft"))
try.data <- try.data[!is.na(try_full$pft)]
