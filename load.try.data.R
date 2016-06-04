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

# data with pfts and missing values
try.na <-  try_full[,c(traits,"pft.factor"), with = F]
setnames(try.na, colnames(try.na), c(traits, "pft"))
try.na <- try.na[!is.na(try_full$pft)]

# data with pfts and no missing values
try <- na.omit(try.na)


