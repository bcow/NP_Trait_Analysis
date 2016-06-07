# Common definitions for scripts
library(data.table)

traits <- c("log.LL", "log.LMA", "log.Nmass", "log.Pmass", "log.Rdmass")

is.error <- function(x) class(x) == "try-error"

replace.with.global <- function(x, global){
    nas <- which(is.na(x) | is.nan(x))
    x[nas] <- global[nas]
    return(x)
}

convert.rownames <- function(dat){
    pattern <- "(.*)\\.na\\..*"
    dat$Model <- gsub(pattern, "\\1", rownames(dat))
    rownames(dat) <- NULL
    return(dat)
}

arrayApply <- function(arr, func, ...){
    arr.dim <- dim(arr)
    out <- array(NA, arr.dim)
    for(i in 1:arr.dim[1]) out[i,,] <- func(arr[i,,], ...)
    return(out)
}

solveArray <- function(arr) arrayApply(arr, solve)
cov2corArray <- function(arr) arrayApply(arr, cov2cor)

try.pfts <- readRDS("data/try.pft.table.rds")
pft.factor <- try.pfts$pft.factor
pft.names <- levels(pft.factor)
pft.names[pft.names == "CAM"] <- "arid_CAM"
npft <- length(pft.names)
