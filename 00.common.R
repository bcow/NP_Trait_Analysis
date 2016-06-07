# Common definitions for scripts

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
