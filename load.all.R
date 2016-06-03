load.all.mcmc <- function(nas = TRUE){
    library(R2jags)
    outdir <- "output"
    na.pattern <- ".na."
    if(nas){
        files <- dir(outdir, na.pattern)
    } else {
        files <- dir(outdir)
        files <- files[!grepl(na.pattern, files)]
    }
    out.all <- list()

    for (f in files){
        load(file.path(outdir, f))
        f.name <- gsub(".Rdata", "", f)
        out.all[[f.name]] <- out
        rm(out)
    }
    return(out.all)
}

mcmc.full <- load.all.mcmc(nas=TRUE)
traits <- c("log.LL", "log.LMA", "log.Nmass", "log.Pmass", "log.Rdmass")
save(mcmc.full, traits, file="output.processed.RData")
