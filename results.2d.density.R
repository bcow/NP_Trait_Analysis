source("00.common.R")
library(magrittr)
library(MASS)

mypng <- function(path){
    png(path, width=800, height = 800)
}

pairs.density <- function(uni.mus, multi.mus, hier.mus, obs.means, obs.global, nsamp=3000, ...){

    dens2d <- function(x,y,...) {
        z <- kde2d(x, y)
        contour(z, nlevels = 2, add = TRUE, drawlabels = FALSE, ...)
    }

    dens2d.panel <- function(x, y){
        dens2d(x[n1], y[n1], col=mod.colors[1])
        dens2d(x[n2], y[n2], col=mod.colors[2])
        dens2d(x[n3], y[n3], col=mod.colors[3])
        if(!is.nan(x[nlast])) abline(v = x[nlast])
        if(!is.nan(x[nlast])) abline(h = y[nlast])
        abline(v = x[nlast2], lty="dashed", col="red")
        abline(h = y[nlast2], lty="dashed", col="red")
    }

    n1 <- 1:nsamp
    n2 <- n1 + 3000
    n3 <- n2 + 3000
    nlast <- nsamp*3 + 1
    nlast2 <- nlast + 1
    mod.colors <- c("uni" = "blue",
                    "multi" = "green",
                    "hier" = "red")
    mus.list <- list(uni.mus[sample.int(nsamp),],
                     multi.mus[sample.int(nsamp),],
                     hier.mus[sample.int(nsamp),],
                     obs.means, 
                     obs.global)
    plot.mus <- do.call(rbind, mus.list)

    pairs(plot.mus, lower.panel = dens2d.panel, upper.panel=NULL, ...)
}

# Load outputs global outputs
load.into.var <- function(path){
    load(path)
    result <- out
    rm(out)
    return(result)
}

load.mu <- function(path){
    all.out <- load.into.var(path)
    mus <- all.out$BUGSoutput$sims.list$mu_trait
    colnames(mus) <- traits
    return(mus)
}

# Draw global plot

message("Loading hierarchical output...")
hier.all <- load.into.var("output/hier.trait.pft.na/hier.trait.pft.na.Rdata")
hier.mus.global <- hier.all$BUGSoutput$sims.list$mu_trait
hier.mus.pft <- hier.all$BUGSoutput$sims.list$mu_pft_trait

message("Loading multivariate global...")
multi.mus <- load.mu("output/multi.trait.na/multi.trait.na.Rdata")

message("Loading univariate global...")
uni.mus <- load.mu("output/uni.trait.na/uni.trait.na.Rdata")

# Get TRY data
source("load.try.data.R")
obs.means.global <- try.na[, lapply(.SD, mean, na.rm=TRUE), 
                           .SDcols = traits] %>% c() %>% unlist()

message("Creating global figure...")
mypng("figures/alexey_pairs/00.global.png")
pairs.density(uni.mus, multi.mus, hier.mus.global, obs.means.global, obs.means.global, main="Global")
dev.off()

# Draw plots by PFT
for(i in 1:npft){
    print(c(i, pft.names[i]))
    hier.mus <- hier.mus.pft[,i,]
    multi.mus <- load.mu(sprintf("output/multi.trait.na/multi.trait.na.pft.%02d.Rdata", i))
    uni.mus <- load.mu(sprintf("output/uni.trait.na/uni.trait.na.pft.%02d.Rdata", i))
    obs.means <- try.na[pft == pft.names[i], 
                        lapply(.SD, mean, na.rm=TRUE),
                        .SDcols = traits] %>% c() %>% unlist()
    print(obs.means)

    mypng(sprintf("figures/alexey_pairs/%02d.pft.png", i))
    pairs.density(uni.mus, multi.mus, hier.mus, obs.means, obs.means.global,
                  main=paste(i, pft.names[i]))
    dev.off()
}
