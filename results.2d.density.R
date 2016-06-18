source("00.common.R")
source("functions.2ddensplot.R")
library(magrittr)
library(MASS)

mypng <- function(path){
    png(path, width=800, height = 800)
}

pairs.density <- function(uni.mus, multi.mus, hier.mus, 
                          obs.means, obs.global=NA, nsamp=5000, ...){

    uni <- 1:nsamp
    multi <- uni + nsamp
    hier <- multi + nsamp
    breaks.dens <- list(uni, multi, hier)

    obs_mean <- nsamp*3 + 1
    global_mean <- obs_mean + 1
    breaks.line <- list(obs_mean, global_mean)

    breaks <- list("density" = breaks.dens, "line" = breaks.line)

    dens.pars <- list(uni = list(col = "blue", lwd=2),
                      multi = list(col = "green4", lwd=2),
                      hier = list(col = "red", lwd=2))

    line.pars <- list(obs_means = list(lty = "dashed",
                                       col = "black", 
                                       lwd = 2),
                      global_means = list(lty = "dotted", 
                                          col="hotpink4", 
                                          lwd = 2))

    mus.list <- list(uni.mus[sample.int(nsamp),],
                     multi.mus[sample.int(nsamp),],
                     hier.mus[sample.int(nsamp),],
                     obs.means, 
                     obs.global)
    plot.mus <- do.call(rbind, mus.list)

    pairs(plot.mus, panel = dens2d.panel, densfunc = ellipse2d,
          breaks = breaks,
          dens.pars = dens.pars, line.pars = line.pars, ...)
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
pairs.density(uni.mus, multi.mus, hier.mus.global, obs.means.global, main="Global")
dev.off()

# Draw plots by PFT
for(i in 1:npft){
    print(paste(i, pft.names[i]))
    hier.mus <- hier.mus.pft[,i,]
    multi.mus <- load.mu(sprintf("output/multi.trait.na/multi.trait.na.pft.%02d.Rdata", i))
    uni.mus <- load.mu(sprintf("output/uni.trait.na/uni.trait.na.pft.%02d.Rdata", i))
    obs.means <- try.na[pft == pft.names[i], 
                        lapply(.SD, mean, na.rm=TRUE),
                        .SDcols = traits] %>% c() %>% unlist()

    mypng(sprintf("figures/alexey_pairs/%02d.pft.png", i))
    pairs.density(uni.mus, multi.mus, hier.mus, 
                  obs.means, 
                  main=paste(i, pft.names[i]))
    dev.off()
}
