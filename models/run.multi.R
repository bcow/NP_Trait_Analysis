run.multi <- function(DT, n.chains=3){
    library(data.table)
    library(magrittr)

    ## DATA FORMATTING #########################################

    obvs <- DT[,traits,with=FALSE] %>% as.matrix
    obvs <- as.matrix(obvs)
    n_obvs=dim(obvs)[1] 
    n_traits = length(traits)

    # Missing values
    miss <- which(is.na(obvs), arr.ind = TRUE)
    pres <- which(!is.na(obvs), arr.ind = TRUE)
    n_miss <- nrow(miss)
    n_pres <- nrow(pres)

    ## JAGS RUN ################################################

    model = "models/multi.bug"
    print(Sys.time())

    data <- list(obvs = obvs,
                 n_traits = n_traits,
                 n_obvs = n_obvs,
                 miss = miss,
                 pres = pres,
                 n_miss = n_miss,
                 n_pres = n_pres,
                 mu0 = rep(0,n_traits), 
                 Sigma0 = diag(0.001,n_traits),
                 Wishart.rate = Wishart.rate,
                 Wishart.df = Wishart.df,
                 tau_obvs_miss = 0.1,
                 tau_obvs_pres = 1000)

    trait_means <- as.numeric(DT[, lapply(.SD, function(x) mean(x, na.rm = T)), 
                              .SDcols = traits][, lapply(.SD, nan2na)])
    trait_means <- replace.with.global(trait_means, global.trait.means)
    inits = function() list(mu_trait = as.numeric(trait_means))

    variable.names <- c("mu_trait", "Sigma_trait")

    out <- custom.jags(model = model, data = data, inits = inits,
                       n.chains = n.chains, 
                       variable.names = variable.names)
    return(out)
}
