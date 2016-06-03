custom.jags <- function(model, data, inits, n.chains, n.iter, variable.names){
    library(R2jags)
    message("Initial attempt...")
    exports <- ls(envir = .GlobalEnv)
    j.begin <- jags.parallel(data = data, inits = inits,
                           parameters.to.save = variable.names,
                           model.file = model, n.chains = n.chains,
                           n.iter = n.iter, export_obj_names = exports)

    rhat <- j.begin$BUGSoutput$summary[,"Rhat"]
    if(all(rhat < 1.1)){
        message("Model converged on first try!")
        j.out <- j.begin
    } else {
        message("Model did not converge yet. Resuming sampling until convergence")
        recompile(j.begin)
        j.out <- autojags(j.begin)
    }
    return(j.out)
}

