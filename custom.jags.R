custom.jags <- function(model,data,inits,n.chains,burnin,n.update, variable.names){
  
  print("Compiling JAGS model...")
  j.model   <- jags.model (file = model, data = data, inits = init, n.chains = n.chains)
  
  print("Updating JAGS model (burnin)...")
  n.update <- 20
  for(i in 1:n.update){
    print(sprintf("[%d%%]", i*100/n.update))
    update(j.model, n.iter = round(burnin/n.update))
  }
  print("Sampling JAGS model...")
  j.out   <- coda.samples (model = j.model, n.iter = n.iter,
                           variable.names = variable.names)
  return(j.out)
}
