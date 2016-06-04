## DATA FORMATTING #############################################################

obvs <- DT[,traits,with=FALSE]
trait_means <- DT[, lapply(.SD, function(x) mean(x, na.rm = T)), 
                  .SDcols = traits]

n_obvs=dim(obvs)[1] 
n_traits = length(traits)

## JAGS RUN ####################################################################

model = "models/multivariate.trait.txt"
print(Sys.time())

data <- list(
  obvs = obvs,
  n_traits = n_traits,
  n_obvs = n_obvs,
  mu0 = rep(0,n_traits), 
  Sigma0 = diag(.001,n_traits))

n.iter <- 15000
inits = function() list(mu_trait = as.numeric(trait_means))
variable.names <- c("mu_trait", "Sigma_trait")

out <- custom.jags(model = model, data = data, inits = inits,
                   n.chains = n.chains, n.iter = n.iter,
                   variable.names = variable.names)
