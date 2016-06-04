## DATA FORMATTING #############################################################

obvs <- DT[,traits,with=FALSE]

trait_means <- as.numeric(DT[, lapply(.SD, function(x) mean(x, na.rm = T)), 
                          .SDcols = traits][, lapply(.SD, nan2na)])
trait_means <- replace.with.global(trait_means, global.trait.means)

pft_means   <- as.matrix(DT[, lapply(.SD, function(x) mean(x, na.rm = T)),
                         by = pft, .SDcols = traits][,traits,with=F])
pft_means <- t(apply(pft_means, 1, replace.with.global, global.trait.means))

n_obvs=dim(obvs)[1] 
n_traits = length(traits)
n_pfts = dim(pft_means)[1]

## JAGS RUN ####################################################################

model = "models/hierarchical.trait.pft.txt"
print(Sys.time())

data <- list(
  pft_obvs = DT[,pft],
  obvs = obvs,
  
  n_pfts = n_pfts,
  n_traits = n_traits,
  n_obvs = n_obvs, 
  mu0 = rep(0,n_traits), 
  Sigma0 = diag(0.001,n_traits),
  Omega = diag(0.001,n_traits))

n.iter <- 30000
inits = function() list(mu_trait = as.numeric(trait_means),
                        mu_pft_trait = as.matrix(pft_means))
variable.names <- c("mu_trait","Sigma_trait", 
                    "mu_pft_trait", "Sigma_pfts")

out <- custom.jags(model = model, data = data, inits = inits,
                   n.chains = n.chains, n.iter = n.iter,
                   variable.names = variable.names)

