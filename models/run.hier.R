## DATA FORMATTING #############################################################

obvs <- DT[,traits,with=FALSE]

trait_means <- DT[, lapply(.SD, function(x) mean(x, na.rm = T)), 
                  .SDcols = traits]
pft_means   <- DT[, lapply(.SD, function(x) mean(x, na.rm = T)),
                  by = pft, .SDcols = traits]

n_obvs=dim(obvs)[1] 
n_traits = length(traits)
n_pfts = dim(pft_means)[1]

## JAGS RUN ####################################################################

model = "models/hierarchical.trait.pft.txt"
print(Sys.time())

data <- list(
  pft_obvs = as.numeric(droplevels(DT[,pft])),
  obvs = obvs,
  
  n_pfts = n_pfts,
  n_traits = n_traits,
  n_obvs = n_obvs, 
  mu0 = rep(0,n_traits), 
  Sigma0 = diag(.001,n_traits),
  Omega = diag(0.001,n_traits))

trait_means_vec <- as.numeric(trait_means)
pft_means_mat <- as.matrix(pft_means[,traits,with=F])

n.iter <- 30000
inits = function() list(mu_trait = as.numeric(trait_means_vec),
                        mu_pft_trait = as.matrix(pft_means_mat))
variable.names <- c("mu_trait","Sigma_trait", 
                    "mu_pft_trait", "Sigma_pfts")

out <- custom.jags(model = model, data = data, inits = inits,
                   n.chains = n.chains, n.iter = n.iter,
                   variable.names = variable.names)

