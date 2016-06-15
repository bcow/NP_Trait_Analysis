## DATA FORMATTING #############################################################

obvs <- DT[,traits,with=FALSE]
trait_means <- as.numeric(DT[, lapply(.SD, function(x) mean(x, na.rm = T)), 
                          .SDcols = traits])
trait_means <- replace.with.global(trait_means, global.trait.means)

n_obvs=dim(obvs)[1] 
n_traits = length(traits)

## JAGS RUN ####################################################################

model = "models/univarite.trait.txt"
print(Sys.time())

data <- list(
  obvs = obvs,
  n_traits = n_traits,
  n_obvs = n_obvs,
  gamma.shape = gamma.shape,
  gamma.rate = gamma.rate       # scale/rate = n
  )

n.iter <- 5000
inits = function() list(mu_trait = as.numeric(trait_means))
variable.names <- c("mu_trait", "tau_obvs", "sigma2_obvs")
out <- try(custom.jags(model = model, data = data, inits = inits,
                       n.chains = n.chains, n.iter = n.iter,
                       variable.names = variable.names))

