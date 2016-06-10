# Testing Wishart and Gamma distributions
library(R2jags)

gamma.wishart <- "
model{
prec.g ~ dgamma(gamma.shape, gamma.rate)
var.g <- 1 / prec.g

prec.w  ~ dwish(Wishart.rate, Wishart.df)
var.w <- inverse(prec.w)
}"

# Based on the JAGS manual and the Wikipedia pages for "Gamma" and "Wishart" 
# distributions:
#   The gamma distribution in JAGS is parameterized as `dgamma(r, lambda)`,
#   where `r` is the "shape" ("alpha") and `lambda` is the "rate" ("beta").  
#   Note that `rate = 1/scale`.
#
#   The Wishart distribution in JAGS is parameterized as `dwish(R, k)`, 
#   where `R` is the "Rate" or "inverse scale" matrix and `k` is the 
#   degrees of freedom.
#
# Based on the Wikipedia page for "Conjugate prior":
#   Normal-gamma -- Precision was estimated from `2 * shape` observations 
#   with sum of squared deviations `2 * scale` (= `2 / rate`).
#   
#   Multivariate Normal-Wishart -- Precision was estimated from `v` 
#   observations (`df = n + v`) with sum of pairwise deviation products 
#   V^-1 (=R). 

#### initial conditions
n_traits = 5
n = 1
Wishart.rate = diag(n, n_traits)
Wishart.df = n_traits
mean = n * Wishart.df
gamma.shape = mean/2    # Precision was estimated from `mean` observations
gamma.rate = 1/2        # ...with sum of squared deviations `1`.

vars <- c("prec.w", "prec.g", "var.w", "var.g")

### analysis of model and data
data = list(Wishart.rate = Wishart.rate,
            Wishart.df = Wishart.df,
            gamma.shape = gamma.shape,
            gamma.rate = gamma.rate)
jags.out <- jags(data = data, inits = NULL, 
                 parameters.to.save = vars,
                 model.file = textConnection(gamma.wishart),
                 n.chains = 3, n.iter = 10000,
                 DIC = FALSE)
samples <- jags.out$BUGSoutput$sims.list

cols <- c("black", "red", "blue", "green4")
par(mfrow=c(1,2))
plot(density(samples$prec.g), col=cols[1], main="Prior comparison: Precision")
lines(density(samples$prec.w[,1,1]), col=cols[2])
lines(density(samples$prec.w[,2,2]), col=cols[3])
lines(density(samples$prec.w[,3,3]), col=cols[4])
legend("topright", 
       c("Gamma", "Wish[1,1]", "Wish[2,2]", "Wish[3,3]"),
       col=cols, lty=1)
plot(density(samples$var.g), col=cols[1], main="Prior comparison: Variance")
lines(density(samples$var.w[,1,1]), col=cols[2])
lines(density(samples$var.w[,2,2]), col=cols[3])
lines(density(samples$var.w[,3,3]), col=cols[4])
legend("topright", 
       c("Gamma", "Wish[1,1]", "Wish[2,2]", "Wish[3,3]"),
       col=cols, lty=1)

