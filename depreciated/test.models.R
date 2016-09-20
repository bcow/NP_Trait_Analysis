library(R2jags)
library(data.table)
library(magrittr)

source("00.common.R")
source("load.try.data.R")
#source("custom.jags.R")
source("custom.jags2.R")

source("models/run.uni.R")
source("models/run.multi.R")
source("models/run.hier.R")

# Gamma and Wishart prior
n_traits = 5
n = 80
Wishart.rate = diag(n, n_traits)
Wishart.df = n_traits
mean = n * Wishart.df
gamma.shape = Wishart.df/2 
gamma.rate = n/2   

# Calculate global trait means
global.trait.means <- try.na[, lapply(.SD, mean, na.rm=TRUE),
                             .SDcols = traits] %>%
                        as.numeric %>%
                        "names<-"(traits)

# Parse na argument
with.na <- TRUE
na <- ifelse(with.na, ".na","") 
DT.run <- copy(get(paste0("try", na)))
DT.run[,pft:=droplevels(pft)][, pft := as.numeric(pft)]
pfts <- unique(DT.run[,pft])

# Test multivariate model
pft.test <- 24
pft.24.out <- run.multi(DT.run[pft == 24], n.chains = 3)
