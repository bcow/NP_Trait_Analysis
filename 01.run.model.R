## LOAD DATA AND PACKAGES #############################################################

library(mvtnorm)
library(data.table)
# library(shinystan)

source("00.common.R")
source("load.try.data.R")
source("custom.jags.R")

source("models/run.uni.R")
source("models/run.multi.R")
source("models/run.hier.R")

# Defining Gamma & Wishart parameters here so we can experiment
# See test.wishart.R for more.
# dgamma(gamma.shape,gamma.rate)
# dwish(Wishart.rate,Wishart.df)

n_traits = 5
n = 1
Wishart.rate = diag(n, n_traits)
Wishart.df = n_traits
mean = n * Wishart.df
gamma.shape = Wishart.df/2 
gamma.rate = n/2   

out.dir <- paste0("output.n",n)
if(!dir.exists(out.dir)) dir.create(out.dir)


if(!exists("cmdargs")) cmdargs <- commandArgs(trailingOnly=TRUE)
# Possible arguments: 
#   uni[_<pft>] -- Run univariate model for specified PFT number (no number means run all PFTs)
#   multi[_<pft>] -- Run multivariate model for specified PFT number
#   hier -- Run hierarchical model (global and PFT means)
#   no_na -- Use only complete rows (i.e. apply `na.omit` filter to data)
#   n.chains=n -- Run with `n` chains, where `n` is an integer.
if(length(cmdargs) == 0){
  cmdargs <- c("uni", "uni_01", "multi", "multi_01", "hier", "n.chains=3")   # For testing
}
message("Running with the following arguments:")
print(cmdargs)

# Get number of chains from arguments
default.chains <- 3
n.chains.index <- grep("n.chains", cmdargs)
n.chains <- ifelse(length(n.chains.index > 0),
                   as.numeric(gsub("n.chains=", "", cmdargs[n.chains.index])),
                   default.chains)

# Determine whether or not to exclude rows with na's from input data
if("no_na" %in% cmdargs){
    DT.run <- na.omit(try.data)
    na <- "no_na"
} else {
    DT.run <- try.data
    na <- ""
}

DT.run[,pft := as.numeric(pft)]
pfts <- unique(DT.run[,pft])

# Calculate global trait means for initial conditions
global.trait.means <- as.numeric(try.data[, lapply(.SD, mean, na.rm=TRUE),
                                 .SDcols = traits])
names(global.trait.means) <- traits

# Determine which models to run from arguments
model.args <- cmdargs[grep("uni|multi|hier", cmdargs)]
model.args.list <- strsplit(model.args, "_")

# Run models
errors <- character()
for (arg in model.args.list){
    model_desc <- sprintf("model %s, pft %s", arg[1], arg[2])
    message(paste("Running", model_desc))
    model_type <- arg[1]
    model <- switch(model_type,
                    uni = run.uni,
                    multi = run.multi, 
                    hier = run.hier)
    pft_number <- as.numeric(arg[2])
    if (!is.na(pft_number)){
        DT <- DT.run[pft == pft_number]
    } else {
        DT <- DT.run
    }
    out <- model(DT)
    if(!all(is.error(out))){
        save(out, file = sprintf("%s/%s.trait%s.Rdata", out.dir, model_type, na))
    } else {
        warning(paste("Error running", model_desc))
        errors <- c(errors, arg)
    }
}

print("=================================")
print("PRINT WARNINGS")

warnings()
