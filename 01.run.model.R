## LOAD DATA AND PACKAGES #############################################################
library(mvtraits)

try_data <- loadTRYData("data/try.data.rds")
nowdate <- strftime(Sys.time(), "%Y_%m_%d")
out_dir <- "output"
dir.create(out_dir, showWarnings = FALSE)

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
    try_data <- na.omit(try_data)
    na <- "no_na"
} else {
    na <- ""
}

# Determine which models to run from arguments
model.args <- cmdargs[grep("uni|multi|hier", cmdargs)]
model.args.list <- strsplit(model.args, "_")

# Run models
errors <- character()
for (model_arg in model.args){
    arg <- strsplit(model_arg, "_")[[1]]
    message(paste("Running", model_arg))
    model_type <- arg[1]
    pft_number <- as.numeric(arg[2])
    out <- runModel(model_type, try_data, pft_number, n.chains, iter = 10000, 
                    save_each = TRUE, refresh = 10)
    if(!all(is.error(out))){
        save(out, file = sprintf("%s/%s%s.%s.Rdata", out_dir, model_arg, na, nowdate))
    } else {
        warning(paste("Error running", model_arg))
        errors <- c(errors, arg)
    }
}

print("=================================")
print("PRINT WARNINGS")

warnings()
