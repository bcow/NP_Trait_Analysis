# Functions to retrieve outputs from RData files
get_sims <- function(filename, vars){
    library(runjags)
    library(dplyr)
    load(filename)
    matchvars <- paste(vars, collapse="|")
    out_sims <- out$mcmc %>% as.matrix() %>% .[, grep(matchvars, colnames(.))]
    remove(out)
    return(out_sims)
}

get_sims_list <- function(filenames, vars){
    l <- list()
    for(f in seq_along(filenames)){
        l[[f]] <- get_sims(filenames[f], vars)
    }
    return(l)
}

# Read in all analysis outputs and store
output_dir <- "output.n1"

vars.uni <- c("mu_trait", "sigma2_obvs")    # Need to change to sigma2_trait for future runs
vars.multi <- c("mu_trait", "Sigma_trait")
vars.hier <- c("mu_trait", "Sigma_trait", "mu_pft_trait", "Sigma_pft")

files.uni_global <- file.path(output_dir, "uni.Rdata")
files.uni_pft <- list.files(output_dir, "uni_[[:digit:]]+", full.names=TRUE)
files.multi_global <- file.path(output_dir, "multi.Rdata")
files.multi_pft <- list.files(output_dir, "multi_[[:digit:]]+", full.names=TRUE)
files.hier <- file.path(output_dir, "hier.Rdata")

storage_dir <- "processed_output"
dir.create(storage_dir)
sims.uni_global <- get_sims(files.uni_global, vars.uni)
saveRDS(sims.uni_global, file = file.path(storage_dir, "sims.uni_global.rds"))
sims.uni_pft <- get_sims_list(files.uni_pft, vars.uni)
saveRDS(sims.uni_pft, file = file.path(storage_dir, "sims.uni_pft.rds"))
sims.multi_global <- get_sims(files.multi_global, vars.multi)
saveRDS(sims.multi_global, file = file.path(storage_dir, "sims.multi_global.rds"))
sims.multi_pft <- get_sims_list(files.multi_pft, vars.multi)
saveRDS(sims.multi_pft, file = file.path(storage_dir, "sims.multi_pft.rds"))
sims.hier <- get_sims(files.hier, vars.hier)
saveRDS(sims.hier, file = file.path(storage_dir, "sims.hier.rds"))


