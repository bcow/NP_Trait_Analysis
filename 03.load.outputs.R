library(mvtraits)

# Read in all analysis outputs and store
output_dir <- "output.n1"

vars.uni <- c("mu_trait", "sigma2_obvs")    # Need to change to sigma2_trait for future runs
vars.multi <- c("mu_trait", "Sigma_trait")
vars.hier <- c("mu_trait", "Sigma_trait", "mu_pft_trait", "Sigma_pft")

files.uni_global <- list.files(output_dir, "uni.Rdata", full.names=TRUE)
files.uni_pft <- list.files(output_dir, "uni_[[:digit:]]+", full.names=TRUE)
files.multi_global <- list.files(output_dir, "multi.Rdata", full.names=TRUE)
files.multi_pft <- list.files(output_dir, "multi_[[:digit:]]+", full.names=TRUE)
files.hier <- list.files(output_dir, "hier.Rdata", full.names=TRUE)

storage_dir <- "processed_output"
dir.create(storage_dir)

if (length(files.uni_global) > 0) {
    message("Loading univariate global results...")
    sims.uni_global <- lapply(vars.uni, function(x) get_sims(files.uni_global, x))
    saveRDS(sims.uni_global, file = file.path(storage_dir, "sims.uni_global.rds"))
} else {
    warning("No univariate global results found")
}

if (length(files.uni_pft) > 0) {
    message("Loading univariate PFT results...")
    sims.uni_pft <- lapply(vars.uni, function(x) get_sims_list(files.uni_pft, x))
    saveRDS(sims.uni_pft, file = file.path(storage_dir, "sims.uni_pft.rds"))
} else {
    warning("No univariate PFT results found")
}

if (length(files.multi_global) > 0) {
    message("Loading multivariate global results...")
    sims.multi_global <- lapply(vars.multi, function(x) get_sims(files.multi_global, x))
    saveRDS(sims.multi_global, file = file.path(storage_dir, "sims.multi_global.rds"))
} else {
    warning("No multivariate global results found")
}

if (length(files.multi_pft) > 0) {
    message("Loading multivariate PFT results...")
    sims.multi_pft <- lapply(vars.multi, function(x) get_sims_list(files.multi_pft, x))
    saveRDS(sims.multi_pft, file = file.path(storage_dir, "sims.multi_pft.rds"))
} else {
    warning("No multivariate PFT results found")
}

if (length(files.hier) > 0) {
    message("Loading hierarchical results...")
    sims.hier <- lapply(vars.hier, function(x) get_sims(files.hier, x))
    saveRDS(sims.hier, file = file.path(storage_dir, "sims.hier.rds"))
} else {
    warning("No hierarchical results found")
}


