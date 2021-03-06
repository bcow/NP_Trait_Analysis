library(mvtraits)

storage_dir <- "processed_output"
dir.create(storage_dir, showWarnings = FALSE)
output_dir <- "output_selected"

# Isolate samples of the mean
vars.uni <- "mu" 
vars.multi <- "mu"
vars.hier <- c("mu_pft", "mu_global")

files.uni_global <- list.files(output_dir, "uni.Rdata", full.names=TRUE)
files.uni_pft <- list.files(output_dir, "uni_[[:digit:]]+", full.names=TRUE)
files.multi_global <- list.files(output_dir, "multi.Rdata", full.names=TRUE)
files.multi_pft <- list.files(output_dir, "multi_[[:digit:]]+", full.names=TRUE)
files.hier <- list.files(output_dir, "hier.Rdata", full.names=TRUE)


if (length(files.uni_global) > 0) {
    message("Loading univariate global results...")
    sims.uni_global <- lapply(vars.uni, function(x) get_sims(files.uni_global, x))
    names(sims.uni_global) <- vars.uni
    saveRDS(sims.uni_global, file = file.path(storage_dir, "sims.uni_global.rds"))
} else {
    warning("No univariate global results found")
}

if (length(files.uni_pft) > 0) {
    message("Loading univariate PFT results...")
    sims.uni_pft <- lapply(vars.uni, function(x) get_sims_list(files.uni_pft, x))
    names(sims.uni_pft) <- vars.uni
    saveRDS(sims.uni_pft, file = file.path(storage_dir, "sims.uni_pft.rds"))
} else {
    warning("No univariate PFT results found")
}

if (length(files.multi_global) > 0) {
    message("Loading multivariate global results...")
    sims.multi_global <- lapply(vars.multi, function(x) get_sims(files.multi_global, x))
    names(sims.multi_global) <- vars.multi
    saveRDS(sims.multi_global, file = file.path(storage_dir, "sims.multi_global.rds"))
} else {
    warning("No multivariate global results found")
}

if (length(files.multi_pft) > 0) {
    message("Loading multivariate PFT results...")
    sims.multi_pft <- lapply(vars.multi, function(x) get_sims_list(files.multi_pft, x))
    names(sims.multi_pft) <- vars.multi
    saveRDS(sims.multi_pft, file = file.path(storage_dir, "sims.multi_pft.rds"))
} else {
    warning("No multivariate PFT results found")
}

if (length(files.hier) > 0) {
    message("Loading hierarchical results...")
    sims.hier <- lapply(vars.hier, function(x) get_sims(files.hier, x))
    names(sims.hier) <- vars.hier
    saveRDS(sims.hier, file = file.path(storage_dir, "sims.hier.rds"))
} else {
    warning("No hierarchical results found")
}


