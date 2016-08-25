library(mvtraits)

# Draw global plot

message("Loading univariate global...")
uni.all.global <- readRDS("processed_output/sims.uni_global.rds")

message("Loading multivariate global...")
multi.all.global <- readRDS("processed_output/sims.multi_global.rds")

message("Loading hierarchical output...")
hier.all <- readRDS("processed_output/sims.hier.rds")

# Get TRY data
try_data <- loadTRYData("data/try.data.rds")
obs.means.global <- try_data[, lapply(.SD, mean, na.rm=TRUE), 
                           .SDcols = traits] %>% c() %>% unlist()

pairs_path <- "figures/alexey_pairs"
dir.create(pairs_path)
message("Creating global figure...")
mypng(file.path(pairs_path, "00.global.png"))
pairs.density(uni.all.global$mu_trait, 
              multi.all.global$mu_trait, 
              hier.all$mu_trait, 
              obs.means.global, 
              main="Global")
dev.off()

# Draw plots by PFT
uni.all.pft <- readRDS("processed_output/sims.uni_pft.rds")
multi.all.pft <- readRDS("processed_output/sims.multi_pft.rds")
for(i in 1:npft){
    current_pft <- pft.names[i]
    print(paste(i, current_pft))
    uni.mus <- uni.all.pft$mu_trait[[i]]
    multi.mus <- multi.all.pft$mu_trait[[i]]
    hier.mus <- getPFTFromHier(hier.all$mu_pft_trait, current_pft)
    obs.means <- try_data[pft == pft.names[i], 
                          lapply(.SD, mean, na.rm=TRUE),
                          .SDcols = traits] %>% c() %>% unlist()

    mypng(file.path(pairs_path, sprintf("%02d.pft.png", i)))
    pairs.density(uni.mus, multi.mus, hier.mus, 
                  obs.means, 
                  main=paste(i, pft.names[i]))
    dev.off()
}
