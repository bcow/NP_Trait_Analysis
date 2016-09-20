library(mvtraits)

########################################
# Write qsub submission script
########################################

out_fname <- "submit.all.sh"
n.chains <- 5

bash_header <- "#!/bin/bash"

qsub_pattern <- "qsub -N %1$s -pe omp %2$d -v OMP_NUM_THREADS=%2$d run.rscript.sh 01.run.model.R %1$s n.chains=%2$d"

pft_numbers <- 1:npft
uni_models <- c("uni", sprintf("uni_%02d", pft_numbers))
multi_models <- c("multi", sprintf("multi_%02d", pft_numbers))

uni_string <- sprintf(qsub_pattern, uni_models, n.chains)
multi_string <- sprintf(qsub_pattern, multi_models, n.chains)
hier_string <- sprintf(qsub_pattern, "hier", n.chains)

args <- commandArgs(trailingOnly = TRUE)
if(length(args) == 0) args <- c("uni", "multi", "hier")

out_file <- bash_header
if("uni" %in% args) out_file <- c(out_file, uni_string)
if("multi" %in% args) out_file <- c(out_file, multi_string)
if("hier" %in% args) out_file <- c(out_file, hier_string)

write(out_file, file = out_fname)

########################################
# Change permissions and run submission script
########################################

system(paste("chmod +x", out_fname))
system(paste0("./", out_fname))
file.remove(out_fname)
