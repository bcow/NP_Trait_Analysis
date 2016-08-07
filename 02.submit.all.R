source("00.common.R")

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

out_file <- c(bash_header, uni_string, multi_string, hier_string)

write(out_file, file = out_fname)

########################################
# Change permissions and run submission script
########################################

system(paste("chmod +x", out_fname))
system(paste0("./", out_fname))
