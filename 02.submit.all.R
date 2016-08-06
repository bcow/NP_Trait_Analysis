source("00.common.R")

########################################
# Write qsub submission script
########################################

out_fname <- "submit.all.qsub"
n.chains <- 5

bash_header <- "#!/bin/bash"
qsub_args <- c('#$ -l h_rt=48:00:00',
               '#$ -N np_trait',
               '#$ -V',
               '#$ -j y',
               sprintf('#$ -pe omp %d -q "geo*"', n.chains),
               sprintf('#$ -v OMP_NUM_THREADS=%d', n.chains))

qsub_pattern <- paste0("qsub 01.run.model.R %s n.chains=", n.chains)

pft_numbers <- 1:npft
uni_models <- c("uni", sprintf("uni_%02d", pft_numbers))
multi_models <- c("multi", sprintf("multi_%02d", pft_numbers))

uni_string <- sprintf(qsub_pattern, uni_models)
multi_string <- sprintf(qsub_pattern, multi_models)
hier_string <- sprintf(qsub_pattern, "hier")

out_file <- c(bash_header, qsub_args,
              uni_string, multi_string, hier_string)

write(out_file, file = out_fname)

########################################
# Change permissions and run submission script
########################################

system(paste("chmod +x", out_fname))
system(paste0("./", out_fname))
