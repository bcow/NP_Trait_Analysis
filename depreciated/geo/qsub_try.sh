#!/bin/sh

#$ -l h_rt=24:00:00
#$ -N try.wrkflw
#$ -V
#$ -pe omp 8 -q "geo*"
#$ -v OMP_NUM_THREADS=1

Rscript /usr2/collab/ecowdery/NP_Trait_Analysis/try.workflow.R c1

