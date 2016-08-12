#!/bin/sh

#$ -l h_rt=24:00:00
#$ -N try.wrkflw
#$ -V
#$ -pe omp 8 -q "geo*"
#$ -v OMP_NUM_THREADS=8

Rscript try.workflow.R