#!/bin/sh

#$ -l h_rt=48:00:00
#$ -N try.run.multi.na
#$ -V
#$ -j y
#$ -pe omp 8 -q "geo*"
#$ -v OMP_NUM_THREADS=8

Rscript 01.run.models.R uni=NULL multi=$1  hier=NULL with.na=TRUE n.chains=8
