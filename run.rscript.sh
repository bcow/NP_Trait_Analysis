#!/bin/bash
#$ -l h_rt=120:00:00
#$ -V
#$ -j y
#$ -o logs_2016_09_15/
#$ -q "geo*"
Rscript $@
