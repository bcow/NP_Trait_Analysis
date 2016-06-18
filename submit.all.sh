#!/bin/bash

for i in $(seq 0 35); do
    qsub qsub.uni.na.sh $i
done

for i in $(seq 0 35); do
    qsub qsub.multi.na.sh $i
done

qsub qsub.hier.na.sh


