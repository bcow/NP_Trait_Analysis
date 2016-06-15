#!/bin/bash

for q in qsub.*.na.sh; do
    qsub $q
done
