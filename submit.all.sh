#!/bin/bash

for q in qsub.*.sh; do
    qsub $q
done
