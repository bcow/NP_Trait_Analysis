#!/bin/bash

while read id; do
    qdel $id
done < qstat.txt
