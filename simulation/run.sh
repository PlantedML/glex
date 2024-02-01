#!/bin/bash

N_SIM=100
for i in {1..8}; do 
    Rscript run.r $i $NSIM &
done

wait 
echo "all jobs finished"