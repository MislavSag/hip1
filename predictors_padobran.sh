#!/bin/bash

#PBS -N spymlpreds
#PBS -l ncpus=4
#PBS -l mem=16GB
#PBS -J 1-1000
#PBS -o logs
#PBS -j oe

cd ${PBS_O_WORKDIR}

apptainer run image.sif predictors_padobran.R

# for i in $(seq 0 500 26200); do
#     apptainer run image.sif predictors_daily_supek.R $i
# done
