#!/bin/bash

#PBS -N PREPREPARE
#PBS -l mem=64GB

cd ${PBS_O_WORKDIR}
apptainer run image.sif prepare_ml_batchmark.R 0
