
#!/bin/bash

#PBS -N H1results
#PBS -l ncpus=1
#PBS -l mem=50GB
#PBS -j oe

cd ${PBS_O_WORKDIR}

apptainer run image.sif results_test.R 1 25
apptainer run image.sif results_test.R 26 50
apptainer run image.sif results_test.R 51 75
apptainer run image.sif results_test.R 76 100
apptainer run image.sif results_test.R 101 125
apptainer run image.sif results_test.R 126 150
apptainer run image.sif results_test.R 151 175
apptainer run image.sif results_test.R 176 200
apptainer run image.sif results_test.R 201 225
apptainer run image.sif results_test.R 226 250
apptainer run image.sif results_test.R 251 275
apptainer run image.sif results_test.R 276 300
apptainer run image.sif results_test.R 301 325
apptainer run image.sif results_test.R 326 350
apptainer run image.sif results_test.R 351 375
apptainer run image.sif results_test.R 376 400
apptainer run image.sif results_test.R 401 425
apptainer run image.sif results_test.R 426 450
apptainer run image.sif results_test.R 451 475
apptainer run image.sif results_test.R 476 500
apptainer run image.sif results_test.R 501 525
apptainer run image.sif results_test.R 526 550
apptainer run image.sif results_test.R 551 575
apptainer run image.sif results_test.R 576 600
apptainer run image.sif results_test.R 601 625
apptainer run image.sif results_test.R 626 650
apptainer run image.sif results_test.R 651 675
apptainer run image.sif results_test.R 676 700
apptainer run image.sif results_test.R 701 725
apptainer run image.sif results_test.R 726 750
apptainer run image.sif results_test.R 751 775
apptainer run image.sif results_test.R 776 800
apptainer run image.sif results_test.R 801 825
apptainer run image.sif results_test.R 826 850
apptainer run image.sif results_test.R 851 875
apptainer run image.sif results_test.R 876 900
apptainer run image.sif results_test.R 901 925
apptainer run image.sif results_test.R 926 950
apptainer run image.sif results_test.R 951 975
apptainer run image.sif results_test.R 976 1000
apptainer run image.sif results_test.R 1001 1025
apptainer run image.sif results_test.R 1026 1034
