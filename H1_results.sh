
#!/bin/bash

#PBS -N H1results
#PBS -l ncpus=1
#PBS -l mem=50GB
#PBS -j oe

cd ${PBS_O_WORKDIR}

apptainer run image.sif H2_results.R 1 25
apptainer run image.sif H2_results.R 26 50
apptainer run image.sif H2_results.R 51 75
apptainer run image.sif H2_results.R 76 100
apptainer run image.sif H2_results.R 101 125
apptainer run image.sif H2_results.R 126 150
apptainer run image.sif H2_results.R 151 175
apptainer run image.sif H2_results.R 176 200
apptainer run image.sif H2_results.R 201 225
apptainer run image.sif H2_results.R 226 250
apptainer run image.sif H2_results.R 251 275
apptainer run image.sif H2_results.R 276 300
apptainer run image.sif H2_results.R 301 325
apptainer run image.sif H2_results.R 326 350
apptainer run image.sif H2_results.R 351 375
apptainer run image.sif H2_results.R 376 400
apptainer run image.sif H2_results.R 401 425
apptainer run image.sif H2_results.R 426 450
apptainer run image.sif H2_results.R 451 475
apptainer run image.sif H2_results.R 476 500
apptainer run image.sif H2_results.R 501 525
apptainer run image.sif H2_results.R 526 550
apptainer run image.sif H2_results.R 551 575
apptainer run image.sif H2_results.R 576 600
apptainer run image.sif H2_results.R 601 625
apptainer run image.sif H2_results.R 626 650
apptainer run image.sif H2_results.R 651 675
apptainer run image.sif H2_results.R 676 700
apptainer run image.sif H2_results.R 701 725
apptainer run image.sif H2_results.R 726 750
apptainer run image.sif H2_results.R 751 775
apptainer run image.sif H2_results.R 776 800
apptainer run image.sif H2_results.R 801 825
apptainer run image.sif H2_results.R 826 850
apptainer run image.sif H2_results.R 851 875
apptainer run image.sif H2_results.R 876 900
apptainer run image.sif H2_results.R 901 925
apptainer run image.sif H2_results.R 926 950
apptainer run image.sif H2_results.R 951 975
apptainer run image.sif H2_results.R 976 1000
apptainer run image.sif H2_results.R 1001 1025
apptainer run image.sif H2_results.R 1026 1034
