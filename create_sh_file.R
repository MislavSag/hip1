
# RESULTS -----------------------------------------------------------------
sh_file = "
#!/bin/bash

#PBS -N H1results
#PBS -l ncpus=1
#PBS -l mem=80GB
#PBS -j oe

cd ${PBS_O_WORKDIR}
"

id_seq = seq(1, 1034, 10)
id_seq_sh = paste0(
  "apptainer run image.sif results_test.R ",
  id_seq, " ",
  c(id_seq[2:length(id_seq)] - 1, 1034),
  collapse = "\n"
)

sh_file_f = paste0(sh_file, "\n", id_seq_sh)

sh_file_name = "H1_results.sh"
file.create(sh_file_name)
writeLines(sh_file_f, sh_file_name)



# PREDICTIONS -------------------------------------------------------------
sh_file = "
#!/bin/bash

#PBS -N H1predictions
#PBS -l ncpus=1
#PBS -l mem=80GB
#PBS -j oe

cd ${PBS_O_WORKDIR}
"

id_seq = seq(1, 1034, 10)
id_seq_sh = paste0(
  "apptainer run image.sif extract_predictions.R ",
  id_seq, " ",
  c(id_seq[2:length(id_seq)] - 1, 1034),
  collapse = "\n"
)

sh_file_f = paste0(sh_file, "\n", id_seq_sh)

sh_file_name = "H1_predictions.sh"
file.create(sh_file_name)
writeLines(sh_file_f, sh_file_name)


