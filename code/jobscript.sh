#!/bin/sh

#SBATCH --job-name=batch_bb_np
#SBATCH --time=3:30:00
#SBATCH --mail-user=lauren.kennedy1@monash.edu
#SBATCH --mail-type=BEGIN,END,FAIL
#SBATCH --mem-per-cpu=4000
#SBATCH --cpus-per-task=1
#SBATCH --array=201-500

module load R/4.0.0-openblas

R CMD BATCH --no-save --no-restore Bias_Var_Comparison.R script_$SLURM_ARRAY_TASK_ID