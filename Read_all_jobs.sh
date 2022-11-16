#!/bin/bash
#SBATCH --job-name=read_all_jobs
#SBATCH --output=read_all_jobs_output
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --time=00:10:00
#SBATCH --partition=normal
#SBATCH --constraint=gold_6130

cd $HOME/results_p10_old
module load 2022
module load R/4.2.1-foss-2022a
Rscript --no-save --slave read_all_jobs.R