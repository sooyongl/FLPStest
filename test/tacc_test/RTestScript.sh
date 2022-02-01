#!/bin/bash
#SBATCH -t 00:05:00
#SBATCH -N 1
#SBATCH --ntasks-per-node  1
#SBATCH -p development
#SBATCH -A <allocation_code>

module load intel/17.0.4
module load impi/17.0.3
module load Rstats
module load RstatsPackages

R CMD BATCH --quiet ~/sample_script_to_batch.R output.txt