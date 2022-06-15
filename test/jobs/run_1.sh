#!/bin/bash
#SBATCH -J sim_flps1
#SBATCH -o jobs/sim_flps1.o
#SBATCH -e jobs/sim_flps1.e
#SBATCH -N 1
#SBATCH -n 64
#SBATCH -p normal
#SBATCH -t 20:00:00
#SBATCH -A FLPS
#SBATCH --mail-user=sooyongl09@utexas.edu
#SBATCH --mail-type=all

module reset
module load Rstats/3.5.1

ibrun RMPISNOW < job_1.r
