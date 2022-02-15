#!/bin/bash
#SBATCH -J sim
#SBATCH -o sim.o
#SBATCH -e sim.e
#SBATCH -N 4
#SBATCH -n 32
#SBATCH -p development
#SBATCH -t 0:30:00
#SBATCH -A test-proj
#SBATCH --mail-user=sooyongl09@utexas.edu
#SBATCH --mail-type=all

module reset
module load Rstats

ibrun RMPISNOW < example6.R
