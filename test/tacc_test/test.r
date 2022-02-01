# The parallel version (for Stampede2)
library(foreach)
library(doParallel)
library(Rmpi)

cl <- getMPIcluster()
registerDoParallel(cl)

n_tasks <- 100
results <-
  foreach (task = 1:n_tasks, .combine = rbind) %dopar% {
    set.seed(task)
    tmp <- runif(1, 1, 2)
    Sys.sleep(tmp)

    c(task, tmp)
  }

# #!/bin/bash
# #SBATCH -J sim
# #SBATCH -o sim.o
# #SBATCH -e sim.e
# #SBATCH -N 4
# #SBATCH -n 32
# #SBATCH -p development
# #SBATCH -t 0:10:00
# #SBATCH -A test-proj
# #SBATCH --mail-user=sooyongl09@utexas.edu
# #SBATCH --mail-type=all
#
# # load R module
# module reset
# module load Rstats/3.5.1
# # call R code from RMPISNOW
# ibrun RMPISNOW < test.r
