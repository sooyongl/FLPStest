# make jobs files ---------------------------------------------------------
mk_rjobs <- function(joblist, model) {
  for(i in 1:length(jobslist)) {
    jn <- stringr::str_split(jobslist[i], "_", simplify = T)[2]
    jn <- stringr::str_split(jn, ".r", simplify = T)[1]
    jn <- as.numeric(jn) + 10
    jobssource <- readLines(jobslist[i])
    jobssource[38] <-
      glue::glue("cond_table <- cond_table[cond_table$lvmodel == \"{model}\",  ]")
    writeLines(jobssource, glue::glue("test/jobs/job_{jn}.r"))
  }
}

jobslist <- fs::dir_ls("test/jobs", regexp = "(_[1-8])\\.r$")
mk_rjobs(joblist, "2pl")

jobslist <- fs::dir_ls("test/jobs", regexp = "(_1[1-8])\\.r$")
mk_rjobs(joblist, "grm")

jobslist <- fs::dir_ls("test/jobs", regexp = "(_2[1-8])\\.r$")
mk_rjobs(joblist, "gpcm")


# make sh files -----------------------------------------------------------
mk_sk <- function(r, time) {

  a1 <-glue::glue(
"#!/bin/bash
#SBATCH -J sim_flps{r}
#SBATCH -o jobs/sim_flps{r}.o
#SBATCH -e jobs/sim_flps{r}.e
#SBATCH -N 1
#SBATCH -n 64
#SBATCH -p normal
#SBATCH -t {time}
#SBATCH -A FLPS
#SBATCH --mail-user=sooyongl09@utexas.edu
#SBATCH --mail-type=all

module reset
module load Rstats/3.5.1

ibrun RMPISNOW < job_{r}.r")
  writeLines(a1, glue::glue("test/jobs/run_{r}.sh"))
}
mk_sk("9999",  "20:30:00")
mk_sk("99999", "20:30:00")
# Rasch
mk_sk("1",  "20:00:00")
mk_sk("2",  "20:50:00")
mk_sk("3",  "20:50:00")
mk_sk("4",  "20:00:00")
mk_sk("5",  "20:00:00")
mk_sk("6",  "20:40:00")
mk_sk("7",  "20:00:00")
mk_sk("8",  "20:00:00")

# 2PL
mk_sk("11",  "20:40:00")
mk_sk("12",  "20:00:00")
mk_sk("13",  "20:00:00")
mk_sk("14",  "20:30:00")
mk_sk("15",  "20:30:00")
mk_sk("16",  "20:30:00")
mk_sk("17",  "20:30:00")
mk_sk("18",  "20:30:00")

# GRM
mk_sk("21",  "20:00:00")
mk_sk("22",  "20:00:00")
mk_sk("23",  "20:00:00")
mk_sk("24",  "20:30:00")
mk_sk("25",  "20:30:00")
mk_sk("26",  "20:00:00")
mk_sk("27",  "20:00:00")
mk_sk("28",  "20:00:00")

# GPCM
mk_sk("31",  "20:00:00")
mk_sk("32",  "20:00:00")
mk_sk("33",  "20:00:00")
mk_sk("34",  "20:30:00")
mk_sk("35",  "20:30:00")
mk_sk("36",  "20:00:00")
mk_sk("37",  "20:00:00")
mk_sk("38",  "20:00:00")
