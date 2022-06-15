files_local <- list.files("D:/FLPS/results/", pattern = "rds$")


# files_TACC <- list.files("results/", pattern = "rds$")
# saveRDS(files_TACC, "files_TACC.rds")

files_TACC <- readRDS("test/files_TACC.rds")

length(files_local)
length(files_TACC)

files_TACC[!files_TACC %in% files_local]
