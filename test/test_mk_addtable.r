# condition filter --------------------------------------------------------
a1 <- list.files("results", pattern = "rds")
a1 <- a1[!grepl("normal|unif", a1)]
a1 <- table(unlist(strsplit(a1,"\\d+.rds", perl = T)))
a1 <- 100 - a1
filename0 <- names(a1)
filename0 <- do.call("rbind", strsplit(filename0, "_"))
filename0 <- filename0[, c(2, 3, 1, 6, 7)]
filename0 <- apply(filename0, 1, function(x) paste(x[1],x[2],x[3],x[4],x[5], sep = "_"))

additional_rep <- c()
for(i in 1:length(a1)) {
  additional_rep <- c(additional_rep,
                      paste0(filename0[i], "_",
                             1:a1[i]))
}

additional_rep <- data.frame(additional_rep)
additional_rep$additional_rep <- as.character(additional_rep$additional_rep)
conditions <- do.call("rbind", strsplit(additional_rep$additional_rep, "_"))
conditions <- data.frame(conditions)
conditions$X1 <- as.character(conditions$X1)
conditions$X1 <- as.numeric(conditions$X1)

conditions$X2 <- as.character(conditions$X2)
conditions$X2 <- as.numeric(conditions$X2)

conditions$X3 <- as.character(conditions$X3)

conditions$X4 <- as.character(conditions$X4)
conditions$X4 <- as.logical(conditions$X4)

conditions$X6 <- as.character(conditions$X6)
conditions$X6 <- as.numeric(conditions$X6)
conditions$X6 <- conditions$X6 + 99900
#
names(conditions) <- c("nsample","nitem","lvmodel","linearity","ydist", "rep")

cond_table <- conditions
saveRDS(cond_table, "cond_table_add.rds")
