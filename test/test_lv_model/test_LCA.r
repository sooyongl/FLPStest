# rm(list= ls()); gc()
library(tidyverse); library(rstan);library(lavaan); #library(FLPS)

for(i in fs::dir_ls("R", regexp = "r$")) source(i); rm(i)
source_funs <- ls()

# LCA -----------------------------------------
for(i in 1:10) {
  # i = 1

  true_eta <- c(rnorm(250), rnorm(250, 0.7))
  info <- parsForLVM(
    theta = true_eta,
    n_class = 2,
    n_indi  = 10,
    seperation = 0.9,
    data_type = "lca")

  data_info <- generate.lca(info)
  # apply(data_info[data_info$class == 1, 1:10], 2, table)
  # apply(data_info[data_info$class == 2, 1:10], 2, table)

  grad <- data_info[, 1:10] - 1

  # idx <- which(!is.na(grad), arr.ind = T)
  # idx <- cbind(idx, 1:dim(idx)[1])
  # idx <- idx[order(idx[,1]), ]
  # nsecWorked <- nrow(idx)
  # nstud <- nrow(grad)
  # nsec <- ncol(grad)
  #
  # studentM <- idx[,1]
  # section <- idx[,2]
  # grad <- sapply(1:dim(idx)[1], function(n) grad[idx[n,1], idx[n,2]] )
  nstud <- 500
  nsec <- 10
  nworked <- sample(1:10,500,replace=TRUE,prob=dexp(1:10,rate=1/6))
  nsecWorked <- sum(nworked)
  studentM <- do.call("c", lapply(seq(500),function(n) rep(n,each=nworked[n])))

  section <- do.call("c", lapply(seq(500),
                                 function(n) {
                                   sort(sample(1:10, nworked[n],
                                               replace = FALSE))}))
  ss <- cbind(studentM, section)

  grad <- sapply(1:dim(ss)[1], function(n) grad[ss[n,1], ss[n,2]] )

  studItem <- unname(table(studentM))
  # cumStudItem<- cumsum(studItem) - studItem[1]
  cumStudItem <- c(0, cumsum(studItem)[-length(studItem)])


  dt <- list(
    nclass = 2,
    studItem = studItem,
    cumStudItem = cumStudItem,
    nsecWorked = nsecWorked,
    nstud = nstud,
    nsec = nsec,
    #idx = idx,
    studentM = studentM,
    section = section,
    grad = grad
  )

  # write_csv(data.frame(data_info), "test/test_lv_model/mplus/lca.csv", col_names = F)
  #
  # MplusAutomation::runModels("test/test_lv_model/mplus/lca.inp")
  # lca.out <- MplusAutomation::readModels("test/test_lv_model/mplus/lca.out")
  #
  # item.prob <- lca.out$parameters$probability.scale %>% filter(category == 2)
  # class.logit <- lca.out$parameters$unstandardized %>% filter(str_detect(paramHeader, "Means")) %>% pull(est)
  # class.prop <- (exp(class.logit) / (1 + exp(class.logit)))


  # stan_model <- paste(read_lines("test/test_lv_model/stan/casestudy_LCA.stan"),
  #                     collapse = "\n")
  stan_model <- paste(read_lines("test/test_lv_model/stan/LCA.stan"),
                      collapse = "\n")
  # cat(stan_model)
  lca.fit <- rstan::stan(
    model_code = stan_model,
    data = dt,
    iter = 4000,
    cores = 1,
    chains = 3
  )

  res <- list(fit = lca.fit, sdat = dt, info = info, data_info = data_info)
  saveRDS(res, paste0("results/res_incompletedata_", "LCA","_",i ,".rds"))

}


extract_res <- function(fit, data_info) {

  fit <- as.data.frame(fit)
  fit$chain <- rep(c(1:3), each = 2000)

  fit.chain1 <- colMeans(fit[fit$chain == 1, ])
  fit.chain2 <- colMeans(fit[fit$chain == 2, ])
  fit.chain3 <- colMeans(fit[fit$chain == 3, ])

  est_res_by_chain <- lapply(list(fit.chain1, fit.chain2, fit.chain3),
         function(x) {
           class_prop <- x[str_detect(names(x), "nu")]
           res_prob <- matrix(x[str_detect(names(x), "p\\[")],
                              ncol = 2, nrow = 10, byrow = T)
           colnames(res_prob) <- c("C1","C2")

           list(est_cp = class_prop, est_rp = res_prob)
         })

  # population
  pop_cp <- table(data_info$class) / length(data_info$class)

  pop_rp <- cbind(
    apply(data_info[data_info$class == 1, 1:10], 2, table)[2, ] / table(data_info$class)[1],
    apply(data_info[data_info$class == 2, 1:10], 2, table)[2, ] / table(data_info$class)[2])

  pop_res <- list(pop_cp = pop_cp, pop_rp = pop_rp)

  list(pop_res = pop_res, est_res_by_chain = est_res_by_chain)
}


fit <- res$fit
data_info <- res$data_info

extract_res(fit, data_info)


# stan_model <- paste(read_lines("test/test_lv_model/stan/LCA_indiprop.stan"),
#                     collapse = "\n")
# cat(stan_model)
# lca.fit <- rstan::stan(
#   model_code = stan_model,
#   data = dt,
#   iter = 4000,
#   cores = 1,
#   chains = 1
# )
#
# lca.fit.df <- as.data.frame(lca.fit)
#
# a1 <- colMeans(lca.fit.df)[str_detect(names(lca.fit.df), "nu")]
#
# matrix(a1, 500, 2)

