# rm(list= ls()); gc()
library(tidyverse); library(rstan);library(lavaan); #library(FLPS)

for(i in fs::dir_ls("R", regexp = "r$")) source(i); rm(i)
source_funs <- ls()

# LCA -----------------------------------------
# for(i in 1:10) {
#   # i = 1
#
#   true_eta <- c(rnorm(250), rnorm(250, 0.7))
#   info <- parsForLVM(
#     theta = true_eta,
#     n_class = 2,
#     n_indi  = 10,
#     seperation = 0.9,
#     data_type = "lca")
#
#   data_info <- generate.lca(info)
#   # apply(data_info[data_info$class == 1, 1:10], 2, table)
#   # apply(data_info[data_info$class == 2, 1:10], 2, table)
#
#   grad <- data_info[, 1:10] - 1
#
#   # idx <- which(!is.na(grad), arr.ind = T)
#   # idx <- cbind(idx, 1:dim(idx)[1])
#   # idx <- idx[order(idx[,1]), ]
#   # nsecWorked <- nrow(idx)
#   # nstud <- nrow(grad)
#   # nsec <- ncol(grad)
#   #
#   # studentM <- idx[,1]
#   # section <- idx[,2]
#   # grad <- sapply(1:dim(idx)[1], function(n) grad[idx[n,1], idx[n,2]] )
#   nstud <- 500
#   nsec <- 10
#   nworked <- sample(1:10,500,replace=TRUE,prob=dexp(1:10,rate=1/6))
#   nsecWorked <- sum(nworked)
#   studentM <- do.call("c", lapply(seq(500),function(n) rep(n,each=nworked[n])))
#
#   section <- do.call("c", lapply(seq(500),
#                                  function(n) {
#                                    sort(sample(1:10, nworked[n],
#                                                replace = FALSE))}))
#   ss <- cbind(studentM, section)
#
#   grad <- sapply(1:dim(ss)[1], function(n) grad[ss[n,1], ss[n,2]] )
#
#   studItem <- unname(table(studentM))
#   # cumStudItem<- cumsum(studItem) - studItem[1]
#   cumStudItem <- c(0, cumsum(studItem)[-length(studItem)])
#
#
#   dt <- list(
#     nclass = 2,
#     studItem = studItem,
#     cumStudItem = cumStudItem,
#     nsecWorked = nsecWorked,
#     nstud = nstud,
#     nsec = nsec,
#     #idx = idx,
#     studentM = studentM,
#     section = section,
#     grad = grad
#   )
#
#   # write_csv(data.frame(data_info), "test/test_lv_model/mplus/lca.csv", col_names = F)
#   #
#   # MplusAutomation::runModels("test/test_lv_model/mplus/lca.inp")
#   # lca.out <- MplusAutomation::readModels("test/test_lv_model/mplus/lca.out")
#   #
#   # item.prob <- lca.out$parameters$probability.scale %>% filter(category == 2)
#   # class.logit <- lca.out$parameters$unstandardized %>% filter(str_detect(paramHeader, "Means")) %>% pull(est)
#   # class.prop <- (exp(class.logit) / (1 + exp(class.logit)))
#
#
#   # stan_model <- paste(read_lines("test/test_lv_model/stan/casestudy_LCA.stan"),
#   #                     collapse = "\n")
#   stan_model <- paste(read_lines("test/test_lv_model/stan/LCA.stan"),
#                       collapse = "\n")
#   # cat(stan_model)
#   lca.fit <- rstan::stan(
#     model_code = stan_model,
#     data = dt,
#     iter = 4000,
#     cores = 1,
#     chains = 3
#   )
#
#   res <- list(fit = lca.fit, sdat = dt, info = info, data_info = data_info)
#   saveRDS(res, paste0("results/res_incompletedata_", "LCA","_",i ,".rds"))
#
# }


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


post_plot <- function(fit, parname) {
  posterior <- as.matrix(fit)

  # bayes plot ------------------------
  b1_post <- tibble(b1 = posterior[,parname],
                    chains = factor(rep(1:fit@sim$chains, each = fit@sim$iter-fit@sim$warmup)))
  b1_mean <- b1_post %>%
    group_by(chains) %>%
    summarise(b1_m = round(mean(b1), 3))

  b1_total_mean <- round(mean(posterior[,parname]), 3)

  theme_set(theme_bw(base_size = 14))
  area_p <-
    b1_post %>%
    ggplot(aes(x = b1)) +
    geom_density(aes(fill = chains), alpha = 0.1) +
    geom_vline(data = b1_mean,
               aes(xintercept = b1_m, color = chains), size = 1) +
    geom_text(data = b1_mean,
              aes(x = b1_m, y = as.numeric(chains),
                  label = b1_m, color = chains),
              size = 5) +
    labs(y = "density", x = parname) +
    annotate("label", label = paste0("MEAN:",b1_total_mean),
             x = b1_total_mean-0.02, y = 0)

  posterior2 <- rstan::extract(fit, inc_warmup = TRUE, permuted = FALSE, pars = "nu[1]")
  color_scheme_set("mix-blue-pink")
  trace_p <- mcmc_trace(posterior2,  pars = c(parname), n_warmup = fit@sim$warmup) + geom_hline(yintercept = b1_total_mean, size = 1, color = "blue")


  trace_by_chain <- data.frame(posterior2[,,1]) %>%
    rowid_to_column() %>%
    gather("chain", "samples", -rowid) %>%
    ggplot() +
    geom_line(aes(x = rowid, y = samples), alpha = .5) +
    # labs(title = res_model_fit) +
    facet_wrap(. ~ chain)

  sampler_plot <- list(area_p = area_p, trace_p = trace_p,
                       trace_by_chain = trace_by_chain)

  sampler_plot
}

library(coda)
library(ggmcmc)
library(bayesplot)

res_list <- fs::dir_ls("results", regexp = "rds$")
res_list <- res_list[str_detect(res_list, "complete")]

param_list <- vector("list", length(res_list))
for(i in 1:length(param_list)) {

  data_type <- str_split(res_list[i], "_", simplify = T)[2]
  model_type <- str_split(res_list[i], "_", simplify = T)[3]

  res <- readRDS(res_list[i])
  fit <- res$fit
  data_info <- res$data_info

  est_res <- extract_res(fit, data_info)
  postplot_res <- post_plot(fit, "nu[1]")


  param_list[[i]] <- list(
    data_type,
    model_type,
    est_res = est_res, post_plot = postplot_res)
}

saveRDS(param_list, "report/rds/res_combined_LCA_1129.rds")



# test --------------------------------------------------------------------

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

  idx <- which(!is.na(grad), arr.ind = T)
  idx <- cbind(idx, 1:dim(idx)[1])
  idx <- idx[order(idx[,1]), ]
  nsecWorked <- nrow(idx)
  nstud <- nrow(grad)
  nsec <- ncol(grad)

  studentM <- idx[,1]
  section <- idx[,2]
  grad <- sapply(1:dim(idx)[1], function(n) grad[idx[n,1], idx[n,2]] )

  # nstud <- 500
  # nsec <- 10
  # nworked <- sample(1:10,500,replace=TRUE,prob=dexp(1:10,rate=1/6))
  # nsecWorked <- sum(nworked)
  # studentM <- do.call("c", lapply(seq(500),function(n) rep(n,each=nworked[n])))
  #
  # section <- do.call("c", lapply(seq(500),
  #                                function(n) {
  #                                  sort(sample(1:10, nworked[n],
  #                                              replace = FALSE))}))
  # ss <- cbind(studentM, section)
  #
  # grad <- sapply(1:dim(ss)[1], function(n) grad[ss[n,1], ss[n,2]] )

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

stan_model <- paste(read_lines("test/test_lv_model/stan/LCA_indiprop.stan"),
                    collapse = "\n")
cat(stan_model)
lca.fit <- rstan::stan(
  model_code = stan_model,
  data = dt,
  iter = 4000,
  cores = 1,
  chains = 1
)

lca.fit.df <- as.data.frame(lca.fit)

a1 <- colMeans(lca.fit.df)[str_detect(names(lca.fit.df), "nu")]


matrix(a1, 500, 2)

