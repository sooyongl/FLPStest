# rm(list = ls())
for(i in fs::dir_ls("R", regexp = "r$")) source(i); rm(i)

# output ------------------------------------------------------------------
files <- fs::dir_ls("results", regexp = "rds$")

# n_cores <- detectCores() - 6
# cl <- parallel::makeCluster(n_cores)
# doSNOW::registerDoSNOW(cl)
# 
# pb <- txtProgressBar(max=length(files), style=3)
# progress <- function(n) setTxtProgressBar(pb, n)
# opts <- list(progress=progress)

res1 <- vector("list", length(files))
# res1 <- 
  # foreach(i = 1:length(files),
  #         .export = c("str_split","str_detect","tibble"),
  #         .packages = c("rstan","coda", "ggplot2","bayesplot","purrr","dplyr"),
  #         .options.snow = opts) %dopar%
for(i in 1:length(files))
{
  # i = 1
  print(i)
  
  fit <- readRDS(files[i])
  
  # print(object.size(fit), units="Mb")
  
  pop_data <- fit$sdat
  data <- with(pop_data, data.frame(Y, Z, true_eta, X))
  pop_fit <- lm(Y ~ Z*true_eta + x1 + x2, data = data)
  pop_est <- c(coefficients(pop_fit),var(pop_fit$residuals))
  
  # -------------------------------------
  fit <- fit$fit
  posterior <- as.matrix(fit)
  
  # bayes plot ------------------------
  b1_post <- tibble(b1 = posterior[,"b1"], 
                    chains = factor(rep(1:fit@sim$chains, each = fit@sim$iter-fit@sim$warmup)))
  b1_mean <- b1_post %>% 
    group_by(chains) %>% 
    summarise(b1_m = round(mean(b1), 3))
  
  b1_total_mean <- round(mean(posterior[,"b1"]), 3)
  
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
    labs(y = "density") +
    annotate("label", label = paste0("MEAN:",b1_total_mean), 
             x = b1_total_mean-0.02, y = 0)
  
  posterior2 <- rstan::extract(fit, inc_warmup = TRUE, permuted = FALSE, pars = "b1")
  color_scheme_set("mix-blue-pink")
  trace_p <- mcmc_trace(posterior2,  pars = c("b1"), n_warmup = fit@sim$warmup) + geom_hline(yintercept = b1_total_mean, size = 1, color = "blue") +
    annotate("label", label = b1_total_mean, x = 0, y = b1_total_mean-0.02)
  
  
  trace_by_chain <- data.frame(posterior2[,,1]) %>%
    rowid_to_column() %>% 
    gather("chain", "samples", -rowid) %>% 
    ggplot() +
    geom_line(aes(x = rowid, y = samples), alpha = .5) +
    # labs(title = res_model_fit) +
    facet_wrap(. ~ chain)
  
  sampler_plot <- list(area_p = area_p, trace_p = trace_p, 
                       trace_by_chain = trace_by_chain)
  
  # print(object.size(sampler_plot), units="Mb")
  
  # Bayesian fit -----------------------
  # ESS / Rhat / geweke / heidel / raftery
  sims <- matrix(unlist(lapply(fit@sim$samples, "[[", "b1")), ncol = fit@sim$chains)
  
  r_h <- round(Rhat(sims), 3)
  
  mcmc_object <- As.mcmc.list(fit, pars = "b1")
  # str(mcmc_object)
  # param_df <- data.frame(mcmc_object[[1]])
  # param_nm <- names(param_df)
  # mean(param_df[, "b1"])
  
  # atc <- coda::autocorr(mcmc_object)
  
  # gew <- coda::geweke.diag(mcmc_object, frac1=.1, frac2=.5)
  # gew1 <- coda::geweke.diag(mcmc_object,frac1=.25,frac2=.5)
  # str(gew)
  # lapply(lapply(gew, "[[", "z"), "[[", "b1")
  # lapply(lapply(gew, "[[", "z"), "[[", "a1")
  
  # hei <- coda::heidel.diag(mcmc_object)
  # str(hei)
  # lapply(hei, function(x) {x[][which(rownames(x[]) == "b1"), ][4]})
  # lapply(hei, function(x) {x[][which(rownames(x[]) == "a1"), ][4]})
  
  # gel <- coda::gelman.diag(mcmc_object)
  # str(gel)
  
  # raf <- coda::raftery.diag(mcmc_object) # larger than 5 problem
  # str(raf)
  # lapply(raf, function(x) {
  #   resmatrix <- x$resmatrix
  #   param_pos <- which(rownames(resmatrix) == "b1")
  #   resmatrix[, "I"][param_pos]
  # })
  
  
  # # effectiveSize(mcmc_object)
  gew <- geweke.diag(mcmc_object,frac1=.1,frac2=.5); # gew[[1]]$z
  if(all(abs(unlist(lapply(lapply(gew, "[[", "z"), "[[", "b1"))) < 1.96)){
    gew <- "converge"
  } else {
    gew <- "noncon"
  }
  
  hei <- heidel.diag(mcmc_object); # hei[[1]][]
  temp1 <- all(unlist(lapply(hei, function(x) {x[][which(rownames(x[]) == "b1"), ][4]}))==1) == 1
  if(!is.na(temp1) & temp1){
    hei <- "converge"
  } else{
    hei <- "noncon"
  }
  
  gel <- my.gelman.diag(mcmc_object)
  gel <- gel$psrf[1]
  
  # raf <- raftery.diag(mcmc_object)
  # raf
  
  model_fit <- list(list(rhat = r_h, geweke = gew, heidel = hei))
  
  # Estimates -----------------------
  fit <- as.data.frame(fit)
  
  temp1 <- str_split(files[i], "/", simplify = T)[,2]
  temp2 <- str_split(temp1, "_", simplify = T)
  sample_size <- temp2[1,1]
  lambda <- temp2[1,2]
  nsec <- temp2[1,3]
  covar <- temp2[1,4]
  # if(str_detect(covar, "iter")) {covar <- "xeff"} else {covar <- "noeff"}
  
  rep <- temp2[1,5]
  
  a1 <- mean(fit$a1)
  b0 <- mean(fit$b0)
  b1 <- mean(fit$b1)
  
  b.x1 <- mean(fit$`betaY[1]`)
  b.x2 <- mean(fit$`betaY[2]`)
  
  diff <- list(
    list(
      est = colMeans(fit[str_detect(names(fit), "secEff")]),
      pop = -pop_data$lv.par$b
    )
  )
  
  df <- tibble(temp1, sample_size, lambda, nsec, covar, rep, model_fit, 
               
               p.b0 = pop_est[2], 
               b0,
               p.b1 = pop_est[6], 
               b1,
               p.a1 = pop_est[3], 
               a1,
               p.bx1 = pop_est[4],
               b.x1,
               p.bx2 = pop_est[5],
               b.x2, 
               diff)
  
  o <- list(
    file_name = temp1, sampler_plot = sampler_plot, df = df
  )
  
  # print(object.size(o), units="Mb")
  
  # saveRDS(res1, paste0("report/", "raw_results_", i,".rds"))
  
  res1[[i]] <- o
  
  rm(fit, hei, gew, sampler_plot)
  rm(mcmc_object)
  rm(o)
  
  gc()
  
}
# stopCluster(cl)
print(object.size(res1), units="Mb")

# if(!file.exists("report/rds/raw_results_1101.rds")){
  saveRDS(res1, "report/rds/raw_results_1101_1.rds")
  # }

# tidy results ------------------------------------------------------------
raw_results <- readRDS("report/rds/raw_results_1101_1.rds")

res_filename <- lapply(raw_results, "[[", "file_name")
res_est <- bind_rows(lapply(raw_results, "[[", "df"))
res_plot <- lapply(raw_results, "[[", "sampler_plot")
res_plot_area <- lapply(res_plot, "[[", "area_p")
res_plot_trace <- lapply(res_plot, "[[", "trace_p")

# check fit ---------------------------------------------------------------
res_fit <- res_est %>% select(temp1, sample_size, nsec, covar, rep) %>% 
  bind_cols(., 
            res_est$model_fit %>% unlist() %>%
              matrix(., ncol = 3, byrow = T) %>% 
              data.frame() %>% 
              set_names(c("Rhat", "geweke", "heidel"))
  ) %>% 
  mutate(
    Rhat = as.numeric(Rhat),
    geweke = case_when(geweke == "converge" ~ 1, TRUE ~ 0),
    heidel = case_when(heidel == "converge" ~ 1, TRUE ~ 0)
  )

picked <- res_fit %>% 
  filter(geweke == 1 & heidel == 1) %>% 
  print(n = 100) %>% 
  pull(temp1)

picked <- res_fit %>% 
  filter(geweke == 1 & heidel == 0) %>% 
  print(n = 100) %>% 
  pull(temp1)

picked <- res_fit %>% 
  filter(geweke == 0 & heidel == 1) %>% 
  print(n = 100) %>% 
  pull(temp1)

picked <- res_fit %>% 
  filter(geweke == 0 & heidel == 0) %>% 
  print(n = 100) %>% 
  pull(temp1)

picked_pos <- which(unlist(res_filename) == picked)
ggpubr::ggarrange(res_plot_area[[picked_pos]],
                  res_plot_trace[[picked_pos]], ncol = 2, nrow = 1,
                  common.legend = T)


# -------------------------------------------------------------------------
a1 <- 1:length(res_filename)

nrep <- 20
start_n <- sapply(1:(tail(a1,1)/nrep), function(x) { 1 + 5*(x-1) })
end_n <- sapply(1:(tail(a1,1)/nrep), function(x) { 0 + 5*(x) })

comb_list <- vector("list", 5)
for(jj in 1:5) {
 
  sn <- start_n[jj]
  en <- end_n[jj]
  
  cond_name <- str_split(res_filename[[sn]], pattern = "_[0-9]_\\.rds", simplify = T) %>% .[,1]
  
  res_model_fit <- res_fit %>% slice(sn:en) %>% 
    mutate(model_fit = paste0("Rhat:",Rhat, " Geweke:", geweke, " Heidel:", heidel)) %>% pull(model_fit)
  
  plot_area <- res_plot_area[sn:en]
  for(i in 1:5) {
    plot_area[[i]] <- plot_area[[i]] + 
      annotate("label", label = res_model_fit[i], 
               x = -Inf, y = Inf, hjust = 0, vjust = 1)
  }
  
  comb_area <- ggpubr::ggarrange(plotlist=plot_area, ncol = 2, nrow = 3,
                                 common.legend = T)
  comb_area <- ggpubr::annotate_figure(comb_area, 
                                       top = ggpubr::text_grob(cond_name, color = "blue", face = "bold", size = 14))
  
  plot_trace <- res_plot_trace[sn:en]
  for(i in 1:5) {
    plot_trace[[i]] <- plot_trace[[i]] + 
      annotate("label", label = res_model_fit[i], 
               x = -Inf, y = Inf, hjust = 0, vjust = 1)
  }
  comb_trace <- ggpubr::ggarrange(plotlist=plot_trace, ncol = 2, nrow = 3,
                                  common.legend = T)
  comb_trace <- ggpubr::annotate_figure(comb_trace, 
                                        top = ggpubr::text_grob(cond_name, color = "blue", face = "bold", size = 14))
  
  comb_list[[jj]] <- list(comb_area = comb_area, comb_trace = comb_trace)
}

length(comb_list)
comb_list[[1]]$comb_area
comb_list[[1]]$comb_trace

comb_list[[2]]$comb_area
comb_list[[2]]$comb_trace

comb_list[[3]]$comb_area
comb_list[[3]]$comb_trace

comb_list[[4]]$comb_area
comb_list[[4]]$comb_trace

comb_list[[5]]$comb_area
comb_list[[5]]$comb_trace

# posterior check ---------------------------------------------------------


# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
# res2 <- readRDS("report/combined_result.rds")
# 
# mean_b1 <- res2 %>% 
#   group_by(sample_size, lambda, nsec) %>% 
#   summarise(
#     b1_m = mean(b1)
#   )
# 
# library(ggpubr); library(ggrepel)
# p1 <- res2 %>% 
#   ggplot(aes(y = b1, x = sample_size, colour = nsec)) +
#   geom_boxplot(
#     size = 0,
#     position = position_dodge(1),
#   ) +
#   geom_point(
#     size = 3,
#     alpha = 0.3,
#     position = position_jitterdodge(jitter.width = 0.1, dodge.width = 1)
#   ) +
#   geom_hline(yintercept = -0.06) +
#   
#   geom_point(
#     data = mean_b1,
#     aes(y = b1_m, x = sample_size, colour = nsec),
#     size = 3, 
#     stroke = 1, 
#     shape = 21,
#     alpha = 1,
#     position = position_jitterdodge(jitter.width = 0, dodge.width = 1)
#   ) +
#   geom_label(
#     data = mean_b1,
#     aes(y = b1_m, x = sample_size, colour = nsec, label = round(b1_m, 3)),
#     position = position_jitterdodge(jitter.width = 0, dodge.width = 1)
#   ) +
#   theme_pubclean()
# set_palette(p1, "jco")

# str(fit@model_pars)
# print(fit, pars = 'betaU')
# print(fit, pars = 'betaY')
# 
# print(fit, pars = 'b00')
# print(fit, pars = 'a1')
# 
# methods(print, class = class(fit))
# graphs ------------------------------------------------------------------- 
# stan_hist(fit, bins=40)
# 
# 
# fit.ggmcmc <- ggs(fit$fit)
# theme_set(theme_bw())
# 
# fit.ggmcmc %>% filter(Parameter == "b1") %>% ggs_traceplot(.)
# fit.ggmcmc %>% filter(Parameter == "b1") %>% ggs_density(.)
# fit.ggmcmc %>% filter(Parameter == "b1") %>% ggs_caterpillar(.)

