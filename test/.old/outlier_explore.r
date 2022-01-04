for(i in fs::dir_ls("R", regexp = "r$")) source(i); rm(i)

# res2 <- readRDS(file.path("report", "rds/combined_result_1101.rds"))
raw_results <- readRDS("report/rds/raw_results_1101_1.rds")

res_filename <- lapply(raw_results, "[[", "file_name")
res_est <- bind_rows(lapply(raw_results, "[[", "df"))
res_plot <- lapply(raw_results, "[[", "sampler_plot")
res_plot_area <- lapply(res_plot, "[[", "area_p")
res_plot_trace <- lapply(res_plot, "[[", "trace_p")


res2 <- res_est
picked_file <- res2 %>% arrange(b1) %>% slice(2) %>% pull(temp1)
picked_num <- which(unlist(res_filename) ==picked_file)

res_plot_area[[picked_num]]
res_plot_trace[[picked_num]]

res3 <- readRDS(file.path("results", picked_file))

sdata <- res3$sdat
fit <- res3$fit

posterior <- as.matrix(fit)

# bayes plot ------------------------
res_model_fit <- 
  res_fit %>% filter(temp1 == picked_file) %>% 
  mutate(model_fit = paste0("Rhat:",Rhat, " Geweke:", geweke, " Heidel:", heidel)) %>% pull(model_fit)


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
  labs(y = "density", title = res_model_fit) +
  annotate("label", label = paste0("MEAN:",b1_total_mean), 
           x = b1_total_mean-0.02, y = 0)

posterior2 <- rstan::extract(fit, inc_warmup = TRUE, permuted = FALSE, pars = "b1")
color_scheme_set("mix-blue-pink")
trace_p <- 
  mcmc_trace(posterior2,  pars = c("b1"), n_warmup = fit@sim$warmup) + geom_hline(yintercept = b1_total_mean, size = 1, color = "blue") +
  lab(title = res_model_fit) +
  annotate("label", label = b1_total_mean, x = 0, y = b1_total_mean-0.02)

data.frame(posterior2[,,1]) %>%
  rowid_to_column() %>% 
  gather("chain", "samples", -rowid) %>% 
  ggplot() +
  geom_line(aes(x = rowid, y = samples), alpha = .5) +
  lab(title = res_model_fit) +
  facet_wrap(. ~ chain)






