library(tidyverse)

a1 <- fs::dir_ls("results/cleaned")
# a2 <- readRDS(a1[1])
unique(a2$par_name)

a3 <- a2 %>%
  separate(condition, c("samplesize","nitem","lvmodel","correct","outcome","rep"), "_") %>%
  separate(par_name, c("a","par_name"), "\\.", fill = "left") %>%
  select(-a)

a3 <- a3 %>%
  mutate(
    lvmodel = factor(lvmodel, levels = c("rasch","2pl","gpcm","grm")),
    nitem = factor(nitem, levels = c("50","100","200")),
    samplesize = factor(samplesize, levels = c("500","1000")),
    bias = mean - true_param
  )
a4 <- a3 %>%
  mutate(
    par_name = case_when(str_detect(par_name,"lambda") ~ "lambda",
                         str_detect(par_name,"eta") ~ "eta",
                         str_detect(par_name,"tau") ~ "tau",
                         str_detect(par_name,"^d") ~ "d",
                         TRUE ~ par_name)
  ) %>%
  group_by(samplesize, nitem, lvmodel, correct, outcome, rep, par_name) %>%
  summarise(bias = mean(bias), Rhat = mean(Rhat), n_eff = mean(n_eff)) %>%
  ungroup() %>%
  filter(!is.nan(n_eff) & !is.nan(Rhat)) %>%
  filter(lvmodel != "rasch" & par_name != "lambda")


mk_plot <- function(data, rq = 1, pname, type="bias",  psize = 2) {

  data <- data %>% filter(str_detect(par_name,"a11"))

  if(rq == 1) {
    p1 <- data %>%
      filter(nitem == 100) %>%
      ggplot(aes(x = samplesize, y = !!as.name(type))) +
      # geom_boxplot() +
      geom_violin(fill = "skyblue", alpha = 0.5, color = NA) +
      ggforce::geom_sina(size = psize) +

      facet_grid(nitem ~ lvmodel) +
      theme_bw(base_size = 16)
  } else {
    p1 <- data %>%
      filter(samplesize == 1000) %>%
      ggplot(aes(x = nitem, y = bias)) +
      # geom_boxplot() +
      geom_violin(fill = "skyblue", alpha = 0.5, color = NA) +
      ggforce::geom_sina(size = psize) +

      facet_grid(samplesize ~ lvmodel) +
      theme_bw(base_size = 16)
  }

  if(!type %in% c("Rhat","n_eff")) {
    p1 <- p1 + geom_hline(yintercept = 0, alpha = 0.8, linetype = "dotted")
  }

  p1 + labs(title = pname)
}


bias_top_1_by_param <- a4 %>%
  group_split(par_name) %>%
  # filter(par_name == "a11") %>%
  map(., ~ .x %>% arrange(desc(abs(bias))) %>% slice(1)) %>%
  bind_rows() %>%
  select(-correct, -outcome)
bias_top_1_by_param



a4 %>% mk_plot(1, "a11","Rhat")

a4 %>% mk_plot(1,"a11", "n_eff")


a4 %>% mk_plot(1,"a11")

a4 %>% mk_plot(2,"a11")

a4 %>% mk_plot(1,"b11")

a4 %>% mk_plot(2,"b11")

a4 %>% mk_plot(1,"b0$")

a4 %>% mk_plot(1,"b0$")

a4 %>% mk_plot(1, "bu")

a4 %>% mk_plot(2, "bu")

a4 %>% mk_plot(1, "by")

a4 %>% mk_plot(2, "by")

a4 %>% mk_plot(1, "lambda")

a4 %>% mk_plot(2, "lambda")


