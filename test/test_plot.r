library(tidyverse)

a1 <- fs::dir_ls("results/cleaned")
a2 <- readRDS(a1)
unique(a2$par_name)

a3 <- a2 %>%
  separate(condition, c("samplesize","nitem","lvmodel","correct","outcome","rep"), "_") %>%
  separate(par_name, c("a","par_name"), "\\.", fill = "left") %>%
  select(-a)

a3 <- a3 %>%
  mutate(
    lvmodel = factor(lvmodel, levels = c("rasch","2pl","gpcm","grm")),
    nitem = factor(nitem, levels = c("50","100","200")),
    bias = mean - true_param
  )

mk_plot <- function(data) {
  data %>%
    ggplot(aes(x = samplesize, y = bias)) +
    # geom_boxplot() +
    geom_violin(fill = "skyblue", alpha = 0.5, color = NA) +
    ggforce::geom_sina(size = 0.75) +
    geom_hline(yintercept = 0, alpha = 0.8, linetype = "dotted") +
    facet_grid(nitem ~ lvmodel) +
    theme_bw()
}

a4 <- a3 %>%
  mutate(
    par_name = case_when(str_detect(par_name,"lambda") ~ "lambda",
                         str_detect(par_name,"eta") ~ "eta",
                         str_detect(par_name,"tau") ~ "tau",
                         str_detect(par_name,"^d") ~ "d",
                         TRUE ~ par_name)
  ) %>%
  group_by(samplesize, nitem, lvmodel, correct, outcome, rep, par_name) %>%
  summarise(bias = mean(bias)) %>%
  ungroup()

unique(a4$par_name)

a4 %>%
  filter(par_name == "a11") %>%
  mk_plot()

a4 %>%
  filter(par_name == "b11") %>%
  mk_plot()

a4 %>%
  filter(par_name == "b0") %>%
  mk_plot()

a4 %>%
  filter(str_detect(par_name, "bu")) %>%
  mk_plot()

a4 %>%
  filter(str_detect(par_name, "by")) %>%
  mk_plot()

a4 %>%
  filter(par_name == "lambda") %>%
  mk_plot()



