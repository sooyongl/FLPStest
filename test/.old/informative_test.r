rm(list = ls())
library(rstan); library(tidyverse)
for(i in fs::dir_ls("R", regexp = "r$")) source(i); rm(i)
source_funs <- ls()

param_info_list <- readRDS("report/rds/irt_info_1214.rds")
param_pinfo_list <- readRDS("report/rds/irt_pinfo_1214.rds")
param_noinfo_list <- readRDS("report/rds/irt_noinfo_1214.rds")

test_plot <- function(pickname="flps_param", yname = "b1", yint = 0, p.title = "", p.subt = "", p.cap = "info: informative prior for all parameters; partial: informative prior for factor loading; noinfo: noninformative prior") {
  info.param <- lapply(param_info_list, "[[", pickname) %>% bind_rows(.id = "rep")
  pinfo.param <- lapply(param_pinfo_list, "[[", pickname) %>% bind_rows(.id = "rep")
  noinfo.param <- lapply(param_noinfo_list, "[[", pickname) %>% bind_rows(.id = "rep")

  c.data <- bind_rows(info.param, pinfo.param, noinfo.param, .id = "prior") %>%
    mutate(prior = case_when(prior == 1 ~ "info", prior == 2 ~ "partial", prior == 3 ~ "noinfo"),
           prior = factor(prior, levels = c("info","partial","noinfo")))

  c.data %>%
    filter(chain != 0 & rep %in% c(1:10) ) %>%
    ggplot(aes_string(x = "rep", y = yname)) +
    geom_point(
      aes(colour = as.factor(chain)
          # shape = as.factor(chain)
          ),
      alpha = 0.6,
      size = 4, position = position_jitter(w = 0.3, h = 0)) +
    geom_hline(yintercept = yint, alpha = .5) +
    geom_hline(yintercept = 0, alpha = .2) +
    scale_y_continuous(n.breaks = 20) +
    labs(
      colour = "chain",
      title = p.title,
      subtitle = p.subt,
      caption = p.cap) +
    scale_shape_discrete(guide="none") +
    facet_grid(. ~ prior) +
    # facet_grid(rows = vars(category.2), vars(cols = category.1))
    ggforce::geom_mark_hull() +
    # theme(legend.position = "none") +
    scale_color_brewer(palette = "Set1") +
    theme_bw(base_size = 14)
}

test_plot(pickname="flps_param", yname = "b1", yint = -0.2,
          p.title = "b1 estimate across 10 replications by priors")
test_plot(pickname="comb_lambda", yname = "`lambda[2]`", yint = 0,
          p.title = "a factor loading across 10 replications by priors")
test_plot(pickname="flps_param", yname = "a1", yint = 0,
          p.title = "a1 estimate across 10 replications by priors")
test_plot(pickname="flps_param", yname = "bu1", yint = 0,
          p.title = "bu1 estimate across 10 replications by priors")
test_plot(pickname="flps_param", yname = "bu2", yint = 0,
          p.title = "bu2 estimate across 10 replications by priors")


