library(tidyverse)
library(rstan)
data_idx <- "0529"

res_all <- readRDS(paste0("test/shinydata/", data_idx, "_data.rds"))
mpart <- readRDS(paste0("test/shinydata/", data_idx, "_mpart.rds"))

##
total_res <- res_all
n1 <- total_res %>%
  filter(par_name == "a11") %>%
  group_by(nsample=samplesize, nitem,lvmodel, linearity, ydist=outdist) %>%
  count() %>%
  mutate(additional = 100 - n)
n1 %>% print(n=100)



##
a1 <- mpart %>%
  filter(str_detect(par_name,"lambda") ) %>%
  # filter(lvmodel != "rasch") %>%
  separate(cond,
           c("lvmodel","samplesize","nitem","r2y","r2eta","linearity","outdist","rep"), "_") %>%
  mutate(
    overest = err > 0,
    underest = err < 0,
    evenest  = err == 0
  )

a1 %>%
  filter(err != 0) %>%
  filter(lvmodel %in% c("2plunif","2plnormal","2pllogn")) %>%
  ggplot() +
  geom_histogram(aes(x = err), fill = "white", color = "black") +
  geom_vline(xintercept = 0, color = "blue", size = 1.2) +
  facet_wrap(. ~ lvmodel, scales = "free") +
  theme_bw()

mk_hist <- function(p.lvmodel = "2pl", base_size = 24) {
  a1 %>%
    filter(err != 0) %>%
    filter(lvmodel == p.lvmodel) %>%
    ggplot() +
    geom_histogram(aes(x = err), fill = "white", color = "black") +
    geom_vline(xintercept = 0, color = "blue", size = 1.2) +
    facet_wrap(samplesize ~ nitem, scales = "free") +
    labs(title = toupper(p.lvmodel)) +
    theme_bw(base_size = base_size)
}
mk_hist("2pl")
mk_hist("grm")
mk_hist("gpcm")

mk_density("2pl")
mk_density <- function(p.lvmodel = "2pl", base_size = 24) {
  a1 %>%
    group_by(lvmodel, samplesize, nitem, rep) %>%
    summarise(
      prop_overest = mean(overest),
      prop_underest = mean(underest)
    ) %>%
    filter(lvmodel == "2pl"
           # samplesize == "1000",
           # nitem == "100"
    ) %>%
    # gather("est", "value", -lvmodel, -samplesize, -nitem, -rep) %>%
    ggplot(aes(prop_overest)) +
    # geom_bar(aes(rep, value, fill = "est"), position="stack", stat = "identity")
    geom_histogram(
      # binwidth = 30,
      alpha = 0.5, fill = "blue", color = "black") +
    geom_density(aes(y=..density.. * 20)) +
    labs(title = toupper(p.lvmodel), y = "count") +
    theme_bw(base_size = base_size)
}






