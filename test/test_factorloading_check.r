data_idx <- "0529"
mpart <- readRDS(paste0(base_path,"/","test/shinydata/", data_idx, "_mpart.rds"))


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


a1
a1 %>%
  group_by(lvmodel, samplesize, nitem, rep) %>%
  summarise(
    overestimated = mean(err * overest),
    underestimated = mean(err * underest),
    prop_overest = mean(overest),
    prop_underest = mean(underest)
  ) %>%
  filter(lvmodel == "2pl",
         # samplesize == "1000",
         # nitem == "100"
  ) %>%
  # gather("est", "value", -lvmodel, -samplesize, -nitem, -rep) %>%
  ggplot(aes(prop_overest)) +
  # geom_bar(aes(rep, value, fill = "est"), position="stack", stat = "identity")
  # geom_histogram(
  #   # binwidth = 30,
  #   alpha = 0.5, fill = "blue", color = "black") +
  # geom_density(aes(y=..density.. * den)) +
  geom_histogram(
    # aes(y=..count../sum(..count..) * 10),
    aes(y=..count..),
    binwidth = 0.05,
    alpha = 0.5, fill = "blue", color = "black") +
  # geom_density(aes(y=..scaled..)) +
  # geom_density(aes(y=..count..)) +
  # labs(title = toupper(p.lvmodel), y = "prop") +
  # facet_grid(samplesize ~ nitem) +
  scale_x_continuous(n.breaks = 10) +
  theme_bw(base_size = 12)

a2 <- a1 %>%
  group_by(lvmodel, samplesize, nitem, rep) %>%
  summarise(
    overestimated = mean(err * overest),
    underestimated = mean(err * underest),
    prop_overest = mean(overest),
    prop_underest = mean(underest)
  ) %>%
  filter(lvmodel == "2pl")

a2 %>%
  select(-starts_with("prop")) %>%
  gather("est","value", -lvmodel, -rep, -samplesize, -nitem) %>%
  ggplot(aes(value)) +
  geom_histogram(aes(color = est), alpha = 0.7, fill = "transparent")



