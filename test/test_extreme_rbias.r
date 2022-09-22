library(tidyverse)

mpart_by_rep <- readRDS("test/paper_materials/mpart_by_rep.rds")


mpart_by_rep %>%
  filter(lvmodel == "gpcm", samplesize == "1000", par_name0 == "tau") %>%
  arrange(-abs(rbias))
  ggplot() +
  geom_histogram(aes(rbias))


mpart_raw <- readRDS(paste0("test/shinydata/", "0723", "_mpart.rds"))

mpart_sel <- mpart_raw %>%
  filter(str_detect(cond, "gpcm"),
         str_detect(cond, "_1000_"),
         str_detect(cond, "_100_"),
         str_detect(par_name, "tau")
         ) %>%
  select(par_name, true_param, mean, err, rerr)

mpart_sel %>%
  filter(abs(err) < 0.1, abs(rerr) > 100)
  # arrange(-abs(rerr))

