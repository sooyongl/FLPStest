# rm(list = ls())
library(tidyverse)
library(rstan)
library(ggpubr)
library(cowplot)
library(ggforce)
library(gt)

source("test/paper_materials/gt_template.r")
source("test/paper_materials/gtPaper.r")
source("test/paper_materials/table_function.r")
source("test/paper_materials/table_function_1.r")
# Table ----------------------------------------
# condition table ---------------------------------------------------------

cond_table <- tibble(
  `Simulation factors` = c("Manipulated conditions",
                           "\U0020 \U0020 Measurement model",
                           "  Sample size",
                           "  Number of items",
                           "  Strength of relationship",
                           "    \U03B7_t and Y_c",
                           "    \U03B7_t and Y_t-Y_c",
                           "    Z and Y",
                           "Fixed conditions",
                           "  Number of covariates",
                           "  Percentage of items administered",
                           "  Predictive power of \U03B7",
                           "  Predictive power of Y"),
  Values = c("",
             "Rasch, 2PL, GPCM, GRM",
             "500, 1000, 2000",
             "50, 100, 200",
             "",
             "U(0.1,0.3)",
             "U(-0.2,-0.1)",
             "U(0.2,0.4)",
             "",
             "2",
             "0.6",
             "0.5",
             "0.2"),
  Notation = c(
    "",
    "MM",
    "N",
    "J",
    "",
    "\U1D74E",
    "\U1D749_1",
    "\U1D749_0",
    "",
    "",
    "",
    "",
    "")
) %>%
  gt() %>%
  tab_header(title =md("Table 1. Simulation factors")) %>%
  tab_style(
    style = cell_text(align = "left", indent = px(20)),
    locations = cells_body(rows = c(2:5, 10:13),
                           columns = matches("Simulation factors"))
  ) %>%
  tab_style(
    style = cell_text(align = "left", indent = px(40)),
    locations = cells_body(rows = c(6:8),
                           columns = matches("Simulation factors"))
  ) %>%
  tab_options(
    # Heading -----------------------------
    # heading.align = "center",
    # heading.title.font.size = px(16),
    # heading.padding = NULL,

    # Inside Table border ------------------
    column_labels.border.top.width = px(3),
    column_labels.border.top.color = "black",

    #Remove border around table  -----------
    table_body.border.bottom.color = "black",
    table_body.border.bottom.width = px(3),

    # Outside Table border
    table.border.top.color = "transparent",
    table.border.bottom.color = "transparent",

    # Source note ----------------------------
    source_notes.font.size = 16,
    source_notes.padding = 12
  )


cond_table %>%
  gtsave(filename = "test/paper_materials/cond_table.html")

tex <- cond_table %>% as_latex()
cat(mni_tex[[1]])

# Convergence Results --------------------------

converged_results0 <- readRDS("test/paper_materials/converged_results0.rds")

total_number <- converged_results0 %>%
  distinct(lvmodel, samplesize, nitem, rep) %>%
  count(lvmodel, samplesize, nitem)

converged_results1 <-
  converged_results0 %>%
  filter(conv == "nonc") %>%
  group_by(lvmodel, samplesize, nitem, rep) %>%
  count(lvmodel, samplesize, nitem)

converged_results1 <- converged_results1 %>%
  group_by(lvmodel, samplesize, nitem) %>%
  count(lvmodel, samplesize, nitem)

cond_table <- crossing(lvmodel = c("Rasch","2PL","GRM","GPCM"),
                       samplesize = c("500","1000","2000"),
                       nitem = c("50","100","200")
) %>%
  filter(!(samplesize %in% c("500","2000")& nitem %in% c("50","200"))) %>%
  mutate(
    aaa = paste0(lvmodel, samplesize, nitem)
  )

aa <- converged_results1 %>%
  mutate(
    aaa = paste0(lvmodel, samplesize, nitem)
  )


dummy <- cond_table %>%
  filter(!aaa %in% aa$aaa) %>%
  mutate(n = 0) %>%
  select(-aaa)

converged_results2 <- converged_results1 %>%
  bind_rows(dummy) %>%
  left_join(total_number, by = c("lvmodel", "samplesize", "nitem")) %>%
  mutate(
    prop = round(n.x / n.y, 2),
    n = prop * 100
  ) %>%
  select(-n.x, -n.y, -prop) %>%
  arrange(lvmodel, samplesize, nitem) %>%
  mutate(
    lvmodel = factor(lvmodel, c("Rasch","2PL","GPCM", "GRM")),
    samplesize = factor(samplesize, c("500","1000","2000")),
    nitem = factor(nitem, c("50","100","200"))
  ) %>%

  set_names(c("MM","N","J", "Freq")) %>% ungroup() %>%
  arrange(MM, N, J) %>%
  pivot_wider(names_from = c(MM),
              values_from = c(Freq),
              names_glue = "{MM}.{.value}"
              # values_fill = "-"
  )

wt <- converged_results2 %>%
  wt_nonc(caption = "Table 2. Proportions of non-convergence MCMC results",
          tablenote = "Note. N: sample size; J: number of item")

my.doc <- read_docx()
table_add(my.doc, wt)

converged_results2 <- converged_results2 %>%
  gtPaper(hlines = c(1, 4), delim = ".",
          title = "**Table 2.** Proportions of non-convergence MCMC results",
          sn = "*Note.* N: sample size; J: number of item")

converged_results2 %>%
  gtsave(filename = "test/paper_materials/converged_results.html")

converged_results2 %>%
  gtsave(filename = "test/paper_materials/converged_results.png")

### coverged by parameter
cp <-
  converged_results0 %>%
  filter(conv == "nonc") %>% select(-conv) %>%

  # filter(lvmodel == "2PL", samplesize == "1000",
  #        nitem == "100", par_name == "lambda") %>%
  group_by(lvmodel, samplesize, nitem, par_name,rep) %>%
  count(lvmodel, samplesize, nitem, par_name) %>%
  # print(n=200)
  # ungroup() %>%
  mutate(
    n = as.numeric(n),
    nitem = as.numeric(as.character(nitem)),
    samplesize = as.numeric(as.character(samplesize)),
    prop =
      case_when(
        par_name == "lambda" ~ (n/(nitem-1)),
        par_name == "eta" ~ (n/samplesize),

        par_name == "tau" & lvmodel == "2PL" ~ (n/nitem),
        par_name == "tau" & lvmodel == "Rasch" ~ (n/nitem),
        #
        par_name == "tau" & lvmodel == "GPCM" ~ (n/(nitem*3)),
        par_name == "tau" & lvmodel == "GRM" ~ (n/(nitem*3)),

        TRUE ~ n
      )
  ) %>%
  group_by(lvmodel, samplesize, nitem, par_name) %>%
  count(lvmodel, samplesize, nitem) %>%
  # print(n=1000)
  mutate(
    # prop = round(prop, 3),
    # `Freq(Prop)` = paste0(n, " (",prop, ")"),
    Freq = n
  ) %>%
  mutate(
    lvmodel = factor(lvmodel, c("Rasch","2PL","GPCM","GRM")),
    samplesize = factor(samplesize, c("500","1000","2000")),
    nitem = factor(nitem, c("50","100","200")),
    par_name = factor(par_name, c(
      "b0", "b11","a11","bu","by","lambda","tau", "eta"))
  ) %>%
  select(lvmodel, samplesize, nitem, par_name, Freq) %>%
  set_names(c("MM","N","J", "Param","Freq")) %>% ungroup() %>%

  arrange(MM, N, J,Param) %>%
  pivot_wider(names_from = c(Param),
              values_from = c(Freq),
              names_glue = "{Param}.{.value}",
              values_fill = 0
  )

wt <- cp %>% default_wt(hlines = c(5, 10),
                        caption = "Table 2. Freq of non-convergence MCMC results by parameters",tablenote = "Note. N: sample size; J: number of item")


table_add(my.doc, wt)


cp1 <- cp %>%
  gtPaper(hlines = c(5, 10), delim = ".",
          title = "**Table 2.** Counts of non-convergence MCMC results",
          sn = "*Note.* N: sample size; J: number of item")

cp1 %>% gtsave(filename = "test/paper_materials/converged_results22.html")

# R-hat results --------------------------

mpart_by_rep <- readRDS("test/paper_materials/mpart_by_rep.rds")
total_res1 <- readRDS("test/paper_materials/total_res1.rds")

mpart_rhat <- mpart_by_rep %>% select(lvmodel, samplesize, nitem, par_name= par_name0, rep,Rhat=Rhatmean)

total_rhat <- total_res1 %>% select(lvmodel, samplesize, nitem, par_name, rep, Rhat)

rhat_comb <- bind_rows(mpart_rhat, total_rhat) %>%
  group_by(lvmodel, samplesize, nitem, par_name) %>%
  summarise(
    Rhat_mean = mean(Rhat),
    Rhat_sd  = sd(Rhat),
    Rhat_max = max(Rhat),
    Rhat_min = min(Rhat),
  ) %>%
  mutate(
    lvmodel = case_when(
      lvmodel == "rasch" ~ "Rasch",
      lvmodel == "2pl" ~ "2PL",
      lvmodel == "gpcm" ~ "GPCM",
      lvmodel == "grm" ~ "GRM",
      lvmodel == "2pllogn" ~ "2PL.LogN",
      lvmodel == "2plnormal" ~ "2PL.Normal",
      lvmodel == "2plunif" ~ "2PL.Unif"
    ),
    lvmodel = factor(lvmodel,
                     c("Rasch","2PL","GPCM", "GRM",
                       "2PL.LogN","2PL.Normal","2PL.Unif")),

    samplesize = factor(samplesize, c("500","1000","2000")),
    nitem = factor(nitem, c("50","100","200"))
  ) %>%
  filter(!lvmodel %in% c("2PL.LogN","2PL.Normal","2PL.Unif")) %>%
  ungroup() %>%
  mutate_if(is.numeric, ~ round(.x, 3))

gt_rhat_comb <- rhat_comb %>%
  select(-Rhat_sd) %>%
  pivot_wider(names_from = c(lvmodel),
              values_from = c(Rhat_mean, Rhat_max, Rhat_min),
              # names_sep = ".",
              names_glue = "{lvmodel}.{.value}",
              names_sort = T
              # ,values_fill = ""
  ) %>%
  arrange(samplesize, nitem) %>%
  rename("N" = samplesize, "J" = nitem, "PN" = par_name)

wt <- gt_rhat_comb %>%
  wt_rhat()

table_add(my.doc, wt)
print(my.doc, target = "test/paper_materials/related_to_convergence.docx")


gt_rhat_comb <- gt_rhat_comb %>%
  # collapse_col("lvmodel") %>%
  gtPaper(delim = ".",
          title = "**Table 1.** R-hat for all parmaeters across the simulation conditions",
          sn = "**Note.** N: measurement model; J: Number of items; PN: parameter name",
          hlines = c(11, 22, 33,44))

gt_rhat_comb %>%
  gtsave(filename = "test/paper_materials/gt_rhat_comb.html")

gt_rhat_comb %>%
  gtsave(filename = "test/paper_materials/gt_rhat_comb.png",
         vwidth = 1500)


# Estimates Results -----------------------------------------
mpart_by_cond <- readRDS("test/paper_materials/mpart_by_cond.rds")
total_res3 <- readRDS("test/paper_materials/total_res3.rds")

# mpart_by_cond <- readRDS("test/paper_materials/mpart_by_cond_convadd.rds")
# total_res3 <- readRDS("test/paper_materials/total_res3_convadd.rds")

mpart_by_cond <-
  mpart_by_cond %>%
  mutate(
    lvmodel = factor(lvmodel,
                     c("Rasch","2PL","GPCM", "GRM"))
  ) %>%
  mutate(par_name0 =str_replace(par_name0,"theta","eta")) %>%
  mutate(mse = round(rmse^2,3))

mean(mpart_by_cond[mpart_by_cond$par_name0 != "eta0", ]$bias)
mean(mpart_by_cond[mpart_by_cond$par_name0 != "eta0", ]$rmse)
mean(mpart_by_cond[mpart_by_cond$par_name0 != "eta0", ]$coverage)

cbias <- mpart_by_cond %>%
  filter(par_name0 == "eta0") %>%
  # filter(nitem == "100") %>%
  pull(bias) %>%
  mean(.) %>% round(3)

crbias <- mpart_by_cond %>%
  filter(par_name0 == "eta0") %>%
  # filter(nitem == "100") %>%
  pull(rbias) %>%
  mean(.) %>% round(3)

crmse <- mpart_by_cond %>%
  filter(par_name0 == "eta0") %>%
  # filter(nitem == "100") %>%
  pull(rmse) %>% mean(.) %>% round(3)

ccove <- mpart_by_cond %>%
  filter(par_name0 == "eta0") %>%
  # filter(nitem == "100") %>%
  pull(coverage) %>% mean(.) %>% round(3)

mpart_sample <- mpart_by_cond %>%
  filter(nitem == "100") %>%
  select(-absbias, -Rhatconv, -nitem, -Rhatmean, -mse) %>%
  mutate(rbias = abs(rbias)) %>%
  mutate_if(is.numeric, ~ format(.x, nsmall = 3)) %>%
  # mutate(bias = paste0(rbias, "(",bias, ")")) %>%
  select(-rbias) %>%
  mutate_if(is.numeric, as.character) %>%
  # rename_at(vars(starts_with("rbias")), ~ sub("rbias", "bias", .))  %>%
  pivot_wider(names_from = c(par_name0),
              values_from = c(bias, rmse, coverage)
              # ,values_fill = ""
  ) %>%
  arrange(lvmodel, samplesize) %>%
  mutate(
    # samplesize = case_when(
    #   samplesize == "500" ~ "250",
    #   samplesize == "1000" ~ "500",
    #   samplesize == "2000" ~ "1000"
    # ),
    # samplesize = factor(samplesize, levels = c("250","500","1000"))
  ) %>%
  mutate(nitem = "100", .after = samplesize)

mpart_nitem <- mpart_by_cond %>%
  filter(samplesize == "1000") %>%
  select(-absbias, -Rhatconv, -Rhatmean, -samplesize, -mse) %>%
  # mutate(bias = paste0(abs(rbias), "(",bias, ")")) %>%
  select(-rbias) %>%
  mutate_if(is.numeric, ~ format(.x, nsmall = 3)) %>%
  mutate_if(is.numeric, as.character) %>%

  # rename_at(vars(starts_with("rbias")), ~ sub("rbias", "bias", .))  %>%
  pivot_wider(names_from = c(par_name0),
              values_from = c(bias, rmse, coverage)
              # values_fill = "-"
  ) %>%
  arrange(lvmodel, nitem) %>%
  mutate(samplesize = "1000", .before = nitem)

mpart_combined <-mpart_sample %>%
  bind_rows(mpart_nitem) %>%
  arrange(lvmodel) %>%
  collapse_col("lvmodel") %>%
  select(-ends_with("eta0")) %>%
  select(lvmodel, samplesize, nitem, bias_a,bias_b,bias_eta1, rmse_a,rmse_b, rmse_eta1, coverage_a,coverage_b,coverage_eta1) %>%
  mpart_t(

    bias.label = "Bias",

    source.note = glue::glue("*Note.* MM: Measurement model; N: Sample size; J: The number of items; The control group does not have measurement observations, so eta1 for the treatment group is separately presented; On average, the control group produced {cbias} of bias, {crmse} of RMSE, and {ccove} of coverage rate for their latent abilities (eta0)"))

mpart_combined %>% gtsave(filename = "test/paper_materials/mpart_combined_half.html")
mpart_combined %>% gtsave(filename = "test/paper_materials/mpart_combined_half.png")

tex <- mpart_combined %>% as_latex()
cat(mni_tex[[1]])

## by sample ----------------
cbias <- mpart_by_cond %>%
  filter(par_name0 == "eta0") %>%
  filter(nitem == "100") %>%
  pull(bias) %>%
  mean(.) %>% round(3)

crbias <- mpart_by_cond %>%
  filter(par_name0 == "eta0") %>%
  filter(nitem == "100") %>%
  pull(rbias) %>%
  mean(.) %>% round(3)

crmse <- mpart_by_cond %>%
  filter(par_name0 == "eta0") %>%
  filter(nitem == "100") %>%
  pull(rmse) %>% mean(.) %>% round(3)

ccove <- mpart_by_cond %>%
  filter(par_name0 == "eta0") %>%
  filter(nitem == "100") %>%
  pull(coverage) %>% mean(.) %>% round(3)

mpart_sample <- mpart_by_cond %>%
  filter(nitem == "100") %>%
  select(-absbias, -Rhatconv, -nitem, -Rhatmean, -mse) %>%
  mutate(rbias = abs(rbias)) %>%
  mutate_if(is.numeric, ~ format(.x, nsmall = 3)) %>%
  # mutate(bias = paste0(rbias, "(",bias, ")")) %>%
  select(-rbias) %>%
  mutate_if(is.numeric, as.character) %>%
  # rename_at(vars(starts_with("rbias")), ~ sub("rbias", "bias", .))  %>%
  pivot_wider(names_from = c(par_name0),
              values_from = c(bias, rmse, coverage)
              # ,values_fill = ""
  ) %>%
  arrange(lvmodel, samplesize) %>%
  mutate(
    samplesize = case_when(
      samplesize == "500" ~ "250",
      samplesize == "1000" ~ "500",
      samplesize == "2000" ~ "1000"
    ),
    samplesize = factor(samplesize, levels = c("250","500","1000"))
  )

mpart_sample <-
  mpart_sample %>%
  collapse_col("lvmodel") %>%
  select(-ends_with("eta0")) %>%
  select(lvmodel, samplesize, bias_a,bias_b,bias_eta1, rmse_a,rmse_b, rmse_eta1, coverage_a,coverage_b,coverage_eta1)

wt <- mpart_sample %>%
  mutate_if(is.character, ~ case_when(is.na(.x) ~ "-", TRUE ~ .x) ) %>%
  wt_mpart_ss(
    caption = "Table 3. Results of measurement model by sample size",
    tablenote = glue::glue("Note. MM: Measurement model; N: Sample size;
           The number of items was fixed at 100;
           N represents the half of the total sample size, because the treatment group only have measurement observations;
           The control group does not have measurement observations, so eta1 for the treatment group is separately presented;
           On average, the control group produced {cbias} of bias, {crmse} of RMSE, and {ccove} of coverage rate for their latent abilities (eta0)"))

table_add(my.doc, wt)


mpart_sample <- mpart_sample %>%
  mpart_t_sample(

    bias.label = "Bias",

    source.note = glue::glue("*Note.* MM: Measurement model; N: Sample size;
           The number of items was fixed at 100;
           N represents the half of the total sample size, because the treatment group only have measurement observations;
           The control group does not have measurement observations, so eta1 for the treatment group is separately presented;
           On average, the control group produced {cbias} of bias, {crmse} of RMSE, and {ccove} of coverage rate for their latent abilities (eta0)"))


mpart_sample %>% gtsave(filename = "test/paper_materials/mpart_sample_half.html")
mpart_sample %>% gtsave(filename = "test/paper_materials/mpart_sample_half.png")

# ms_tex <- mpart_sample %>% as_latex()
# cat(ms_tex[[1]])


## by nitem
cbias <- mpart_by_cond %>%
  filter(par_name0 == "eta0") %>%
  filter(samplesize == "1000") %>%
  pull(bias) %>%
  mean(.) %>% round(3)

crbias <- mpart_by_cond %>%
  filter(par_name0 == "eta0") %>%
  filter(samplesize == "1000") %>%
  pull(rbias) %>%
  mean(.) %>% round(3)

crmse <- mpart_by_cond %>%
  filter(par_name0 == "eta0") %>%
  filter(samplesize == "1000") %>%
  pull(rmse) %>% mean(.) %>% round(3)

ccove <- mpart_by_cond %>%
  filter(par_name0 == "eta0") %>%
  filter(samplesize == "1000") %>%
  pull(coverage) %>% mean(.) %>% round(3)

mpart_nitem <- mpart_by_cond %>%
  filter(samplesize == "1000") %>%
  select(-absbias, -Rhatconv, -Rhatmean, -samplesize, -mse) %>%
  # mutate(bias = paste0(abs(rbias), "(",bias, ")")) %>%
  select(-rbias) %>%
  mutate_if(is.numeric, ~ format(.x, nsmall = 3)) %>%
  mutate_if(is.numeric, as.character) %>%

  # rename_at(vars(starts_with("rbias")), ~ sub("rbias", "bias", .))  %>%
  pivot_wider(names_from = c(par_name0),
              values_from = c(bias, rmse, coverage)
              # values_fill = "-"
  ) %>%
  arrange(lvmodel, nitem)


mpart_nitem <- mpart_nitem %>%
  collapse_col("lvmodel") %>%
  select(-ends_with("eta0")) %>%
  select(lvmodel, nitem, bias_a,bias_b,bias_eta1, rmse_a,rmse_b, rmse_eta1, coverage_a,coverage_b,coverage_eta1)

wt <- mpart_nitem %>%
  mutate_if(is.character, ~ case_when(is.na(.x) ~ "-", TRUE ~ .x) ) %>%
  wt_mpart_ss(
    header_name = "J",
    caption = "Table 4. Results of measurement model by the number of items",
    tablenote = glue::glue("Note. MM: Measurement model; J: Number of items;
                       The total sample size was fixed at 1000;
                       The control group does not have measurement observations, so the half of the sample was actually used for estimating the measurement model;
                       On average, the control group produced {cbias} of bias, {crmse} of RMSE, and {ccove} of coverage rate for their latent abilities (eta0)"))

table_add(my.doc, wt)

mpart_nitem <- mpart_nitem %>%
  mpart_t_nitem(
    bias.label = "Bias",
    source.note = glue::glue("*Note.* MM: Measurement model; J: Number of items;
                       The total sample size was fixed at 1000;
                       The control group does not have measurement observations, so the half of the sample was actually used for estimating the measurement model;
                       On average, the control group produced {cbias} of bias, {crmse} of RMSE, and {ccove} of coverage rate for their latent abilities (eta0)"))

mpart_nitem %>% gtsave(filename = "test/paper_materials/mpart_nitem.html")
mpart_nitem %>% gtsave(filename = "test/paper_materials/mpart_nitem.png")

# mni_tex <- mpart_nitem %>% as_latex()
# cat(mni_tex[[1]])

## By combined ---------------------------------------
unique(total_res3$par_name)
total_res3 <- total_res3 %>% mutate(mse = round(rmse^2,3)) %>%
  mutate(
    lvmodel = factor(lvmodel,
                     c("Rasch","2PL","GPCM", "GRM"))
  )

str_sample <- total_res3 %>%
  filter(nitem == "100") %>%
  filter(par_name %in% c("b0","b11", "a11","bu","by")) %>%
  select(lvmodel, samplesize, par_name, bias, rbias,
         rmse, coverage) %>%
  mutate(rbias = abs(rbias)) %>%
  mutate_if(is.numeric, ~ format(.x, nsmall = 3)) %>%

  mutate_if(is.numeric, as.character) %>%

  # mutate(bias = paste0(rbias, "(",bias, ")")) %>%
  select(-rbias) %>%
  # rename_at(vars(starts_with("rbias")), ~ sub("rbias", "bias", .))  %>%
  pivot_wider(names_from = c(par_name),
              values_from = c(bias, rmse, coverage),
              values_fill = "-") %>%
  arrange(lvmodel, samplesize) %>%
  mutate(nitem = "100", .after = samplesize)

# total_res3 %>%
#   filter(par_name == "a11") %>%
#   select(lvmodel, samplesize, nitem, coverage) %>%
#   print(n = 1000)
#   select(starts_with("coverage"))

str_ni <- total_res3 %>%
  filter(samplesize == "1000") %>%
  # mutate(mse = sqrt(mse)) %>%
  filter(par_name %in% c("b0","b11", "a11","bu","by")) %>%
  select(lvmodel, nitem, par_name, bias, rbias, rmse, coverage) %>%

  mutate(rbias = abs(rbias)) %>%

  mutate_if(is.numeric, ~ format(.x, nsmall = 3)) %>%
  mutate_if(is.numeric, as.character) %>%

  # mutate(bias = paste0(rbias, "(",bias, ")")) %>%
  select(-rbias) %>%

  # rename_at(vars(starts_with("rbias")), ~ sub("rbias", "bias", .))  %>%
  pivot_wider(names_from = c(par_name),
              values_from = c(bias, rmse, coverage),
              values_fill = "-") %>%
  arrange(lvmodel, nitem) %>%
  mutate(samplesize = "1000", .before = nitem)


str_combined <- bind_rows(str_sample, str_ni) %>%
  arrange(lvmodel) %>%
  collapse_col("lvmodel") %>%
  struc_t(
    bias.label = "Bias")


str_combined %>% gtsave(filename = "test/paper_materials/str_combined.html")
str_combined %>% gtsave(filename = "test/paper_materials/str_combined.png")

# sample ------------------------------------------------------------------
str_sample <- total_res3 %>%
  filter(nitem == "100") %>%
  filter(par_name %in% c("b0","b11", "a11","bu","by")) %>%
  select(lvmodel, samplesize, par_name, bias, rbias,
         rmse, coverage) %>%
  mutate(rbias = abs(rbias)) %>%
  mutate_if(is.numeric, ~ format(.x, nsmall = 3)) %>%

  mutate_if(is.numeric, as.character) %>%

  # mutate(bias = paste0(rbias, "(",bias, ")")) %>%
  select(-rbias) %>%
  # rename_at(vars(starts_with("rbias")), ~ sub("rbias", "bias", .))  %>%
  pivot_wider(names_from = c(par_name),
              values_from = c(bias, rmse, coverage),
              values_fill = "-") %>%
  arrange(lvmodel, samplesize) %>%
  collapse_col("lvmodel")

wt <- str_sample %>%
  wt_struc_ss(
    header_name = "N",
    caption = "Table 5. Results of structural model by sample size",
    tablenote = "Note. MM: Measurement model; N: Sample size; The number of items was fixed at 100"
  )

my.doc <- body_end_section_portrait(my.doc)
table_add(my.doc, wt)


# body_end_section_portrait()

str_sample <- str_sample %>%
  struc_t_sample(
    bias.label = "Bias")

str_sample %>% gtsave(filename = "test/paper_materials/str_sample.html")
str_sample %>% gtsave(filename = "test/paper_materials/str_sample.png")

# ss_tex <- str_sample %>% as_latex()
# cat(ss_tex[[1]])

## By Nitem
str_ni <- total_res3 %>%
  filter(samplesize == "1000") %>%
  # mutate(mse = sqrt(mse)) %>%
  filter(par_name %in% c("b0","b11", "a11","bu","by")) %>%
  select(lvmodel, nitem, par_name, bias, rbias, rmse, coverage) %>%

  mutate(rbias = abs(rbias)) %>%

  mutate_if(is.numeric, ~ format(.x, nsmall = 3)) %>%
  mutate_if(is.numeric, as.character) %>%

  # mutate(bias = paste0(rbias, "(",bias, ")")) %>%
  select(-rbias) %>%

  # rename_at(vars(starts_with("rbias")), ~ sub("rbias", "bias", .))  %>%
  pivot_wider(names_from = c(par_name),
              values_from = c(bias, rmse, coverage),
              values_fill = "-") %>%
  arrange(lvmodel, nitem) %>%
  collapse_col("lvmodel")

wt <- str_ni %>%
  wt_struc_ss(
    header_name = "J",
    caption = "Table 6. Results of structural model by the number of items",
    tablenote = "Note. MM: Measurement model; J: Number of items; The sample size was fixed at 1000"
  )

table_add(my.doc, wt)
my.doc <- body_end_section_landscape(my.doc)

print(my.doc, target = "test/paper_materials/results_table.docx")

str_ni <- str_ni %>%
  struc_t_nitem(    bias.label = "Bias")

str_ni %>% gtsave(filename = "test/paper_materials/str_nitem.html")
str_ni %>% gtsave(filename = "test/paper_materials/str_nitem.png")

# sni_tex <- str_ni %>% as_latex()
# cat(sni_tex[[1]])


## Pilot Study -------------------------------------------------
# ggtexttable()

mpart_by_cond <- readRDS("test/paper_materials/mpart_by_cond.rds")
total_res3 <- readRDS("test/paper_materials/total_res3.rds")

pilot.measure <- mpart_by_cond %>%
  filter(nitem == "50", samplesize == "500") %>%
  select(-Rhatmean, -Rhatconv, -absbias, -rbias) %>%
  # mutate_if(is.numeric, ~ format(.x, nsmall = 3)) %>%
  # mutate_if(is.numeric, as.character) %>%
  pivot_wider(names_from = c(par_name0),
              values_from = c(bias, rmse, coverage)
              # ,values_fill = ""
  ) %>%
  arrange(lvmodel) %>%
  separate(lvmodel,
           c("MM","Prior"), sep = "\\.") %>%
  set_names(c(
    "MM","Prior","N","J",
    "Bias_eta0","Bias_eta1","Bias_a","Bias_b",
    "RMSE_eta0","RMSE_eta1","RMSE_a","RMSE_b",
    "Coverage_eta0","Coverage_eta1","Coverage_a","Coverage_b"
  )) %>%
  select(MM, Prior, N, J,
         Bias_a, Bias_b, Bias_eta1, Bias_eta0,
         RMSE_a, Bias_b, RMSE_eta1, RMSE_eta0,
         Coverage_a, Bias_b, Coverage_eta1, Coverage_eta0)

pilot.struc <- total_res3 %>%
  filter(nitem == "50", samplesize == "500") %>%
  filter(!par_name %in% c("b00","by", "bu","tau","eta","lambda")) %>%

  select(lvmodel, samplesize, nitem, par_name, bias, rmse, coverage) %>%

  mutate_if(is.numeric, ~ format(.x, nsmall = 3)) %>%
  mutate_if(is.numeric, as.character) %>%
  pivot_wider(names_from = c(par_name),
              values_from = c(bias, rmse, coverage),
              values_fill = "-") %>%
  arrange(lvmodel) %>%
  separate(lvmodel,
           c("MM","Prior"), sep = "\\.") %>%
  set_names(c(
    "MM","Prior","N","J",
    "Bias_tau0","Bias_tau1","Bias_omega",
    "RMSE_tau0","RMSE_tau1","RMSE_omega",
    "Coverage_tau0","Coverage_tau1","Coverage_omega"
  ))

pm <- pilot.measure %>%
  gtPaper(
    # condition = c("MM","Prior","SS","NI"),
    delim = "_",
    title = "**Table 1.** Summary of Pilot Study: measurement parts",
    sn = "**_Note._** MM: measurement model; N: sample size; J: number of items; a: discrimination; b: difficulty; eta1: latent factor scores for the treatment group; eta0: latent factor scores for the control group")

pm %>% gtsave(filename = "test/paper_materials/pm.html")
pm %>% gtsave(filename = "test/paper_materials/pm.png")

# webshot::webshot("test/paper_materials/pm.html",
#                  "test/paper_materials/pm.png", vwidth = 1500, vheight = 600)

ps <- pilot.struc %>%
  gtPaper(
    delim = "_",
    title = "**Table 2.** Summary of Pilot Study: structural parts",
    sn = "**_Note._** MM: measurement model; N: sample size; J: number of items; a: discrimination; b: difficulty; eta1: latent factor scores for the treatment group; eta0: latent factor scores for the control group")

ps %>% gtsave(filename = "test/paper_materials/ps.html")
ps %>% gtsave(filename = "test/paper_materials/ps.png")

# webshot::webshot("test/paper_materials/ps.html",
#                  "test/paper_materials/ps.png", vwidth = 1500, vheight = 600)

# pm %>% gtsave("pm.png", path = "test/paper_materials")
# ps %>% gtsave("ps.png", path = "test/paper_materials")
# library(cowplot)
# pm <- ggdraw() + draw_image("test/paper_materials/pm.png", scale = 0.8)
# ps <- ggdraw() + draw_image("test/paper_materials/ps.png", scale = 0.8)
# plot_grid(p111, p112)

pm_tex <- pm %>% as_latex()
cat(pm_tex[[1]])

ps_tex <- ps %>% as_latex()
cat(ps_tex[[1]])

# ps <- pilot.struc %>% ggtexttable(rows = NULL)
# pm <- pilot.measure %>% ggtexttable(rows = NULL)

ggarrange(ps, pm, ncol = 1)

### Save as PDF
# library(magick)
# fl = list.files("test/paper_materials", full.names = TRUE, pattern = '.png')
# # magick
# main <- fl[c(1,4,3,8,7)]
# supp <- fl[-c(1,4,3,8,7)]
#
# img = image_read(main) # read from vector of paths
# img2 = image_append(img, stack = TRUE) # places pics above one another
#
# image_write(img2, format = "pdf", file.path("test/paper_materials",
#                             'main_tables.pdf'))
#
#
# img = image_read(supp) # read from vector of paths
# img2 = image_append(img, stack = TRUE) # places pics above one another
#
# image_write(img2, format = "pdf", file.path("test/paper_materials",
#                             'supp_tables.pdf'))

# --------------------------------------------------------------------
library(ggforce); library(cowplot); library(ggpubr); library(tidyverse)

total_res1 <- total_res3 <- readRDS("test/paper_materials/total_res1.rds")

plotdata <- total_res1 %>%
  # filter(nitem == "100") %>%
  filter(!par_name %in% c("eta","tau","lambda")) %>%
  mutate(
    lvmodel = case_when(
      lvmodel == "rasch" ~ "Rasch",
      lvmodel == "2pl" ~ "2PL",
      lvmodel == "gpcm" ~ "GPCM",
      lvmodel == "grm" ~ "GRM",
      lvmodel == "2pllogn" ~ "2PL.LogN",
      lvmodel == "2plnormal" ~ "2PL.Normal",
      lvmodel == "2plunif" ~ "2PL.Unif"
    ),
    lvmodel = factor(lvmodel,
                     c("Rasch","2PL","GPCM", "GRM",
                       "2PL.LogN","2PL.Normal","2PL.Unif")),

    samplesize = factor(samplesize, c("500","1000","2000")),
    nitem = factor(nitem, c("50","100","200"))
  ) %>%
  filter(lvmodel %in% c("Rasch","2PL","GRM","GPCM"))

mean_data <- plotdata %>%
  filter(!par_name %in% c("eta","tau","lambda")) %>%
  group_by(lvmodel, samplesize, nitem, par_name) %>%
  summarise(bias = round(mean(err),4))


plotdata <- plotdata %>%
  group_by(lvmodel, samplesize, nitem, par_name) %>%
  slice_sample(n = 100)

b11_nitem100 <- plotdata %>%

  # filter(par_name %in% c("b11", "b0")) %>%
  filter(nitem %in% c("100")) %>%
  ggplot(aes(
    x = lvmodel, y = bias, color = samplesize,
    shape = samplesize)) +

  geom_violin() +
  geom_sina(alpha = 0.2) +
  geom_hline(yintercept = 0) +

  # stat_summary(
  #   aes(color = samplesize, shape = samplesize),
  #   geom = "point",
  #   fun = "mean",
  #
  #   size = 3,
  #   # shape = 24
  #   # fill = "red",
  #   # position = position_dodge(width = 0.7)
  # )
geom_point(
  data = mean_data %>%
    # filter(par_name %in% c("b11","b0")) %>%
    filter(nitem %in% c("100")),
  aes(
    x = lvmodel, y = bias, #color = samplesize,
    shape = samplesize),
  # color = "red",
  size = 4,
  position = position_dodge(width = 0.1)
) +
  facet_wrap(. ~ par_name) +
  labs(y = "", x = "") +
  cowplot::theme_minimal_hgrid() +
  theme(plot.margin = margin(1, 0, 0, 0, "cm"))

b11_ss1000 <- plotdata %>%
  filter(par_name %in% c("b11")) %>%
  filter(samplesize %in% c("1000")) %>%
  ggplot(aes(x = lvmodel, y = bias)) +

  geom_violin() +
  geom_sina(alpha = 0.2) +
  geom_hline(yintercept = 0) +

  geom_point(
    data = mean_data %>%
      filter(par_name %in% c("b11")) %>%
      filter(samplesize %in% c("1000")),
    aes(x = lvmodel, y = bias),
    shape = 17,
    color = "red",
    size = 4
  ) +
  facet_grid(. ~ nitem) +
  labs(y = "", x = "") +
  cowplot::theme_minimal_hgrid() +
  theme(plot.margin = margin(1, 0, 0, 0, "cm"))


pp <- ggarrange(b11_nitem100, b11_ss1000, nrow = 2,
                labels = c("By sample size", "By the number of items"))

annotate_figure(pp,
                bottom = text_grob("Measurement Model", size = 14),
                left = text_grob("Bias of Tau1", rot = 90))
#
# scale_y_continuous(
# don't expand y scale at the lower end
# expand = expansion(mult = c(0, 0.05))
# )
# geom_col(fill = "#6297E770") +
#   scale_y_continuous(
#     expand = expansion(mult = c(0, 0.05)),
#     position = "right"
#   ) +
#   theme_minimal_hgrid(11, rel_small = 1) +
#   theme(
#     panel.grid.major = element_line(color = "#6297E770"),
#     axis.line.x = element_blank(),
#     axis.text.x = element_blank(),
#     axis.title.x = element_blank(),
#     axis.ticks = element_blank(),
#     axis.ticks.length = grid::unit(0, "pt"),
#     axis.text.y = element_text(color = "#6297E7"),
#     axis.title.y = element_text(color = "#6297E7")
#   )
# scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
# theme_minimal_hgrid(12) +
#   panel_border(color = "black") +
#   theme(strip.background = element_rect(fill = "gray80"))


# scale_x_discrete(guide = guide_axis(n.dodge=2))


