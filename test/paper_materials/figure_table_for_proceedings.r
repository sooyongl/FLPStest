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
                           "    \U03B7_T and Y_C",
                           "    \U03B7_T and Y_T-Y_C",
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
    "\U1D749 \U2081",
    "\U1D749 \U2080",
    "",
    "",
    "",
    "",
    "")
)

my.doc <- read_docx()
wt <- cond_table %>%
  flextable() %>%
  set_caption(caption = "Table 1. Description of the simulation study") %>%
  # add_footer_lines(values = tablenote) %>%
  width(j = c(1,2), width = 3) %>%
  padding(., i=c(2:5), j=1, padding.left=20) %>%
  padding(., i=c(6:8), j=1, padding.left=40) %>%
  padding(., i=c(10:13), j=1, padding.left=20) %>%
  font(fontname = "Times", part = "all") %>%
  fontsize(size = 11, part = "header") %>%
  fontsize(size = 11, part = "body") %>%
  border(.,
         border.top = fp_border(color = "black", width = 1.5),
         border.bottom = fp_border(color = "black", width = 1.5),
         part = "header"
  ) %>%
  border(border.bottom = fp_border(color = "black", width = .75),
         i = c(1,8,9)
  ) %>%
  bold(i = c(1), part = 'header') %>%
  bold(i = c(1,9), part = 'body')

table_add(my.doc, wt)

cond_table <- cond_table %>%
  gt() %>%
  tab_header(title =md("Table 1. Description of the simulation study")) %>%
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


# cond_table %>%
#   gtsave(filename = "test/paper_materials/cond_table.html")

tex <- cond_table %>% as_latex()
cat(tex[[1]])

# Estimates Results -----------------------------------------
mpart_by_cond <- readRDS("test/paper_materials/mpart_by_cond.rds")
total_res3 <- readRDS("test/paper_materials/total_res3.rds")

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
  pull(rmse) %>% mean(.) %>% round(3)

ccove <- mpart_by_cond %>%
  filter(par_name0 == "eta0") %>%
  pull(coverage) %>% mean(.) %>% round(3)

#############################################################
aaa <- mpart_by_cond %>%
  filter(nitem == "100"& par_name0 != "eta0") %>%
  select(-absbias, -Rhatconv, -nitem, -Rhatmean, -mse)


aaa %>%
  group_by(samplesize) %>%
  summarise(mean(rmse))

aaa <- mpart_by_cond %>%
  filter(samplesize == "1000" & par_name0 != "eta0") %>%
  select(-absbias, -Rhatconv, -Rhatmean, -mse)

aaa %>%
  # group_by(nitem) %>%
  summarise(mean(rmse), mean(bias))
#############################################################

mpart_sample <- mpart_by_cond %>%
  filter(nitem == "100") %>%
  select(-absbias, -Rhatconv, -nitem, -Rhatmean, -mse) %>%
  mutate(rbias = abs(rbias)) %>%
  mutate_if(is.numeric, ~ format(.x, nsmall = 3)) %>%
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
  select(-rbias) %>%
  mutate_if(is.numeric, ~ format(.x, nsmall = 3)) %>%
  mutate_if(is.numeric, as.character) %>%
  pivot_wider(names_from = c(par_name0),
              values_from = c(bias, rmse, coverage)
              # values_fill = "-"
  ) %>%
  arrange(lvmodel, nitem) %>%
  mutate(samplesize = "1000", .before = nitem)

mpart_combined <- mpart_sample %>%
  bind_rows(mpart_nitem) %>%
  arrange(lvmodel) %>%
  collapse_col("lvmodel") %>%
  select(-ends_with("eta0")) %>%
  select(lvmodel, samplesize, nitem, bias_a,bias_b,bias_eta1, rmse_a,rmse_b, rmse_eta1, coverage_a,coverage_b,coverage_eta1)

mpart_combined <- mpart_combined %>%
  mutate(
    samplesize = as.numeric(samplesize),
    samplesize = samplesize/2,
    samplesize = as.character(samplesize)
  )

wt <- mpart_combined %>%
  mutate_all(~ case_when(is.na(.x) ~ "-", TRUE ~ .x))

# Header 1
header1 <- c()
header1[1:3] <- c("","","")
header1[4:dim(wt)[2]] <-
  rep(c("Bias","RMSE","Coverage"), each = 3)

# Header 2
header2 <- c()
header2[1:3] <- c("Model", "N","J")
header2[4:dim(wt)[2]] <- rep(c("Slope","Intercept","Eta1"), 3)

multiple_header <- data.frame(
  col_keys = names(wt),
  header1 = header1,
  header2 = header2,
  stringsAsFactors = FALSE )

ft_1 <- wt %>% flextable( . )
ft_1 <- set_header_df(ft_1, mapping = multiple_header, key = "col_keys" )
ft_1 <- merge_h(ft_1, part = "header")

ft_1 <- ft_1 %>%
  compose(part = "header", i=2, j = c(2),
          value = as_paragraph("N", as_sub("T"))) %>%

  compose(part = "header", i=2, j = c(4, 7, 10),
          value = as_paragraph("\U03B1")) %>%
  compose(part = "header", i=2, j = c(5, 8, 11),
          value = as_paragraph("\U0064")) %>%
  compose(part = "header", i=2, j = c(6, 9, 12),
          value = as_paragraph("\U03B7", as_sub("T")))

wt <- ft_1 %>%
  set_caption(caption = "Table 2. Results of measurement model across the sample sizes, the number of items, and the measurement models") %>%
  add_footer_lines(values = glue::glue("Note. N_T: Size of the treatment group. a: Slope parameter of the item response model. d: Intercept parameter of the item response model. η_T: Latent trait score of the treatment group. The trait estimates of the control group subjects showed average bias of {cbias}, RMSE of {crmse}, and coverage rate of {ccove}. The number of measurement items was fixed at 100 throughout.")) %>%

  font(fontname = "Times", part = "all") %>%
  fontsize(size = 11, part = "header") %>%
  fontsize(size = 11, part = "body") %>%
  border(.,
         border.top = fp_border(color = "black", width = 1.5),
         border.bottom = fp_border(color = "black", width = 1.5),
         part = "header"# partname of the table (all body header footer)
  ) %>%
  border_inner_h(.,
                 border = fp_border(color="transparent", width = 1)) %>%

  border(.,
         # border.top = fp_border(color = "black", width = 1.5),
         border.bottom = fp_border(color = "black", width = .75),
         i = c(3,6,9,12,15,18,21,24)

  ) %>%
  border(.,
         # border.top = fp_border(color = "black", width = 1.5),
         border.bottom = fp_border(color = "transparent", width = .75),
         i = c(3,9,15,21),
         j = 1

  ) %>%

  border(.,
         # border.top = fp_border(color = "black", width = 1.5),
         border.right = fp_border(color = "black", width = .75),
         j = c(3,6,9)

  ) %>%

  hline_bottom(.,
               part="body",
               border = fp_border(color="black", width = 1.5) ) %>%

  bold(i = c(1), part = 'header') %>%
  align(., align = 'center', part = "body") %>%
  align(align = "center", part = "header")

table_add(my.doc, wt)



mpart_combined <- mpart_combined %>%
  mpart_t(

    bias.label = "Bias",

    source.note = glue::glue("*Note.* N_T: Size of the treatment group. a: Slope parameter of the item response model. d: Intercept parameter of the item response model. η_T: Latent trait score of the treatment group. The trait estimates of the control group subjects showed average bias of {cbias}, RMSE of {crmse}, and coverage rate of {ccove}. The number of measurement items was fixed at 100 throughout."))




mpart_combined #%>% gtsave(filename = "test/paper_materials/mpart_combined_half.html")


tex <- mpart_combined %>% as_latex()
cat(tex[[1]])


## By combined ---------------------------------------
# unique(total_res3$par_name)
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
  select(-rbias) %>%
  pivot_wider(names_from = c(par_name),
              values_from = c(bias, rmse, coverage),
              values_fill = "-") %>%
  arrange(lvmodel, samplesize) %>%
  mutate(nitem = "100", .after = samplesize)

str_ni <- total_res3 %>%
  filter(samplesize == "1000") %>%
  filter(par_name %in% c("b0","b11", "a11","bu","by")) %>%
  select(lvmodel, nitem, par_name, bias, rbias, rmse, coverage) %>%

  mutate(rbias = abs(rbias)) %>%

  mutate_if(is.numeric, ~ format(.x, nsmall = 3)) %>%
  mutate_if(is.numeric, as.character) %>%
  select(-rbias) %>%
  pivot_wider(names_from = c(par_name),
              values_from = c(bias, rmse, coverage),
              values_fill = "-") %>%
  arrange(lvmodel, nitem) %>%
  mutate(samplesize = "1000", .before = nitem)


str_combined <- bind_rows(str_sample, str_ni) %>%
  arrange(lvmodel) %>%
  collapse_col("lvmodel")

ft_data <- str_combined
# Header 1
header1 <- c()
header1[1:3] <- c("","","")
header1[4:dim(ft_data)[2]] <-
  rep(c("Bias","RMSE","Coverage"), each = 5)

# Header 2
header2 <- c()
header2[1:3] <- c("Model", "N", "J")
header2[4:dim(ft_data)[2]] <- rep(c("tau0","tau1","a11","beta","gamma"), 3)

multiple_header <- data.frame(
  col_keys = names(ft_data),
  header1 = header1,
  header2 = header2,
  stringsAsFactors = FALSE )

ft_1 <- ft_data %>% flextable( . )
ft_1 <- set_header_df(ft_1, mapping = multiple_header, key = "col_keys" )
ft_1 <- merge_h(ft_1, part = "header")

ft_1 <- ft_1 %>%
  compose(part = "header", i=2, j = c(4, 9, 14),
          value = as_paragraph("\U1D749", as_sub("0"))) %>%
  compose(part = "header", i=2, j = c(5, 10, 15),
          value = as_paragraph("\U1D749", as_sub("1"))) %>%
  compose(part = "header", i=2, j = c(6, 11, 16),
          value = as_paragraph("\U1D74E")) %>%
  compose(part = "header", i=2, j = c(7, 12, 17),
          value = as_paragraph("\U1D737")) %>%
  compose(part = "header", i=2, j = c(8, 13, 18),
          value = as_paragraph("\U1D738"))

wt <- ft_1 %>%
  set_caption(caption = "Table 3. Results of structural model across the sample sizes, the number of items, and the measurement models") %>%
  add_footer_lines(values = glue::glue("Note. N: Sample size; J: The number of items;")) %>%

  font(fontname = "Times", part = "all") %>%
  fontsize(size = 11, part = "header") %>%
  fontsize(size = 11, part = "body") %>%
  border(.,
         border.top = fp_border(color = "black", width = 1.5),
         border.bottom = fp_border(color = "black", width = 1.5),
         part = "header"# partname of the table (all body header footer)
  ) %>%
  border_inner_h(.,
                 border = fp_border(color="transparent", width = 1)) %>%

  border(.,
         # border.top = fp_border(color = "black", width = 1.5),
         border.bottom = fp_border(color = "black", width = .75),
         i = c(3,6,9,12,15,18,21,24)

  ) %>%
  border(.,
         # border.top = fp_border(color = "black", width = 1.5),
         border.bottom = fp_border(color = "transparent", width = .75),
         i = c(3,9,15,21),
         j = 1

  ) %>%
  border(.,
         # border.top = fp_border(color = "black", width = 1.5),
         border.right = fp_border(color = "black", width = .75),
         j = c(3,8,13)

  ) %>%
  hline_bottom(.,
               part="body",
               border = fp_border(color="black", width = 1.5) ) %>%

  bold(i = c(1), part = 'header') %>%
  align(., align = 'center', part = "body") %>%
  align(align = "center", part = "header")


table_add(my.doc, wt, landscape = T)

str_combined <- str_combined %>%
  struc_t(
    bias.label = "Bias",
    source.note = glue::glue("*Note.* N: Sample size; J: The number of items;"))

tex <- str_combined %>% as_latex()
cat(mni_tex[[1]])




print(my.doc,
      target = "test/paper_materials/table_proceedings.docx")
