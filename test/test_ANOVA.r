library(tidyverse)
library(gt)

source("test/paper_materials/gt_template.r")
source("test/paper_materials/gtPaper.r")
source("test/paper_materials/table_function.r")
source("test/paper_materials/table_function_1.r")

# mpart0 <- readRDS("test/paper_materials/mpart0.rds")
# mpart0 %>%
#   group_by(lvmodel, samplesize, nitem, par_name0) %>%
#   summarise(
#     bias   = mean(err),
#     rbias  = mean(rerr),
#
#     absbias = mean(abs(err)),
#     rmse = sqrt(mean(err^2)),
#     coverage = mean(coverage),
#
#     Rhatconv = mean(converge_Rhat),
#     Rhatmean = mean(Rhat)
#   ) %>%
#   filter(lvmodel == "2pl", par_name0 == "lambda")


total_res1 <- readRDS("test/paper_materials/total_res1.rds") %>%
  mutate(
    lvmodel = as.character(lvmodel),
    lvmodel = case_when(lvmodel == "rasch" ~ "Rasch",
                        lvmodel == "2pl" ~ "2PL",
                        lvmodel == "gpcm" ~ "GPCM",
                        lvmodel == "grm" ~ "GRM",
                        TRUE ~ lvmodel),
    lvmodel = factor(lvmodel, levels = c("Rasch","2PL","GPCM","GRM"))
  )
total_res2 <- readRDS("test/paper_materials/total_res2.rds") %>%
  mutate(
    lvmodel = as.character(lvmodel),
    lvmodel = case_when(lvmodel == "rasch" ~ "Rasch",
                        lvmodel == "2pl" ~ "2PL",
                        lvmodel == "gpcm" ~ "GPCM",
                        lvmodel == "grm" ~ "GRM",
                        TRUE ~ lvmodel),
    lvmodel = factor(lvmodel, levels = c("Rasch","2PL","GPCM","GRM"))
  )
total_res3 <- readRDS("test/paper_materials/total_res3.rds")
# fs::dir_ls("test/paper_materials")

mpart_by_rep <- readRDS("test/paper_materials/mpart_by_rep.rds") %>%
  rename(par_name = par_name0) %>%
  mutate(par_name =
           case_when(
             par_name == "lambda" ~ "a",
             par_name == "tau" ~ "b",
             TRUE ~ par_name
           )) %>%
  mutate(
    lvmodel = as.character(lvmodel),
    lvmodel = case_when(lvmodel == "rasch" ~ "Rasch",
                        lvmodel == "2pl" ~ "2PL",
                        lvmodel == "gpcm" ~ "GPCM",
                        lvmodel == "grm" ~ "GRM",
                        TRUE ~ lvmodel),
    lvmodel = factor(lvmodel, levels = c("Rasch","2PL","GPCM","GRM"))
  )


unique(mpart_by_rep$par_name)

mpart_by_cond <- readRDS("test/paper_materials/mpart_by_cond.rds") %>%
  rename(par_name = par_name0)

unique(total_res1$par_name)

total_res1 <-
  total_res1 %>%
  bind_rows(mpart_by_rep) %>%
  mutate(
    par_name = as.character(par_name),
    par_name = factor(
      par_name, levels = c("b00","b0", "b11", "a11", "bu", "by", "a", "b", "eta0","eta1")
    )
  )

unique(total_res1$par_name)

total_res3 <-
  total_res3 %>%
  bind_rows(mpart_by_cond) %>%
  mutate(
    par_name = as.character(par_name),
    par_name = factor(
      par_name, levels = c("b00","b0", "b11", "a11", "bu", "by", "a", "b", "eta0","eta1")
    )
  )

# R program to illustrate
# MANOVA test

# # Import required library
# library(dplyr)
#
# # Taking iris data set
# myData = iris
#
# # Show a random sample
# set.seed(1234)
# dplyr::sample_n(myData, 10)
#
# sepal = iris$Sepal.Length
# petal = iris$Petal.Length
#
# # MANOVA test
# result = manova(
#   cbind(Sepal.Length,
#         Petal.Length,
#         Sepal.Width,
#         Petal.Width) ~ Species,
#   data = iris)
# summary(result)

posthoc_tukey <- function(a1, what = "model") {
  TukeyHSD(a1, what)
}

effectsize <- function(a1, partialeta = F) {

  tidy_ad_aov <- broom::tidy(a1) %>%
    mutate_if(is.numeric, round, 3)
  eta <- effectsize::eta_squared(a1, partial = partialeta, ci = .95)

  if(partialeta){
    eta0 <- eta$Eta2_partial
  } else {
    eta0 <- eta$Eta2
  }

  tidy_ad_aov %>% filter(term != "Residuals") %>%
    bind_cols(data.frame(Eta2 = eta0, cil=eta$CI_low, cih =eta$CI_high))
}
# sqrt(0.001)
# sqrt(0.0001)
runanova <- function(o1, lvmodel0=NA, par_name0, type = "ncombined") {

  if(type != "combined") {
    foranova <- o1 %>%
      filter(lvmodel %in% c("Rasch","2PL","GRM","GPCM")) %>%
      filter(par_name == par_name0)
  } else {
    foranova <- o1 %>%
      mutate(rmse = err^2) %>%
      filter(lvmodel %in% c("Rasch","2PL","GRM","GPCM")) %>%
      filter(par_name == par_name0)
  }

  # o1 %>%
  #   filter(par_name == "eta0") %>%
  #   group_by(lvmodel, samplesize, nitem, par_name) %>%
  #   summarise(
  #     mse= mean(bias^2),
  #     rmse = sqrt(mse)
  #   )


  ano.formula <- paste0("rmse", " ~ samplesize + nitem + lvmodel")
  # ano.formula <- paste0("bias", " ~ samplesize + nitem")
  # ano.formula <- paste0("rbias", " ~ model * sample_size * num_of_growth * num_of_timepoint * ICC_size")
  aov(as.formula(ano.formula), data=foranova)
}

mk_ano_table <- function(data, par_name0, posthoc = T, type = "ncombined") {

  fit <- data %>%
    runanova(par_name0 = par_name0, type = type)

  fit.es <- effectsize(fit)
  fit.es <- fit.es[ , c("term","Eta2")] %>% bind_rows(data.frame(term="Residuals", Eta2=0))

  fit.table <- broom::tidy(fit) %>% left_join(fit.es, by = "term") %>%
    mutate_if(is.numeric, round, 4)

  anov1 <- fit.table %>%
    mutate(term = c("N","J","MM","Residuals"))


  anov1 <- anov1 %>%
    mutate_if(is.numeric, round, 3) %>%
    flextable( . ) %>%
    set_caption(caption = glue::glue("Table. ANOVA for {par_name0}")) %>%
    add_footer_lines(values = "Note. N: Sample size; J: Number of items; MM: Measurement model")

  # anov1 <- anov1 %>%
  #   gt_template(title = glue::glue("{par_name0} ANOVA"),
  #               sourcenotes = "*Note*. N: Sample size; J: Number of items; MM: Measurement model") %>%
  #   fmt_number(columns = c(3:7), decimals = 3)

  if(posthoc) {

    posth_fit <- TukeyHSD(fit)
    posth1 <- broom::tidy(posth_fit)

    posth1 <- posth1 %>%
      mutate_if(is.numeric, round, 3) %>%
      flextable( . ) %>%
      set_caption(glue::glue("Table. ANOVA post-hoc for {par_name0}"))

    # posth1 <- posth1 %>%
    #   gt_template(title = glue::glue("{par_name0} ANOVA post-hoc")) %>%
    #   fmt_number(columns = c(4:7), decimals = 3)

    list(anov_table = anov1, posthoc_table = posth1)

  } else {

    anov1
  }
}

# unique(total_res1$par_name)
# total_res1 %>%
#   filter(lvmodel == "2PL") %>%
#   filter(par_name == "a") %>%
#   arrange(samplesize, nitem) %>%
#   select(lvmodel, samplesize, nitem, par_name, rep, err, bias, rmse) %>%
#   group_by(lvmodel, samplesize, nitem, par_name) %>%
#   summarise(
#     rmse_0 = sqrt(mean(bias^2)),
#     rmse = mean(rmse)
#   )
#
# total_res3 %>%
#   filter(lvmodel == "2PL") %>%
#   filter(par_name == "a") %>%
#   arrange(samplesize, nitem)

ff <- total_res1 %>% runanova(par_name0 = "b11", type = "combined")
summary(ff)
effectsize(ff)

ff <- total_res3 %>% runanova(par_name0 = "b11")
summary(ff)
effectsize(ff)

a <-
  total_res1 %>%
  # total_res3 %>%
  mk_ano_table(par_name0 = "a")
# a[[1]]; a[[2]]
# saveRDS(a, "test/paper_materials/ANOVA_rmse_a.rds")
b <-
  total_res1 %>%
  # total_res3 %>%
  mk_ano_table(par_name0 = "b")
# b[[1]]
# saveRDS(b, "test/paper_materials/ANOVA_rmse_b.rds")
eta1 <-
  total_res1 %>%
  # total_res3 %>%
  mk_ano_table(par_name0 = "eta1")
# eta1[[1]]
# saveRDS(eta1, "test/paper_materials/ANOVA_rmse_eta1.rds")

b0 <-
  # total_res1 %>%
  total_res3 %>%
  mk_ano_table(par_name0 = "b0")
# b0[[1]]
# saveRDS(b0, "test/paper_materials/ANOVA_rmse_b0.rds")

b11 <-
  # total_res1 %>%
  total_res3 %>%
  mk_ano_table(par_name0 = "b11")
# b11[[1]]
# saveRDS(b11, "test/paper_materials/ANOVA_rmse_b11.rds")

a11 <-
  # total_res1 %>%
  total_res3 %>%
  mk_ano_table(par_name0 = "a11")
# a11[[1]]
# saveRDS(a11, "test/paper_materials/ANOVA_rmse_a11.rds")

bu <-
  # total_res1 %>%
  total_res3 %>%
  mk_ano_table(par_name0 = "bu")
# bu[[1]]
# saveRDS(bu, "test/paper_materials/ANOVA_rmse_bu.rds")

by <-
  # total_res1 %>%
  total_res3 %>%
  mk_ano_table(par_name0 = "by")
# by[[1]]
# saveRDS(by, "test/paper_materials/ANOVA_rmse_by.rds")


my.doc <- read_docx()

table_add(my.doc, a[[1]])
table_add(my.doc, b[[1]])
table_add(my.doc, eta1[[1]])
table_add(my.doc, b0[[1]])
table_add(my.doc, b11[[1]])
table_add(my.doc, a11[[1]])
table_add(my.doc, bu[[1]])
table_add(my.doc, by[[1]])

print(my.doc, target = "test/paper_materials/results_ANOVA_table.docx")


# ---------------------------------------------------------------------
# b11 <- total_res3 %>%
#   mk_ano_table(par_name0 = "b11")
# saveRDS(b11, "test/paper_materials/ANOVA_res_b11.rds")
# b0 <- total_res3 %>%
#   mk_ano_table(par_name0 = "b0")
# saveRDS(b0, "test/paper_materials/ANOVA_res_b0.rds")
# ####
#
# b11 <- total_res3 %>%
#   # filter(lvmodel %in% c("Rasch","2PL","GRM","GPCM")) %>%
#   # filter(par_name == "b11") %>%
#   runanova(par_name0 = "b11")
#
# summary(b11)
#
# effectsize(b11, partialeta = F)
# b11.es <- effectsize(b11)
# b11.es <- b11.es[ , c("term","Eta2")] %>% bind_rows(data.frame(term="Residuals", Eta2=0))
#
# b11.table <- broom::tidy(b11) %>% left_join(b11.es, by = "term") %>%
#   mutate_if(is.numeric, round, 4)
#
# b11.table %>%
#   mutate(term = c("N","J","MM","Residuals")) %>%
#   gt_template(title = "tau1 ANOVA",
#               sourcenotes = "*Note*. N: Sample size; J: Number of items; MM: Measurement model") %>%
#   fmt_number(columns = c(3:7), decimals = 3) %>%
#   gtsave(filename = "test/paper_materials/ANOVA_tau1.png")
# ####
# b0 <- total_res3 %>%
#   runanova(par_name0 = "b0")
# summary(b0)
#
# b0.es <- effectsize(b0)
# b0.es <- b0.es[ , c("term","Eta2")] %>% bind_rows(data.frame(term="Residuals", Eta2=0))
#
# b0.table <-
#   broom::tidy(b0) %>%
#   left_join(b0.es, by = "term") %>%
#   mutate_if(is.numeric, round, 4)
#
# b0.table %>%
#   mutate(term = c("N","J","MM","Residuals")) %>%
#   gt_template(title = "tau0 ANOVA",
#               sourcenotes = "*Note*. N: Sample size; J: Number of items; MM: Measurement model") %>%
#   fmt_number(columns = c(3:7), decimals = 3) %>%
#   gtsave(filename = "test/paper_materials/ANOVA_tau0.png")
#
#
#
#
# #####
# posth_b0 <- TukeyHSD(b0)
# broom::tidy(posth_b0) %>%
#   gt_template(title = "ANOVA post-hoc for tua1") %>%
#   fmt_number(columns = c(4:7), decimals = 3) %>%
#   gtsave(filename = "test/paper_materials/ANOVA_tau0_posthoc.png")
#
# posth_b11 <- TukeyHSD(b11)
# broom::tidy(posth_b11) %>%
#   gt_template(title = "tau1 ANOVA post-hoc") %>%
#   fmt_number(columns = c(4:7), decimals = 3) %>%
#   gtsave(filename = "test/paper_materials/ANOVA_tau1_posthoc.png")
#
#
#
#
#
#
# plot(TukeyHSD(b0))
# plot(TukeyHSD(b11))
#
library(cowplot)

png_plots <- fs::dir_ls("test/paper_materials", regexp = "b0|b1")

p3 <- ggdraw() + draw_image(png_plots[1], scale = 1) +
  theme(
    plot.margin = margin(0, 0, 0, 0)
  )
p2 <- ggdraw() + draw_image(png_plots[2], scale = 1) +
  theme(
    plot.margin = margin(0, 0, 0, 0)
  )
p1 <- ggdraw() + draw_image(png_plots[3], scale = 1) +
  theme(
    plot.margin = margin(0, 0, 0, 0)
  )

plot_row <- plot_grid(p1, p2, p3, ncol = 2,
                      scale = c(1,1,1),
                      rel_widths = c(1,1,1)
)
title <- ggdraw() +
  draw_label(
    "Post-hoc test results for tau0",
    fontface = 'bold',
    x = 0,
    hjust = 0
  ) +
  theme(
    # add margin on the left of the drawing canvas,
    # so title is aligned with left edge of first plot
    plot.margin = margin(0, 0, 0, 7)
  )
plot_grid(
  title, plot_row,
  ncol = 1,
  # rel_heights values control vertical title margins
  rel_heights = c(0.1, 1)
)

p3 <- ggdraw() + draw_image(png_plots[4], scale = 1) +
  theme(
    plot.margin = margin(0, 0, 0, 0)
  )
p2 <- ggdraw() + draw_image(png_plots[5], scale = 1) +
  theme(
    plot.margin = margin(0, 0, 0, 0)
  )
p1 <- ggdraw() + draw_image(png_plots[6], scale = 1) +
  theme(
    plot.margin = margin(0, 0, 0, 0)
  )

plot_row <- plot_grid(p1, p2, p3, ncol = 2,
                      scale = c(1,1,1), rel_widths = c(1,1,1))
title <- ggdraw() +
  draw_label(
    "Post-hoc test results for tau1",
    fontface = 'bold',
    x = 0,
    hjust = 0
  ) +
  theme(
    # add margin on the left of the drawing canvas,
    # so title is aligned with left edge of first plot
    plot.margin = margin(0, 0, 0, 7)
  )
plot_grid(
  title, plot_row,
  ncol = 1,
  # rel_heights values control vertical title margins
  rel_heights = c(0.1, 1)
)





# -------------------------------------------------------------------------
#
#
# a1 <- total_res3 %>%
#   runanova(lvmodel0 = "2pl", par_name0 = "eta")
# summary(a1)
# effectsize(a1)
#
# a1 <- total_res3 %>%
#   runanova(lvmodel0 = "grm", par_name0 = "a11")
# summary(a1)
# effectsize(a1)
#
# a1 <- total_res3 %>%
#   runanova(lvmodel0 = "gpcm", par_name0 = "b0")
# summary(a1)
# effectsize(a1)
