library(tidyverse)
library(rstan)
library(ggpubr)
library(cowplot)
library(ggforce)

total_res <- readRDS("test/shinydata/0608_data.rds")
mpart <- readRDS("test/shinydata/0608_mpart.rds") %>% rename(est_mean = mean)

total_res1 <-
  total_res %>%
  mutate(
    par_name = as.character(par_name),
    par_name = case_when(
      str_detect(par_name, "bu") ~ "bu",
      str_detect(par_name, "by") ~ "by",
      TRUE ~ par_name
    ),
    par_name = factor(par_name,
                      levels = c("b00","b0", "b11" ,"a11","bu","by",
                                 "lambda","tau","eta"))
  ) %>%
  group_by(lvmodel, samplesize, nitem, par_name, rep) %>%
  summarise(
    err = mean(value),
    rerr = mean(rvalue),

    bias = mean(value),
    rbias = mean(rvalue),

    X2.5. = mean(X2.5.),
    X97.5. = mean(X97.5.),
    coverage = mean(coverage)
  ) %>%
  mutate(
    abserr = abs(err),
    absrerr = abs(rerr),

    absbias = abs(err),
    absrbias = abs(rerr)
    ) %>%
  mutate_if(is.numeric, ~ round(.x, 3))


total_res2 <- total_res1 %>%
  group_by(lvmodel, samplesize, nitem, par_name) %>%
  summarise(
    N = n(),
    bias = mean(err),
    absbias = mean(abs(err)),

    rbias = mean(rerr),
    absrbias = mean(abs(rerr)),

    rmse = sqrt(mean(err^2)),
    absrmse = sqrt(mean(abs(err)^2)),

    absrrmse = sqrt(mean(rerr^2)),
    absrrmse = sqrt(mean(abs(rerr)^2)),

    VAR.bias = var(err),

    X2.5. = mean(X2.5.),
    X97.5. = mean(X97.5.),
    coverage = mean(coverage)
  ) %>%
  ungroup() %>%
  mutate(
    MSE.bias = bias^2
  ) %>%
  arrange(lvmodel, nitem, par_name, samplesize)

yvalue <- "absbias"

total_res1 %>%
  filter(lvmodel == "rasch") %>%
  filter(nitem == "100") %>%
  ggplot(aes_string(x = "par_name", y = yvalue)) +
  geom_violin(
    trim=F,
    fill = "skyblue", alpha = 0.5, color = NA) +
  geom_sina(
    position = position_jitter(width = 0.05),
    alpha = 0.2) +
  # geom_hline(yintercept = 0) +
  geom_point(data =
               total_res2 %>%
               filter(lvmodel == "rasch") %>%
               filter(nitem == "100"),
             aes_string(x = "par_name", y = yvalue),
             size = 3, shape = 24, alpha = 0.8, fill = 'red') +
  facet_grid(nitem ~  samplesize) +
  scale_y_continuous(limits = c(0, 0.3), expand = c(0,0))

total_res2 %>%
  filter(lvmodel == "rasch") %>%
  filter(nitem == "100") %>%
  ggplot(aes_string(x = "par_name", y = yvalue)) +
  geom_col(fill = "grey50", color = "white") +
  facet_grid(nitem ~  samplesize)

total_res2 %>%
  filter(lvmodel == "rasch") %>%
  filter(nitem == "100") %>%
  ggplot(aes_string(x = "par_name", y = yvalue)) +
  geom_point(shape = 21, color = 'black',
             fill = 'white', size = 3, stroke = 3) +
  facet_grid(nitem ~  samplesize)


p_ni100 <- total_res2 %>%
  filter(lvmodel == "rasch") %>%
  filter(nitem == "100") %>%
  ggplot(aes_string(x = "par_name", y = yvalue)) +
  geom_col(fill = "grey50", color = "white") +
  facet_grid(nitem ~  samplesize) +
  theme_half_open() +
  panel_border()

p_ss1000 <- total_res2 %>%
  filter(lvmodel == "rasch") %>%
  filter(samplesize == "1000") %>%

  ggplot(aes_string(x = "par_name", y = yvalue)) +
  geom_col(fill = "grey50", color = "white") +
  facet_grid(samplesize ~ nitem ) +
  theme_half_open() +
  # theme_minimal_hgrid(12) +
  panel_border()


title <- ggdraw() +
  draw_label(
    "FLPS results with Rasch model",
    fontface = 'bold',
    x = 0,
    hjust = 0
  ) +
  theme(
    # add margin on the left of the drawing canvas,
    # so title is aligned with left edge of first plot
    plot.margin = margin(0, 0, 0, 3)
  )

# theme_half_open(11, rel_small = 1) +
# panel_border()  # always place this after the theme
plot_row <- plot_grid(
  p_ni100, p_ss1000,
  align = "v",
  ncol = 1, axis = 'l',
  labels = "AUTO",
  label_size = 12,
  label_x = 0, label_y = 1,
  hjust = -0.5, vjust = 1.0)


plot_grid(
  title, plot_row,
  ncol = 1,
  # rel_heights values control vertical title margins
  rel_heights = c(0.1, 1)
)









# -------------------------------------------------------------------------


# theme_minimal_hgrid()
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


