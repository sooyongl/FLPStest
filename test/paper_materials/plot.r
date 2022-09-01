library(tidyverse)
library(ggpubr)
library(cowplot)
library(ggforce)
# Plot --------------------------------------------------------------
total_res1 <- readRDS("test/paper_materials/total_res1_convadd.rds")
total_res2 <- readRDS("test/paper_materials/total_res2_convadd.rds")

total_res1 %>%
  ungroup() %>%
  distinct(lvmodel, samplesize, nitem, rep) %>%
  count(lvmodel, samplesize, nitem) %>%
  print(n = 23)

total_res1 <-
  total_res1 %>%
  filter(!lvmodel %in% c("2plunif","2plnormal","2pllogn"))

p.for.sample<- function(lvmodel0, bysample = T) {
  yvalue = 'bias'

  # theme_set(ggpubr::theme_pubclean(base_size = 16))

  theme_set(theme_half_open(font_size = 14) +
              panel_border() +
              theme(

                axis.text.x = element_text(size = 16, face = 'bold')

              ))

  p <- total_res1 %>%
    filter(par_name %in% c("b0", "b11", "a11", "eta", "lambda", "tau")) %>%
    filter(lvmodel == lvmodel0) %>%
    {
      if(bysample) {
        filter(., nitem == "100")
      } else {
        filter(., samplesize == "1000")
      }

    } %>%
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
                 filter(par_name %in% c("b0", "b11", "a11", "eta", "lambda", "tau")) %>%
                 filter(lvmodel == lvmodel0) %>%
                 {
                   if(bysample) {
                     filter(., nitem == "100")
                   } else {
                     filter(., samplesize == "1000")
                   }

                 },
               aes_string(x = "par_name", y = yvalue),
               size = 3, shape = 24, alpha = 0.8, fill = 'red') +

    geom_hline(yintercept = 0, alpha = 0.6) +
    labs(x = "Parameters", y = "Bias") +
    scale_y_continuous(limits = c(-0.5, 0.5), expand = c(0,0)) +
    scale_x_discrete(labels = c('lambda' = "a",
                                'eta'   = expression(eta),
                                'tau'   = "b",
                                'b0'   = expression(tau[0]),
                                'b11'   = expression(tau[1]),
                                'a11'   = expression(omega)
                                )

                     )

    if(bysample) {
      p + facet_grid(nitem ~  samplesize)
    } else {
      p + facet_grid(samplesize ~ nitem)
    }
}
p1 <- p.for.sample("rasch")
p2 <- p.for.sample("2pl")
p3 <- p.for.sample("grm")
p4 <- p.for.sample("gpcm")

pp <- ggarrange(p1,p2,p3,p4, ncol = 2, nrow = 2,
          labels = c("Rasch","2PL","GRM","GPCM"))

ggexport(pp,
         width = 1005,
         height = 697,
         filename = "test/paper_materials/bysample.png")

p1 <- p.for.sample("rasch", F)
p2 <- p.for.sample("2pl", F)
p3 <- p.for.sample("grm", F)
p4 <- p.for.sample("gpcm", F)

pp <- ggarrange(p1,p2,p3,p4, ncol = 2, nrow = 2,
          labels = c("Rasch","2PL","GRM","GPCM"))

ggexport(pp,
         width = 1005,
         height = 697,
         filename = "test/paper_materials/bynitem.png")


# total_res2 %>%
#   filter(lvmodel == "rasch") %>%
#   filter(nitem == "100") %>%
#   ggplot(aes_string(x = "par_name", y = yvalue)) +
#   geom_col(fill = "grey50", color = "white") +
#   facet_grid(nitem ~  samplesize + lvmodel)
#
# total_res2 %>%
#   filter(lvmodel == "rasch") %>%
#   filter(nitem == "100") %>%
#   ggplot(aes_string(x = "par_name", y = yvalue)) +
#   geom_point(shape = 21, color = 'black',
#              fill = 'white', size = 3, stroke = 3) +
#   facet_grid(nitem ~  samplesize)
#
#
# p_ni100 <- total_res2 %>%
#   filter(lvmodel == "rasch") %>%
#   filter(nitem == "100") %>%
#   ggplot(aes_string(x = "par_name", y = yvalue)) +
#   geom_col(fill = "grey50", color = "white") +
#   facet_grid(nitem ~  samplesize) +
#   theme_half_open() +
#   panel_border()
#
# p_ss1000 <- total_res2 %>%
#   filter(lvmodel == "rasch") %>%
#   filter(samplesize == "1000") %>%
#
#   ggplot(aes_string(x = "par_name", y = yvalue)) +
#   geom_col(fill = "grey50", color = "white") +
#   facet_grid(samplesize ~ nitem ) +
#   theme_half_open() +
#   # theme_minimal_hgrid(12) +
#   panel_border()
#
#
# title <- ggdraw() +
#   draw_label(
#     "FLPS results with Rasch model",
#     fontface = 'bold',
#     x = 0,
#     hjust = 0
#   ) +
#   theme(
#     # add margin on the left of the drawing canvas,
#     # so title is aligned with left edge of first plot
#     plot.margin = margin(0, 0, 0, 3)
#   )
#
# # theme_half_open(11, rel_small = 1) +
# # panel_border()  # always place this after the theme
# plot_row <- plot_grid(
#   p_ni100, p_ss1000,
#   align = "v",
#   ncol = 1, axis = 'l',
#   labels = "AUTO",
#   label_size = 12,
#   label_x = 0, label_y = 1,
#   hjust = -0.5, vjust = 1.0)
#
#
# plot_grid(
#   title, plot_row,
#   ncol = 1,
#   # rel_heights values control vertical title margins
#   rel_heights = c(0.1, 1)
# )
#
#
