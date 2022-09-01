library(tidyverse)

total_res1 <- readRDS("test/paper_materials/total_res1.rds")
total_res2 <- readRDS("test/paper_materials/total_res2.rds")
total_res3 <- readRDS("test/paper_materials/total_res3.rds")


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


runanova <- function(o1, lvmodel0=NA, par_name0) {

  foranova <- o1 %>%

    # filter(lvmodel == lvmodel0) %>%
    filter(lvmodel %in% c("Rasch","2PL","GRM","GPCM")) %>%
    filter(par_name == par_name0)

  ano.formula <- paste0("rmse", " ~ samplesize + nitem + lvmodel")
  # ano.formula <- paste0("bias", " ~ samplesize + nitem")
  # ano.formula <- paste0("rbias", " ~ model * sample_size * num_of_growth * num_of_timepoint * ICC_size")
  aov(as.formula(ano.formula), data=foranova)
}

unique(total_res1$par_name)
####
b11 <- total_res3 %>%
  # mutate(bias = abs(err)) %>%
  runanova(par_name0 = "b11")

effectsize(b11, partialeta = F)
b11.es <- effectsize(b11)
b11.es <- b11.es[ , c("term","Eta2")] %>% bind_rows(data.frame(term="Residuals", Eta2=0))

b11.table <- broom::tidy(b11) %>% left_join(b11.es, by = "term") %>%
  mutate_if(is.numeric, round, 4)

b11.table %>% gt_template(title = "tau1 ANOVA") %>%
  fmt_number(columns = c(3:7), decimals = 3) %>%
  gtsave(filename = "test/paper_materials/ANOVA_tau1.png")
####
b0 <- total_res3 %>%
  runanova(lvmodel0 = "gpcm", par_name0 = "b0")
summary(b0)

b0.es <- effectsize(b0)
b0.es <- b0.es[ , c("term","Eta2")] %>% bind_rows(data.frame(term="Residuals", Eta2=0))

b0.table <- broom::tidy(b0) %>% left_join(b0.es, by = "term") %>%
  mutate_if(is.numeric, round, 4)

b0.table %>% gt_template(title = "tau0 ANOVA") %>%
  fmt_number(columns = c(3:7), decimals = 3) %>%
  gtsave(filename = "test/paper_materials/ANOVA_tau0.png")




#####
TukeyHSD(b11)
TukeyHSD(b0)
plot(TukeyHSD(a1))

a1 <- total_res3 %>%
  runanova(lvmodel0 = "2pl", par_name0 = "eta")
summary(a1)
effectsize(a1)

a1 <- total_res3 %>%
  runanova(lvmodel0 = "grm", par_name0 = "a11")
summary(a1)
effectsize(a1)

a1 <- total_res3 %>%
  runanova(lvmodel0 = "gpcm", par_name0 = "b0")
summary(a1)
effectsize(a1)
