library(tidyverse)
temp1 <- bayes_res %>%
  tibble() %>%
  mutate(errmed = X50. - true_param) %>%
  mutate(par_name = case_when(
    par_name == "betaU[1]" ~ "bu11",
    par_name == "betaU[2]" ~ "bu12",
    par_name == "betaY[1]" ~ "by1",
    par_name == "betaY[2]" ~ "by2",
    par_name == "a1" ~ "a11",
    par_name == "b1" ~ "b11",
    str_detect(par_name, "^eta") ~ "eta",
    str_detect(par_name, "^tau") ~ "tau",
    str_detect(par_name, "^lambda") ~ "lambda")
    ) %>%
  separate(cond,
           c("lvmodel","samplesize","nitem","r2y","r2eta","linearity","outdist","rep"), "_")

temp2 <- temp1 %>%
  select("lvmodel","samplesize","nitem", "rep",par_name, true_param,
         mean, X50.,
         err, errmed) %>%
  filter(par_name == "lambda")

temp2 %>%
  filter(lvmodel == "2pl",samplesize == "1000", nitem == "100",
         rep == "10") %>%
  ggplot() +

  geom_point(aes(x = true_param, y = mean), color = "blue") +
  geom_point(aes(x = true_param, y = X50.), color = "red") +
  geom_abline(slope = 1, intercept = 0, color="black",
              linetype="dashed", size=1.5)
  # geom_hline(yintercept = 0, color="black",
  #             linetype="dashed", size=1.5)
