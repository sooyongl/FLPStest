# https://cran.r-project.org/web/packages/sensemakr/vignettes/sensemakr.html

# Y_c, Y_T indep m | \eta_T, x - (4)

#
res1 <- fs::dir_ls('D:\\FLPS\\results', regex = "rds$")
res2 <- fs::dir_ls('D:\\FLPS\\rerun', regex = "rds$")
res3 <- fs::dir_ls("D:\\FLPS\\results_poly", regex = "rds$")

sfit <- readRDS(res1[1])
sres <- data.frame(summary(sfit$fit)$summary)
sres$par_name <- rownames(sres)

sfit$sdat$stan_dt$Y
sfit$sdat$stan_dt$Y

inp_data <- bind_cols(
  data.frame(
    Y = sdat$stan_dt$Y,
    Z = sdat$stan_dt$Z,
    sdat$stan_dt$X,
    theta = sdat$theta),
  data.frame(sdat$lv.resp)
)

sres %>%
  filter(str_detect(par_name, 'eta'))
