# # refer to figures_tables.r
#
# converged_results0 <- mpart0 %>%
#   filter(converge_Rhat == 0) %>%
#   select(lvmodel, samplesize, nitem, rep, par_name = par_name0, Rhat) %>%
#   bind_rows(total_res1 %>%
#               filter(Rhat > 1.1) %>%
#               select(lvmodel, samplesize, nitem, rep, par_name, Rhat))
#
#
# converged_results0 %>%
#   group_by(lvmodel, samplesize, nitem, par_name) %>%
#   count(lvmodel, samplesize, nitem, par_name) %>%
#   mutate(
#     prop =
#       case_when(
#         par_name == "lambda" ~ n/(as.numeric(nitem)*100),
#         par_name == "eta" ~ n/(as.numeric(samplesize)*100),
#         TRUE ~ n/100
#       )
#   ) %>%
#   print(n = 50)
#
# nc_rep <- converged_results0 %>%
#   group_by(lvmodel, samplesize, nitem, rep) %>%
#   count(lvmodel, samplesize, nitem)
#
# nc_rep %>%
#   mutate(
#     cond =
#       paste(lvmodel, samplesize, nitem, "0.2", "0.5", "T","n",rep, sep = "_")
#   )
#
#
# nc_con <- nc_rep %>%
#   group_by(lvmodel, samplesize, nitem) %>%
#   count(lvmodel, samplesize, nitem)
#
# nc_con %>%
#   mutate(prop = n / 100)
#
# sum(nc_con$n)
# # saveRDS(converged_results, "test/paper_materials/converged_results.rds")
