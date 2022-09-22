library(officer); library(flextable)

default_wt <- function(ft_data, hlines = 1,
                       caption = "Table 1.", tablenote = "") {

  ft_1 <- ft_data %>% flextable( . )
  ft_1 %>%

    set_caption(caption = caption) %>%
    add_footer_lines(values = tablenote) %>%

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
           i = hlines

    ) %>%

    hline_bottom(.,
                 part="body",
                 border = fp_border(color="black", width = 1.5) ) %>%

    bold(i = c(1), part = 'header') %>%
    align(., align = 'center', part = "body") %>%
    align(align = "center", part = "header") %>%
    set_table_properties(width = 1, layout = "autofit")
}

wt_nonc <- function(ft_data,
                      caption = "Table 1.", tablenote = "") {

  # Header 1
  header1 <- c()
  header1[1:2] <- c("","")
  header1[3:dim(ft_data)[2]] <- c("Rasch","2PL","GPCM","GRM")

  # Header 2
  header2 <- c()
  header2[1:2] <- c("N", "J")
  header2[3:dim(ft_data)[2]] <- rep(c("Freq"), 4)

  multiple_header <- data.frame(
    col_keys = names(ft_data),
    header1 = header1,
    header2 = header2,
    stringsAsFactors = FALSE )

  ft_1 <- ft_data %>% flextable( . )
  ft_1 <- set_header_df(ft_1, mapping = multiple_header, key = "col_keys" )
  # ft_1 <- merge_h(ft_1, part = "header")


  ft_1 %>%

    set_caption(caption = caption) %>%
    add_footer_lines(values = tablenote) %>%

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
           i = c(1,4)

    ) %>%

    hline_bottom(.,
                 part="body",
                 border = fp_border(color="black", width = 1.5) ) %>%

    bold(i = c(1), part = 'header') %>%
    align(., align = 'center', part = "body") %>%
    align(align = "center", part = "header") %>%
    width(width = 1.2) %>%
    set_table_properties(width = 1, layout = "autofit")
}


wt_rhat <- function(ft_data,
                    caption = "Table 1. R-hat for all parmaeters across the simulation conditions", tablenote = "Note. N: measurement model; J: Number of items; PN: parameter name") {

  # Header 1
  header1 <- c()
  header1[1:3] <- c("","","")
  header1[4:dim(ft_data)[2]] <- rep(c("Rhat_mean","Rhat_max","Rhat_min"), each = 4)

  # Header 2
  header2 <- c()
  header2[1:3] <- c("N", "J","PN")
  header2[4:dim(ft_data)[2]] <- rep(c("Rasch","2PL","GPCM","GRM"), 3)

  multiple_header <- data.frame(
    col_keys = names(ft_data),
    header1 = header1,
    header2 = header2,
    stringsAsFactors = FALSE )

  ft_1 <- ft_data %>% flextable( . )
  ft_1 <- set_header_df(ft_1, mapping = multiple_header, key = "col_keys" )
  ft_1 <- merge_h(ft_1, part = "header")


  ft_1 %>%

    set_caption(caption = caption) %>%
    add_footer_lines(values = tablenote) %>%

    font(fontname = "Times", part = "all") %>%
    fontsize(size = 10, part = "header") %>%
    fontsize(size = 10, part = "body") %>%
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
           i = c(10,20,30,40)

    ) %>%

    border(.,
           # border.top = fp_border(color = "black", width = 1.5),
           border.right = fp_border(color = "black", width = .75),
           j = c(3,7,11)

    ) %>%

    hline_bottom(.,
                 part="body",
                 border = fp_border(color="black", width = 1.5) ) %>%

    bold(i = c(1), part = 'header') %>%
    align(., align = 'center', part = "body") %>%
    align(align = "center", part = "header") %>%
    set_table_properties(width = 1, layout = "autofit")
}


wt_mpart_ss <- function(ft_data,header_name="N",
                    caption = "Table 1.", tablenote = "") {

  # Header 1
  header1 <- c()
  header1[1:2] <- c("","")
  header1[3:dim(ft_data)[2]] <-
    rep(c("Bias","RMSE","Coverage"), each = 3)

  # Header 2
  header2 <- c()
  header2[1:2] <- c("MM", header_name)
  header2[3:dim(ft_data)[2]] <- rep(c("Slope","Intercept","Eta1"), 3)

  multiple_header <- data.frame(
    col_keys = names(ft_data),
    header1 = header1,
    header2 = header2,
    stringsAsFactors = FALSE )

  ft_1 <- ft_data %>% flextable( . )
  ft_1 <- set_header_df(ft_1, mapping = multiple_header, key = "col_keys" )
  ft_1 <- merge_h(ft_1, part = "header")

  ft_1 <- ft_1 %>%
    compose(part = "header", i=2, j = c(3, 6, 9),
            value = as_paragraph("\U03B1")) %>%
    compose(part = "header", i=2, j = c(4, 7, 10),
            value = as_paragraph("\U0064")) %>%
    compose(part = "header", i=2, j = c(5, 8, 11),
            value = as_paragraph("\U03B7", as_sub("T")))


  ft_1 %>%

    set_caption(caption = caption) %>%
    add_footer_lines(values = tablenote) %>%

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
           i = c(3,6,9)

    ) %>%
    border(.,
           # border.top = fp_border(color = "black", width = 1.5),
           border.right = fp_border(color = "black", width = .75),
           j = c(2,5,8)

    ) %>%

    hline_bottom(.,
                 part="body",
                 border = fp_border(color="black", width = 1.5) ) %>%

    bold(i = c(1), part = 'header') %>%
    align(., align = 'center', part = "body") %>%
    align(align = "center", part = "header") %>%
    set_table_properties(width = 1, layout = "autofit")
}


wt_struc_ss <- function(ft_data, header_name="N",
                        caption = "Table 1.", tablenote = "") {

  # Header 1
  header1 <- c()
  header1[1:2] <- c("","")
  header1[3:dim(ft_data)[2]] <-
    rep(c("Bias","RMSE","Coverage"), each = 5)

  # Header 2
  header2 <- c()
  header2[1:2] <- c("MM", header_name)
  header2[3:dim(ft_data)[2]] <- rep(c("tau0","tau1","a11","beta","gamma"), 3)

  multiple_header <- data.frame(
    col_keys = names(ft_data),
    header1 = header1,
    header2 = header2,
    stringsAsFactors = FALSE )

  ft_1 <- ft_data %>% flextable( . )
  ft_1 <- set_header_df(ft_1, mapping = multiple_header, key = "col_keys" )
  ft_1 <- merge_h(ft_1, part = "header")

  ft_1 <- ft_1 %>%
    compose(part = "header", i=2, j = c(3, 8, 13),
          value = as_paragraph("\U1D749", as_sub("0"))) %>%
    compose(part = "header", i=2, j = c(4, 9, 14),
            value = as_paragraph("\U1D749", as_sub("1"))) %>%
    compose(part = "header", i=2, j = c(5, 10, 15),
            value = as_paragraph("\U1D74E")) %>%
    compose(part = "header", i=2, j = c(6, 11, 16),
            value = as_paragraph("\U1D737")) %>%
    compose(part = "header", i=2, j = c(7, 12, 17),
            value = as_paragraph("\U1D738"))

  ft_1 %>%
    set_caption(caption = caption) %>%
    add_footer_lines(values = tablenote) %>%

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
           i = c(3,6,9)

    ) %>%
    border(.,
           # border.top = fp_border(color = "black", width = 1.5),
           border.right = fp_border(color = "black", width = .75),
           j = c(2,7,12)

    ) %>%

    hline_bottom(.,
                 part="body",
                 border = fp_border(color="black", width = 1.5) ) %>%

    bold(i = c(1), part = 'header') %>%
    align(., align = 'center', part = "body") %>%
    align(align = "center", part = "header") %>%
    set_table_properties(width = 1, layout = "autofit")
}

table_add <- function(x.doc, x.tb, landscape = F) {

  if(landscape)
    x.doc <- body_end_section_landscape(x.doc)

  flextable::body_add_flextable(
    x.doc,
    value = x.tb,
    align = "left"
  )

  body_add_par(x.doc, " ")

  if(landscape)
    end.landscape(x.doc)
}

# my.doc <- read_docx()
#
# test_table <- wordtable(table_modelfit)
# table_add(my.doc, test_table)
#
# test_table <- wordtable(table_uncond)
# table_add(my.doc, test_table)
#
# test_table <- wordtable(table_cond)
# table_add(my.doc, test_table)
#
#
# print(my.doc, target = "4_report/test_table.docx")
