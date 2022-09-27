collapse_col <- function(data, .column) {
  temp0 <- as.character(data[[.column]])
  temp1 <- duplicated(temp0)
  temp2 <- which(!temp1)

  for(i in 1:length(temp1)) {

    if(temp1[i]) {
      temp0[i] <- ""
    } else {
      temp0[i] <- temp0[i]
    }
  }

  data[[.column]] <- temp0

  data
}

mpart_t <- function(x, title = "**Table 2.** Results of measurement model across the sample sizes, the number of items, and the measurement models",

                           source.note = "",
                           bias.label = "Bias") {
  x %>%
    gt() %>%

    sub_missing(columns = everything()) %>%

    tab_header(title =md(title)) %>%
    opt_align_table_header(align = "left") %>%

    tab_source_note(
      source_note =
        md(source.note)
    ) %>%

    tab_spanner(
      label = bias.label,
      columns = c(4:6)
    ) %>%
    tab_spanner(
      label = "RMSE",
      columns = c("rmse_a", "rmse_b", "rmse_eta1")
      # columns = 3:6
    ) %>%
    tab_spanner(
      label = "Coverage",
      columns = c(
        coverage_a,
        coverage_b,
        coverage_eta1)
    ) %>%

    cols_label(
      lvmodel    = "Mpdel",
      samplesize = "N",
      nitem = "J",

      # bias_eta0 = "eta0", # escape_latex(text)
      bias_eta1 = html("&eta;<sup>1</sup>"), #"eta1",
      bias_a = "a",
      bias_b = "d",

      # rmse_eta0 = "eta0", # escape_latex(text)
      rmse_eta1 = html("&eta;<sup>1</sup>"), #"eta1",
      rmse_a = "a",
      rmse_b = "d",

      # coverage_eta0 = "eta0",
      coverage_eta1 = html("&eta;<sup>1</sup>"), #"eta1",
      coverage_a = "a",
      coverage_b = "d"
    ) %>%

    cols_width(
      c(lvmodel,samplesize) ~ px(70),
      # matches("eta")      ~ px(60),
      # matches("bias") ~ px(120),
      everything()          ~ px(60)
    ) %>%

    cols_align(
      align = c("right"),
      # columns = matches("eta")
      columns = matches("bias|rmse|coverage")
    ) %>%

    cols_align(
      align = c("center"),
      # columns = matches("eta")
      columns = matches("lvmodel|samplesize|nitem")
    ) %>%

    # Column names ------------------------------------------
  tab_style(
    locations = cells_column_labels(columns = everything()),
    style     = list(cell_text(align = "center",weight = "bold"),
                     cell_borders(sides = "bottom", weight = px(3))
    )
  ) %>%

    # Second Column ----------------------------------------
  tab_style(
    locations = cells_column_spanners(spanners = everything()),
    style     = list(cell_text(weight = "bold",align = "center"))
  ) %>%


    tab_style(
      locations = cells_body(rows = 1:3, #lvmodel == "Rasch",
                             columns = matches("_a")),
      style     = list(cell_text(align = "center"))
    ) %>%
    tab_style(
      locations = cells_body(columns = c(nitem, bias_eta1, rmse_eta1)),
      style     = list(cell_borders(sides = "right", weight = px(1)))
    ) %>%
    tab_style(
      locations = list(cells_body(rows = c(1:24),
                                  columns = c(1)

      )),
      style     = list(cell_borders(sides = "bottom",
                                    color = "transparent"))
    ) %>%
    tab_style(
      locations = list(cells_body(rows = c(1,2, 4,5, 7,8, 10,11,
                                           13,14, 16,17, 19,20, 22,23),
                                  columns = c(2:12)

                                  )),
      style     = list(cell_borders(sides = "bottom",
                                    color = "transparent"))
    ) %>%
    tab_style(
      locations = cells_body(rows = c(6,12,18,24)),
      style     = list(cell_borders(sides = "bottom", color = "black"))
    ) %>%

    # tab_style(
    #   style =
    #     cell_borders(sides = c("bottom"),
    #                  color = "black",
    #                  weight = px(2)),
    #
    #   locations = cells_column_spanners(
    #     spanners = "rmse")
    # ) %>%

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

}

mpart_t_sample <- function(x, title = "**Table 3.** Results of measurement model by sample size",

                           source.note = "",
                           bias.label = "Bias") {
  x %>%
    gt() %>%

    sub_missing(columns = everything()) %>%

    tab_header(title =md(title)) %>%
    opt_align_table_header(align = "left") %>%

    tab_source_note(
      source_note =
        md(source.note)
    ) %>%

    tab_spanner(
      label = bias.label,
      columns = c(3:5)
    ) %>%
    tab_spanner(
      label = "RMSE",
      columns = c("rmse_a", "rmse_b", "rmse_eta1")
      # columns = 3:6
    ) %>%
    tab_spanner(
      label = "Coverage",
      columns = c(
                  coverage_a,
                  coverage_b,
                  coverage_eta1)
    ) %>%

    cols_label(
      lvmodel    = "MM",
      samplesize = "N",

      # bias_eta0 = "eta0", # escape_latex(text)
      bias_eta1 = "eta1",
      bias_a = "a",
      bias_b = "b",

      # rmse_eta0 = "eta0", # escape_latex(text)
      rmse_eta1 = "eta1",
      rmse_a = "a",
      rmse_b = "b",

      # coverage_eta0 = "eta0",
      coverage_eta1 = "eta1",
      coverage_a = "a",
      coverage_b = "b"
    ) %>%

    cols_width(
      c(lvmodel,samplesize) ~ px(70),
      # matches("eta")      ~ px(60),
      # matches("bias") ~ px(120),
      everything()          ~ px(60)
    ) %>%

    cols_align(
      align = c("right"),
      # columns = matches("eta")
      columns = matches("bias|rmse|coverage")
    ) %>%

    cols_align(
      align = c("center"),
      # columns = matches("eta")
      columns = matches("lvmodel|samplesize")
    ) %>%

    # Column names ------------------------------------------
  tab_style(
    locations = cells_column_labels(columns = everything()),
    style     = list(cell_text(align = "center",weight = "bold"),
                     cell_borders(sides = "bottom", weight = px(3))
    )
  ) %>%

    # Second Column ----------------------------------------
  tab_style(
    locations = cells_column_spanners(spanners = everything()),
    style     = list(cell_text(weight = "bold",align = "center"))
  ) %>%


  tab_style(
    locations = cells_body(rows = 1:3, #lvmodel == "Rasch",
                           columns = matches("_a")),
    style     = list(cell_text(align = "center"))
  ) %>%
    tab_style(
      locations = cells_body(columns = c(samplesize, bias_eta1, rmse_eta1)),
      style     = list(cell_borders(sides = "right", weight = px(1)))
    ) %>%
    tab_style(
      locations = list(cells_body(rows = c(1,2,4,5,7,8,10,11))),
      style     = list(cell_borders(sides = "bottom",
                                    color = "transparent"))
    ) %>%
    tab_style(
      locations = cells_body(rows = c(3,6,9,12)),
      style     = list(cell_borders(sides = "bottom", color = "black"))
    ) %>%

    # tab_style(
    #   style =
    #     cell_borders(sides = c("bottom"),
    #                  color = "black",
    #                  weight = px(2)),
    #
    #   locations = cells_column_spanners(
    #     spanners = "rmse")
    # ) %>%

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

}


mpart_t_nitem <- function(x, title = "**Table 4**. Results of measurement model by the number of items", bias.label = "Bias",source.note = "") {
  x %>% gt(
    # caption = "Table 3. Results of measurement model by the number of items"
    # rowname_col = "lvmodel"
  ) %>%
    sub_missing(
      columns = everything()
    ) %>%
    tab_header(
      title = md(title)
    ) %>%
    tab_spanner(
      label = bias.label,
      columns = c(3:5)
    ) %>%
    tab_spanner(
      label = "RMSE",
      columns = c(rmse_a, rmse_b, rmse_eta1)
    ) %>%
    tab_spanner(
      label = "Coverage",
      columns = c(coverage_a, coverage_b, coverage_eta1)
    ) %>%

    cols_label(
      lvmodel    = "MM",
      nitem = "J",

      # bias_eta0 = "eta0",
      bias_eta1 = "eta1",
      bias_a = "a",
      bias_b = "b",

      # rmse_eta0 = "eta0",
      rmse_eta1 = "eta1",
      rmse_a = "a",
      rmse_b = "b",

      # coverage_eta0 = "eta0",
      coverage_eta1 = "eta1",
      coverage_a = "a",
      coverage_b = "b"
    ) %>%
    cols_width(
      c(lvmodel,nitem) ~ px(70),

      # matches("eta")      ~ px(60),
      # matches("bias") ~ px(120),
      everything()          ~ px(60)
    ) %>%
    #
    cols_align(
      align = c("right"),
      # columns = matches("eta")
      columns = matches("bias|rmse|coverage")
    ) %>%

    cols_align(
      align = c("center"),
      # columns = matches("eta")
      columns = matches("lvmodel|nitem")
    ) %>%

    tab_source_note(
      source_note = source.note
    ) %>%

    tab_style(
      locations = cells_body(rows = 1:3, #lvmodel == "Rasch",
                             columns = matches("_a")),
      style     = list(cell_text(align = "center"))
    ) %>%

    tab_style(
      locations =
        cells_body(columns = c(nitem, bias_eta1, rmse_eta1)),
      style     = list(
        #Give a thick border below
        cell_borders(sides = "right", weight = px(1))
      )
    ) %>%
    tab_style(
      locations =
        cells_column_labels(columns = everything()),
      style     = list(
        cell_text(align = "center"),
        #Give a thick border below
        cell_borders(sides = "bottom", weight = px(3)),
        #Make text bold
        cell_text(weight = "bold")
      )
    ) %>%
    tab_style(
      locations =
        cells_column_spanners(spanners = everything()),
      style     = list(
        #Make text bold
        cell_text(weight = "bold")
      )
    ) %>%
    tab_style(
      locations = cells_body(rows =
                               c(1,2,4,5,7,8,10,11)),
      style     = list(
        cell_borders(sides = "bottom", color = "transparent")
      )
    ) %>%
    tab_style(
      locations = cells_body(rows = c(3,6,9,12)),
      style     = list(
        cell_borders(sides = "bottom", color = "black")
      )
    ) %>%
    tab_options(
      #Remove border between column headers and title
      column_labels.border.top.width = px(3),
      column_labels.border.top.color = "black",
      #Remove border around table
      table_body.border.bottom.color = "black",
      table_body.border.bottom.width = px(3),

      table.border.bottom.color = "transparent",
      table.border.top.color = "transparent",

      source_notes.font.size = 16,

      heading.align = "center",
      # heading.title.font.size = 12,
      # heading.padding = NULL,

      source_notes.padding = 12
    ) %>%

    opt_align_table_header(align = "left")



  # %>% opt_table_font(font = google_font("Fira Mono"))
}




# --------------------------------------------------------------
struc_t <- function(x, title = "**Table 3.** Results of structural model across the sample sizes, the number of items, and the measurement models",bias.label = "Bias",
                    source.note = "") {
  x %>%
    gt() %>%

    tab_header(
      title = md(title)
    ) %>%
    tab_spanner(
      label = bias.label,
      columns = c(4:8)
    ) %>%
    tab_spanner(
      label = "RMSE",
      # columns = c(rmse_b0, rmse_b11, rmse_a11)
      columns = c(rmse_b0, rmse_b11, rmse_a11, rmse_bu, rmse_by)
    ) %>%
    tab_spanner(
      label = "Coverage",
      # columns = c(coverage_b0, coverage_b11, coverage_a11)
      columns = c(coverage_b0, coverage_b11,
                  coverage_a11,coverage_bu,coverage_by)
    ) %>%
    # cols_label("Estimate" = html("&beta;"))
    cols_label(
      lvmodel    = "Model",
      samplesize = "N",
      nitem = "J",

      bias_b0  = html("&tau;<sup>0</sup>"), #"tau0",
      bias_b11 = html("&tau;<sup>1</sup>"), #"tau1",
      bias_a11 = html("&omega;"), #"omega",
      bias_bu = html("&beta;"), # "beta",
      bias_by = html("&gamma;"),# "gamma",

      rmse_b0  = html("&tau;<sup>0</sup>"), #"tau0",
      rmse_b11 = html("&tau;<sup>1</sup>"),
      rmse_a11 = html("&omega;"),
      rmse_bu = html("&beta;"), # "beta",
      rmse_by = html("&gamma;"), #"gamma",

      coverage_b0  = html("&tau;<sup>0</sup>"), #"tau0",
      coverage_b11 = html("&tau;<sup>1</sup>"), #"tau1",
      coverage_a11 = html("&omega;"),
      coverage_bu = html("&beta;"),
      coverage_by = html("&gamma;"),

    ) %>%
    cols_width(
      c(lvmodel,samplesize) ~ px(70),
      # matches("eta")      ~ px(60),
      # matches("bias") ~ px(120),
      everything()          ~ px(60)
    ) %>%

    cols_align(
      align = c("right"),
      # columns = matches("eta")
      columns = matches("bias|rmse|coverage")
    ) %>%

    cols_align(
      align = c("center"),
      # columns = matches("eta")
      columns = matches("lvmodel|samplesize|nitem")
    ) %>%

    tab_source_note(
      source_note = md(source.note)
    ) %>%

    tab_style(
      locations =
        cells_body(columns = c(samplesize,
                               bias_by, rmse_by)),
      style     = list(
        #Give a thick border below
        cell_borders(sides = "right", weight = px(1))
      )
    ) %>%
    tab_style(
      locations =
        cells_column_labels(columns = everything()),
      style     = list(
        cell_text(align = "center"),
        #Give a thick border below
        cell_borders(sides = "bottom", weight = px(3)),
        #Make text bold
        cell_text(weight = "bold")
      )
    ) %>%
    tab_style(
      locations =
        cells_column_spanners(spanners = everything()),
      style     = list(
        #Make text bold
        cell_text(weight = "bold")
      )
    ) %>%

    tab_style(
      locations = list(cells_body(rows = c(1:24),
                                  columns = c(1)

      )),
      style     = list(cell_borders(sides = "bottom",
                                    color = "transparent"))
    ) %>%
    tab_style(
      locations = list(cells_body(rows = c(1,2, 4,5, 7,8, 10,11,
                                           13,14, 16,17, 19,20, 22,23),
                                  columns = c(2:18)

      )),
      style     = list(cell_borders(sides = "bottom",
                                    color = "transparent"))
    ) %>%

    tab_style(
      locations = cells_body(rows = c(6,12,18,24)),
      style     = list(
        cell_borders(sides = "bottom", color = "black")
      )
    ) %>%
    tab_options(
      #Remove border between column headers and title
      column_labels.border.top.width = px(3),
      column_labels.border.top.color = "black",
      #Remove border around table
      table_body.border.bottom.color = "black",
      table_body.border.bottom.width = px(3),

      table.border.bottom.color = "transparent",
      table.border.top.color = "transparent",

      source_notes.font.size = 16,

      heading.align = "center",
      # heading.title.font.size = 12,
      # heading.padding = NULL,

      source_notes.padding = 12,

      table.width = px(600)
    ) %>%
    opt_align_table_header(align = "left")
}

struc_t_sample <- function(x, title = "**Table 5.** Results of structural model by sample size",bias.label = "Bias") {
  x %>%
    gt() %>%

    tab_header(
      title = md(title)
    ) %>%
    tab_spanner(
      label = bias.label,
      columns = c(3:7)
    ) %>%
    tab_spanner(
      label = "RMSE",
      # columns = c(rmse_b0, rmse_b11, rmse_a11)
      columns = c(rmse_b0, rmse_b11, rmse_a11, rmse_bu, rmse_by)
    ) %>%
    tab_spanner(
      label = "Coverage",
      # columns = c(coverage_b0, coverage_b11, coverage_a11)
      columns = c(coverage_b0, coverage_b11,
                  coverage_a11,coverage_bu,coverage_by)
    ) %>%
    cols_label(
      lvmodel    = "MM",
      samplesize = "N",

      bias_b0  = "tau0",
      bias_b11 = "tau1",
      bias_a11 = "omega",
      bias_bu = "beta",
      bias_by = "gamma",

      rmse_b0  = "tau0",
      rmse_b11 = "tau1",
      rmse_a11 = "omega",
      rmse_bu = "beta",
      rmse_by = "gamma",

      coverage_b0  = "tau0",
      coverage_b11 = "tau1",
      coverage_a11 = "omega",
      coverage_bu = "beta",
      coverage_by = "gamma"

    ) %>%
    cols_width(
      c(lvmodel,samplesize) ~ px(70),
      # matches("eta")      ~ px(60),
      # matches("bias") ~ px(120),
      everything()          ~ px(60)
    ) %>%

    cols_align(
      align = c("right"),
      # columns = matches("eta")
      columns = matches("bias|rmse|coverage")
    ) %>%

    cols_align(
      align = c("center"),
      # columns = matches("eta")
      columns = matches("lvmodel|samplesize")
    ) %>%

    tab_source_note(
      source_note = md("*Note.* MM: Measurement model; N: Sample size; The number of items was fixed at 100")
    ) %>%

    tab_style(
      locations =
        cells_body(columns = c(samplesize,
                               bias_by, rmse_by)),
      style     = list(
        #Give a thick border below
        cell_borders(sides = "right", weight = px(1))
      )
    ) %>%
    tab_style(
      locations =
        cells_column_labels(columns = everything()),
      style     = list(
        cell_text(align = "center"),
        #Give a thick border below
        cell_borders(sides = "bottom", weight = px(3)),
        #Make text bold
        cell_text(weight = "bold")
      )
    ) %>%
    tab_style(
      locations =
        cells_column_spanners(spanners = everything()),
      style     = list(
        #Make text bold
        cell_text(weight = "bold")
      )
    ) %>%
    tab_style(
      locations = cells_body(rows =
                               c(1,2,4,5,7,8,10,11)),
      style     = list(
        cell_borders(sides = "bottom", color = "transparent")
      )
    ) %>%
    tab_style(
      locations = cells_body(rows = c(3,6,9,12)),
      style     = list(
        cell_borders(sides = "bottom", color = "black")
      )
    ) %>%
    tab_options(
      #Remove border between column headers and title
      column_labels.border.top.width = px(3),
      column_labels.border.top.color = "black",
      #Remove border around table
      table_body.border.bottom.color = "black",
      table_body.border.bottom.width = px(3),

      table.border.bottom.color = "transparent",
      table.border.top.color = "transparent",

      source_notes.font.size = 16,

      heading.align = "center",
      # heading.title.font.size = 12,
      # heading.padding = NULL,

      source_notes.padding = 12,

      table.width = px(600)
    ) %>%
    opt_align_table_header(align = "left")
}


struc_t_nitem <- function(x, title = "**Table 6.** Results of structural model by the number of items",bias.label = "Bias") {
  x %>% gt() %>%

    tab_source_note(
      source_note =
        md(source.note)
    ) %>%

    tab_header(
      title = md(title)
    ) %>%
    tab_spanner(
      label = bias.label,
      columns = c(3:7)
    ) %>%
    tab_spanner(
      label = "RMSE",
      # columns = c(rmse_b0, rmse_b11, rmse_a11)
      columns = c(rmse_b0, rmse_b11, rmse_a11, rmse_bu, rmse_by)
    ) %>%
    tab_spanner(
      label = "Coverage",
      # columns = c(coverage_b0, coverage_b11, coverage_a11)
      columns = c(coverage_b0, coverage_b11,
                  coverage_a11,coverage_bu,coverage_by)
    ) %>%
    cols_label(
      lvmodel    = "MM",
      nitem = "J",

      bias_b0  = "tau0",
      bias_b11 = "tau1",
      bias_a11 = "omega",
      bias_bu = "beta",
      bias_by = "gamma",

      rmse_b0  = "tau0",
      rmse_b11 = "tau1",
      rmse_a11 = "omega",
      rmse_bu = "beta",
      rmse_by = "gamma",

      coverage_b0  = "tau0",
      coverage_b11 = "tau1",
      coverage_a11 = "omega",
      coverage_bu = "beta",
      coverage_by = "gamma"

    ) %>%
    cols_width(
      c(lvmodel,nitem) ~ px(70),
      # matches("eta")      ~ px(60),
      # matches("bias") ~ px(120),
      everything()     ~ px(60)
    ) %>%

    cols_align(
      align = c("right"),
      # columns = matches("eta")
      columns = matches("bias|rmse|coverage")
    ) %>%

    cols_align(
      align = c("center"),
      # columns = matches("eta")
      columns = matches("lvmodel|nitem")
    ) %>%

    tab_source_note(
      source_note = md("*Note.* MM: Measurement model; J: Number of items; The sample size was fixed at 1000")
    ) %>%
    tab_style(
      locations =
        cells_body(columns = c(nitem, bias_by,rmse_by)),
      style     = list(
        #Give a thick border below
        cell_borders(sides = "right", weight = px(1))
      )
    ) %>%
    tab_style(
      locations =
        cells_column_labels(columns = everything()),
      style     = list(
        cell_text(align = "center"),
        #Give a thick border below
        cell_borders(sides = "bottom", weight = px(3)),
        #Make text bold
        cell_text(weight = "bold")
      )
    ) %>%
    tab_style(
      locations =
        cells_column_spanners(spanners = everything()),
      style     = list(
        #Make text bold
        cell_text(weight = "bold")
      )
    ) %>%
    tab_style(
      locations = cells_body(rows =
                               c(1,2,4,5,7,8,10,11)),
      style     = list(
        cell_borders(sides = "bottom", color = "transparent")
      )
    ) %>%
    tab_style(
      locations = cells_body(rows = c(3,6,9,12)),
      style     = list(
        cell_borders(sides = "bottom", color = "black")
      )
    ) %>%

    tab_options(
      #Remove border between column headers and title
      column_labels.border.top.width = px(3),
      column_labels.border.top.color = "black",
      #Remove border around table
      table_body.border.bottom.color = "black",
      table_body.border.bottom.width = px(3),

      table.border.bottom.color = "transparent",
      table.border.top.color = "transparent",

      source_notes.font.size = 16,

      heading.align = "center",
      # heading.title.font.size = 12,
      # heading.padding = NULL,

      source_notes.padding = 12,

      table.width = px(600)
    ) %>%
    opt_align_table_header(align = "left")
}
