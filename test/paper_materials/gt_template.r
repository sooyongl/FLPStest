gt_template <- function(x, condition = NULL, delim = NA,
                        title = "**Table 2.** Results of measurement model by sample size",
                        sourcenotes = "") {
  # delim = "_"
  x %>%
    gt() %>%
    sub_missing(columns = everything()) %>%

    tab_header(title =md(title)) %>%
    opt_align_table_header(align = "left") %>%

    tab_source_note(
      source_note = md(sourcenotes)
    ) %>%

    {

      if(!is.na(delim)) {
        tab_spanner_delim(., delim = delim)
      } else {
        .
      }
    } %>%

    cols_align(
      align = c("right"),
      columns = which(!names(x) %in% condition)
    ) %>%

    tab_style(
      locations = cells_column_spanners(spanners = everything()),
      style     = list(cell_text(align = "center"),
                       cell_text(weight = "bold")
      )
    ) %>%

    tab_style(
      locations = cells_column_labels(columns = everything()),
      style     = list(cell_text(align = "center",weight = "bold"),

                       cell_borders(sides = "bottom", weight = px(3))
      )
    ) %>%

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
  # %>%
#
#     # additional_
#     tab_style(
#       locations = cells_body(rows = lvmodel == "Rasch",
#                              columns = matches("_a|_b")),
#       style     = list(cell_text(align = "center"))
#     ) %>%
#     tab_style(
#       locations = cells_body(columns = c(samplesize, rmse_b, coverage_b)),
#       style     = list(cell_borders(sides = "right", weight = px(1)))
#     ) %>%
#     tab_style(
#       locations = list(cells_body(rows = c(1,2,4,5,7,8,10,11))),
#       style     = list(cell_borders(sides = "bottom",
#                                     color = "transparent"))
#     ) %>%
#     tab_style(
#       locations = cells_body(rows = c(3,6,9,12)),
#       style     = list(cell_borders(sides = "bottom", color = "black"))
#     )

}


# a1 %>%
#   gt_template(
#     condition = c("lvmodel","samplesize"), delim = "_",
#     title = "**Table 1.** ddd",
#     sourcenotes = "ddd")


