gtPaper <- function(x, title="", sn="",
                    hlines = NA, vlines = NA, delim = NA,
                    rowpadding = 5, colpadding = NA
) {
  # colwidth <- paste0("x %>% cols_width(everything() ~ px(", colpadding, "))"); x <- eval(parse(text=colwidth))

  x.names <- names(x)
  x.dim <- dim(x)
  x.numericc <- which(apply(x, 2, is.numeric))

  x %>%
    gt() %>%
    sub_missing(columns = everything()) %>%
    fmt_number(columns = x.numericc, decimals = 3) %>%

    tab_style(
      locations = cells_body(everything()),
      style     = cell_borders(sides = "bottom",color = "transparent")
    ) %>%
    {
      if(is.na(colpadding)) {
        .
      } else {
        cols_width(., as.formula(paste0("everything()~px(",colpadding,")")))
      }
    } %>%
    cols_align(align=c("center"), columns=eval(parse(text="everything()"))) %>%
    {
      if(any(is.na(hlines))) {
        .
      } else {
        tab_style(.,
                  locations = cells_body(rows = hlines),
                  style     = cell_borders(sides="bottom",color="black",weight = px(1)))
      }
    } %>%

    {
      if(any(is.na(vlines))) {
        .
      } else {
        tab_style(.,
                  locations = cells_body(columns = all_of(vlines)),
                  style     = cell_borders(sides="right",color="black",weight=px(1)))
      }
    } %>%

    tab_header(title = md(title)) %>%
    {
      if(is.na(delim)) {
        .
      } else {
        tab_spanner_delim(., delim = delim, columns = everything())
      }

    } %>%
    tab_source_note(source_note = md(sn)) %>%

    tab_style(
      locations = cells_column_labels(columns = everything()),
      style     = list(cell_text(align = "center",weight = "bold"),
                       cell_borders(sides = "bottom", weight = px(3))
      )
    ) %>%
    tab_style(
      locations = cells_column_spanners(spanners = everything()),
      style     = list(cell_text(weight = "bold",align = "center"))
    ) %>%

    tab_options(
      data_row.padding = px(rowpadding),

      # Heading -----------------------------
      heading.align = "left",
      # heading.title.font.size = px(16),
      # heading.padding = 10,

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
      source_notes.padding = 10
    )
}
