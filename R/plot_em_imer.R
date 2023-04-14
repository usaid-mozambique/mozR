#' Generate summary table of IMER results for last 6 months
#'
#' @param df Cleaned IMER historic dataframe
#'
#' @return GT Table of 6 months indicator trends
#' @export
#'
#' @examples
#' \dontrun{
#'
#' df <- plot_em_imer()}


plot_em_imer <- function(df) {

  period_lag6 <- max(as.Date(df$period)) - months(5)

  tbl <- df %>%
    filter(period >= period_lag6) %>%
    select(indicator, period, value) %>%
    arrange((period)) %>%
    mutate(row_n = row_number(),
           period = as.character(period, format = "%b %y")) %>%
    pivot_wider(names_from = period, values_from = value) %>%
    group_by(indicator) %>%
    summarize(across(where(is.double), ~ sum(.x, na.rm = TRUE))) %>%
    gt(rowname_col = "indicator") %>%

    fmt_number(
      columns = !c(indicator),
      rows = everything(),
      sep_mark = ",",
      decimals = 0) %>%

    cols_width(
      indicator ~ px(200),
      everything() ~ px(100)) %>%

    tab_style(
      style = cell_borders(
        sides = "right",
        weight = px(1),),
      locations = cells_body(
        columns = everything(),
        rows = everything())) %>%

    tab_options(
      table.font.size = 18,
      table.font.names = "SourceSansPro-Regular",
      footnotes.font.size = 8) %>%

    tab_header(title = "Mozambique TX & ER Enhanced Monitoring - 6 Month Trend") %>%
    tab_source_note("Source: AJUDA Enhanced Monitoring")


  tbl


}
