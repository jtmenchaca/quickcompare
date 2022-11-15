#' Save summary tables made with 'compare_cols_by_group'
#'
#' @param data A data frame or data frame extension
#' @param file_name The name for the .xlsx file
#' @param sheet_name The XLSX worksheet to write to - can be index or name
#' @param title Title for the table, optional
#' @export
#'
#' @return A data frame
#'
#' @examples
#' data(mtcars) |>
#'   compare_cols_by_group(
#'     group_col = "cyl",
#'     binary_or_cat_cols = c("vs", "am", "gear", "carb"),
#'     continuous_cols = c("mpg", "disp", "hp", "drat", "wt", "qsec")
#'   ) |>
#'   save_comparison_to_xlsx(
#'     file_name = "Comparison.xlsx",
#'     title = "Table 1: Outcomes in Population by Group"
#'   )

save_comparison_to_xlsx = function(data,
                           file_name,
                           sheet_name = "Sheet 1",
                           title = "") {

  y_table_start = 5
  y_border_start = y_table_start - 1
  y_title = y_table_start - 2
  y_table_end = y_table_start + nrow(data) # subtracting by 1 not needed because column titles
  y_border_end = y_table_end + 1

  x_table_start = 3
  x_border_start = x_table_start - 1
  x_table_end = x_table_start + (ncol(data) - 1)
  x_border_end = x_table_end + 1

  wb = openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, sheet_name)

  bg_style = openxlsx::createStyle(fgFill = "#FFFFFF")
  openxlsx::addStyle(wb, sheet_name,
                     bg_style,
                     rows = 1:(y_border_end+50),
                     cols = 1:(x_border_end+50), gridExpand = T, stack = T)

  top_row_style = openxlsx::createStyle(halign = "center", valign = "center", numFmt = "TEXT",
                                        textDecoration = "bold", fgFill = "#FFFFFF")
  openxlsx::addStyle(wb, sheet_name,
                     top_row_style,
                     rows = y_table_start,
                     cols = x_table_start:x_table_end, gridExpand = T, stack = T)

  first_col_style = openxlsx::createStyle(halign = "left", valign = "center",
                                          numFmt = "TEXT", fgFill = "#FFFFFF")
  openxlsx::addStyle(wb, sheet_name,
                     first_col_style,
                     rows = (y_table_start + 1):y_table_end,
                     cols = x_table_start, gridExpand = T, stack = T)

  other_cols_style = openxlsx::createStyle(halign = "center", valign = "center",
                                           numFmt = "TEXT", fgFill = "#FFFFFF")
  openxlsx::addStyle(wb, sheet_name,
                     other_cols_style,
                     rows = (y_table_start+1):y_table_end,
                     cols = (x_table_start+1):x_table_end, gridExpand = T, stack = T)

  # Function call example
  OutsideBorders(
    wb,
    sheet = sheet_name,
    rows = y_border_start:y_border_end,
    cols = x_border_start:x_border_end
  )

  openxlsx::setRowHeights(wb, sheet_name,
                          rows = y_table_start,
                          heights = 30)

  openxlsx::setColWidths(wb, sheet_name,
                         cols = x_table_start:x_table_end,
                         widths = "auto")

  openxlsx::setColWidths(wb, sheet_name,
                         cols = c(x_border_start, x_border_end),
                         widths = c(1, 1))

  openxlsx::writeData(wb, sheet = sheet_name, x = data,
                      startRow = y_table_start, startCol = x_table_start)

  if (title != ""){

    openxlsx::mergeCells(wb, sheet_name,
               rows = y_title,
               cols = x_border_start:x_border_end)

    title_style = openxlsx::createStyle(halign = "left", valign = "center", numFmt = "TEXT",
                                        textDecoration = "bold", fgFill = "#FFFFFF",
                                        wrapText = T)
    openxlsx::addStyle(wb, sheet_name,
                       title_style,
                       rows = y_title,
                       cols = x_border_start:x_border_end,
                       gridExpand = T, stack = T)

    openxlsx::writeData(wb,
                        sheet = sheet_name,
                        data.frame(value = title),
                        startRow = y_title,
                        startCol = x_border_start,
                        colNames = FALSE)
  }

  openxlsx::saveWorkbook(wb, file = file_name, overwrite = TRUE)
  return(data)
}




