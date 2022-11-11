#' Quickly create publication-ready summary tables summarizing population sub-groups and testing for statistical differences.
#'
#' @export
#'
#' @param data A data frame or data frame extension
#' @param group_col The column/variable by which the data will be grouped
#' @param binary_or_cat_cols Columns to be summarized as binary/categorical variables.
#' @param continuous_cols Columns to be summarized as continuous variables.
#' @param remove_group_col_NA A logical value indicating whether NA values in the 'group_col' should be dropped before the computation proceeds
#' @param cols_to_remove_NA Columns for which rows will be dropped if value is missing.
#' @param binary_or_cat_cols_subpop If specified, columns to be used for population counts for the binary_or_cat_cols.
#' @param binary_or_cat_cols_subpop_val Values defining the appropriate sub-populations within the 'binary_or_cat_cols_subpop' argument
#' @param rounding_digits Digits used for rounding table values and p-values
#'
#'
#' @return A data frame
#'
#' @examples
#' summary = iris |>
#'   summarize_cols_by_group(
#'     group_col = "Species",
#'     continuous_cols = c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width"),
#'     rounding_digits = 1
#'   )
#'
#' summary = mtcars |>
#'   summarize_cols_by_group(
#'     group_col = "cyl",
#'     binary_or_cat_cols = c("vs", "am", "gear", "carb"),
#'     continuous_cols = c("mpg", "disp", "hp", "drat", "wt", "qsec")
#'   )

summarize_cols_by_group = function(data, group_col,
                                   binary_or_cat_cols = NULL,
                                   continuous_cols = NULL,
                                   remove_group_col_NA = T,
                                   cols_to_remove_NA = NULL,
                                   binary_or_cat_cols_subpop = NULL,
                                   binary_or_cat_cols_subpop_val = NULL,
                                   rounding_digits = 1) {

  message("Checking variables...")

  # Check to make sure there is at least one variable listed
  if (is.null(binary_or_cat_cols) & is.null(continuous_cols)){
    stop("Either the binary_or_cat_cols or continuous_cols arguments must have at least one value")
  }

  # Checks for the binary/categorical variables
  if (!is.null(binary_or_cat_cols)) {

    check = argument_check(data, binary_or_cat_cols, "binary_or_cat_cols")
    if (check$error == 1) stop(check$error_val)

    # If a subpopulation is specified, do the same for those variables
    if (!is.null(binary_or_cat_cols_subpop)){
      check = argument_check(data, binary_or_cat_cols_subpop, "binary_or_cat_cols_subpop")
      if (check$error == 1) stop(check$error_val)

      if (length(binary_or_cat_cols_subpop) != length(binary_or_cat_cols)){
        stop("'binary_or_cat_cols' and 'binary_or_cat_cols_subpop' must be the same length")
      }

      if (!is.null(binary_or_cat_cols_subpop_val)) {

        if (length(binary_or_cat_cols_subpop != length(binary_or_cat_cols_subpop_val))){
          stop("If 'binary_or_cat_cols_subpop_val' is specified, it must be the same length as 'binary_or_cat_cols_subpop'")
        }
      }
    }
  }

  # Now make checks for continuous variables
  if (!is.null(continuous_cols)){
    check = argument_check(data, continuous_cols, "continuous_cols")

    if (check$error == 1) stop(check$error_val)
  }

  # Now make checks for the "remove NA cols"

  # Note: I've yet to implement multiple group variables...should that
  # even be a feature?

  if (!is.null(cols_to_remove_NA)){
    check = argument_check(data, cols_to_remove_NA, "cols_to_remove_NA")
    if (check$error == 1) stop(check$error_val)

    message("Removing NAs from listed columns...")
    data = remove_na_cols(data, group_col, cols_to_remove_NA, remove_group_col_NA)
  }



  # If group variable is factor/integer, convert to character
  #message("we got here")
  if (typeof(unlist(data[[group_col]])) == "integer") {
    message("Converting group column...")
    data[[group_col]] = as.character(data[[group_col]])
  }

  # Start the summary table with the total count of the unique group_col
  message("Summarizing groups...")
  all_summ = summarize_group(data, group_col)

  message("Analyzing the binary/categorical columns")
  if (!is.null(binary_or_cat_cols)) {
    bin_cat_summ = summarize_bin_cat_vars(data,
                                          groups = group_col,
                                          bin_cat_vars = binary_or_cat_cols,
                                          bin_cat_vars_subpop = binary_or_cat_cols_subpop)
    all_summ = dplyr::bind_rows(all_summ, bin_cat_summ)
  }

  message("Analyzing the continuous columns")
  if (!is.null(continuous_cols)) {

    cont_summ = summarize_cont_vars(data,
                                    groups = group_col,
                                    cont_vars = continuous_cols,
                                    rounding_digits = rounding_digits)

    all_summ = dplyr::bind_rows(all_summ, cont_summ)

  }

  # Prettify the column names
  message("Cleaning up column names...")
  all_summ = clean_group_names(all_summ, data, group_col)

  # Clean the p-values
  all_summ = clean_p_values(all_summ)


  # Replace all NA values with ""
  all_summ[is.na(all_summ)] = ""

  return(all_summ)

}

