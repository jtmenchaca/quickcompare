utils::globalVariables(c(":=", "characteristic", "group_n", "med",
                         "n", "na", "na_n", "na_p", "num_removed", "p-value",
                         "p_values", "q1", "q3", "sample_n", "sd",
                         "subgroup_n", "value", "varcheck"))


clean_p_values = function(data){
  data = data |>
    dplyr::mutate(
      `p-value` = round(`p-value`, 2),
    ) |>
    dplyr::mutate_if(is.numeric, as.character) |>
    dplyr::mutate(
      `p-value` = dplyr::if_else(`p-value` == "0", "<0.01", `p-value`),
      `p-value` = stringr::str_replace_all(`p-value`, "0\\.", "\\.")
    )
  return(data)
}


clean_group_names = function(summary, data, groups){
  #message("clean_group_names")
  # Fix the group names

  dataset = data
  groups_var = groups
  group_values = dataset[[groups_var]] |> unique() |> sort()
  group_values_for_col = paste0(groups_var, "_", group_values)


  groups_var_nice = groups_var |>
    stringr::str_replace_all("_", " ") |>
    stringr::str_to_title() |>
    paste0(" - ") |>
    paste0(group_values)

  names(summary)[stringr::str_detect(names(summary), groups_var)] = groups_var_nice

  summary = summary |>
    dplyr::rename(Characteristic=characteristic, `p-value` = p_values)

  return(summary)
}

remove_na_cols = function(data, groups, cols_to_exclude_NA, remove_group_NA) {
  #message("remove_na_cols")

  # If selected, remove all rows where the group value is NA
  if (remove_group_NA == T){
    n_rows_original = data |> nrow()
    data = data |> dplyr::filter(!is.na(!!as.name(groups)))
    n_rows_after = n_rows_original - nrow(data)
    message(paste0(num_removed, " rows removed due to NA values in ", groups, " group"))
  }

  # If other columns should have NA removed
  if (length(cols_to_exclude_NA) > 0){
    if (typeof(cols_to_exclude_NA) != "character"){
      stop("argument 'cols_to_exclude_NA' must be of type 'character'")
    }

    # Convert empty string to NA_character_
    for(var in cols_to_exclude_NA){
      if (typeof(unlist(data[[var]])) == "character"){
        data[[var]][data[[var]] == ""] = NA_character_
      } else {
        # Can't remember why I did this...doesn't seem like it should matter
        data[[var]][data[[var]] == ""] = NA_integer_
      }
    }

    orig_nrow = nrow(data)
    data = data |>
      tidyr::drop_na(
        tidyselect::all_of(cols_to_exclude_NA)
      )
    nrow_removed = orig_nrow - nrow(data)
    message(paste0("Additional", nrow_removed, " rows removed due to
                   missing/NA values in the following columns: ",
                   paste(cols_to_exclude_NA, collapse = ",")))

  }
  return(data)
}


summarize_cont_vars = function(data, groups, cont_vars, rounding_digits){
  #message("summarize_cont_vars")
  #message("Continous Variables...")

  dataset = data
  groups_var = groups
  cont_variables = cont_vars
  cont_summary = data.frame(stringsAsFactors = F)

  for (variable_name in cont_variables){
    #print(variable_name)


    # Define the NA variable to determine if missing data
    dataset = dataset |>
      dplyr::mutate(
        !!as.name(paste0(variable_name, "_NA")) := dplyr::if_else(is.na(!!as.name(variable_name)), 1, 0)
      )

    accuracy_val = dplyr::case_when(
      rounding_digits == 4 ~ .0001,
      rounding_digits == 3 ~ .001,
      rounding_digits == 2 ~ .01,
      rounding_digits == 1 ~ .1,
      rounding_digits == 0 ~ 1,
      T ~ .001
    )

    # Calculate the relevant mean/sd + p_values
    var_sum = dataset |>
      dplyr::group_by(!!as.name(groups_var)) |>
      dplyr::summarize(
        mean = mean(!!as.name(variable_name), na.rm = T) |> round(rounding_digits),
        sd = stats::sd(!!as.name(variable_name), na.rm = T) |> round(rounding_digits),
        med = stats::median(!!as.name(variable_name), na.rm = T) |> round(rounding_digits),
        q1 = stats::quantile(!!as.name(variable_name), 0.25, na.rm = T) |> round(rounding_digits),
        q3 = stats::quantile(!!as.name(variable_name), 0.75, na.rm = T) |> round(rounding_digits)
      ) |>
      dplyr::mutate(
        mean_sd = paste0((mean |> scales::number(big.mark = ",", accuracy = accuracy_val)), " (", (sd |> scales::number(big.mark = ",", accuracy = accuracy_val)), ")"),
        med_iqr = paste0((med |> scales::number(big.mark = ",", accuracy = accuracy_val)), " [", (q1 |> scales::number(big.mark = ",", accuracy = accuracy_val)), ", ",
                         (q3 |> scales::number(big.mark = ",", accuracy = accuracy_val)), "]")
      )

    #message(paste0("Variable name: ", variable_name))
    #message(var_sum)

    # Shapiro test sample size must be between 3 and 5000
    if (nrow(dataset) >= 5000){
      shapiro_df = dataset |> sample_n(5000)
    } else {
      shapiro_df = dataset
    }

    non_normal_dist = (broom::tidy(stats::shapiro.test(shapiro_df[[variable_name]])))$p.value < 0.05


    if (non_normal_dist) {
      summary_type = "med_iqr"
      other_var = "mean_sd"
      label = ", median [IQR]"
    } else {
      summary_type = "mean_sd"
      other_var = "med_iqr"
      label = ", mean (SD)"
    }

    var_sum = var_sum |>
      dplyr::rename(value = !!as.name(summary_type)) |>
      dplyr::select(!!as.name(groups_var), value)

    #print(var_sum$value)

    var_sum1 = var_sum |>
      tidyr::pivot_wider(names_from = !!as.name(groups_var),
                  values_from = c("value"))

    names(var_sum1) = paste0(groups_var, "_", names(var_sum1))


    # Calculate the NA values
    var_sum_na = dataset |>
      dplyr::group_by(!!as.name(groups_var)) |>
      dplyr::summarize(
        n = dplyr::n(),
        na_n = sum(!!as.name(paste0(variable_name, "_NA"))),
      ) |>
      dplyr::mutate(
        hold_val = !!as.name(groups_var),
        na_p = (na_n / n * 100),
        na = paste0(na_n |> scales::number(big.mark = ",", accuracy = 1), " (", round(na_p, 1), ")")
      ) |>
      dplyr::select(!!as.name(groups_var), na)|>
      tidyr::pivot_wider(names_from = !!as.name(groups_var),
                  values_from = c("na"))

    #print("VAR SUM NA")
    names(var_sum_na) = names(var_sum1)
    #print(var_sum_na)


    var_sum1 = var_sum1 |>
      dplyr::mutate(
        characteristic = (variable_name |>
                            stringr::str_replace_all("_", " ") |>
                            stringr::str_to_title()),

        characteristic = paste0(characteristic, label)
      )

    var_sum_na = var_sum_na |>
      dplyr::mutate(
        characteristic = (variable_name |>
                            stringr::str_replace_all("_", " ") |>
                            stringr::str_to_title()),

        characteristic = paste0("     Missing, n (%)")
      )

    # Calculate the p-values for the continuous variable
    if ((dataset[[groups_var]] |> unique() |> length()) > 2) {
      anova_results = stats::aov(dataset[[variable_name]] ~
                            dataset[[groups_var]]) |> broom::tidy()
      p_val = (anova_results$p.value[1]) |> round(3)
      test_type = "ANOVA"
    } else if (non_normal_dist) {

      # If Shapiro test p-value is <0.05, then variable is not normally distributed
      # and needs wilcoxon rank sum test

      wilcoxon_rank_sum_results = stats::wilcox.test(dataset[[variable_name]] ~
                                                dataset[[groups_var]], exact = F) |> broom::tidy()
      p_val = wilcoxon_rank_sum_results$p.value |> round(3)
      test_type = "Wilcoxon Rank Sum"
    } else {
      ttest_results = stats::t.test(dataset[[variable_name]] ~
                               dataset[[groups_var]]) |> broom::tidy()
      p_val = ttest_results$p.value |> round(3)
      test_type = "Student's t-Test"
    }
    var_sum1$p_values = p_val
    var_sum1$statistical_test = test_type

    #print(head(var_sum1))

    # Calculate the p-values for the NA values of the continuous variables

    if (sum(dataset[[paste0(variable_name, "_NA")]], na.rm = T) > 0){
      dataset_subset = dataset |> dplyr::filter(!is.na(!!as.name(paste0(variable_name, "_NA"))))
      fisher_results = stats::fisher.test(dataset_subset[[paste0(variable_name, "_NA")]],
                                   dataset_subset[[groups_var]], simulate.p.value=TRUE) |> broom::tidy()
      p_val = fisher_results$p.value |> round(3)
      var_sum_na$p_values = p_val
      var_sum_na$statistical_test = "Fisher's Exact"

      cont_summary = dplyr::bind_rows(cont_summary, var_sum1, var_sum_na)
    } else {
      cont_summary = dplyr::bind_rows(cont_summary, var_sum1)
    }



  }


  return(cont_summary)
}

summarize_bin_cat_vars = function(data, groups, bin_cat_vars, bin_cat_vars_subpop = c()){
  #message("summarize_bin_cat_vars")

  dataset = data

  dataset = dataset |>
    dplyr::mutate(
      # Convert all values to character (easier for analysis later)
      dplyr::across(tidyselect::all_of(bin_cat_vars), ~ as.character(.x)),
      # Convert all empty strings to NA
      dplyr::across(tidyselect::all_of(bin_cat_vars), ~ dplyr::if_else(.x == "", NA_character_, .x)),
      # Convert all NA strings to Missing
      dplyr::across(tidyselect::all_of(bin_cat_vars), ~ dplyr::if_else(is.na(.x), "Missing", .x)),
    )


  dataset = dataset |>
    # Create dummy columns for all variables
    fastDummies::dummy_cols(bin_cat_vars)

  # Standardize the naming convention
  names(dataset) = names(dataset) |>
    stringr::str_replace_all("[:punct:]|[:symbol:]|[:space:]", "_") |>
    stringr::str_to_lower()

  # Hold onto the original group names
  orig_groups_var = groups

  # Clean up the group and variable names
  groups_var = groups |>
    stringr::str_replace_all("[:punct:]|[:symbol:]|[:space:]", "_") |>
    stringr::str_to_lower()

  variables = bin_cat_vars |>
    stringr::str_replace_all("[:punct:]|[:symbol:]|[:space:]", "_") |>
    stringr::str_to_lower()

  # Now clean up the variable names to make them look a little nicer
  variables_nice = variables |>
    stringr::str_replace_all("_", " ") |>
    stringr::str_to_title()

  # Get the unique values in the group variable
  group_values = dataset[[groups_var]] |>
    unique() |>
    sort()

  # Create column titles for the group variable (this matches the other variable analysis)
  group_values_for_col = paste0(orig_groups_var, "_", group_values)


  # Now, for each variable, will need to do the statistical analysis
  all_summary = data.frame(stringsAsFactors = F)
  for (variable_name in variables){
    #variable_name = variables
    #print(variable_name)
    dataset_hold = dataset

    # If there is a variable that needs to be assessed relative to a sub-
    # population
    if (length(bin_cat_vars_subpop) > 0) {

      # Get the original variable name (this is janky because I am
      # looping through the "nice" variable names, and have to go find the
      # original variable name in the original array)
      original_var = bin_cat_vars[stringr::str_detect(variable_name, bin_cat_vars)]

      # Get the index of the variable name so that it can be paired up with
      # the respective index of the bin_cat_vars_subpop array
      index = purrr::detect_index(bin_cat_vars, ~ . == original_var)
      subpop_var = bin_cat_vars_subpop[index]

      # print(paste0("Original var: ", original_var))
      # print(paste0("Index: ", index))
      # print(paste0("Subpop_val: ", subpop_var))

      # Now filter the dataset to where the subpop_val is equal to 1
      # ...there should probably be a way to make this any value

      # Check to make sure subpop value is not empty string
      if (subpop_var != ""){
        dataset_hold = dataset |> dplyr::filter(!!as.name(subpop_var) == 1)
      }
    }

    #print(variable_name)

    # Get population of group and subgroups
    subgroup_pop = dataset_hold |>
      dplyr::group_by(!!as.name(variable_name), !!as.name(groups_var)) |>
      dplyr::summarize(
        subgroup_n = dplyr::n()
      )

    group_pop = dataset_hold |>
      dplyr::group_by(!!as.name(groups_var)) |>
      dplyr::summarize(
        group_n = dplyr::n()
      )

    sub_group_pop_w_p = subgroup_pop |>
      dplyr::inner_join(group_pop, by = groups_var) |>
      dplyr::mutate(
        subgroup_p_of_group = (subgroup_n / group_n * 100)
      )

    var_values = dataset_hold[[variable_name]] |>
      unique() |>
      sort() |>
      stringr::str_replace_all("[:punct:]|[:symbol:]|[:space:]", "_") |>
      stringr::str_to_lower()

    # Special circumstance if binary only
    is_binary = identical(var_values, c("0", "1"))
    if (is_binary){var_values = c("1")}

    var_values = paste0(variable_name, "_", var_values)

    # # There is a possibility that if a subpop is used, one of the
    # # categories is no longer present in the data
    # if (length(dataset[[variable_name]] |> unique) == 0){next}


    # Calculate p_values for each unique value in the column between groups
    p_vals = c()

    # For each value in variable ()
    for (value in var_values){

      #print(value)
      #value = "bb__on_med_1"

      dataset_subset = dataset_hold |>
        dplyr::filter(!is.na(!!as.name(value)))

      num_subgroups = dataset_subset |>
        dplyr::group_by(!!as.name(groups_var), !!as.name(value)) |>
        dplyr::summarize(n = dplyr::n()) |>
        nrow()

      if (num_subgroups <= 2) {
        p_val = NA_real_
      } else {
        #print("doing fisher")
        fisher_results = stats::fisher.test(dataset_subset[[value]],
                                     dataset_subset[[groups_var]],
                                     simulate.p.value=TRUE) |>
          broom::tidy()
        p_val = fisher_results$p.value |> round(3)
      }

      p_vals = c(p_vals, p_val)
    }


    summary3 = sub_group_pop_w_p |>
      tidyr::pivot_wider(names_from = !!as.name(groups_var),
                  values_from = c("subgroup_n", "group_n", "subgroup_p_of_group"))

    for (i in 1:length(group_values)){

      # NOTE: PIPE DOES NOT WORK HERE FOR SOME REASON
      summary3 = summary3 |>
        dplyr::mutate(
          !!as.name(group_values_for_col[i]) :=
            paste0(
              scales::number(!!as.name(paste0("subgroup_n_", group_values[i])), big.mark = ",", accuracy = 1),
              " (",
              round(!!as.name(paste0("subgroup_p_of_group_", group_values[i])), 1),
              ")"),
          !!as.name(group_values_for_col[i]) :=
            stringr::str_replace_all(!!as.name(group_values_for_col[i]), "NA", "0")
        )
    }

    summary3 = summary3 |>
      dplyr::mutate(
        !!as.name(variable_name) := paste0("     ", !!as.name(variable_name))
      ) |>
      dplyr::rename(characteristic = !!as.name(variable_name)) |>
      dplyr::select(characteristic, tidyselect::all_of(group_values_for_col))
    summary3$p_values = p_vals

    category_title = variables_nice[purrr::detect_index(variables, ~ . == variable_name)]
    category_title = paste0(category_title, ", n (%)")

    if (is_binary){
      summary4 = summary3 |>
        dplyr::filter(stringr::str_detect(characteristic, "1")) |>
        dplyr::mutate(
          characteristic = category_title
        )
    } else {
      top_row = list(characteristic = category_title,  p_values = NA_real_)
      summary4 = dplyr::bind_rows(top_row, summary3)
    }




    # Place missing values summary at end
    #print(any(stringr::str_detect(summary4$characteristic, "     Missing$")))
    if (any(stringr::str_detect(summary4$characteristic, "     Missing$"))){
      missing_row = summary4 |>
        dplyr::filter(stringr::str_detect(characteristic, "     Missing$"))
      hold = summary4 |>
        dplyr::filter(!stringr::str_detect(characteristic, "     Missing$"))
      summary4 = dplyr::bind_rows(hold, missing_row)
    }

    all_summary = all_summary |> dplyr::bind_rows(summary4)
  }

  all_summary = all_summary |>
    dplyr::mutate(
      statistical_test = dplyr::if_else(p_values != "", "Fisher's Exact", "")
    )

  return(all_summary)
}


# Create the first row
summarize_group = function(data, groups) {
  #message("summarize_group")
  group_col_name = groups

  pop = data |>
    dplyr::mutate(
      !!as.name(groups) := paste0(group_col_name, "_", !!as.name(groups))
    ) |>
    dplyr::group_by(!!as.name(groups)) |>
    dplyr::summarize(
      group_n = dplyr::n() |>
        scales::number(big.mark = ",", accuracy = 1)
    ) |>
    tidyr::pivot_wider(names_from = groups, values_from = group_n) |>
    dplyr::mutate(
      characteristic = "Total Count, n",
      p_values = NA_real_
    ) |>
    dplyr::select(characteristic, tidyselect::contains(groups), p_values)


  return(pop)
}


argument_check = function(data, argument, argument_name){
  #message("argument_check")
  # message(argument_name)
  # message(argument)


  out = list(error = 0, error_val = "")
  # Make sure it's a character type
  if (typeof(argument) != "character"){
    out$error = 1
    out$error_val = paste0("The argument ", argument, " must be of type 'character'")
  }

  var_check = (argument[argument!=""]) %in% names(data)
  #message(var_check)
  # Make sure all of the values are in the dataset
  if (!all(var_check)) {
    missing_vars = argument[!varcheck] |> paste0(collapse = ", ")
    out$error = 1
    out$error_val = paste0("The following columns in the ", argument_name, " argument are not in the dataset: ", missing_vars)
  }
  return(out)
}

