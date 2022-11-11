


# data = hold
# groups = "ckd"
# cont_vars = "k_change_from_last"
# bin_cat_vars = "sex"

summarize_cont_vars = function(data, groups, cont_vars, rounding_digits){

  #message("Continous Variables...")

  dataset = data
  groups_var = groups
  cont_variables = cont_vars
  cont_summary = data.frame(stringsAsFactors = F)

  for (variable_name in cont_variables){
    print(variable_name)


    # Define the NA variable to determine if missing data
    dataset = dataset %>%
      mutate(
        !!as.name(paste0(variable_name, "_NA")) := if_else(is.na(!!as.name(variable_name)), 1, 0)
      )

    accuracy_val = case_when(
      rounding_digits == 4 ~ .0001,
      rounding_digits == 3 ~ .001,
      rounding_digits == 2 ~ .01,
      rounding_digits == 1 ~ .1,
      rounding_digits == 0 ~ 1,
      T ~ .001
    )

    # Calculate the relevant mean/sd + p_values
    var_sum = dataset %>%
      group_by(!!as.name(groups_var)) %>%
      summarize(
        mean = mean(!!as.name(variable_name), na.rm = T) %>% round(rounding_digits),
        sd = sd(!!as.name(variable_name), na.rm = T) %>% round(rounding_digits),
        med = median(!!as.name(variable_name), na.rm = T) %>% round(rounding_digits),
        q1 = quantile(!!as.name(variable_name), 0.25, na.rm = T) %>% round(rounding_digits),
        q3 = quantile(!!as.name(variable_name), 0.75, na.rm = T) %>% round(rounding_digits)
      ) %>%
      mutate(
        mean_sd = paste0((mean %>% number(big.mark = ",", accuracy = accuracy_val)), " (", (sd %>% number(big.mark = ",", accuracy = accuracy_val)), ")"),
        med_iqr = paste0((med %>% number(big.mark = ",", accuracy = accuracy_val)), " [", (q1 %>% number(big.mark = ",", accuracy = accuracy_val)), ", ",
                         (q3 %>% number(big.mark = ",", accuracy = accuracy_val)), "]")
      )

    #message(paste0("Variable name: ", variable_name))
    #message(var_sum)

    # Shapiro test sample size must be between 3 and 5000
    if (nrow(dataset) >= 5000){
      shapiro_df = dataset |> sample_n(5000)
    } else {
      shapiro_df = dataset
    }

    non_normal_dist = (tidy(shapiro.test(shapiro_df[[variable_name]])))$p.value < 0.05


    if (non_normal_dist) {
      summary_type = "med_iqr"
      other_var = "mean_sd"
      label = ", median [IQR]"
    } else {
      summary_type = "mean_sd"
      other_var = "med_iqr"
      label = ", mean (SD)"
    }

    var_sum = var_sum %>%
      rename(value = !!as.name(summary_type)) %>%
      select(!!as.name(groups_var), value)

    #print(var_sum$value)

    var_sum1 = var_sum %>%
      pivot_wider(names_from = !!as.name(groups_var),
                  values_from = c("value"))

    names(var_sum1) = paste0(groups_var, "_", names(var_sum1))


    # Calculate the NA values
    var_sum_na = dataset %>%
      group_by(!!as.name(groups_var)) %>%
      summarize(
        n = n(),
        na_n = sum(!!as.name(paste0(variable_name, "_NA"))),
      ) %>%
      mutate(
        hold_val = !!as.name(groups_var),
        na_p = (na_n / n * 100),
        na = paste0(na_n %>% number(big.mark = ",", accuracy = 1), " (", round(na_p, 1), ")")
      ) %>%
      select(!!as.name(groups_var), na) %>%
      pivot_wider(names_from = !!as.name(groups_var),
                  values_from = c("na"))

    #print("VAR SUM NA")
    names(var_sum_na) = names(var_sum1)
    #print(var_sum_na)


    var_sum1 = var_sum1 %>%
      mutate(
        characteristic = (variable_name %>% str_replace_all("_", " ") %>% str_to_title),
        characteristic = paste0(characteristic, label)
      )

    var_sum_na = var_sum_na %>%
      mutate(
        characteristic = (variable_name %>% str_replace_all("_", " ") %>% str_to_title),
        characteristic = paste0("     Missing, n (%)")
      )

    # Calculate the p-values for the continuous variable
    if ((dataset[[groups_var]] %>% unique %>% length) > 2) {
      anova_results = aov(dataset[[variable_name]] ~
                               dataset[[groups_var]]) %>% tidy()
      p_val = (anova_results$p.value[1]) %>% round(3)
      test_type = "ANOVA"
    } else if (non_normal_dist) {

      # If Shapiro test p-value is <0.05, then variable is not normally distributed
      # and needs wilcoxon rank sum test

      wilcoxon_rank_sum_results = wilcox.test(dataset[[variable_name]] ~
                                                dataset[[groups_var]], exact = F) %>% tidy()
      p_val = wilcoxon_rank_sum_results$p.value %>% round(3)
      test_type = "Wilcoxon Rank Sum"
    } else {
      ttest_results = t.test(dataset[[variable_name]] ~
                               dataset[[groups_var]]) %>% tidy()
      p_val = ttest_results$p.value %>% round(3)
      test_type = "Student's t-Test"
    }
    var_sum1$p_values = p_val
    var_sum1$statistical_test = test_type

    #print(head(var_sum1))

    # Calculate the p-values for the NA values of the continuous variables

    if (sum(dataset[[paste0(variable_name, "_NA")]], na.rm = T) > 0){
      dataset_subset = dataset %>% filter(!is.na(!!as.name(paste0(variable_name, "_NA"))))
      fisher_results = fisher.test(dataset_subset[[paste0(variable_name, "_NA")]],
                                   dataset_subset[[groups_var]], simulate.p.value=TRUE) %>% tidy()
      p_val = fisher_results$p.value %>% round(3)
      var_sum_na$p_values = p_val
      var_sum_na$statistical_test = "Fisher's Exact"

      cont_summary = bind_rows(cont_summary, var_sum1, var_sum_na)
    } else {
      cont_summary = bind_rows(cont_summary, var_sum1)
    }



  }


  return(cont_summary)
}

summarize_bin_cat_vars = function(data, groups, bin_cat_vars, bin_cat_vars_subpop = c()){

  #message("Binary and Categorical Variables")

  # data = hold
  # bin_cat_vars = "bb__on_med"
  # groups = "gt1_hf_visit"
  # bin_cat_vars_subpop = c()

  dataset = data

  dataset = dataset %>%
    mutate(
      # Convert all values to character (easier for analysis later)
      across(all_of(bin_cat_vars), ~ as.character(.x)),
      # Convert all empty strings to NA
      across(all_of(bin_cat_vars), ~ if_else(.x == "", NA_character_, .x)),
      # Convert all NA strings to Missing
      across(all_of(bin_cat_vars), ~ if_else(is.na(.x), "Missing", .x)),
    )


  dataset = dataset %>%
    # Create dummy columns for all variables
    dummy_cols(bin_cat_vars)

  # Standardize the naming convention
  names(dataset) = names(dataset) |> str_replace_all("[:punct:]|[:symbol:]|[:space:]", "_") |> str_to_lower()

  # Hold onto the original group names
  orig_groups_var = groups

  # Clean up the group and variable names
  groups_var = groups |> str_replace_all("[:punct:]|[:symbol:]|[:space:]", "_") |> str_to_lower()
  variables = bin_cat_vars |> str_replace_all("[:punct:]|[:symbol:]|[:space:]", "_") |> str_to_lower()

  # Now clean up the variable names to make them look a little nicer
  variables_nice = variables %>% str_replace_all("_", " ") %>% str_to_title()

  # Get the unique values in the group variable
  group_values = dataset[[groups_var]] %>% unique %>% sort

  # Create column titles for the group variable (this matches the other variable analysis)
  group_values_for_col = paste0(orig_groups_var, "_", group_values)


  # Now, for each variable, will need to do the statistical analysis
  all_summary = data.frame(stringsAsFactors = F)
  for (variable_name in variables){
    #variable_name = variables
    print(variable_name)
    dataset_hold = dataset

    # If there is a variable that needs to be assessed relative to a sub-
    # population
    if (length(bin_cat_vars_subpop) > 0) {

      # Get the original variable name (this is janky because I am
      # looping through the "nice" variable names, and have to go find the
      # original variable name in the original array)
      original_var = bin_cat_vars[str_detect(variable_name, bin_cat_vars)]

      # Get the index of the variable name so that it can be paired up with
      # the respective index of the bin_cat_vars_subpop array
      index = detect_index(bin_cat_vars, ~ . == original_var)
      subpop_var = bin_cat_vars_subpop[index]

      # print(paste0("Original var: ", original_var))
      # print(paste0("Index: ", index))
      # print(paste0("Subpop_val: ", subpop_var))

      # Now filter the dataset to where the subpop_val is equal to 1
      # ...there should probably be a way to make this any value

      # Check to make sure subpop value is not empty string
      if (subpop_var != ""){
        dataset_hold = dataset %>% filter(!!as.name(subpop_var) == 1)
      }
    }

    #print(variable_name)

    # Get population of group and subgroups
    subgroup_pop = dataset_hold %>%
      group_by(!!as.name(variable_name), !!as.name(groups_var)) %>%
      summarize(
        subgroup_n = n()
      )

    group_pop = dataset_hold %>%
      group_by(!!as.name(groups_var)) %>%
      summarize(
        group_n = n()
      )

    sub_group_pop_w_p = subgroup_pop %>%
      inner_join(group_pop, by = groups_var) %>%
      mutate(
        subgroup_p_of_group = (subgroup_n / group_n * 100)
      )

    var_values = dataset_hold[[variable_name]] |>
      unique() |>
      sort() |>
      str_replace_all("[:punct:]|[:symbol:]|[:space:]", "_") |>
      str_to_lower()

    # Special circumstance if binary only
    is_binary = identical(var_values, c("0", "1"))
    if (is_binary){var_values = c("1")}

    var_values = paste0(variable_name, "_", var_values)

    # # There is a possibility that if a subpop is used, one of the
    # # categories is no longer present in the data
    # if (length(dataset[[variable_name]] %>% unique) == 0){next}


    # Calculate p_values for each unique value in the column between groups
    p_vals = c()

    # For each value in variable ()
    for (value in var_values){

      #print(value)
      #value = "bb__on_med_1"

      dataset_subset = dataset_hold |>
        filter(!is.na(!!as.name(value)))

      num_subgroups = dataset_subset |>
        group_by(!!as.name(groups_var), !!as.name(value)) |>
        summarize(n = n()) |>
        nrow()

     if (num_subgroups <= 2) {
       p_val = NA_real_
     } else {
        #print("doing fisher")
        fisher_results = fisher.test(dataset_subset[[value]],
                                     dataset_subset[[groups_var]],
                                     simulate.p.value=TRUE) |>
          tidy()
        p_val = fisher_results$p.value |> round(3)
     }

      p_vals = c(p_vals, p_val)
    }


    summary3 = sub_group_pop_w_p %>%
      pivot_wider(names_from = !!as.name(groups_var),
                  values_from = c("subgroup_n", "group_n", "subgroup_p_of_group"))

    for (i in 1:length(group_values)){

      # NOTE: PIPE DOES NOT WORK HERE FOR SOME REASON
      summary3 = summary3 %>%
        mutate(
          !!as.name(group_values_for_col[i]) :=
            paste0(
              number(!!as.name(paste0("subgroup_n_", group_values[i])), big.mark = ",", accuracy = 1),
                   " (",
                   round(!!as.name(paste0("subgroup_p_of_group_", group_values[i])), 1),
                    ")"),
          !!as.name(group_values_for_col[i]) :=
              str_replace_all(!!as.name(group_values_for_col[i]), "NA", "0")
        )
    }

    summary3 = summary3 %>%
      mutate(
        !!as.name(variable_name) := paste0("     ", !!as.name(variable_name))
      ) %>%
      rename(characteristic = !!as.name(variable_name)) %>%
      select(characteristic, all_of(group_values_for_col))
    summary3$p_values = p_vals

    category_title = variables_nice[detect_index(variables, ~ . == variable_name)]
    category_title = paste0(category_title, ", n (%)")

    if (is_binary){
      summary4 = summary3 |>
        filter(str_detect(characteristic, "1")) |>
        mutate(
          characteristic = category_title
        )
    } else {
      top_row = list(characteristic = category_title,  p_values = NA_real_)
      summary4 = bind_rows(top_row, summary3)
    }




    # Place missing values summary at end
    #print(any(str_detect(summary4$characteristic, "     Missing$")))
    if (any(str_detect(summary4$characteristic, "     Missing$"))){
      missing_row = summary4 %>% filter(str_detect(characteristic, "     Missing$"))
      hold = summary4 %>% filter(!str_detect(characteristic, "     Missing$"))
      summary4 = bind_rows(hold, missing_row)
    }

    all_summary = all_summary |> bind_rows(summary4)
  }

  all_summary = all_summary |>
    mutate(
      statistical_test = if_else(p_values != "", "Fisher's Exact", "")
    )

  return(all_summary)
}


# Create the first row
summarize_group = function(data, groups) {

  group_col_name = groups

  pop = data %>%
    mutate(
      !!as.name(groups) := paste0(group_col_name, "_", !!as.name(groups))
    ) |>
    group_by(!!as.name(groups)) %>%
    summarize(
      group_n = n() |> number(big.mark = ",", accuracy = 1)
    ) |>
    pivot_wider(names_from = groups, values_from = group_n)%>%
    mutate(
      characteristic = "Total Count, n",
      p_values = NA_real_
    ) |>
    select(characteristic, contains(groups), p_values)


  return(pop)
}

clean_group_names = function(summary, data, groups){
  # Fix the group names
  dataset = data
  groups_var = groups
  group_values = dataset[[groups_var]] %>% unique %>% sort
  group_values_for_col = paste0(groups_var, "_", group_values)


  groups_var_nice = groups_var %>%
    str_replace_all("_", " ") %>%
    str_to_title() %>%
    paste0(" - ") %>%
    paste0(group_values)

  names(summary)[str_detect(names(summary), groups_var)] = groups_var_nice

  summary = summary %>%
    rename(Characteristic=characteristic, `p-value` = p_values)

  return(summary)
}

remove_na_cols = function(data, groups, cols_to_exclude_NA) {
  # If selected, remove all rows where the group value is NA
  if (remove_group_NA == T){
    n_rows_original = data |> nrow()
    data = data |> filter(!is.na(!!as.name(groups)))
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
    data = data %>% drop_na(all_of(cols_to_exclude_NA))
    nrow_removed = orig_nrow - nrow(data)
    message(paste0("Additional", nrow_removed, " rows removed due to
                   missing/NA values in the following columns: ",
                   paste(cols_to_exclude_NA, collapse = ",")))

  }
  return(data)
}

argument_check = function(data, argument, argument_name){

  # message(argument_name)
  # message(argument)


  out = list(error = 0, error_val = "")
  # Make sure it's a character type
  if (typeof(argument) != "character"){
    out$error = 1
    out$error_val = paste0("The argument ", argument, " must be of type 'character'")
  }

  var_check = (argument %>% .[.!=""]) %in% names(data)
  #message(var_check)
  # Make sure all of the values are in the dataset
  if (!all(var_check)) {
    missing_vars = argument[!varcheck] |> paste0(collapse = ", ")
    out$error = 1
    out$error_val = paste0("The following columns in the ", argument_name, " argument are not in the dataset: ", missing_vars)
  }
  return(out)
}



summarize_vars_by_group = function(data, groups,
                                   bin_cat_vars = NULL,
                                   cont_vars = NULL,
                                   cols_to_exclude_NA = NULL,
                                   bin_cat_vars_subpop = NULL,
                                   bin_cat_vars_subpop_val = NULL,
                                   remove_group_NA = T,
                                   rounding_digits = 1) {

  message("Checking variables...")

  # Check to make sure there is at least one variable listed
  if (is.null(bin_cat_vars) & is.null(cont_vars)){
    stop("Either the bin_cat_vars or cont_vars arguments must have at least one value")
  }


  # Checks for the binary/categorical variables
  if (!is.null(bin_cat_vars)) {

    check = argument_check(data, bin_cat_vars, "bin_cat_vars")
    if (check$error == 1) stop(check$error_val)

    # If a subpopulation is specified, do the same for those variables
    if (!is.null(bin_cat_vars_subpop)){
      check = argument_check(data, bin_cat_vars_subpop, "bin_cat_vars_subpop")
      if (check$error == 1) stop(check$error_val)

      if (length(bin_cat_vars_subpop) != length(bin_cat_vars)){
        stop("'bin_cat_vars' and 'bin_cat_vars_subpop' must be the same length")
      }

      if (!is.null(bin_cat_vars_subpop_val)) {

        if (length(bin_cat_vars_subpop != length(bin_cat_vars_subpop_val))){
          stop("If 'bin_cat_vars_subpop_val' is specified, it must be the same length as 'bin_cat_vars_subpop'")
        }
      }
    }
  }

  # Now make checks for continuous variables
  if (!is.null(cont_vars)){
    check = argument_check(data, cont_vars, "cont_vars")

    if (check$error == 1) stop(check$error_val)
  }

  # Now make checks for the "remove NA cols"

  # Note: I've yet to implement multiple group variables...should that
  # even be a feature?

  if (!is.null(cols_to_exclude_NA)){
    check = argument_check(data, cols_to_exclude_NA, "cols_to_exclude_NA")
    if (check$error == 1) stop(check$error_val)

    message("Removing NAs from listed columns...")
    data = remove_na_cols(data, groups, cols_to_exclude_NA)
  }



  # If group variable is factor/integer, convert to character
  #message("we got here")
  if (typeof(unlist(data[[groups]])) == "integer") {
    message("Converting group column...")
    data[[groups]] = as.character(data[[groups]])
  }

  # Start the summary table with the total count of the unique groups
  message("Summarizing groups...")
  all_summ = summarize_group(data, groups)

  message("Analyzing the binary/categorical columns")
  if (!is.null(bin_cat_vars)) {
    bin_cat_summ = summarize_bin_cat_vars(data,
                                          groups = groups,
                                          bin_cat_vars = bin_cat_vars,
                                          bin_cat_vars_subpop = bin_cat_vars_subpop)
    all_summ = bind_rows(all_summ, bin_cat_summ)
  }

  message("Analyzing the continuous columns")
  if (!is.null(cont_vars)) {

    cont_summ = summarize_cont_vars(data,
                                    groups = groups,
                                    cont_vars = cont_vars,
                                    rounding_digits = rounding_digits)

    all_summ = bind_rows(all_summ, cont_summ)

  }

  # Prettify the column names
  message("Cleaning up column names...")
  all_summ = clean_group_names(all_summ, data, groups)

  # Clean the p-values
  all_summ = all_summ %>%
    mutate(
      `p-value` = round(`p-value`, 2),
    ) |>
    mutate_if(is.numeric, as.character) %>%
    mutate(
      `p-value` = if_else(`p-value` == "0", "<0.01", `p-value`),
      `p-value` = str_replace_all(`p-value`, "0\\.", "\\.")
    )

  # Replace all NA values with ""
  all_summ[is.na(all_summ)] = ""

  return(all_summ)

}




