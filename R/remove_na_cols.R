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
