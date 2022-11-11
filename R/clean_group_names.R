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
