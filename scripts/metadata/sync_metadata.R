library(purrr)
library(tidyr)
library(glue)
library(here)
library(stringr)
library(readr)
suppressPackageStartupMessages(library(dplyr))
source(here("scripts/utils/list_datasets.R"))
source(here("scripts/utils/airtable.R"))

wlog <- \(msg, file) {
  message(msg)
  write_lines(msg, file, append = TRUE)
}

# fetch metdata from contents of airtable table
fetch_metadata <- \(src_base, src_table) {
  
  # field names to export
  export_fields <- c("name", "label", "description", "tables")
  records <- fetch_airtable(src_base = src_base, src_table = src_table, export_fields = export_fields)
  
  # replace NAs with empty strings
  df <- records |>
    arrange(name) |>
    # mutate(across(everything(), \(s) if_else(s %in% c("NA", "NULL"), "", s))) |>
    mutate(across(everything(), \(s) replace_na(s, "")))
  
  # restructure tibble into nested list
  df |> as.list() |> list_transpose() |> map(as.list)
}

# sync metadata to redivis dataset
sync_dataset <- \(ds_name, metadata, log_file) {
  wlog(glue("\n\nSyncing metadata to dataset <{ds_name}>..."), log_file)
  
  # fetch dataset
  ds <- redivis::redivis$organization("levante")$dataset(ds_name)
  # fetch all variable names in dataset
  ds_var <- ds$list_tables() |> map(\(tbl) tbl$list_variables()) |> list_flatten()
  if (length(ds_var) == 0) {
    wlog("Dataset has zero variables, skipping", log_file)
    return(TRUE)
  }
  
  # compare set of airtable variable names against dataset variable names
  ds_var_names <- ds_var |> map(\(v) v$name) |> unlist() |> unique()
  md_survey <- metadata |> keep(\(d) all(d$tables == "surveys"))
  md_survey_names <- unlist(transpose(md_survey)$name)
  md_var_names <- unlist(transpose(metadata)$name)
  missing_vars <- setdiff(ds_var_names, md_var_names)
  extra_vars <- setdiff(md_var_names, ds_var_names) |> setdiff(md_survey_names)
  
  # issue warnings for variable discrepancies
  for (v in missing_vars) {
    wlog(glue("Variable <{v}> is present in Redivis dataset but not in Airtable table"),
         log_file)
  }
  for (v in extra_vars) {
    wlog(glue("Variable <{v}> is present in Airtable table but not in Redivis dataset"),
         log_file)
  }
  
  # update dataset metadata
  ds$update_variables(metadata)
  return(TRUE)
}
