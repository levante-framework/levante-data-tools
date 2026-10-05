library(tidyverse)
library(redivis)
library(glue)
library(levantemodels)
library(here)

# shared task_code -> task_label/task_category mapping
source(here("..", "levante-data-meta", "task_info.R"))

scoring_dataset <- redivis$organization("levante")$dataset("levante_metadata_scoring:e97h")$get()

scoring_models_table <- scoring_dataset$table("scoring_models:t416")
scoring_table <- scoring_models_table$to_tibble()

registry_dir <- scoring_dataset$table("model_registry:rqwv")$to_directory()
registry_tbl <- scoring_dataset$table("model_registry:rqwv")$to_tibble()
registry_df <- registry_tbl |> select(file_name, file_id, added_at)

scoring_table_distinct <- scoring_table |> select(-dataset) |> distinct()
scoring_specs <- scoring_table_distinct |> as.list() |> transpose()

scoring_files <- scoring_specs |> map(model_spec_filename)
scoring_table_distinct$file_name <- unlist(scoring_files)
scoring_mods <- scoring_files |>
  map(\(mod_filename) get_registry_file(mod_filename, registry_dir))

# group-level parameters: the latent distribution of each group, i.e. the
# item == "GROUP" rows of mod2values that mod_coefs() drops. single-group
# (by_language) models have these fixed at mean 0 / var 1 by definition, so
# they contribute nothing and are skipped.
mod_group_coefs <- \(mod_rec) {
  if (model_class(mod_rec) != "MultipleGroupClass") return(NULL)
  stopifnot(mod_rec@nfact == 1)

  n_runs <- tibble(group = mod_rec@groups) |> count(group, name = "n_runs")

  n_resp <- bind_cols(group = mod_rec@groups, mod_rec@data) |>
    pivot_longer(cols = -group, names_to = "item", values_to = "correct") |>
    filter(!is.na(correct)) |>
    count(group, name = "n_responses")

  model_vals(mod_rec) |>
    as_tibble() |>
    filter(item == "GROUP") |>
    select(group, name, value, est) |>
    pivot_wider(names_from = name, values_from = c(value, est)) |>
    transmute(group,
              group_mean = value_MEAN_1,
              group_var = value_COV_11,
              group_sd = sqrt(value_COV_11),
              mean_estimated = est_MEAN_1,
              var_estimated = est_COV_11) |>
    left_join(n_runs, by = "group") |>
    left_join(n_resp, by = "group") |>
    arrange(group_mean) |>
    mutate(model_n_runs = length(mod_rec@runs))
}

scoring_group_params <- map(scoring_mods, mod_group_coefs)

scoring_table_distinct$group_params <- scoring_group_params
# unnest drops the NULL entries, i.e. the single-group models
scoring_group_params_tbl <- scoring_table_distinct |>
  left_join(registry_df, by = "file_name") |>
  unnest(group_params) |>
  select(task_id, task_code = item_task, group,
         group_mean, group_var, group_sd, mean_estimated, var_estimated,
         n_runs, n_responses,
         model_set, subset, itemtype, nfact, invariance, model_n_runs,
         file_name, file_id, added_at) |>
  mutate(across(c(group_mean, group_var, group_sd), \(x) round(x, 2)),
         across(c(n_runs, n_responses), as.integer)) |>
  distinct() |>
  arrange(task_code, group)

group_parameters <- scoring_group_params_tbl |>
  left_join(task_info) |>
  select(task_category, task_label, task_id, task_code, everything()) |>
  mutate(model_registry = scoring_dataset$properties$scopedReference,
         model_registry_version = scoring_dataset$properties$version$tag)

scoring_dataset <- scoring_dataset$create_next_version(if_not_exists = TRUE)

group_params_table <- scoring_dataset$table("group_parameters:1yy8")
group_params_table$update(upload_merge_strategy = "replace")
group_params_table$upload("group_parameters")$create(group_parameters, if_not_exists = FALSE, rename_on_conflict = TRUE)

scoring_dataset$release()
