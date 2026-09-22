library(tidyverse)
library(glue)
library(levantemodels)

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

mod_coefs <- \(mod_rec, item_sep = "-") {
  n_resp <- colSums(!is.na(mod_rec@data)) |>
    enframe(name = "item", value = "n_responses")
  levantemodels:::model_vals(mod_rec) |>
    as_tibble() |>
    filter(group != "GROUP", item != "GROUP") |>
    select(group, item, name, value) |>
    pivot_wider(names_from = name, values_from = value) |>
    left_join(n_resp) |>
    mutate(item = str_remove(item, glue("{item_sep}[0-9]+$"))) |>
    group_by(group, item, d, a1, g) |>
    summarise(n_responses = sum(n_responses), .groups = "drop") |>
    select(item_uid = item, n_responses, d, a1, g) |>
    distinct() |>
    mutate(difficulty = -d / a1) |>
    arrange(difficulty) |>
    mutate(model_n_runs = length(mod_rec@runs))
}

scoring_params <- map(scoring_mods, mod_coefs)

scoring_table_distinct$params <- scoring_params
scoring_params_tbl <- scoring_table_distinct |>
  left_join(registry_df, by = "file_name") |>
  unnest(params) |>
  select(task_id, task_code = item_task, item_uid, n_responses,
         difficulty, discrimination = a1, guessing = g,
         model_set, subset, itemtype, nfact, invariance, model_n_runs,
         file_name, file_id, added_at) |>
  mutate(across(c(difficulty, discrimination, guessing), \(x) round(x, 2)),
         n_responses = as.integer(n_responses)) |>
  distinct() |>
  arrange(item_uid)

corpus <- fetch_corpus_items() |> distinct()
item_metadata <- corpus |>
  select(item_uid_join = item_uid, item_group = group,
         item_entry = entry, item_group_label = group_label)
  
task_info <- dplyr::tribble(
  ~task_code , ~task_label              , ~task_category,
  "hf"       , "hearts & flowers"       , "executive function",
  "sds"      , "same & different"       , "executive function",
  "mg"       , "memory"                 , "executive function",
  "math"     , "math"                   , "math",
  "matrix"   , "pattern matching"       , "reasoning",
  "mrot"     , "shape rotation"         , "spatial cognition",
  "trog"     , "sentence understanding" , "language",
  "vocab"    , "vocabulary"             , "language",
  "tom"      , "stories"                , "social cognition",
  "pa"       , "language sounds"        , "reading",
  "sre"      , "sentence reading"       , "reading",
  "swr"      , "word reading"           , "reading",
) |>
  mutate(task_label = task_label |> stringr::str_to_title() |> forcats::fct_inorder(),
         task_category = task_category |> stringr::str_to_title() |> forcats::fct_inorder())

item_parameters <- scoring_params_tbl |>
  # rename(n_factors = nfact, model_registry = redivis_source) |>
  mutate(item_uid_join = item_uid |>
           str_remove("_start|_stay|_switch") |>
           str_remove("_[0-9]m[0-9]n") |>
           str_replace("(?<=sds_same_).*", "same") |>
           str_remove("story[0-9]+_")) |>
  left_join(item_metadata) |>
  left_join(task_info) |>
  arrange(item_uid) |>
  select(-item_uid_join) |>
  select(task_category, task_label, task_id, task_code, contains("item_"), everything()) |>
  mutate(model_registry = scoring_dataset$properties$scopedReference,
         model_registry_version = scoring_dataset$properties$version$tag)

scoring_dataset <- scoring_dataset$create_next_version(if_not_exists = TRUE)

params_table <- scoring_dataset$table("item_parameters:4cvk")
params_table$update(upload_merge_strategy = "replace")
params_table$upload("item_parameters")$create(item_parameters, if_not_exists = FALSE, rename_on_conflict = TRUE)

scoring_dataset$release()
