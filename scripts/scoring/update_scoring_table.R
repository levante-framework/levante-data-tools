library(dplyr)
library(purrr)
library(tidyr)
library(stringr)
library(rairtable)
library(redivis)

# info for scoring table
scoring_table <- list(table = "Scoring",
                      base = "appIDUfcKdekzTiIJ")

# corpus_item field names to export
export_fields <- c(
  "item_task",
  "task_id",
  "datasets",
  "model_set",
  "subset",
  "itemtype",
  "nfact",
  "invariance",
  "registry_version"
)

# fetch records in corpus_item table
scoring_df <- rlang::exec(airtable, !!!scoring_table) |>
  read_airtable(fields = export_fields) |>
  as_tibble() |>
  select(!!!export_fields) |>
  mutate(across(where(\(v) is.list(v) & all(map_int(v, length) == 1)), as.character))

scoring <- scoring_df |> unnest(datasets) |> rename(dataset = datasets) |>
  mutate(dataset = str_replace_all(dataset, "-", "_")) |>
  arrange(item_task, dataset) |>
  select(-registry_version) # not properly used for now

# connect to item_metadata redivis dataset, create next version if needed
scoring_dataset <- redivis$organization("levante")$dataset("levante_metadata_scoring:e97h")
scoring_dataset <- scoring_dataset$create_next_version(if_not_exists = TRUE)

# connect to survey_items table, upload new survey_items df
scoring_models_table <- scoring_dataset$table("scoring_models:t416")
scoring_models_table$update(upload_merge_strategy = "replace")
scoring_models_table$upload("scoring")$create(scoring, if_not_exists = FALSE, rename_on_conflict = TRUE)

# test that reading back gives right result
# scoring_models_table$to_tibble()

# release new item_metadata dataset
# registry_dataset$release()
