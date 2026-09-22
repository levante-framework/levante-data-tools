# sync between airtable (exclusions table)
# and redivis (dataset item_metadata, table exclusions)

library(dplyr)
library(purrr)
library(rairtable)
library(redivis)
library(tidyr)

# info for exclusion table
# https://airtable.com/appe2p0S3xk4DL2qc/tblnnFrYQfMOFhIF9/viwkKX2qgFMYg5J7Q
corpus_item_table <- list(table = "exclusion",
                          base = "appe2p0S3xk4DL2qc")

# corpus_item field names to export
export_fields <- c(
  "item_uid",
  "exclusion_datasets"
)

# fetch records in corpus_item table
exclusions_items <- rlang::exec(airtable, !!!corpus_item_table) |>
  read_airtable(fields = export_fields) |>
  as_tibble() |>
  select(!!!export_fields) |>
  mutate(across(where(\(v) is.list(v) & all(map_int(v, length) == 1)), as.character)) |>
  arrange(item_uid)

exclusions <- exclusions_items |>
  unnest(exclusion_datasets) |>
  rename(dataset = exclusion_datasets)

# connect to item_metadata redivis dataset, create next version if needed
item_metadata <- redivis$organization("levante")$dataset("item_metadata:czjv")
item_metadata <- item_metadata$create_next_version(if_not_exists = TRUE)

# connect to survey_items table, upload new survey_items df
exclusion_table <- item_metadata$table("exclusions:0b5t")
exclusion_table$update(upload_merge_strategy = "replace")
exclusion_table$upload("exclusions")$create(exclusions, if_not_exists = FALSE, rename_on_conflict = TRUE)

# test that reading back gives right result
# table_items <- exclusion_table$to_tibble()

# release new item_metadata dataset
item_metadata$release()
