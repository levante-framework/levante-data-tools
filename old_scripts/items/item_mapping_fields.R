# sync between airtable (pilot-item-mapping-fields table for coding missing item UIDs)
# and redivis (dataset item_metadata, table item_mapping_fields)

library(dplyr)
library(purrr)
library(stringr)
library(rairtable)
library(redivis)

mapping_table <- list(table = "pilot-item-mapping-fields", base = "appIk9XNTZZns1F1F")

export_fields <- c(
  "item_uid",
  "task_id",
  "corpus_trial_type",
  "item",
  "answer",
  "distractors"
)

# fetch records in corpus_item table
mapping_items <- rlang::exec(airtable, !!!mapping_table) |>
  read_airtable(fields = export_fields) |>
  as_tibble() |>
  select(!!!export_fields) |>
  mutate(across(where(is.list), as.character)) |>
  arrange(item_uid)
  # mutate(item = replace_na(item, ""))

# connect to item_metadata redivis dataset, create next version if needed
item_metadata <- redivis$organization("levante")$dataset("item_metadata:czjv")
item_metadata <- item_metadata$create_next_version(if_not_exists = TRUE)

# connect to survey_items table, upload new survey_items df
mapping_table <- item_metadata$table("item_mapping_fields")
mapping_table$update(upload_merge_strategy = "replace")
mapping_table$upload("mapping_items")$create(mapping_items, if_not_exists = FALSE, rename_on_conflict = TRUE)

# test that reading back gives right result
# table_items <- mapping_table$to_tibble()

# release new item_metadata dataset
item_metadata$release()
