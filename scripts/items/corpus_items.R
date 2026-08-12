# sync between airtable (item table)
# and redivis (dataset item_metadata, table corpus_items)

library(dplyr)
library(purrr)
library(rairtable)
library(redivis)

# info for corpus_item table
# https://airtable.com/appe2p0S3xk4DL2qc/tblnnFrYQfMOFhIF9/viwkKX2qgFMYg5J7Q
corpus_item_table <- list(table = "item",
                          base = "appe2p0S3xk4DL2qc")

# corpus_item field names to export
export_fields <- c(
  "item_uid",
  "item_task",
  "group",
  "entry",
  "chance",
  "group_label"
)

# fetch records in corpus_item table
corpus_items <- rlang::exec(airtable, !!!corpus_item_table) |>
  read_airtable(fields = export_fields) |>
  as_tibble() |>
  select(!!!export_fields) |>
  arrange(item_uid) |>
  # linked records fields are stored as list even if containing one record
  # so need to be make into character
  mutate(across(where(is.list), as.character))
  # replace "NULL" with NA
  # mutate(across(where(is.character), \(s) na_if(s, "NULL"))) |>
  # mutate(chance = as.numeric(chance))

# connect to item_metadata redivis dataset, create next version if needed
item_metadata <- redivis$organization("levante")$dataset("item_metadata:czjv")
item_metadata <- item_metadata$create_next_version(if_not_exists = TRUE)

# connect to survey_items table, upload new survey_items df
item_table <- item_metadata$table("corpus_items")
item_table$update(upload_merge_strategy = "replace")
item_table$upload("corpus_items")$create(corpus_items, if_not_exists = FALSE, rename_on_conflict = TRUE)

# test that reading back gives right result
# table_items <- item_table$to_tibble()
# table_items |> unnest(trials) |> count(trials) |> count(n) |> filter(n != 1)

# release new item_metadata dataset
item_metadata$release()
