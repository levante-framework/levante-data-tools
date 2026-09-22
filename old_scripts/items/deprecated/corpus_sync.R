# sync between airtable (corpus_item table) and GCP storage

library(rairtable)
library(googleCloudStorageR)

library(dplyr)
library(purrr)
library(tibble)
library(tidyr)
library(glue)
library(readr)
library(stringr)

# info for corpus_item table
# https://airtable.com/appe2p0S3xk4DL2qc/tblnnFrYQfMOFhIF9/viwkKX2qgFMYg5J7Q
corpus_item_table <- list(table = "corpus_item",
                          base = "appe2p0S3xk4DL2qc",
                          view = "for_corpus_export")

# corpus_item field names to export
export_fields <- c(
  "corpus_id",
  "corpus_file_name",
  "task_id",
  "task_dir",
  "answer",
  "assessment_stage",
  "audio_file",
  "block_index",
  "chance_level",
  "d",
  "difficulty",
  "image",
  "item",
  "item_id",
  "item_uid",
  "orig_item_num",
  "prompt",
  "randomize",
  "required_selections",
  "response_alternatives",
  "source",
  "task",
  "time_limit",
  "trial_num",
  "trial_type"
)

# fetch records in corpus_item table
corpus_items <- rlang::exec(airtable, !!!corpus_item_table) |>
  read_airtable(fields = export_fields) |>
  as_tibble() |>
  select(-airtable_record_id) |>
  # linked records fields are stored as list even if containing one record
  # so need to be make into character
  mutate(across(where(is.list), as.character)) |>
  # replace "NULL" with NA
  mutate(across(where(is.character), \(s) na_if(s, "NULL")))

# list of available corpus IDs
list_corpora <- \() unique(corpus_items$corpus_id)

# gcs_proj <- "hs-levante-admin-dev"
# given df to upload (each row is a corpus, columns corpus_file_name, task_dir, corpus)
# save corpus as csv in bucket
upload_corpora <- \(upload_items, bucket = "levante-corpora-airtable") {
  pwalk(upload_items, \(task_dir, corpus_file_name, corpus) {
    corpus_file <- glue("{task_dir}/shared/corpora/{corpus_file_name}.csv")
    gcs_upload(file = corpus,
               name = corpus_file,
               object_function = \(input, output) write_csv(input, output, na = ""),
               bucket = bucket,
               predefinedAcl = "bucketLevel")
  })
}

# given vector of task IDs, update all corpora for those tasks
update_task_corpora <- \(task_ids) {
  stopifnot(all(task_ids %in% corpus_items$task_id))
  
  task_corpus_ids <- corpus_items |> filter(task_id %in% task_ids) |>
    distinct(corpus_id) |> pull(corpus_id)
  update_corpora(task_corpus_ids)
}

# given vector of corpus IDs, update all those corpora
update_corpora <- \(corpus_ids) {
  stopifnot(all(corpus_ids %in% corpus_items$corpus_id))
  
  update_items <- corpus_items |> filter(corpus_id %in% corpus_ids) |>
    nest(corpus = -c(corpus_file_name, task_dir))
  upload_corpora(update_items)
}

# tasks that are exported
export_tasks <- c(
  "adult-reasoning",
  "egma-math",
  "matrix-reasoning",
  "mental-rotation",
  "same-different-selection",
  "theory-of-mind",
  "trog",
  "vocab"
)

# update all exported tasks
update_task_corpora(export_tasks)
