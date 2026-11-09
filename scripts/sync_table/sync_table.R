# script to sync an airtable table with a redivis table
# set of allowed sync operations is defined in sync_tables.yaml
# call on the command line with:
# Rscript scripts/sync_table/sync_table.R <operation name> <release flag>
# where <operation name> is one of the `name` values in sync_tables.yaml
# and <release flag> is a boolean indicating where to release the redivis dataset
# after updating

library(glue)
library(here)
library(purrr)
suppressPackageStartupMessages(library(dplyr))
source(here("scripts/utils/airtable.R"))

# read source airtable table and replace content of target redivis table
sync_airtable_to_redivis <- \(name, src_base, src_table, src_view = NULL, export_fields,
                              target_dataset, target_table, fields_sort = NULL,
                              fields_unnest = NULL, fields_jsonify = NULL,
                              field_tom_story = NULL, release = FALSE) {

  # fetch records in source airtable table
  message(glue("Fetching records from table <{src_table}> in Airtable base <{src_base}>"))
  records <- fetch_airtable(src_base, src_table, src_view, export_fields)

  # unnest by each field specified, keeping records where the field is empty
  # (as NA, also when fetch_airtable() turned an empty field into "NULL")
  unnest_field <- \(df, field) {
    df |>
      tidyr::unnest(all_of(field), keep_empty = TRUE) |>
      mutate("{field}" := na_if(.data[[field]], "NULL"))
  }
  if (!is.null(fields_unnest)) records <- reduce(fields_unnest, unnest_field, .init = records)

  # insert story number into theory of mind item IDs, which are story-less
  # (tom_{group}_{entry} -> tom_story{story}_{group}_{entry})
  if (!is.null(field_tom_story)) {
    story <- records[[field_tom_story]]
    if (any(!is.na(story) & !stringr::str_detect(records$item_uid, "^tom_"))) {
      stop(glue("Field <{field_tom_story}> is set for item IDs that are not theory of mind"))
    }
    records <- records |>
      mutate(item_uid = if_else(is.na(story), .data$item_uid,
                                stringr::str_replace(.data$item_uid, "^tom_", paste0("tom_story", story, "_")))) |>
      select(-all_of(field_tom_story))
  }

  # jsonify each field specified
  jsonify_field <- \(df, field) {
    df |>
      # turn comma-separated strings into character vectors
      mutate("{field}" := map(.data[[field]], \(tt) stringr::str_split(tt, ",") |> unlist())) |>
      # encode non-missing values to JSON
      mutate("{field}" := if_else(is.na(.data[[field]]), "", map_chr(.data[[field]], jsonlite::toJSON)))
  }
  if (!is.null(fields_jsonify)) records <- reduce(fields_jsonify, jsonify_field, .init = records)

  # sort by field specified
  if (!is.null(fields_sort)) records <- records |> arrange(pick(all_of(fields_sort)))

  # connect to target redivis dataset, create next version if needed
  message(glue("Uploading data to Redivis table <{target_table}> in dataset <{target_dataset}>"))
  ds <- redivis::redivis$dataset(target_dataset)
  ds <- suppressWarnings(ds$create_next_version(if_not_exists = TRUE))
  
  # connect to target redivis table, upload records
  tbl <- ds$table(target_table)
  tbl$update(upload_merge_strategy = "replace")
  tbl$upload(name)$create(records, if_not_exists = FALSE, replace_on_conflict = TRUE)
  message("Upload to Redivis complete")

  if (release) {
    ds$release()
    message("Dataset released")
  }
}

# call sync_airtable_to_redivis() using arguments specified in sync_tables.yaml
# given operation name
run_table_sync <- \() {
  cargs <- commandArgs(TRUE)
  if (length(cargs) == 0) stop("Must supply operation name")
  op_name <- cargs[[1]]
  
  if (length(cargs) == 1) {
    message("Release flag not supplied, defaulting to not releasing")
    release <- FALSE
  } else {
    release <- cargs[[2]]
  }

  message(glue("Running Airtable to Redivis sync operation <{op_name}>"))
  operations <- yaml::read_yaml(here("scripts/sync_table/sync_tables.yaml"))
  operation <- keep(operations, \(opt) opt$name == op_name)
  
  if (length(operation) == 0) stop(glue("No operation with name <{op_name}> is specified"))
  if (length(operation) > 1) stop(glue("Multiple operations with name <{op_name}> are specified"))
  
  opts <- list_flatten(operation)
  opts$release <- as.logical(release)
  rlang::exec(sync_airtable_to_redivis, !!!opts)
}

run_table_sync()
