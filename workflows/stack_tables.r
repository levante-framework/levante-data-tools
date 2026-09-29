library(redivis)
library(tidyverse)
library(glue)

org <- redivis$organization("levante")
ds <- org$list_datasets()

# list of dataset references
ds_refs <- map(ds, \(d) d$get()$properties$scopedReference) |> unlist()

# parse references into components and subset to site-specific datasets
site_ds <- tibble(reference = ds_refs) |>
  separate_wider_delim(reference, ":", names = c("name", "ref", "version"), cols_remove = FALSE) |>
  separate_wider_delim(name, "_", names = c("relationship", "other"),
                              cols_remove = FALSE, too_many = "merge") |>
  filter(!(relationship %in% c("test", "levante"))) |>
  arrange(reference)

# filter to only processed datasets
processed_ds <- site_ds |>
  filter(!str_detect(other, "_raw")) |>
  arrange(desc(version))

# check that inferred processed dataset names have corresponding raw datasets
assertthat::assert_that(all(paste0(processed_ds$other, "_raw") %in% site_ds$other))

proc_names <- processed_ds$name |> set_names()
proc_names

# get list of workflow datasources to add any missing ones
wf <- org$workflow("update levante-data-latest:w841")
sources <- wf$list_datasources()
source_names <- map_chr(sources, \(sc) {
  ref <- sc$source_dataset()$scoped_reference
  str_extract(ref, "[^:]*(?=:)")
})

# update all datasources, adding datasource by dataset name if needed
update_ds <- \(ds_name) {
  dsrc <- wf$datasource(paste0("levante.", ds_name))
  if (!(dsrc$exists())) dsrc$create()
  dsrc$update(version = "current")
}

# add any missing datasources
walk(proc_names, update_ds)

get_ds_table <- \(ds_name, tbl_name) {
  message(glue("Getting table <{tbl_name}> for dataset <{ds_name}>"))
  sc <- org$dataset(ds_name)$table(tbl_name)
  if (!sc$exists()) return()
  sc$to_tibble()
}

# get items/participants/scores/surveys/trials of all processed datasets
items <- map(proc_names, partial(get_ds_table, tbl_name = "items"))
participants <- map(proc_names, partial(get_ds_table, tbl_name = "participants"))
scores <- map(proc_names, partial(get_ds_table, tbl_name = "scores"))
surveys <- map(proc_names, partial(get_ds_table, tbl_name = "surveys"))
trials <- map(proc_names, partial(get_ds_table, tbl_name = "trials"))

items_df <- items |> list_rbind(names_to = "redivis_datasource")
participants_df <- participants |> list_rbind(names_to = "redivis_datasource")
scores_df <- scores |> list_rbind(names_to = "redivis_datasource")
surveys_df <- surveys |> list_rbind(names_to = "redivis_datasource")
trials_df <- trials |> list_rbind(names_to = "redivis_datasource")

out_ds <- redivis$organization("levante")$dataset("levante-data-latest:e9pf")
if (!out_ds$exists()) out_ds$create()
out_ds_next <- out_ds$create_next_version(if_not_exists = TRUE)

sync_table <- \(out_ds, table_name, df) {
  if (is.null(df)) return()
  out_table <- out_ds$table(table_name)
  if (!out_table$exists()) out_table$create()
  out_table$update(upload_merge_strategy = "replace")
  out_table$upload(table_name)$create(df, if_not_exists = FALSE, replace_on_conflict = TRUE)
}

sync_table(out_ds_next, "items:tpe2", items_df)
sync_table(out_ds_next, "participants:b1zp", participants_df)
sync_table(out_ds_next, "scores:d0hm", scores_df)
sync_table(out_ds_next, "surveys", surveys_df)
sync_table(out_ds_next, "trials:bxf8", trials_df)

out_ds$release()
