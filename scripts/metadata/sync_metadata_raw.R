source(here("scripts/metadata/sync_metadata.R"))

run_md_sync <- \() {
  # fetch data from raw variables table
  md_raw <- fetch_metadata(src_base = "appS887QvoGBVC8H5",
                           src_table = "raw variables")
  
  # get list of raw datasets
  ds_raw <- list_raw_datasets()

  # flush log file
  log_file <- "sync_metadata_raw.log"
  if (file.exists(log_file)) file.remove(log_file)
  
  # sync metadata to each raw dataset
  walk(ds_raw, \(ds) sync_dataset(ds, md_raw, log_file))
}

run_md_sync()
