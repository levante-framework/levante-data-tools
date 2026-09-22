source(here("scripts/metadata/sync_metadata.R"))

run_md_sync <- \() {
  # fetch data from processed variables table
  md_proc <- fetch_metadata(src_base = "appS887QvoGBVC8H5",
                            src_table = "processed variables")
  
  # get list of processed datasets
  ds_proc <- list_processed_datasets()
  
  # flush log file
  log_file <- "sync_metadata_processed.log"
  file.remove(log_file)
  
  # sync metadata to each processed dataset
  walk(ds_proc, \(ds) sync_dataset(ds, md_proc, log_file))
}

run_md_sync()
