list_processed_datasets <- \() {
  # list of levante dataset names
  org <- redivis::redivis$organization("levante")
  ds <- org$list_datasets()
  ds_names <- map_chr(ds, \(d) d$name)
  
  # parse names into components and subset to site-specific datasets
  site_ds <- tibble(name = ds_names) |>
    separate_wider_delim(name, "-", names = c("relationship", "other"),
                         cols_remove = FALSE, too_many = "merge") |>
    filter(!(relationship %in% c("test", "levante"))) |>
    arrange(name)
  
  # filter to only processed datasets
  processed_ds <- site_ds |>
    filter(!str_detect(other, "-raw$"))
  
  # check that inferred processed dataset names have corresponding raw datasets
  assertthat::assert_that(all(paste0(processed_ds$other, "-raw") %in% site_ds$other))
  
  # add special case levante-data-pilots to processed dataset names
  proc_names <- c("levante-data-pilots", processed_ds$name)
}

list_raw_datasets <- \() {
  # list of levante dataset names
  org <- redivis::redivis$organization("levante")
  ds <- org$list_datasets()
  ds_names <- map_chr(ds, \(d) d$name)
  
  keep(ds_names, \(s) str_detect(s, "-raw$"))
  
  # parse names into components and subset to site-specific datasets
  site_ds <- tibble(name = ds_names) |>
    separate_wider_delim(name, "-", names = c("relationship", "other"),
                         cols_remove = FALSE, too_many = "merge") |>
    filter(!(relationship %in% c("test", "levante"))) |>
    arrange(name)
  
  # filter to only raw datasets
  raw_ds <- site_ds |>
    filter(str_detect(other, "-raw$"))
  
  # add special case levante-data-pilots to processed dataset names
  raw_names <- c("levante-data-pilots-raw", raw_ds$name)
}
