# wrapper for rairtable::read_airtable that takes care of some formatting issues
fetch_airtable <- \(src_base, src_table, export_fields) {
  at_table <- list(base = src_base, table = src_table)
  
  # fetch records in source airtable table
  rlang::exec(rairtable::airtable, !!!at_table) |>
    rairtable::read_airtable(fields = export_fields) |>
    dplyr::as_tibble() |>
    dplyr::select(!!!export_fields) |>
    # linked records fields are stored as list even if containing one record
    # so need to be made into character when appropriate
    dplyr::mutate(dplyr::across(dplyr::where(\(v) is.list(v) & all(purrr::map_int(v, length) %in% c(0, 1))), as.character))
  # replace "NULL" with NA
  # mutate(across(where(is.character), \(s) na_if(s, "NULL")))
}
