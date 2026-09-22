library(dplyr)
library(purrr)
library(tidyr)
library(rairtable)
library(redivis)

# info for codebook table
codebook_table <- list(table = "codebook",
                       base = "appgtsWRtjSQJiqZ6")

# corpus_item field names to export
export_fields <- c(
  "name",
  "label",
  "description"
)

# fetch records in corpus_item table
codebook_df <- rlang::exec(airtable, !!!codebook_table) |>
  read_airtable(fields = export_fields) |>
  as_tibble() |>
  select(!!!export_fields)

write_csv(codebook_df, "codebook.csv", na = "", col_names = FALSE)
