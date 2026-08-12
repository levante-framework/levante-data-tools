library(levantemodels)
library(dplyr)
library(tidyr)
library(tibble)
library(stringr)
library(glue)
library(forcats)
library(rairtable)

scoring_dataset <- redivis$organization("levante")$dataset("levante_metadata_scoring:e97h")
scoring_params_tbl <- scoring_dataset$table("item_parameters:4cvk")$to_tibble()

params <- scoring_params_tbl |>
  select(item_uid, difficulty, discrimination, n_responses, itemtype,
         model_n_runs, file_id, added_at, model_registry_version) |>
  arrange(item_uid)
         # groups = list(mod_rec@group_names),
# write_csv(params, "params.csv")

# airtable table target
param_table <- airtable(table = "parameters", base = "appe2p0S3xk4DL2qc")

# upload params for all corpus tasks to airtable
insert_records(params, param_table, typecast = TRUE)
