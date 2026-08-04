library(redivis)
library(purrr)
library(stringr)
library(glue)

registry_dataset <- redivis$organization("levante")$dataset("levante-metadata-scoring:e97h")
registry_dataset <- registry_dataset$create_next_version(if_not_exists = TRUE)
registry_table <- registry_dataset$table("model_registry:rqwv")

regdir <- "../levante-pilots/02_scoring_outputs/model_registry" # in levante-pilots
regfiles <- list(name = list.files(regdir, recursive = TRUE),
                 path = list.files(regdir, recursive = TRUE, full.names = TRUE)) |>
  transpose() |>
  discard(\(x) str_detect(x$name, "multigroup_site"))

upload_registry <- \(regfiles) {
  registry_table$add_files(files = regfiles)
}

upload_registry_task <- \(regfiles, task) {
  task_files <- regfiles |> keep(\(rf) str_detect(rf$name, glue("^{task}/")))
  registry_table$add_files(files = task_files)
}

# delete in GUI first, no file delete API call
upload_registry(regfiles)
# upload_registry_task(regfiles, "swr")
# upload_registry_task(regfiles, "hf")

registry_dataset$release()
