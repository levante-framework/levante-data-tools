library(redivis)
library(purrr)

# connect to workflow that builds levante_data_latest
wf <- redivis$user("levante")$workflow("update_levante_data_latest:w841")

# # update all workflow datasources to current version
# wf_ds <- wf$list_datasources()
# walk(wf_ds, \(ds) ds$get())
# walk(wf_ds, \(ds) ds$update(version = "current"))

# run notebook that stacks per-dataset tables into combined tables
wf$notebook("stack_tables:1150")$run()
