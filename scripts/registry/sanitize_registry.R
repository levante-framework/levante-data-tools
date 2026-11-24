library(purrr)
library(readr)
library(levantemodels)

# sanitized copies of the model registry for public release: model records
# without their calibration data (see ?levantemodels::sanitize_record), which
# upload_registry.R can then upload
regdir <- "../levante-pilots/02_scoring_outputs/model_registry" # in levante-pilots
outdir <- "../levante-pilots/02_scoring_outputs/model_registry_sanitized"

regfiles <- list.files(regdir, pattern = "\\.rds$", recursive = TRUE)

walk(regfiles, \(regfile) {
  mod_rec <- read_rds(file.path(regdir, regfile))
  outfile <- file.path(outdir, regfile)
  dir.create(dirname(outfile), recursive = TRUE, showWarnings = FALSE)
  write_rds(sanitize_record(mod_rec), outfile, compress = "gz")
})
