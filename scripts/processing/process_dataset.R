library(tidyverse)
library(levantemodels)
library(redivis)

ds <- c(
  # "partner_mpib_de_main_raw:7n7w",
  # "partner_sparklab_us_downex_raw:4n9e",
  # "pilot_langcog_us_downex_raw:a6kb",
  # "pilot_uniandes_co_bogota_raw:3j4z",
  # "pilot_mpieva_de_main_raw:6c0n",
  # "pilot_uniandes_co_rural_raw:66d2",
  # "pilot_western_ca_main_raw:97mt"
  # "levante_data_pilots_raw:48qk"
  # "levante_data_example_raw:bm7r"
  "rfp1-utdt-intl-ys-raw"
  # "rfp1-mpib-intl-ys-raw"
)
dataset_spec <- list(list(name = ds, version = "current"))

raw_name <- dataset_spec[[1]]$name |> str_extract("^[^:]*")
out_name <- str_extract(raw_name, "(.*)[_-]raw", group = TRUE)
assertthat::assert_that(!is.na(out_name))

### process participants ###
participants <- process_participants(dataset_spec)

### process runs ###
runs <- process_runs(dataset_spec,
                     remove_incomplete_runs = FALSE,
                     remove_invalid_runs = FALSE)

### process trials ###
trial_data <- process_trials(dataset_spec,
                             remove_incomplete_runs = FALSE,
                             remove_invalid_runs = FALSE,
                             remove_invalid_trials = FALSE) |>
  filter(!is.na(item_task), item_task != "ha")

# add team/dataset info from participants to runs
run_data <- runs |>
  left_join(participants |> select(user_id, team, dataset), by = "user_id") |>
  relocate(team, dataset, .after = redivis_source)

# code runs for various exclusion criteria
runs_coded <- run_data |>
  mutate(validation_msg_run = validation_msg_run |> replace_na("")) |>
  mutate(ex_no_data = !(run_id %in% trial_data$run_id),
         ex_straightlining = str_detect(validation_msg_run, "straightlining"),
         ex_incomplete = !completed,
         ex_task_bug = !is.na(task_version) & task_version == "1.0.0-beta.19" &
           task_id %in% c("matrix-reasoning", "mental-rotation", "theory-of-mind")) |>
  mutate(included_1 = !ex_no_data & !ex_task_bug & !ex_incomplete & !ex_straightlining)

# filter out runs excluded so far, code runs that are duplicates within administration
runs_coded_dup <- runs_coded |>
  filter(included_1) |>
  group_by(user_id, task_id, variant_id, administration_id) |>
  arrange(time_started) |>
  mutate(ex_duplicate = row_number() != 1) |>
  ungroup()

# filter out duplicate runs
runs_filtered <- runs_coded_dup |> filter(!ex_duplicate)

# subset trials to filtered runs
trial_data_subset <- trial_data |>
  inner_join(runs_filtered |> select(run_id, task_version, adaptive, language))

# filter out invalid trials (too slow/fast RTs), drop HA items
trial_data_valid <- trial_data_subset |>
  mutate(validation_msg_trial = validation_msg_trial |> replace_na(""),
         slow_rt = rt_numeric > 30000,
         fast_rt = str_detect(validation_msg_trial, "fast")) |>
  filter(task_id == "same-different-selection" | is.na(rt_numeric) | !slow_rt,
         !fast_rt) |>
  filter(item_task != "ha") |>
  select(-slow_rt, -fast_rt, -valid_trial, -validation_msg_trial) |>
  mutate(ex_few_valid_trials = n() < 10, .by = run_id)

# filter out trials from runs that ended with too few valid trials
trial_data_filtered <- trial_data_valid |> filter(!ex_few_valid_trials)

# do various trial-level recoding of items/correctness
trial_data_coded <- trial_data_filtered |>
  left_join(participants |> select(user_id, team, dataset), by = "user_id") |>
  relocate(team, dataset, .after = redivis_source) |>
  recode_trials()

# prep trial data for output
trial_data_out <- trial_data_coded |>
  arrange(item_task, dataset, user_id, run_id, trial_number) |>
  select(-ex_few_valid_trials)

# extract run exclusion criteria values
run_exclusions <- runs_coded |>
  select(redivis_source, dataset, task_id, user_id, run_id, starts_with("ex_")) |>
  left_join(runs_coded_dup |> select(run_id, ex_duplicate)) |>
  mutate(ex_few_valid_trials = !(run_id %in% unique(trial_data_filtered$run_id)))

# subset to one exclusion criterion per run based on hierarchy
ex_order <- c("no_data", "task_bug", "duplicate", "incomplete",
              "few_valid_trials", "straightlining")
exclusion_summary <- run_exclusions |>
  pivot_longer(cols = starts_with("ex_"), names_to = "exclusion") |>
  filter(value) |> select(-value) |>
  mutate(exclusion = exclusion |> str_remove("ex_") |> fct_relevel(ex_order)) |>
  group_by(run_id) |>
  arrange(exclusion) |>
  slice(1) |>
  ungroup() |>
  select(run_id, exclusion)

# get scoring specification table
scoring_table <- fetch_scoring_table(version = "current")
# get model registry directory
registry_dir <- fetch_registry_dir(version = "current")

# dataset_tasks <- run_data |> distinct(task_id, dataset)
# dataset_tasks |> left_join(scoring_table)

# nest runs by task x dataset
task_runs <- run_data |>
  nest(runs = -c(task_id, dataset))

# nest trials by task x dataset, add runs
task_trials <- trial_data_coded |>
  filter(!is.na(item_task), !is.na(dataset)) |>
  mutate(dataset2 = dataset) |>
  nest(trials = -c(task_id, item_task, dataset2)) |>
  rename(dataset = dataset2) |>
  left_join(task_runs)

# prevent errors from stopping iteration
safe_score <- safely(levantemodels::score)
# score each task x dataset set of trials
task_scores <- task_trials |>
  mutate(score_output = pmap(list(item_task, dataset, trials, runs),
                             partial(safe_score, scoring_table = scoring_table,
                                     registry_dir = registry_dir)),
         scores = map(score_output, \(s) s$result),
         error = map(score_output, \(s) s$error$message))

# flatten scores (if none use run_id from runs)
scores <- task_scores |>
  filter(item_task != "ha") |>
  mutate(scores = map2(scores, runs,
                       \(sc, ru) if (is.null(sc)) select(ru, run_id) |> mutate(error = unlist(error)) else sc)) |>
  select(item_task, scores) |>
  unnest(scores)

# combine runs with scores and exclusions, subset columns
runs_scored <- run_data |>
  left_join(scores) |>
  left_join(exclusion_summary, by = join_by(run_id)) |>
  mutate(exclusion = coalesce(exclusion, error)) |>
  mutate(score = round(score, 2), score_se = round(score_se, 2)) |>
  select(redivis_source, team, dataset, task_id, task_code = item_task,
         user_id, run_id, score, score_se, age, num_attempted, time_started,
         time_finished, language, adaptive, task_version, administration_id,
         exclusion, score_type, scoring_model, registry_version,
         max_incorrect, max_time, sequential_stimulus, corpus) |>
  arrange(task_code, dataset, user_id, run_id)

assertthat::assert_that(nrow(run_data) == nrow(runs_scored))
assertthat::assert_that(all(is.na(runs_scored$score) != is.na(runs_scored$exclusion)))
# runs_scored |>
#   filter(is.na(score) == is.na(exclusion)) |>
#   count(task_id)

# surveys
surveys <- process_surveys(dataset_spec)
survey_data <- if (is.null(surveys)) surveys else link_surveys(surveys, participants)

# item parameters
item_params <- fetch_scoring_parameters(version = "current")

# connect to output dataset
out_ds <- redivis$organization("levante")$dataset(out_name)
# if (!out_ds$exists()) out_ds$create()
# increment version to "next" if necessary
out_ds_next <- out_ds$create_next_version(if_not_exists = TRUE)

# given dataset, table name, and data, uploads data to table in dataset
sync_table <- \(out_ds, table_name, df) {
  if (is.null(df)) return()
  out_table <- out_ds$table(table_name)
  if (!out_table$exists()) out_table$create()
  out_table$update(upload_merge_strategy = "replace")
  out_table$upload(table_name)$create(df, if_not_exists = FALSE, replace_on_conflict = TRUE)
}

# sync trial_data and runs_scored
sync_table(out_ds_next, "trials", trial_data_out)
sync_table(out_ds_next, "scores", runs_scored)
sync_table(out_ds_next, "participants", participants)
sync_table(out_ds_next, "surveys", survey_data)

# release
# out_ds_next$release()
