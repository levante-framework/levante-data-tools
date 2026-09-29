library(levantemodels)
library(tidyverse)
library(glue)
library(assertthat)

# get reference to notebook's source dataset
dataset_spec <- notebook_dataset(redivis)

raw_name <- dataset_spec[[1]]$name |> str_extract("^[^:]*")
out_name <- str_extract(raw_name, "(.*)_raw", group = TRUE)
assert_that(!is.na(out_name))
message(glue("Processing input dataset <{raw_name}> to output dataset <{out_name}>"))

# # get values of workflow parameters
# wf <- redivis$current_workflow()
# out_dataset_ref <- wf$parameter("out_dataset_ref:bk2j")$get_values() |> first()

# # check that output dataset name from parameters matches expected naming scheme
# raw_name <- dataset_spec[[1]]$name |> str_extract("^[^:]*")
# out_name <- out_dataset_ref |> str_extract("^[^:]*")
# assert_that(raw_name == paste0(out_name, "_raw"))

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

# determined by binning RT and computing accuracy for each, then eyeballing
# where accuracy from at or below chance to substantially above
min_rts <- tribble(
  ~item_task, ~min_rt,
      "hf", -Inf,
    "math",  500,
  "matrix", 1000,
      "mg",  900,
    "mrot",  500,
      "pa", -Inf,
     "sds",  500,
     "sre",  500,
     "swr",  300,
     "tom",  300,
    "trog", 1000,
   "vocab",  600
)

resolve_rt <- \(rt_str, rt_numeric) {
  # if numeric RT exists, use it
  if (!is.na(rt_numeric)) rt_numeric
  # if both numeric and string RT are NA, use NA
  else if (is.na(rt_str)) NA
  # otherwise parse string RT into list and sum its elements
  else {
    rt_str |>
      map(\(r) jsonlite::fromJSON(r, simplifyVector = TRUE)) |>
      map(unlist) |> map_dbl(sum)
  }
}

# code RTs for trials with multiple sub-trial RTs (as sum)
trial_data_rt <- trial_data_subset |>
  mutate(rt = str_replace_all(rt, "'", '"')) |>
  mutate(rt_numeric =  map2_dbl(rt, rt_numeric, resolve_rt) |> na_if(0))

# filter out trials with too slow/fast RTs
# new RT filtering (fast set by task, slow set to 60s)
trial_data_valid <- trial_data_rt |>
  left_join(min_rts, by = join_by(item_task)) |>
  mutate(slow_rt = rt_numeric > 60000,
         fast_rt = rt_numeric < min_rt) |>
  filter(is.na(rt_numeric) | (!slow_rt & !fast_rt)) |>
  select(-slow_rt, -fast_rt, -min_rt, -valid_trial, -validation_msg_trial) |>
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
safe_score <- safely(score)
# score each task x dataset set of trials
task_scores <- task_trials |>
  mutate(score_output = pmap(list(item_task, dataset, trials, runs),
                             partial(safe_score, scoring_table = scoring_table,
                                     registry_dir = registry_dir)),
         scores = map(score_output, \(s) s$result))

# flatten scores (if none use run_id from runs)
scores <- task_scores |>
  filter(item_task != "ha") |>
  mutate(scores = map2(scores, runs,
                       \(sc, ru) if (is.null(sc)) ru |> select(run_id) |> mutate(score = NA, score_se = NA) else sc)) |>
  select(item_task, dataset, scores) |>
  unnest(scores) |>
  mutate(score = round(score, 2), score_se = round(score_se, 2))

# combine runs with scores and exclusions, subset columns
runs_scored <- run_data |>
  left_join(scores) |>
  left_join(exclusion_summary, by = join_by(run_id)) |>
  select(any_of(c("redivis_source", "team", "dataset", "task_id", task_code = "item_task",
         "user_id", "run_id", "score", "score_se", "age", "num_attempted", "time_started",
         "time_finished", "language", "adaptive", "task_version", "administration_id",
         "exclusion", "score_type", "scoring_model", "registry_version",
         "max_incorrect", "max_time", "sequential_stimulus", "corpus"))) |>
    arrange(task_code, dataset, user_id, run_id)

# surveys
surveys <- process_surveys(dataset_spec)
survey_data <- if (is.null(surveys)) surveys else {
  surveys_linked <- link_surveys(surveys, participants)
  surveys_linked |>
    filter(valid_survey, valid_survey_response) |>
    filter_out(!is_complete) |>
    select(-contains("valid"), -"is_complete")
}

# participants renaming
participants <- participants |>
  rename(caregiver1_id = "parent1_id", caregiver2_id = "parent2_id")

# item parameters
item_params <- fetch_scoring_parameters(version = "current")

items <- item_params |>
  select(-redivis_source) |>
  rename(n_factors = nfact) |>
  arrange(item_uid)

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

# sync output tables
sync_table(out_ds_next, "trials", trial_data_out)
sync_table(out_ds_next, "scores", runs_scored)
sync_table(out_ds_next, "participants", participants)
sync_table(out_ds_next, "surveys", survey_data)
sync_table(out_ds_next, "items", items)

# sync_table(out_ds_next, "trials", trial_data_out)
# sync_table(out_ds_next, "scores", runs_scored)
# sync_table(out_ds_next, "participants", participants)
# sync_table(out_ds_next, "surveys", survey_data)
# sync_table(out_ds_next, "items", items)


