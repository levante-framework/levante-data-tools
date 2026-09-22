# LEVANTE data tools

This repo contains a bunch of scripts that are used to perform various data flow
tasks.

## Variable metadata

Scripts that read data from an Airtable table and use it to update the variable
metadata for a defined set of Redivis datasets.

```
# update variable metadata for all "processed" datasets
Rscript scripts/metadata/sync_metadata_processed.R
```

```
# update variable metadata for all "raw" datasets
Rscript scripts/metadata/sync_metadata_raw.R
```

## Tables

The `sync_table.R` script reads data from an Airtable table and uploads it to
Redivis table. The specific set of possible sync operations is defined in
`sync_tables.yaml`. Each operation is specified with the following fields:

- `name`: Key identifying operation
- `src_base`: ID of source Airtable base
- `src_table`: ID of source Airtable table
- `export_fields`: List of fields to export
- `target_dataset`: Qualified reference for target Redivis table
- `target_table`: Name or reference id for target Redivis table
- `sort_field`: (optional) Field to sort output by
- `fields_unnest`: (optional) List of fields to unnest (e.g. fields that are
                   multiselects in Airtable and each selection value should get
                   its own row in Redivis)
- `fields_jsonify`: (optional) List of fields to jsonify (e.g. fields that are
                    rollups in Airtable and should by stored as JSON in Redivis)

The script includes a rudimentary command line interface -- the first argument
(required) is the name of one of the defined sync operations, and the second
argument (optional) is a boolean indicating whether to release the updated
Redivis dataset (default to false).

```
# sync table levante_metadata_scoring.scoring_models from Airtable
Rscript scripts/sync_table/sync_table.R scoring_table
```

```
# sync each table in levante_metadata_items from Airtable
Rscript scripts/sync_table/sync_table.R corpus_items
Rscript scripts/sync_table/sync_table.R exclusions
Rscript scripts/sync_table/sync_table.R survey_items_caregiver
Rscript scripts/sync_table/sync_table.R survey_items_teacher
Rscript scripts/sync_table/sync_table.R item_mapping_fields
Rscript scripts/sync_table/sync_table.R item_mapping_id
Rscript scripts/sync_table/sync_table.R item_mapping_trials
```

## Model registry

Scripts to perform various operations related to the model registry.

```
# upload local model registry to levante_metadata_scoring model_registry folder
# note: no redivis API call for deleting files, so need to first manually delete
# all existing files in the model_registry folder
Rscript registry/upload_registry.R
```

```
# internally synchronize levante_metadata_scoring by extracting parameters from
# the model files in the model_registry folder that are referenced in the
# levante_metadata_scoring.scoring_models table,
# plus adding item metadata from table levante_metadata_items.corpus_items,
# and updating table levante_metadata_scoring.item_parameters
Rscript registry/update_registry_params.R
```

```
# sync table levante_metadata_scoring.item_parameters *to* Airtable
Rscript registry/update_corpus_params.R
```
