# cam2model

<p align="center">
  <img src="man/figures/cam2model-logo.jpg" alt="cam2model for SAMP logo: a camera and fish" width="320">
</p>

`cam2model` provides a workflow for preparing camera trap images for annotation and model training.

## Recommended workflow

1. `rename_images()`
2. `process_metadata()`
3. `generate_plot()` *(optional)*
4. `process_training_images()`
5. `copy_in_batches()`

## Quick reference by stage

| Stage | Minimum input | Main output | File operations |
|---|---|---|---|
| `rename_images()` | Directory containing images | Result tibble (or a list with `plan` and `result`) | Creates a site folder in `output_dir` and copies images |
| `process_metadata()` | Directory of renamed images | Metadata tibble | Writes the `output_file` CSV |
| `generate_plot()` | Data frame with the columns used for aesthetics | `ggplot` object | No files written |
| `process_training_images()` | CSV with `Directory` and `FileName` | Sample tibble | Creates `dest_dir`, writes two CSV files, and optionally copies images |
| `copy_in_batches()` | Data frame with `id` and `path` | Per-file report data frame | Creates batch subfolders and copies files (or simulates the operation with `dry_run`) |

## End-to-end example (temporary paths)

```r
library(cam2model)

# 1) Set up temporary paths
raw_dir <- tempfile("cam2model_raw_")
renamed_dir <- tempfile("cam2model_renamed_")
train_dir <- tempfile("cam2model_train_")
batch_dir <- tempfile("cam2model_batches_")

dir.create(raw_dir, recursive = TRUE)
dir.create(renamed_dir, recursive = TRUE)
dir.create(train_dir, recursive = TRUE)

# Copy real camera trap images into raw_dir before running the next stages
# file.copy("path_to_real_image.jpg", file.path(raw_dir, "cam1", "image.jpg"), recursive = TRUE)

# 2) Rename and flatten (requires real images)
# renamed <- rename_images(input_dir = raw_dir, output_dir = renamed_dir, site_name = "demo")

# 3) Extract metadata (requires real EXIF data)
# metadata <- process_metadata(output_dir = renamed_dir, output_file = "metadata.csv", UserComment = FALSE)
# metadata_csv <- file.path(renamed_dir, "metadata.csv")

# 4) Self-contained training example without real EXIF data
src_dir <- tempfile("cam2model_src_")
dir.create(src_dir, recursive = TRUE)
file_a <- file.path(src_dir, "a.jpg")
file_b <- file.path(src_dir, "b.jpg")
writeBin(charToRaw("a"), file_a)
writeBin(charToRaw("b"), file_b)

metadata_tbl <- data.frame(
  Directory = c(src_dir, src_dir),
  FileName = c("a.jpg", "b.jpg"),
  File_hour = c(8, 11),
  stringsAsFactors = FALSE
)
metadata_csv <- tempfile("cam2model_metadata_", fileext = ".csv")
readr::write_csv(metadata_tbl, metadata_csv)

sampled <- process_training_images(
  metadata_file = metadata_csv,
  dest_dir = train_dir,
  output_sample_file = "sampled_images.csv",
  output_annotation_file = "annotation_data.csv",
  sample_proportion = 1,
  copy_images = FALSE,
  show_progress = FALSE
)

# 5) Reorganize into batches (dry run)
batch_report <- copy_in_batches(
  df = data.frame(
    id = sprintf("img_%03d", seq_len(nrow(sampled))),
    path = sampled$file_path,
    stringsAsFactors = FALSE
  ),
  output_dir = batch_dir,
  batch_size = 250L,
  dry_run = TRUE,
  verbose = FALSE
)

head(batch_report)
```

## Workflow vignette

See `vignettes/cam2model-workflow.Rmd` for a step-by-step guide with a table of inputs, outputs, and generated files.
