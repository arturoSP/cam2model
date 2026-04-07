
# cam2model: Preprocessing Camera Trap Data for Machine Learning Models

## Overview

`cam2model` is a developing R package designed to facilitate the
preprocessing of camera trap data, preparing datasets for training image
classification models in machine learning. This package automates common
tasks such as metadata extraction, selecting relevant images, sampling,
and file handling, significantly reducing data preparation time for
wildlife analysis projects.

## Key Features

-   Extract EXIF metadata directly from images.

-   Dynamically process and analyze metadata columns, such as
    UserComment.

-   Automatically select daytime images for training.

-   Tools for generating image subsets, copying files to destination
    folders, and creating annotation databases.

-   Generate custom plots to explore and visualize metadata.

## Core Functions 

### 1. `rename_images()`

Renames images based on their EXIF metadata and organizes files into
structured directories. 

#### Parameters

- input_dir: Main directory where the original images are stored.

- output_dir: Directory where the renamed images will be saved.

#### Features

- Extracts the file modification date from EXIF data.

- Generates unique names that include the date, camera name, and subfolder.

- Copies the renamed images without modifying the originals.

### 2.  `process_metadata()`

Extracts EXIF metadata from all images in a directory and organizes the
information into a tibble. 

#### Parameters

- output_dir: Main directory containing folders with renamed images.

- output_file: Path to the output CSV file where metadata will be saved.

#### Features

- Reads EXIF metadata such as FileModifyDate, Camera, and other key attributes.

- Returns a consolidated tibble with metadata from all cameras.

### 3.  `generate_plot()`

Creates custom plots based on metadata. 

#### Parameters

- processed_metadata: Data frame with the metadata from the images.

- x, y: Columns for the x and y axes.

- color: Optional. Column to assign colors.

- facets: Optional. Column for dividing the plot into facets.

- filter_expr: Optional. Expression to filter the data.

- angle: Optional. Rotation angle for x-axis labels.

#### Features

- Supports line and point plots with facets.

- Allows customization of axis format and labels.

- Useful for exploring trends in image data.

### 4.  `process_training_images()`

Filters daytime images, samples a subset, copies the selected files to a
destination folder, and creates an annotation database. 

#### Parameters

- metadata_file: Path to the CSV file containing image metadata.

- dest_dir: Destination directory where selected images will be copied.

- output_sample_file: Path to save the sampled images' metadata as a CSV file.

- output_annotation_file: Path to save the annotation file.

- sample_proportion: Proportion of images to sample within each directory.

- filter_by: Column name of the luminosity variable (e.g. `bLuma`)

- min_lum, max_lum: Luminosity thresholds. 

- min_per_dir, max_per_dir: Range for the acceptable number of images to work with within each directory. 

#### Features

- Filters images by luminosity.

- Randomly selects a percentage of images per directory.

- Copies selected images to a new directory and saves annotation information.

## Example Workflow 

#### Step 1: Rename images

```{r, eval=FALSE}
rename_images(main_dir = "C:/raw_images", output_dir = "C:/renamed_images")
```

#### Step 2: Extract and process metadata

```{r, eval=FALSE}
metadata <- process_metadata(output_dir = "C:/renamed_images", output_file = "metadata.csv")
```

#### Step 3: Generate exploratory plots

```{r, eval=FALSE}
generate_plot(processed_metadata = metadata, x = Image_dttm, y = temp,
color = Camera, facets = Camera, filter_expr = File_hms <=
"08:55:00", angle = 30 )
```

#### Step 4: Select images for training

```{r, eval=FALSE}
process_training_images( metadata_file = "processed_metadata.csv",
dest_dir = "C:/train_images", output_sample_file = "sampled_images.csv",
output_annotation_file = "annotation_data.csv", sample_proportion = 0.5, filter_by = "bLuma")
```

## Requirements

R version 4.4.2 or higher.

Required packages:

- dplyr

- tidyr

- lubridate

- stringr

- ggplot2

- readr

- fs

- purrr

- exifr (for working with EXIF metadata).
