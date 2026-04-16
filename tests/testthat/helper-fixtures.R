make_temp_image_file <- function(dir, name = "img.jpg", contents = "x") {
  path <- file.path(dir, name)
  writeBin(charToRaw(contents), path)
  path
}

make_training_metadata <- function(path, rows) {
  readr::write_csv(rows, path)
  path
}
