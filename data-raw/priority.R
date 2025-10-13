data_priority <- haven::read_sas("data-raw/priority.sas7bdat")

# Keep only id, group, time and outcome variables
priority <- data_priority |>
  dplyr::select(
    id = record_id,
    trt = group,
    time,
    outcome = vo2_max
  ) |>
  # Remove observations with missing outcome
  dplyr::filter(!is.na(outcome)) |>
  # Make id, group and time factors
  dplyr::mutate(
    id = as.factor(id),
    trt = as.factor(trt),
    time = as.factor(time)
  )

usethis::use_data(priority, overwrite = TRUE)
