### const ----
credentals <- jsonlite::fromJSON(Sys.getenv("credentals"))
auth_clickup <- credentals[["CLICKUP"]]
auth_google <- Sys.getenv("GKEY")
name_google <- credentals[["GNAME"]]
list_id <- credentals[["WEBLIST"]]
link <- credentals[["GLINK_ACCOUNT"]]
sheet_websites <- credentals[["GWEBSITES"]]
delay <- 0.6
pages <- 0:100
time_local <- "Europe/Moscow"

### get list and number of tasks ----
# bug with length of list, block doesn't work
list <- httr::GET(
  glue::glue("https://api.clickup.com/api/v2/list/{list_id}"),
  httr::add_headers(Authorization = auth_clickup)
)

list_json <- httr::content(list, "parsed")

### get tasks ----
get_tasks <- function(.page, .space_id, .delay) {
  tryCatch(
    {
      tasks <- httr::GET(
        glue::glue("https://api.clickup.com/api/v2/list/{list_id}/task?archived=false&page={.page}&subtasks=true&include_closed=true"),
        httr::add_headers(Authorization = auth_clickup)
      )

      tasks_json <- httr::content(tasks, "parsed")
      tasks_ds <- dplyr::tibble(result = tasks_json[["tasks"]]) |>
        tidyr::unnest_wider(col = .data[["result"]])

      ### seconds
      Sys.sleep(.delay)
      tasks_ds
    },
    error = function(e) {
      data.frame()
    }
  )
}

tasks_all <- purrr::map_df(pages, function(x) get_tasks(x, space_id, delay)) |>
  tidyr::unnest_wider(.data$status, names_sep = "_")

### get custom_fields ----
custom_fields <- tasks_all |>
  dplyr::select(.data$id, .data$custom_fields) |>
  tidyr::unnest_longer(.data$custom_fields) |>
  tidyr::unnest_wider(.data$custom_fields, names_sep = "_") |>
  tidyr::hoist(.data$custom_fields_type_config, options = "options")

# get name
name_custom <- custom_fields |>
  dplyr::select(
    .data$id,
    .data$custom_fields_name,
    .data$custom_fields_value
  ) |>
  dplyr::rowwise() |>
  dplyr::mutate(custom_fields_value = paste(.data$custom_fields_value, collapse = ",")) |>
  dplyr::ungroup() |>
  tidyr::separate_rows(.data$custom_fields_value, sep = ",") |>
  dplyr::rename(options_id = .data$custom_fields_value)

# get options
options_custom <- custom_fields |>
  dplyr::select(
    .data$id,
    .data$custom_fields_name,
    .data$options
  ) |>
  tidyr::unnest_longer(.data$options) |>
  tidyr::unnest_wider(.data$options, names_sep = "_") |>
  dplyr::mutate(
    options_label = ifelse(is.na(.data$options_label), "", as.character(.data$options_label)),
    options_name = ifelse(is.na(.data$options_name), "", as.character(.data$options_name))
  ) |>
  dplyr::mutate(value_fields = paste(.data$options_label, .data$options_name, sep = " ")) |>
  dplyr::mutate(value_fields = stringr::str_trim(.data$value_fields)) |>
  dplyr::select(
    .data$id,
    .data$custom_fields_name,
    .data$options_id,
    .data$value_fields,
    .data$options_orderindex
  ) |>
  dplyr::mutate(options_orderindex = as.character(.data$options_orderindex))

### join name and options ----
name_val_custom <- name_custom |>
  dplyr::left_join(options_custom, by = c(
    "id",
    "custom_fields_name",
    "options_id"
  )) |>
  dplyr::mutate(value_text = ifelse(is.na(.data$value_fields),
    .data$options_id,
    .data$value_fields
  )) |>
  dplyr::select(-c(.data$options_orderindex, .data$value_fields)) |>
  dplyr::rename(options_orderindex = .data$options_id) |>
  dplyr::left_join(options_custom, by = c(
    "id",
    "custom_fields_name",
    "options_orderindex"
  )) |>
  dplyr::mutate(value_text = ifelse(!is.na(.data$value_fields),
    .data$value_fields,
    .data$value_text
  )) |>
  dplyr::select(-.data$options_orderindex, .data$value_fields) |>
  dplyr::group_by(
    .data$id,
    .data$custom_fields_name
  ) |>
  dplyr::mutate(value = paste(.data$value_text, collapse = ", ")) |>
  dplyr::distinct(
    .data$id,
    .data$custom_fields_name,
    .data$value
  ) |>
  tidyr::pivot_wider(
    id_cols = .data$id,
    names_from = .data$custom_fields_name,
    values_from = .data$value
  ) |>
  dplyr::select(sort(tidyselect::peek_vars()))

### join desc and custom fields ----
tasks_desc <- tasks_all |>
  dplyr::select(
    .data$id,
    .data$name,
    .data$url,
    .data$parent,
    .data$status_status
  )

### parent_name ----
parent_name <- tasks_desc |>
  dplyr::select(
    .data$id,
    .data$name
  ) |>
  dplyr::rename(
    "parent" = .data$id,
    "parent_name" = .data$name
  )

### arrange and rename result ----
to_sheet <- tasks_desc |>
  dplyr::left_join(name_val_custom, by = "id") |>
  dplyr::relocate(.data$parent, .after = .data$id) |>
  dplyr::mutate(parent_num = ifelse(is.na(.data$parent), 1, 2)) |>
  dplyr::mutate(parent_id = ifelse(is.na(.data$parent), .data$id, .data$parent)) |>
  dplyr::arrange(.data$parent_id, .data$parent_num) |>
  dplyr::select(-c(.data$parent_id, .data$parent_num)) |>
  dplyr::left_join(parent_name, by = "parent") |>
  dplyr::mutate(parent = ifelse(!is.na(.data$parent), .data$parent_name, .data$parent)) |>
  dplyr::select(-.data$parent_name) |>
  dplyr::relocate(.data$parent, .after = .data$name) |>
  dplyr::rename(
    ID = .data$id,
    Parent = .data$parent,
    Name = .data$name,
    Url = .data$url,
    `Task status` = .data$status_status
  ) |>
  dplyr::mutate(dt_load = as.character(lubridate::now(tzone = time_local)))

### auth ----
file_con <- file(name_google)
writeLines(auth_google, file_con)
close(file_con)
googlesheets4::gs4_auth(path = name_google)

## write to table ----
googlesheets4::write_sheet(
  to_sheet,
  link,
  sheet_websites
)
