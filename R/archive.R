### const ----
credentals <- jsonlite::fromJSON(Sys.getenv("credentals"))
auth_google <- Sys.getenv("GKEY")
name_google <- credentals[["GNAME"]]
link <- credentals[["GLINK_DB"]]
sheet_tm <- credentals[["GTT_MON"]]
sheet_archive <- credentals[["GSHEET_ARCH"]]
team_name <- credentals[["TNAME"]]
### utils ----
time_local <- "Europe/Moscow"

### auth ----
file_con <- file(name_google)
writeLines(auth_google, file_con)
close(file_con)
googlesheets4::gs4_auth(path = name_google)

### get archive ----
month_archive <- googlesheets4::read_sheet(link, sheet_tm, col_types = "cccccccccTTdcc") |>
  dplyr::select(.data$`Team member`, .data$Start, .data$End, .data$Hours) |>
  dplyr::mutate(
    month_arr = as.numeric(format(.data$Start, "%m")),
    year_arr = as.numeric(format(.data$Start, "%Y")),
    Period = format(.data$Start, "%m.%Y")
  ) |>
  dplyr::group_by(
    .data$`Team member`,
    .data$month_arr,
    .data$year_arr,
    .data$Period
  ) |>
  dplyr::summarise(
    `Sum hours` = sum(.data$Hours),
    `Start Period` = min(.data$Start),
    `End Period` = max(.data$End)
  ) |>
  dplyr::ungroup() |>
  dplyr::mutate(`Sum hours` = round(`Sum hours`, 2)) |>
  dplyr::arrange(.data$`Team member`, .data$year_arr, .data$month_arr) |>
  dplyr::select(-c(.data$month_arr, .data$year_arr)) |>
  dplyr::mutate(
    dt_load = as.character(lubridate::now(tzone = time_local)),
    Project = team_name
  ) |>
  dplyr::relocate(Project, .before = `Team member`) |>
  dplyr::relocate(`Sum hours`, .after = `End Period`)

cache_archive <- googlesheets4::read_sheet(link, sheet_archive, col_types = "cccTTdc")

archive <- cache_archive |>
  dplyr::rows_upsert(month_archive, by = c("Team member", "Period")) |>
  dplyr::arrange(.data$`Team member`, .data$`Start Period`)

### write to table ----
googlesheets4::write_sheet(
  archive,
  link,
  sheet_archive
)
