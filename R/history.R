# const ----
credentals <- jsonlite::fromJSON(Sys.getenv("credentals"))
auth_clickup <- credentals[["CLICKUP"]]
auth_google <- Sys.getenv("GKEY")
name_google <- credentals[["GNAME"]]
team_id <- credentals[["TID"]]
link <- credentals[["GLINK_DB"]]
sheet_tm <- credentals[["GTT"]]
sheet_hs <- credentals[["GHISTORY"]]
manual_stdate <- lubridate::as_date(Sys.getenv("STDATE"))

### utils ----
miliseconds <- 1000
time_local <- "Europe/Moscow"
delay <- 0.6
st_date <- lubridate::now(tzone = time_local) - lubridate::days(45)
start_posix <- lubridate::as_datetime("1970-01-01 00:00:00")
err_desc <- "error with task"

### get startdate
start_date <- if (is.na(manual_stdate)) {
  st_date
} else {
  manual_stdate
}

### auth ----
file_con <- file(name_google)
writeLines(auth_google, file_con)
close(file_con)
googlesheets4::gs4_auth(path = name_google)

### get timers ----
timers <- googlesheets4::read_sheet(link, sheet_tm, col_types = "cccccccccTTdcc")
timers <- timers |>
  dplyr::mutate(start = lubridate::as_datetime(.data$Start)) |>
  dplyr::filter(start >= start_date) |>
  dplyr::select(-.data$start)

### get team members ----
teams <- httr::GET(
  "https://api.clickup.com/api/v2/team",
  httr::add_headers(Authorization = auth_clickup)
)
teams_json <- httr::content(teams, "parsed")

teams_ds <- tibble::tibble(result = teams_json[["teams"]]) |>
  tidyr::unnest_wider(col = .data[["result"]]) |>
  tidyr::unnest_longer(.data$members) |>
  tidyr::unnest_wider(.data$members, names_sep = "_") |>
  tidyr::unnest_wider(.data$members_user, names_sep = "_") |>
  dplyr::select(.data$members_user_id, .data$members_user_username) |>
  dplyr::mutate(`Who change` = stringr::str_remove(.data$members_user_username, "(?<=[A-Z])[a-z]+")) |>
  dplyr::rename(user_id = .data$members_user_id) |>
  dplyr::mutate(user_id = as.character(.data$user_id)) |>
  dplyr::select(.data$user_id, .data$`Who change`)


### get history ----
timer_ids <- unique(timers$ID)

get_history <- function(.id) {
  history <- httr::GET(
    glue::glue("https://api.clickup.com/api/v2/team/{team_id}/time_entries/{.id}/history"),
    httr::add_headers(Authorization = auth_clickup)
  )
  history_json <- httr::content(history, "parsed")


  history_ds <- tryCatch(tibble::tibble(result = history_json[["data"]]) |>
    tidyr::unnest_wider(col = .data[["result"]]) |>
    dplyr::mutate(id_time = .id),
  error = function(e) {
    data.frame(
      id_time = .id,
      err_history = err_desc
    )
  }
  )

  ### seconds
  Sys.sleep(delay)
  history_ds
}

to_chrdate <- function(posix_vec) {
  nums <- as.numeric(posix_vec) / miliseconds
  dt <- lubridate::as_datetime(nums)
  dt_tz <- lubridate::with_tz(dt, tz = time_local)
  as.character(dt_tz)
}

hist_ds <- purrr::map_df(timer_ids, function(x) get_history(x))

hist <- hist_ds |>
  dplyr::mutate(dplyr::across(c(.data$before, .data$after), ~ ifelse(is.na(.), "", as.character(.)))) |>
  dplyr::mutate(
    before = ifelse(grepl("^[0-9]+$", .data$before), to_chrdate(.data$before), as.character(.data$before)),
    after = ifelse(grepl("^[0-9]+$", .data$after), to_chrdate(.data$after), as.character(.data$after)),
    date = to_chrdate(.data$date)
  ) |>
  dplyr::mutate(before = ifelse(.data$field == "duration",
    prettyunits::pretty_dt(difftime(lubridate::as_datetime(.data$before),
      start_posix,
      units = "secs"
    )),
    as.character(.data$before)
  )) |>
  dplyr::mutate(after = ifelse(.data$field == "duration",
    prettyunits::pretty_dt(difftime(lubridate::as_datetime(.data$after),
      start_posix,
      units = "secs"
    )),
    as.character(.data$after)
  )) |>
  dplyr::select(-c(.data$task_id, .data$custom_task_id)) |>
  dplyr::rename(
    ID = .data$id_time,
    Field = .data$field,
    Before = .data$before,
    After = .data$after,
    Date = .data$date
  )

### join table ----
result <- timers |>
  dplyr::left_join(hist, by = c("ID")) |>
  dplyr::left_join(teams_ds, by = c("user_id")) |>
  dplyr::select(-c(.data$user_id, .data$err, .data$dt_load)) |>
  dplyr::mutate(
    dt_load = as.character(lubridate::now(tzone = time_local)),
    Date = lubridate::as_datetime(.data$Date),
    Start = lubridate::as_datetime(.data$Start),
    End = lubridate::as_datetime(.data$End)
  ) |>
  dplyr::relocate(.data$id, .after = .data$dt_load)

### get cache history ----
cache_history <- googlesheets4::read_sheet(link, sheet_hs, col_types = "cccccccccTTdcccTcccc")

update_history <- cache_history |>
  dplyr::rows_upsert(result, by = c("ID", "id")) |>
  dplyr::arrange(.data$`Team member`, .data$Start)

### write to table ----
googlesheets4::write_sheet(
  update_history,
  link,
  sheet_hs
)
