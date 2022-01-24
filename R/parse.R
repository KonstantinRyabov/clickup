### const ----
link <- Sys.getenv("GTEST")
targets_parse <- Sys.getenv("GPARSE_TARG")
result_parse <- Sys.getenv("GPARSE_RES")
time_local <- "Europe/Moscow"
delay <- 0.3

### get links ----
links_parse <- googlesheets4::read_sheet(link, targets_parse)

### get props of links ----
get_prop <- \(item) {
  link_parse <- links_parse[item, ] |>
    dplyr::select(.data$links) |>
    dplyr::pull()

  html <- link_parse |>
    rvest::read_html()

  name <- html |>
    rvest::html_element(".col-sm-12") |>
    rvest::html_text2() |>
    stringr::str_extract(".+(?=(Review|review))") |>
    stringr::str_trim()

  site <- html |>
    rvest::html_element(".extra-info") |>
    rvest::html_element("a") |>
    rvest::html_attr("href")


  rating_reviews <- html |>
    rvest::html_element(".count") |>
    rvest::html_text2() |>
    stringr::str_split(" · ")

  rating <- rating_reviews[[1]][1] |>
    stringr::str_trim() |>
    as.numeric()

  reviews <- rating_reviews[[1]][2] |>
    stringr::str_extract("[0-9]+") |>
    stringr::str_trim() |>
    as.numeric()

  scam <- html |>
    rvest::html_element(".scam-text") |>
    rvest::html_text2()

  yellow_labels <- html |>
    rvest::html_elements(".alert-warning") |>
    rvest::html_text2() |>
    tibble::enframe() |>
    dplyr::filter(grepl("(Reviews|reviews)", .data$value)) |>
    dplyr::mutate(text = glue::glue("{name} - {value}")) |>
    dplyr::select(.data$text) |>
    dplyr::pull() |>
    paste(collapse = ";\n")

  result <- tibble::tibble(
    Name = name,
    Site = site,
    Link = link_parse,
    Rating = rating,
    Reviews = reviews,
    Scam = scam,
    `Yellow labels` = yellow_labels
  )

  result
}

### get results ----
links <- seq_len(nrow(links_parse))

all_prop <- purrr::map_df(links, \(x) get_prop(x)) |>
            dplyr::mutate(dt_load = lubridate::now(tzone = time_local)) |>
            dplyr::mutate(`Yellow labels` = ifelse(.data$`Yellow labels` == "", NA,
                                                   .data$`Yellow labels`)) |>
            dplyr::mutate(dt_load = as.character(.data$dt_load))

### get cache prop ----
cache_prop <- googlesheets4::read_sheet(link, result_parse, col_types = "cccddccdc") |>
              dplyr::select(-.data$`Diff rating`)

update_prop <- cache_prop |>
  dplyr::rows_upsert(all_prop, by = c("Link", "Rating", "Reviews","Yellow labels", "Scam")) |>
  dplyr::mutate(dt_load = lubridate::as_datetime(.data$dt_load)) |>
  dplyr::arrange(.data$Name, .data$dt_load) |>
  dplyr::group_by(.data$Name) |>
  dplyr::mutate(Prev_rating = dplyr::lag(.data$Rating)) |>
  dplyr::mutate(`Diff rating` =.data$Rating - .data$Prev_rating) |>
  dplyr::ungroup() |>
  dplyr::relocate(`Diff rating`, .before = .data$dt_load) |>
  dplyr::mutate(dt_load = as.character(.data$dt_load)) |>
  dplyr::select(-.data$Prev_rating)

### write to table ----
googlesheets4::write_sheet(
  update_prop,
  link,
  result_parse
)
