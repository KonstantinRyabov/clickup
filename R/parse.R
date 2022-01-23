### const ----
link <- Sys.getenv("GLINK")
targets_parse <- Sys.getenv("GPARSE_TARG")
result_parse <- Sys.getenv("GPARSE_RES")

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

  rating_reviews <- html |>
    rvest::html_element(".count") |>
    rvest::html_text2() |>
    stringr::str_split(" · ")

  rating <- rating_reviews[[1]][1] |>
    stringr::str_remove("\\.") |>
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
    paste(collapse = "\n")

  result <- tibble::tibble(
    Name = name,
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

all_prop <- purrr::map_df(links, \(x) get_prop(x))

### write to table ----
googlesheets4::write_sheet(
  all_prop,
  link,
  result_parse
)
