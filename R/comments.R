### const ----
credentals <- jsonlite::fromJSON(Sys.getenv("credentals"))
auth_google <- Sys.getenv("GKEY")
name_google <- credentals[["GNAME"]]
link <- credentals[["GLINK_PARSE"]]
targets_parse <- credentals[["GPARSE_TARG"]]
comments_parse <- credentals[["GPARSE_COMMENTS"]]
time_local <- "Europe/Moscow"
delay <- 0.3

### auth ----
file_con <- file(name_google)
writeLines(auth_google, file_con)
close(file_con)
googlesheets4::gs4_auth(path = name_google)

### get links ----
links_parse <- googlesheets4::read_sheet(link, targets_parse) |>
  dplyr::mutate(links = paste0(.data$links, "?per-page=50"))

### get comments ----
get_comments <- \(num) {
  link <- links_parse[num, ] |>
    dplyr::select(.data$links) |>
    dplyr::pull()


  ### get comment items ----
  html <- link |>
    rvest::read_html()

  site <- html |>
    rvest::html_element(".extra-info") |>
    rvest::html_element("a") |>
    rvest::html_attr("href") |>
    stringr::str_remove_all("https://|www\\.")

  comments <- html |>
    rvest::html_elements(".c-comment")

  get_comment_items <- \(comment) {
    author <- comment |>
      rvest::html_element(".c-comment__author-name") |>
      rvest::html_text()

    details <- comment |>
      rvest::html_elements(".c-comment__author-details")

    date_detail <- details[2]

    date <- date_detail |>
      rvest::html_text()

    text <- comment |>
      rvest::html_element(".c-comment__description") |>
      rvest::html_text()

    rating <- comment |>
      rvest::html_element(".rating-stars") |>
      rvest::html_attr("style") |>
      stringr::str_extract("[0-9]+") |>
      as.numeric()

    result <- tibble::tibble(
      Site = site,
      Link = link,
      Author = author,
      Date = date,
      Text = text,
      Rating = rating,
    )

    result
  }

  result_comments <- purrr::map_df(comments, \(x) get_comment_items(x))
  Sys.sleep(delay)

  result_comments
}

### get result comments ----
num_links <- seq_len(nrow(links_parse))
dt_load_now <- as.character(lubridate::now(tzone = time_local))
all_comments <- purrr::map_df(num_links, \(x) get_comments(x)) |>
  dplyr::mutate(Rating = (.data$Rating / 100) * 5) |>
  dplyr::mutate(
    Text = stringr::str_trim(.data$Text, side = "both"),
    Author = stringr::str_trim(.data$Author, side = "both")
  ) |>
  dplyr::mutate(Author = gsub(",$", "", .data$Author)) |>
  dplyr::mutate(dt_load = dt_load_now) |>
  dplyr::mutate(Date = format(lubridate::mdy(.data$Date), "%Y-%m-%d")) |>
  dplyr::mutate(Date = lubridate::as_date(.data$Date))

### get cache comments ----
cache_comments <- googlesheets4::read_sheet(link, comments_parse, col_types = "cccDcdcc") |>
  dplyr::select(-.data$Status)

### update comments ----
update_comments <- cache_comments |>
  dplyr::rows_upsert(all_comments, by = c("Link", "Text")) |>
  dplyr::mutate(Status = ifelse(.data$dt_load == dt_load_now, "", "Deleted")) |>
  dplyr::arrange(.data$Site, dplyr::desc(.data$Date)) |>
  dplyr::relocate(.data$Status, .before = .data$dt_load)

### write to table ----
googlesheets4::write_sheet(
  update_comments,
  link,
  comments_parse
)
