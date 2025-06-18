#################################
#                               #
# 04C_APP_BILL_LOOKUP.R         #
#                               #
#################################
# Build a dataset summarizing roll-call votes for a bill lookup tab.
# This merges bill metadata with roll call statistics and labels
# votes as Unanimous, Bipartisan, Partisan, or Failed.
# 2025-06-04

# Helper: safely get or NA
`%||%` <- function(x, y) if (!is.null(x)) x else y

# Parse one bill JSON file (returns a list of tibbles/data.frames, one for each subobject)
parse_bill_file <- function(path) {
  bill_data <- jsonlite::fromJSON(path, simplifyVector = FALSE)$bill
  # High-level bill info
  bill_main <- tibble(
    bill_id = bill_data$bill_id,
    session_id = bill_data$session_id,
    change_hash = bill_data$change_hash,
    bill_number = bill_data$bill_number,
    title = bill_data$title,
    description = bill_data$description,
    status = bill_data$status,
    status_date = bill_data$status_date,
    url = bill_data$url,
    state_link = bill_data$state_link
  )
  # Each of these is a list-column
  # Convert each array to a tibble/data.frame, add bill_id for joinability
  expand_sublist <- function(sublist, .id = NULL) {
    if (is.null(sublist) || length(sublist) == 0) return(tibble())
    x <- map_dfr(sublist, as_tibble)
    if (!is.null(.id)) x[[.id]] <- bill_data$bill_id
    x
  }
  result <- list(
    bill = bill_main,
    history = expand_sublist(bill_data$history, "bill_id"),
    referrals = expand_sublist(bill_data$referrals, "bill_id"),
    sponsors = expand_sublist(bill_data$sponsors, "bill_id"),
    sasts = expand_sublist(bill_data$sasts, "bill_id"),
    subjects = expand_sublist(bill_data$subjects, "bill_id"),
    texts = expand_sublist(bill_data$texts, "bill_id"),
    votes = expand_sublist(bill_data$votes, "bill_id"),
    amendments = expand_sublist(bill_data$amendments, "bill_id"),
    supplements = expand_sublist(bill_data$supplements, "bill_id"),
    calendar = expand_sublist(bill_data$calendar, "bill_id")
  )
  result
}

# Example: read all bills from files (assuming a list of JSON files)
all_bills <- map(text_paths_bills, parse_bill_file)
# Optionally: bind all rows together for each sub-object type
all_bill_tables <- function(key) bind_rows(map(all_bills, ~ .x[[key]]))

# Example usage:
all_supplements <- all_bill_tables("supplements")
all_texts <- all_bill_tables("texts")
all_votes       <- map_dfr(all_bills, "votes")
all_amendments  <- map_dfr(all_bills, "amendments")
all_calendars  <- map_dfr(all_bills, "calendar")
all_subjects  <- map_dfr(all_bills, "subjects")
all_sponsors  <- map_dfr(all_bills, "sponsors")
all_referrals  <- map_dfr(all_bills, "referrals")
all_histories  <- map_dfr(all_bills, "history")


library(pdftools)
library(httr)

# Function: download and extract text from a PDF supplement
download_and_extract_pdf_text <- function(url, save_dir = "pdf_supplements") {
  if (!dir.exists(save_dir)) dir.create(save_dir)
  file_name <- basename(url)
  file_path <- file.path(save_dir, file_name)
  # Download only if not already downloaded
  if (!file.exists(file_path)) {
    GET(url, write_disk(file_path, overwrite = TRUE))
  }
  # Extract text
  pdf_text <- pdftools::pdf_text(file_path)
  paste(pdf_text, collapse = "\n")
}

# Apply to all supplements with PDF MIME type
all_supplements_pdf <- filter(all_supplements, str_detect(mime, "pdf"), !is.na(url))
all_supplements_pdf$text <- map_chr(all_supplements_pdf$url, ~ download_and_extract_pdf_text(.x))

extract_pdf_text_for_table <- function(table, url_col = "url") {
  filter(table, str_detect(!!sym(url_col), "\\.pdf$")) %>%
    mutate(pdf_text = map_chr(!!sym(url_col), ~ download_and_extract_pdf_text(.x)))
}
all_amendments_pdf <- extract_pdf_text_for_table(all_bill_tables("amendments"))
all_texts_pdf <- extract_pdf_text_for_table(all_bill_tables("texts"))

pdf_links <- unique(na.omit(c(
  all_supplements$state_link, all_supplements$url,
  all_texts$alt_state_link, all_texts$alt_bill_text,
  all_amendments$state_link, all_amendments$url
)))


dir.create("pdf_docs", showWarnings = FALSE)
library(purrr)

walk(pdf_links, function(url) {
  outfile <- file.path("pdf_docs", basename(url))
  if (!file.exists(outfile)) {
    try(download.file(url, outfile, mode = "wb"), silent = TRUE)
  }
})

library(pdftools)

get_pdf_text <- function(path) {
  if (!file.exists(path)) return(NA_character_)
  tryCatch({
    text <- pdf_text(path)
    paste(text, collapse = "\n")
  }, error = function(e) {
    NA_character_
  })
}

pdf_files <- list.files("pdf_docs", pattern = "\\.pdf$", full.names = TRUE)
pdf_texts <- tibble(
  file = pdf_files,
  text = map_chr(pdf_files, get_pdf_text)
)

# Suppose you want to merge back to all_supplements
all_supplements$pdf_file <- file.path("pdf_docs", basename(all_supplements$state_link))
all_supplements$pdf_text <- map_chr(all_supplements$pdf_file, get_pdf_text_robust)

# For alt bill texts (use alt_state_link)
all_texts$pdf_file <- file.path("pdf_docs", basename(all_texts$alt_state_link))
all_texts$pdf_text <- map_chr(all_texts$pdf_file, get_pdf_text_robust)


library(legiscanrr)
library(purrr)

api_texts <- map_df(all_texts$doc_id, function(doc_id) {
  bt <- legiscanrr::get_bill_text(doc_id)
  legiscanrr::decode_bill_text(bt)
})

all_texts_head <- head(all_texts)

api_texts <- map_df(all_texts_head$doc_id, function(doc_id) {
  bt <- legiscanrr::get_bill_text(doc_id)
  legiscanrr::decode_bill_text(bt)
})

