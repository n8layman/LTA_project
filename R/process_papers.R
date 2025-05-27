# Load required libraries
library(pdftools)
library(httr)
library(stringr)
library(rcrossref)
library(bib2df)
library(tidyverse)

papers_dir <- "resources/papers"
references_file <- "resources/papers/references.bib"

# Regex to find DOI
doi_regex <- "(?i)\\b10\\.\\d{4,9}/[\\w.:;()/-]+\\b"

# Function to extract DOI from text
extract_doi_from_pdf <- function(pdf_file) {
  text <- pdftools::pdf_text(pdf_file)
  first_match <- str_match(text, doi_regex) |> na.omit() |> purrr::pluck(1)
  metadata <- cr_cn(dois = first_match, format = "bibentry") |> 
    as_tibble() |> 
    suppressWarnings()
  return(metadata)
}

# Main script
pdf_files <- list.files(papers_dir, pattern = "\\.pdf$", full.names = TRUE)

references <- bib2df::bib2df(references_file) %>% setNames(tolower(names(.))) |>
  mutate_if(is.list, ~paste(unlist(.), collapse = ", ")) |>
  mutate_all(~as.character(.))

new_references <- purrr::map_dfr(pdf_files, ~extract_doi_from_pdf(.x))

new_references |> left_join(references) |>
  janitor::remove_empty("cols") |> 
  dplyr::distinct()

# Write the updated .bib file with a comment header
comment_header <- sprintf(
  "%% This file was last updated on %s by the `process_papers.yml` GitHub Actions workflow.\n%% Do not edit manually unless necessary.\n\n",
  Sys.time()
)

writeLines(comment_header, references_file)

bib2df::df2bib(new_references, file = references_file, append = TRUE)