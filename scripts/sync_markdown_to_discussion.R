library(httr)

# GitHub API settings
token <- Sys.getenv("GITHUB_TOKEN")
repo <- "your_username/your_repo"  # Replace with your repo
base_url <- "https://api.github.com"

# Path to notes
notes_dir <- "resources/notes"

# Function to get or create a discussion
get_or_create_discussion <- function(title, body) {
  # Check if discussion already exists
  response <- GET(
    paste0(base_url, "/repos/", repo, "/discussions"),
    add_headers(Authorization = paste("token", token)),
    accept("application/vnd.github.v3+json")
  )
  
  if (status_code(response) == 200) {
    discussions <- content(response)
    for (discussion in discussions) {
      if (discussion$title == title) {
        # Update existing discussion
        update_response <- PATCH(
          paste0(base_url, "/repos/", repo, "/discussions/", discussion$number),
          add_headers(Authorization = paste("token", token)),
          content_type_json(),
          body = list(body = body)
        )
        if (status_code(update_response) == 200) {
          message("Updated discussion: ", title)
        } else {
          message("Failed to update discussion: ", title)
        }
        return()
      }
    }
  }
  
  # Create new discussion
  create_response <- POST(
    paste0(base_url, "/repos/", repo, "/discussions"),
    add_headers(Authorization = paste("token", token)),
    content_type_json(),
    body = list(title = title, body = body, category = "General")
  )
  if (status_code(create_response) == 201) {
    message("Created discussion: ", title)
  } else {
    message("Failed to create discussion: ", title)
  }
}

# Function to sync Markdown files to Discussions
sync_markdown_to_discussion <- function() {
  md_files <- list.files(notes_dir, pattern = "\\.md$", full.names = TRUE)
  for (file in md_files) {
    title <- tools::file_path_sans_ext(basename(file))  # Use filename as discussion title
    body <- paste(readLines(file), collapse = "\n")
    get_or_create_discussion(title, body)
  }
}

# Run the sync
sync_markdown_to_discussion()
