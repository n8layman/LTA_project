library(httr)

# GitHub API settings
token <- Sys.getenv("GITHUB_TOKEN")
repo <- "your_username/your_repo"  # Replace with your repo
base_url <- "https://api.github.com"

# Path to notes
notes_dir <- "resources/notes"

# Function to update Markdown file from discussion
update_markdown_from_discussion <- function(discussion_id) {
  # Fetch discussion details
  response <- GET(
    paste0(base_url, "/repos/", repo, "/discussions/", discussion_id),
    add_headers(Authorization = paste("token", token)),
    accept("application/vnd.github.v3+json")
  )
  
  if (status_code(response) == 200) {
    discussion <- content(response)
    title <- discussion$title
    body <- discussion$body
    
    # Write to Markdown file
    file_path <- file.path(notes_dir, paste0(title, ".md"))
    writeLines(body, file_path)
    message("Updated Markdown file: ", file_path)
  } else {
    message("Failed to fetch discussion: ", status_code(response))
  }
}

# Run the sync
discussion_id <- Sys.getenv("DISCUSSION_ID")
update_markdown_from_discussion(discussion_id)