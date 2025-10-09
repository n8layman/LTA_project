#' Export Shinylive App with Giscus Comments
#'
#' This function exports a Shiny app using shinylive::export() and then
#' injects the giscus comment system into the generated HTML file.
#'
#' @param app_dir Path to the Shiny app directory (default: "app")
#' @param output_dir Path to the output directory (default: "docs")
#' @param repo GitHub repository in format "owner/repo" (default: "n8layman/LTA_project")
#' @param repo_id GitHub repository ID from giscus configuration
#' @param discussion_number GitHub discussion number to link comments to
#' @param theme Giscus theme (default: "preferred_color_scheme")
#'
#' @examples
#' export_with_giscus()
#' export_with_giscus(repo = "myuser/myrepo", discussion_number = 5)
export_with_giscus <- function(
  app_dir = "app",
  output_dir = "docs",
  repo = "n8layman/LTA_project",
  repo_id = "R_kgDOOxmmjA",
  discussion_number = 7,
  theme = "preferred_color_scheme"
) {

  # Check if shinylive is installed
  if (!requireNamespace("shinylive", quietly = TRUE)) {
    stop("Package 'shinylive' is required but not installed. Install with: install.packages('shinylive')")
  }

  # Export the app using shinylive with explicit package list
  message("Exporting Shiny app with shinylive...")

  # Only include packages actually used in the app
  required_packages <- c(
    "shiny",
    "dplyr",           # For pipe operator and data manipulation
    "tidyr",           # For pivot_longer, replace_na
    "leaflet",         # For maps
    "leaflet.providers", # For map tile providers
    "DT",              # For data tables
    "ggplot2",         # For plots
    "glue",            # For string interpolation
    "jsonlite"         # For JSON parsing
  )

  message("Including packages: ", paste(required_packages, collapse = ", "))

  shinylive::export(
    appdir = app_dir,
    destdir = output_dir
  )
  message("✓ Shinylive export complete")

  # Path to the generated HTML file
  html_file <- file.path(output_dir, "index.html")

  if (!file.exists(html_file)) {
    stop("HTML file not found at: ", html_file)
  }

  # Read the HTML file
  message("Updating page title and adding giscus comments to HTML...")
  html_content <- readLines(html_file, warn = FALSE)

  # Create giscus div with proper indentation
  giscus_code <- c(
    "",
    "    <div style=\"max-width: 1200px; margin: 0 auto; padding: 20px;\">",
    "      <script src=\"https://giscus.app/client.js\"",
    sprintf("              data-repo=\"%s\"", repo),
    sprintf("              data-repo-id=\"%s\"", repo_id),
    "              data-mapping=\"number\"",
    sprintf("              data-term=\"%s\"", discussion_number),
    "              data-reactions-enabled=\"0\"",
    "              data-emit-metadata=\"0\"",
    "              data-input-position=\"bottom\"",
    sprintf("              data-theme=\"%s\"", theme),
    "              data-lang=\"en\"",
    "              crossorigin=\"anonymous\"",
    "              async>",
    "      </script>",
    "    </div>",
    ""
  )

  # Find the closing </body> tag and insert giscus before it
  body_close_index <- which(grepl("</body>", html_content))

  if (length(body_close_index) == 0) {
    stop("Could not find </body> tag in HTML file")
  }

  # Insert giscus code before </body>
  modified_html <- c(
    html_content[1:(body_close_index - 1)],
    giscus_code,
    html_content[body_close_index:length(html_content)]
  )

  # Update the page title
  modified_html <- gsub("<title>Shiny App</title>",
                        "<title>Regional Tick Surveillance Data Explorer</title>",
                        modified_html, fixed = TRUE)

  # Write the modified HTML back
  writeLines(modified_html, html_file)
  message("✓ Giscus comments added successfully")
  message("\nExport complete! The app is ready at: ", output_dir)

  invisible(html_file)
}
