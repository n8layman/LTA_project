#' Export Shinylive App with GraphComment Comments
#'
#' This function exports a Shiny app using shinylive::export() and then
#' injects the GraphComment comment system into the generated HTML file.
#'
#' @param app_dir Path to the Shiny app directory (default: "app")
#' @param output_dir Path to the output directory (default: "docs")
#' @param graphcomment_id GraphComment website ID (default: "Tick-dashboard-1")
#'
#' @examples
#' export_with_giscus()
#' export_with_giscus(graphcomment_id = "Your-Website-Name")
export_with_giscus <- function(
  app_dir = "app",
  output_dir = "docs",
  graphcomment_id = "Tick-dashboard-1"
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
    "jsonlite",        # For JSON parsing
    "munsell"          # For color operations (leaflet dependency)
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
  message("Updating page title and adding GraphComment comments to HTML...")
  html_content <- readLines(html_file, warn = FALSE)

  # Create GraphComment embed code
  graphcomment_code <- c(
    "",
    "    <!-- GraphComment Comments -->",
    "    <div style=\"max-width: 1200px; margin: 2rem auto; padding: 20px;\">",
    "      <h2 style=\"font-family: 'Inter', sans-serif; margin-bottom: 1rem;\">Comments</h2>",
    "      <div id=\"graphcomment\"></div>",
    "      <script type=\"text/javascript\">",
    "        /* - - - CONFIGURATION VARIABLES - - - */",
    "        var __semio__params = {",
    sprintf("          graphcommentId: \"%s\", // make sure the id is yours", graphcomment_id),
    "          behaviour: {",
    "            // HIGHLY RECOMMENDED",
    "            //  uid: \"...\", // uniq identifer for the comments thread on your page (ex: your page id)",
    "          },",
    "          // configure your variables here",
    "        }",
    "",
    "        /* - - - DON'T EDIT BELOW THIS LINE - - - */",
    "        function __semio__onload() {",
    "          __semio__gc_graphlogin(__semio__params)",
    "        }",
    "",
    "        (function() {",
    "          var gc = document.createElement('script'); gc.type = 'text/javascript'; gc.async = true;",
    "          gc.onload = __semio__onload; gc.defer = true;",
    "          gc.src = 'https://integration.graphcomment.com/gc_graphlogin.js?' + Date.now();",
    "          (document.getElementsByTagName('head')[0] || document.getElementsByTagName('body')[0]).appendChild(gc);",
    "        })();",
    "      </script>",
    "    </div>",
    ""
  )

  # Find the closing </body> tag and insert GraphComment before it
  body_close_index <- which(grepl("</body>", html_content))

  if (length(body_close_index) == 0) {
    stop("Could not find </body> tag in HTML file")
  }

  # Insert GraphComment code before </body>
  modified_html <- c(
    html_content[1:(body_close_index - 1)],
    graphcomment_code,
    html_content[body_close_index:length(html_content)]
  )

  # Update the page title
  modified_html <- gsub("<title>Shiny App</title>",
                        "<title>Regional Tick Surveillance Data Explorer</title>",
                        modified_html, fixed = TRUE)

  # Write the modified HTML back
  writeLines(modified_html, html_file)
  message("✓ GraphComment comments added successfully")
  message("\nExport complete! The app is ready at: ", output_dir)

  invisible(html_file)
}
