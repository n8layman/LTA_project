# Anonymous-Friendly Comments for ShinyLive App

This document explains how to add an **anonymous-friendly comment section** to your ShinyLive app, with two comment threads for "Mianus" and "Mohonk".

---

## 1. Prerequisites

1. **ShinyLive App** hosted on GitHub Pages or locally.
2. **JSON Storage**: Create a file `www/comments.json` in your app folder:

```json
{
  "Mianus": [],
  "Mohonk": []
}
```

3. **R Packages**:
   - `shiny` (for UI/server)
   - `jsonlite` (to read/write JSON)

---

## 2. UI Setup

Below your data table, add comment sections for each site:

```r
library(shiny)
library(jsonlite)

ui <- fluidPage(
  titlePanel("Shiny App with Anonymous Comments"),

  # Example data table
  tableOutput("my_table"),

  # Comments section for Mianus
  h3("Comments - Mianus"),
  textAreaInput("mianus_comment", "Add a comment:", ""),
  actionButton("submit_mianus", "Submit"),
  uiOutput("mianus_comments"),

  # Comments section for Mohonk
  h3("Comments - Mohonk"),
  textAreaInput("mohonk_comment", "Add a comment:", ""),
  actionButton("submit_mohonk", "Submit"),
  uiOutput("mohonk_comments")
)
```

---

## 3. Server Logic

```r
server <- function(input, output, session) {
  comments_file <- "www/comments.json"

  # Function to load comments
  load_comments <- function() {
    fromJSON(comments_file, simplifyVector = FALSE)
  }

  # Function to save comments
  save_comments <- function(data) {
    write(toJSON(data, pretty = TRUE, auto_unbox = TRUE), comments_file)
  }

  # Render table
  output$my_table <- renderTable({
    data.frame(
      Site = c("Mianus", "Mohonk"),
      Value = c(42, 37)
    )
  })

  # Render comments for Mianus
  output$mianus_comments <- renderUI({
    comments <- load_comments()[["Mianus"]]
    if(length(comments) == 0) return("No comments yet.")
    tagList(lapply(comments, function(c) tags$p(c)))
  })

  # Render comments for Mohonk
  output$mohonk_comments <- renderUI({
    comments <- load_comments()[["Mohonk"]]
    if(length(comments) == 0) return("No comments yet.")
    tagList(lapply(comments, function(c) tags$p(c)))
  })

  # Add new comment for Mianus
  observeEvent(input$submit_mianus, {
    req(input$mianus_comment)
    comments <- load_comments()
    comments[["Mianus"]] <- c(comments[["Mianus"]], input$mianus_comment)
    save_comments(comments)
    updateTextAreaInput(session, "mianus_comment", value = "")
  })

  # Add new comment for Mohonk
  observeEvent(input$submit_mohonk, {
    req(input$mohonk_comment)
    comments <- load_comments()
    comments[["Mohonk"]] <- c(comments[["Mohonk"]], input$mohonk_comment)
    save_comments(comments)
    updateTextAreaInput(session, "mohonk_comment", value = "")
  })
}

shinyApp(ui, server)
```

---

## 4. How It Works

- Comments are stored locally in `comments.json`.  
- Users do **not need any account** to post comments.  
- Each site has a separate comment list.  
- UI updates reactively whenever a new comment is submitted.

---

## 5. Deployment Notes

- Ensure `www/comments.json` is **writable**.  
- For **GitHub Pages**, note that ShinyLive runs client-side, so writing to JSON on GitHub Pages **won’t persist across sessions**. For persistence, consider a **server-side API** or **GitHub Gist storage**.

---

## 6. Optional Enhancements

- **Timestamp**: Store submission time with the comment.  
- **Styling**: Wrap comments in a card or bordered container.  
- **Markdown support**: Render simple formatting with `htmltools::HTML()` or the `markdown` package.  
- **Dynamic sites**: Use `renderUI()` to automatically generate comment sections for multiple sites.

---

This setup provides **full anonymous commenting** while remaining simple and compatible with ShinyLive.