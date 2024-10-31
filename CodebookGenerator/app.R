# Load required libraries
library(shiny)
library(rhandsontable)

# Define UI
ui <- fluidPage(
  # Main layout with sidebar and main panel
  sidebarLayout(
    # Sidebar with logo, file input, instructions, and download button
    sidebarPanel(
      # Company logo at the top left
      tags$div(
        style = "display: flex; align-items: center; margin-bottom: 20px;",
        tags$img(src = "alliance_logo.png", height = "60px", style = "margin-right: 10px;")
      ),
      
      # Descriptive text below the logo
      p("Welcome to the Codebook Generator App! This tool helps you create a codebook for your scientific data files with ease. Please contact us at curators@frdr-dfdr.ca for improvements or comments.", style = "font-size: 16px;"),
      
      # Instructions for using the app
      h4("How to use the app:"),
      tags$ul(
        tags$li("Upload your CSV or TSV data file using the 'Upload your data file' button above."),
        tags$li("After uploading, preview your data in the main panel."),
        tags$li("Edit the 'Label', 'Type', and 'Units' columns in the variable attributes table."),
        tags$li("The 'Range_or_Levels' column updates automatically based on your selections."),
        tags$li("When you're ready, click 'Download the Codebook' to save your codebook as a CSV file."),
        style = "font-size: 16px;" ),
      
      fileInput("datafile", "Upload your data file",
                accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv", ".tsv")),
      
      # Download button
      downloadButton("download_codebook", "Download the Codebook")
    ),
    # Main panel with centered title and tables
    mainPanel(
      # Centered title in the main panel
      tags$div(
        h1("Codebook Generator for Scientific Data Files", style = "margin: 0; text-align: center;"),
        style = "margin-bottom: 20px;"
      ),
      h3("Data Preview"),  # Subtitle for data preview
      DT::dataTableOutput("data_preview"),
      br(),
      h3("Variable Attributes"),  # Subtitle for variable attributes table
      rHandsontableOutput("variable_attributes_table")
    )
  )
)

# Define Server
server <- function(input, output, session) {
  # (Server code remains the same as before)
  
  # Reactive value to store data
  data <- reactiveVal()
  
  # Reactive value to store attributes
  attributes <- reactiveVal()
  
  # Load data when file is uploaded
  observeEvent(input$datafile, {
    req(input$datafile)
    df <- read.csv(input$datafile$datapath, stringsAsFactors = FALSE)
    data(df)
    
    # Map variable types to predefined levels
    mapped_types <- sapply(df, function(x) {
      t <- class(x)[1]
      if (t %in% c("integer", "numeric", "double")) {
        "numeric"
      } else if (t %in% c("character")) {
        "character"
      } else if (t %in% c("factor")) {
        "factor"
      } else if (t %in% c("Date")) {
        "Date"
      } else {
        "character"  # Default to 'character' for other types
      }
    })
    
    # Initialize attributes with adjusted column order
    attr <- data.frame(
      Variable = colnames(df),
      Label = rep("", ncol(df)),
      Type = mapped_types,
      Range_or_Levels = sapply(df, function(x) {
        if (is.numeric(x)) {
          paste0("Range: ", min(x, na.rm = TRUE), " - ", max(x, na.rm = TRUE))
        } else if (is.factor(x) || is.character(x)) {
          paste("Levels: ", paste(sort(unique(x)), collapse = ", "))
        } else if (inherits(x, "Date")) {
          paste0("Date Range: ", min(x, na.rm = TRUE), " - ", max(x, na.rm = TRUE))
        } else {
          ""
        }
      }),
      Units = rep("", ncol(df)),  # 'Units' after 'Range_or_Levels'
      stringsAsFactors = FALSE
    )
    
    # Convert Type column to factor with predefined levels
    attr$Type <- factor(attr$Type, levels = c("numeric", "character", "factor", "Date"))
    
    attributes(attr)
  })
  
  # Display data preview
  output$data_preview <- DT::renderDataTable({
    req(data())
    DT::datatable(data(), options = list(scrollX = TRUE, scrollY = "400px"))
  })
  
  # Display variable attributes table with editable columns and adjusted widths
  output$variable_attributes_table <- renderRHandsontable({
    req(attributes())
    attr <- attributes()
    rhandsontable(attr, rowHeaders = NULL) %>%
      hot_col("Label", type = "text", width = 200) %>%
      hot_col("Type", type = "dropdown", source = c("numeric", "character", "factor", "Date")) %>%
      hot_col("Range_or_Levels", readOnly = TRUE, renderer = "
        function(instance, td, row, col, prop, value, cellProperties) {
          Handsontable.renderers.TextRenderer.apply(this, arguments);
          td.style.maxWidth = '150px';  // Set desired max width
          td.style.whiteSpace = 'nowrap';
          td.style.overflow = 'hidden';
          td.style.textOverflow = 'ellipsis';
          td.title = value;
        }
      ") %>%
      hot_col("Units", type = "text", width = 150)
  })
  
  # Update attributes based on user editing the table
  observeEvent(input$variable_attributes_table, {
    attr <- hot_to_r(input$variable_attributes_table)
    df <- data()
    
    # Loop over each variable to update data types and Range_or_Levels
    for (i in seq_len(nrow(attr))) {
      variable_name <- attr$Variable[i]
      updated_type <- as.character(attr$Type[i])
      
      # Check if updated_type is NA
      if (is.na(updated_type)) {
        attr$Range_or_Levels[i] <- ""
        next  # Skip to the next iteration
      }
      
      # Transform the column to the new type using tryCatch
      if (updated_type == "numeric") {
        # Try to convert to numeric
        result <- tryCatch({
          df[[variable_name]] <- as.numeric(df[[variable_name]])
          if (all(is.na(df[[variable_name]]))) {
            attr$Range_or_Levels[i] <- "incompatible data type"
          } else {
            attr$Range_or_Levels[i] <- paste0("Range: ", min(df[[variable_name]], na.rm = TRUE), " - ", max(df[[variable_name]], na.rm = TRUE))
          }
        }, warning = function(w) {
          if (all(is.na(df[[variable_name]]))) {
            attr$Range_or_Levels[i] <- "incompatible data type"
          } else {
            attr$Range_or_Levels[i] <- paste0("Range: ", min(df[[variable_name]], na.rm = TRUE), " - ", max(df[[variable_name]], na.rm = TRUE))
          }
        }, error = function(e) {
          attr$Range_or_Levels[i] <- "incompatible data type"
        })
      } else if (updated_type == "factor") {
        # Try to convert to factor
        result <- tryCatch({
          df[[variable_name]] <- factor(df[[variable_name]], levels = sort(unique(df[[variable_name]])))
          attr$Range_or_Levels[i] <- paste("Levels: ", paste(levels(df[[variable_name]]), collapse = ", "))
        }, error = function(e) {
          attr$Range_or_Levels[i] <- "incompatible data type"
        })
      } else if (updated_type == "character") {
        # Try to convert to character
        result <- tryCatch({
          df[[variable_name]] <- as.character(df[[variable_name]])
          attr$Range_or_Levels[i] <- paste("Values: ", paste(sort(unique(df[[variable_name]]), na.last = TRUE), collapse = ", "))
        }, error = function(e) {
          attr$Range_or_Levels[i] <- "incompatible data type"
        })
      } else if (updated_type == "Date") {
        # Try to convert to Date
        result <- tryCatch({
          # Specify the date format depending on your data
          df[[variable_name]] <- as.Date(df[[variable_name]], format = "%Y-%m-%d")
          attr$Range_or_Levels[i] <- paste0("Date Range: ", min(df[[variable_name]], na.rm = TRUE), " - ", max(df[[variable_name]], na.rm = TRUE))
        }, error = function(e) {
          attr$Range_or_Levels[i] <- "incompatible data type"
        })
      } else {
        attr$Range_or_Levels[i] <- ""
      }
    }
    
    # Update reactive values
    data(df)
    attributes(attr)
    
    # Re-render the table with updated data
    output$variable_attributes_table <- renderRHandsontable({
      rhandsontable(attr, rowHeaders = NULL) %>%
        hot_col("Label", type = "text", width = 400) %>%
        hot_col("Type", type = "dropdown", source = c("numeric", "character", "factor", "Date")) %>%
        hot_col("Range_or_Levels", readOnly = TRUE, renderer = "
          function(instance, td, row, col, prop, value, cellProperties) {
            Handsontable.renderers.TextRenderer.apply(this, arguments);
            td.style.maxWidth = '150px';  // Set desired max width
            td.style.whiteSpace = 'nowrap';
            td.style.overflow = 'hidden';
            td.style.textOverflow = 'ellipsis';
            td.title = value;
          }
        ") %>%
        hot_col("Units", type = "text", width = 150)
    })
  })
  
  # Download Codebook CSV with filename based on uploaded file's name
  output$download_codebook <- downloadHandler(
    filename = function() {
      req(input$datafile)  # Ensure a file has been uploaded
      original_name <- input$datafile$name
      # Remove the file extension from the original filename
      file_base <- tools::file_path_sans_ext(original_name)
      # Sanitize the filename by replacing spaces and special characters
      file_base <- gsub("[^A-Za-z0-9_]", "_", file_base)
      # Construct the new filename
      paste0(file_base, "_codebook.csv")
    },
    content = function(file) {
      write.csv(attributes(), file, row.names = FALSE)
    }
  )
}

# Run the Shiny app
shinyApp(ui, server)
