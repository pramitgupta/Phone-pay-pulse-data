# Load necessary libraries
library(shiny)
library(readr)
library(dplyr)
library(ggplot2)
library(stringr)

# Load the combined dataset
combined_data <- read_csv("data/combined_hover_data.csv")

# Capitalize the first letter of each state
combined_data$State <- str_to_title(combined_data$State)

# Calculate the average transaction value (Amount / Count) for each row
combined_data <- combined_data %>%
  mutate(Average_Transaction = Amount / Count)

# UI for the app
ui <- fluidPage(
  # Add a logo at the top right
  tags$div(
    tags$img(src = "www/logo.png", height = "50px", align = "right"), # Path to your logo image or URL
    titlePanel("Phone-Pay State-wise Transaction Data (2024)")
  ),
  
  sidebarLayout(
    sidebarPanel(
      # Dropdown for selecting quarter
      selectInput(
        inputId = "selected_quarter",
        label = "Select Quarter",
        choices = unique(combined_data$Quarter),
        selected = "Q1 2024"
      ),
      
      # Dropdown for selecting state
      uiOutput("state_selector"),
      
      # Submit button to show the selected data
      actionButton("submit_button", "See Data"),
      
      # Compare button to compare the data with the other quarter
      actionButton("compare_button", "Compare with Other Quarter"),
      
      # Button to download the filtered dataset
      downloadButton("download_data", "Download CSV")
    ),
    
    # Main panel to display the table and comparison plots
    mainPanel(
      tableOutput("data_table"),
      plotOutput("comparison_plot"),
      plotOutput("average_plot")  # Add a new plot for the average transaction
    )
  )
)

# Server logic
server <- function(input, output, session) {
  
  # Update state options based on the selected quarter and sort them alphabetically
  output$state_selector <- renderUI({
    selected_states <- combined_data %>%
      filter(Quarter == input$selected_quarter) %>%
      select(State) %>%
      distinct() %>%
      arrange(State)  # Sort the states alphabetically
    
    selectInput(
      inputId = "selected_state",
      label = "Select State",
      choices = selected_states$State
    )
  })
  
  # Reactive function to filter the dataset based on user inputs
  filtered_data <- reactive({
    combined_data %>%
      filter(Quarter == input$selected_quarter, State == input$selected_state)
  })
  
  # Render the table when the submit button is clicked
  observeEvent(input$submit_button, {
    output$data_table <- renderTable({
      filtered_data()
    })
  })
  
  # Render the comparison plot when the compare button is clicked
  observeEvent(input$compare_button, {
    output$comparison_plot <- renderPlot({
      # Get the selected state
      selected_state <- input$selected_state
      
      # Get data for both quarters (Q1 2024 and Q2 2024)
      comparison_data <- combined_data %>%
        filter(State == selected_state, Quarter %in% c("Q1 2024", "Q2 2024"))
      
      # Create the bar plot to compare the two quarters' transaction counts
      ggplot(comparison_data, aes(x = Quarter, y = Count, fill = Quarter)) +
        geom_bar(stat = "identity", position = "dodge", width = 0.6) +
        labs(title = paste("Comparison of Transactions for", selected_state),
             x = "Quarter", y = "Transaction Count") +
        theme_minimal()
    })
    
    # Render the average transaction plot when the compare button is clicked
    output$average_plot <- renderPlot({
      # Get the selected state
      selected_state <- input$selected_state
      
      # Get data for both quarters (Q1 2024 and Q2 2024)
      comparison_data <- combined_data %>%
        filter(State == selected_state, Quarter %in% c("Q1 2024", "Q2 2024"))
      
      # Create the line plot to compare average transaction values across quarters
      ggplot(comparison_data, aes(x = Quarter, y = Average_Transaction, group = 1)) +
        geom_line(aes(color = Quarter), size = 1.2) +
        geom_point(aes(color = Quarter), size = 3) +
        labs(title = paste("Comparison of Average Transaction Value for", selected_state),
             x = "Quarter", y = "Average Transaction Value (Amount / Count)") +
        theme_minimal()
    })
  })
  
  # Download handler to extract the filtered dataset as CSV
  output$download_data <- downloadHandler(
    filename = function() {
      paste("filtered_data_", input$selected_quarter, "_", input$selected_state, ".csv", sep = "")
    },
    content = function(file) {
      write.csv(filtered_data(), file, row.names = FALSE)
    }
  )
}

# Run the application
shinyApp(ui = ui, server = server)
