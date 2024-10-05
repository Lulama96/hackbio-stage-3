# Install required packages
if (!require(shiny)) install.packages("shiny")
if (!require(leaflet)) install.packages("leaflet")
if (!require(ggplot2)) install.packages("ggplot2")
if (!require(dplyr)) install.packages("dplyr")
if (!require(rmarkdown)) install.packages("rmarkdown")
if (!require(plotly)) install.packages("plotly")
if (!require(DT)) install.packages("DT")
if (!require(readxl)) install.packages("readxl")
if (!require(sf)) install.packages("sf")
if (!require(cachem)) install.packages("cachem")
if (!require(curl)) install.packages("curl")
if (!require(writexl)) install.packages("writexl")
if (!require(wordcloud)) install.packages("wordcloud")
if (!require(RColorBrewer)) install.packages("RColorBrewer")
if (!require(orca)) install.packages("orca")
if (!require(viridisLite)) install.packages("viridisLite")
if (!require(leaflet)) install.packages("leaflet")
if (!require(maps)) install.packages("maps")
if (!require(rmapshaper)) install.packages("rmapshaper")


# Load necessary libraries
library(shiny)
library(ggplot2)
library(dplyr)
library(DT)
library(plotly)
library(readxl)
library(sf)
library(cachem)
library(curl)
library(writexl)
library(wordcloud)
library(RColorBrewer)
library(orca)
library(viridis)
library(leaflet)
library(maps)
library(rmapshaper)

# Set the working directory and loading files
#setwd("C:/Users/mthet/Documents/Stage 3/data")
cholera_reported_cases <- read.csv("CHOLERA_REPORTED_CASES.csv")
cholera_reported_deaths <- read.csv("CHOLERA_REPORTED_DEATHS.csv")
cholera_fatality_rate <- read.csv("CHOLERA_FATALITY_RATE.csv")

# Convert columns to numeric
cholera_reported_cases$Number.of.reported.cases.of.cholera <- as.numeric(as.character( cholera_reported_cases$Number.of.reported.cases.of.cholera))
cholera_reported_deaths$Number.of.reported.deaths.from.cholera <- as.numeric(as.character(cholera_reported_deaths$Number.of.reported.deaths.from.cholera))
cholera_fatality_rate$Cholera.case.fatality.rate <- as.numeric(as.character(cholera_fatality_rate$Cholera.case.fatality.rate))

# Check for and handle NAs 
cholera_reported_cases <- na.omit(cholera_reported_cases)
cholera_reported_death <- na.omit(cholera_reported_deaths)
cholera_fatality <- na.omit(cholera_fatality_rate)


# UI for the app
ui <- fluidPage(
  tags$style(HTML("
        .btn-custom {
            background-color: #007bff; /* Blue color */
            color: white;
            border: none;
            padding: 10px 20px;
            font-size: 16px;
            border-radius: 4px;
            cursor: pointer;
        }
        .btn-custom:hover {
            background-color: #0056b3; /* Darker blue color on hover */
        }body {
                background-color: #F3E5F5;
            }
            .shiny-output-error {
                color: red;
            }
            .title-box {
                background-color: blue;
                padding: 20px;
                text-align: center;
                border-radius: 10px;
                color: white;
                margin-bottom: 20px;
            }
            .title-box h1 {
                margin: 0;
                font-size: 32px;
            }
    ")),
  
  # Title in Header Box
  div(class = "title-box",
      h1("Cholera Outbreak")),
  # Text description to explain the app
  fluidRow(
    column(12,
           h2("Overview of Cholera Outbreaks"),
           p("This application provides an interactive visualization of cholera outbreak data worldwide. You can explore the number of cases, fatalities, and fatality rates for different countries and time periods."),
           p("Cholera is an acute diarrheal infection caused by the ingestion of food or water contaminated with the bacterium Vibrio cholerae. Cholera remains a global threat to public health and is an indicator of inequity and lack of social development."),
           h3("How to Use the App"),
           p("1. Select a country from the dropdown menu to view cholera data for that region."),
           p("2. The map displays outbreak information such as the number of cases, deaths, and fatality rates."),
           p("3. The colors on the map correspond to the severity of outbreaks, with darker colors indicating more cases."),
           p("4. Click on a country to view additional outbreak details.")
    )
  ),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("Countries..territories.and.areas", "Select country", choices = NULL),
      sliderInput("year", "Select Year Range:", min = 1949, max = 2024, value = c(1949, 2024), sep = ""),
      downloadButton("report", "Download Report")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Map", leafletOutput("choleraMap")),
        tabPanel("Total Cases in Selected Country", plotOutput("total_cases_line_plot")),  # Line plot for cases
        tabPanel("Total Deaths in Selected Country", plotOutput("total_deaths_line_plot")),  # Line plot for deaths
        tabPanel("Fatality Rate in Selected Country", plotOutput("fatality_rate_line_plot")),  # Line plot for fatality rate
        tabPanel("Trends", plotlyOutput("trendsPlot")),
        tabPanel("Summary", verbatimTextOutput("summary"))
      )
    )
  )
)

# Server logic
 server <- function(input, output, session) {
 
  # Reactive expression for total cases for the selected country
  total_cases_selected_country <- reactive({
    req(input$Countries..territories.and.areas)  # Ensure a country is selected
    
    cholera_reported_cases %>%
      filter(Countries..territories.and.areas == input$Countries..territories.and.areas) %>%
      group_by(Year) %>%
      summarise(Total_Cases = sum(Number.of.reported.cases.of.cholera, na.rm = TRUE)) %>%
      arrange(Year)
  })
  
  # Reactive expression for total deaths for the selected country
  total_deaths_selected_country <- reactive({
    req(input$Countries..territories.and.areas)  # Ensure a country is selected
    
    cholera_reported_deaths %>%
      filter(Countries..territories.and.areas == input$Countries..territories.and.areas) %>%
      group_by(Year) %>%
      summarise(Total_Deaths = sum(Number.of.reported.deaths.from.cholera, na.rm = TRUE)) %>%
      arrange(Year)
  })
  
  # Reactive expression for fatality rate for the selected country
  fatality_rate_selected_country <- reactive({
    req(input$Countries..territories.and.areas)  # Ensure a country is selected
    
    cholera_fatality_rate %>%
      filter(Countries..territories.and.areas == input$Countries..territories.and.areas) %>%
      group_by(Year) %>%
      summarise(Average_Fatality_Rate = mean(Cholera.case.fatality.rate, na.rm = TRUE)) %>%
      arrange(Year)
  })
  
  # Line plot for total cases in the selected country over time
  output$total_cases_line_plot <- renderPlot({
    ggplot(total_cases_selected_country(), aes(x = Year, y = Total_Cases)) +
      geom_line(color = "blue") +
      geom_point(color = "blue") +
      labs(title = paste("Total Cholera Cases in", input$Countries..territories.and.areas, "Over Time"),
           x = "Year", y = "Total Cases") +
      theme_minimal() +
      theme(
        axis.title.x = element_text(face = "bold", size = 12),
        axis.title.y = element_text(face = "bold", size = 12),
        plot.title = element_text(face = "bold", size = 14, hjust = 0.5)
      )
  })
  
  # Line plot for total deaths in the selected country over time
  output$total_deaths_line_plot <- renderPlot({
    ggplot(total_deaths_selected_country(), aes(x = Year, y = Total_Deaths)) +
      geom_line(color = "red") +
      geom_point(color = "red") +
      labs(title = paste("Total Cholera Deaths in", input$Countries..territories.and.areas, "Over Time"),
           x = "Year", y = "Total Deaths") +
      theme_minimal() +
      theme(
        axis.title.x = element_text(face = "bold", size = 12),
        axis.title.y = element_text(face = "bold", size = 12),
        plot.title = element_text(face = "bold", size = 14, hjust = 0.5)
      )
  })
  
  # Line plot for fatality rate in the selected country over time
  output$fatality_rate_line_plot <- renderPlot({
    ggplot(fatality_rate_selected_country(), aes(x = Year, y = Average_Fatality_Rate * 100)) +
      geom_line(color = "green") +
      geom_point(color = "green") +
      labs(title = paste("Cholera Fatality Rate in", input$Countries..territories.and.areas, "Over Time (%)"),
           x = "Year", y = "Fatality Rate (%)") +
      theme_minimal() +
      theme(
        axis.title.x = element_text(face = "bold", size = 12),
        axis.title.y = element_text(face = "bold", size = 12),
        plot.title = element_text(face = "bold", size = 14, hjust = 0.5)
      )
  })
  

    # Calculate summary statistics
  total_cases <- reactive({
    sum(cholera_reported_cases$Number.of.reported.cases.of.cholera, na.rm = TRUE)
  })
  
  total_deaths <- reactive({
    sum(cholera_reported_deaths$Number.of.reported.deaths.from.cholera, na.rm = TRUE)
  })
  
  fatality_rate_avg <- reactive({
    mean(cholera_fatality_rate$Cholera.case.fatality.rate, na.rm = TRUE)
  })
  
  # Update country selection input
  observe({
    updateSelectInput(session, "Countries..territories.and.areas",
                      choices = unique(cholera_reported_cases$Countries..territories.and.areas))
  })
  
  # Map plot
  output$choleraMap <- renderLeaflet({
    # Load world map
    world <- st_as_sf(maps::map("world", fill = TRUE, plot = FALSE))
    
    # Transform the world map to WGS84
    world <- st_transform(world, crs = "+proj=longlat +datum=WGS84")
    
    # Calculate total cases per country
    total_cases_country <- cholera_reported_cases %>%
      group_by(Countries..territories.and.areas) %>%
      summarise(total_cases = sum(Number.of.reported.cases.of.cholera, na.rm = TRUE)) %>%
      rename(ID = Countries..territories.and.areas)
    
    # Join world map with total cases
    world_cases <- world %>%
      left_join(total_cases_country, by = "ID")
    
    # Create the leaflet map
    leaflet(world_cases) %>%
      addTiles() %>%
      addPolygons(
        fillColor = ~colorNumeric("viridis", total_cases)(total_cases),
        color = "grey", 
        weight = 1,
        opacity = 0.7,  # Make borders slightly transparent
        fillOpacity = 0.7,  # Fill transparency
        popup = ~paste("<strong>", ID, "</strong><br>Total Cases: ", total_cases)
      ) %>%
      addLegend(
        position = "bottomright", 
        pal = colorNumeric("viridis", NULL), 
        values = ~total_cases, 
        title = "Total Cases",
        labFormat = labelFormat(suffix = " cases"),  # Formatting legend labels
        opacity = 1
      )
  })
  
  # Trends plot
  output$trendsPlot <- renderPlotly({
    cases_yearly <- cholera_reported_cases %>%
      group_by(Year) %>%
      summarise(total_cases = sum(Number.of.reported.cases.of.cholera, na.rm = TRUE))
    
    deaths_yearly <- cholera_reported_deaths %>%
      group_by(Year) %>%
      summarise(total_deaths = sum(Number.of.reported.deaths.from.cholera, na.rm = TRUE))
    
    fatality_yearly <- cholera_fatality_rate %>%
      group_by(Year) %>%
      summarise(average_fatality_rate = mean(Cholera.case.fatality.rate, na.rm = TRUE))
    
    combined_yearly_trends <- cases_yearly %>%
      left_join(deaths_yearly, by = "Year") %>%
      left_join(fatality_yearly, by = "Year")
    
    ggplot() +
      geom_line(data = combined_yearly_trends, aes(x = Year, y = total_cases, color = "Total Cases")) +
      geom_line(data = combined_yearly_trends, aes(x = Year, y = total_deaths, color = "Total Deaths")) +
      geom_line(data = combined_yearly_trends, aes(x = Year, y = average_fatality_rate * 1000, color = "Fatality Rate")) +
      labs(title = "Cholera Trends Over Time", x = "Year", y = "Counts / Rate") +
      scale_y_continuous(sec.axis = sec_axis(~./1000, name = "Fatality Rate (%)")) +
      theme_minimal()
  })
  
  # Summary table
  output$summary <- renderPrint({
    summary_data <- cholera_reported_cases %>%
      group_by(Countries..territories.and.areas) %>%
      summarise(
        Total_Cases = sum(Number.of.reported.cases.of.cholera, na.rm = TRUE),
        Total_Deaths = sum(cholera_reported_deaths$Number.of.reported.deaths.from.cholera, na.rm = TRUE),
        Avg_Fatality_Rate = mean(cholera_fatality_rate$Cholera.case.fatality.rate, na.rm = TRUE)
      ) %>%
      arrange(desc(Total_Cases))
    
    print(summary_data)
  })
  
  # Download report functionality
  output$report <- downloadHandler(
    filename = function() {
      paste("Cholera_Report_", Sys.Date(), ".pdf", sep = "")
    },
    content = function(file) {
      rmarkdown::render("cholera_report.Rmd", output_file = file)
      
      
      
      
    }
  )
}

# Run the application 
shinyApp(ui = ui, server = server)
