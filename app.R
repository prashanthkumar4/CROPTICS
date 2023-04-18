#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(readxl)
library(conflicted)
library(tidyverse)
library(dplyr)


# Define UI
ui <- fluidPage(
  title = "CROPTICS",
  tags$head(
    tags$style(
      HTML("
        .title {
          color: white;
          border-radius: 8px;
          font-weight: 780;
          padding: 30px;
          text-align: center;
          background-image: linear-gradient(45deg,#663300,#b35900,#009933,#aaff80);
          
        }
        .nav-tabs > li > a{
        font-weight: 700;
        }
        .nav-tabs > li.active > a, 
      .nav-tabs > li.active > a:hover, 
      .nav-tabs > li.active > a:focus {
      
        background-color: lightgreen;
      }
      ")
    )
  ),
  div(
    titlePanel("CROPTICS"),
    class = "title"
  ),
  sidebarLayout(
    sidebarPanel(
      selectInput("crop_type", "Select a crop type:", choices = c("Cereals", "Pulses", "Fruits", "Fiber crops", "Beverages")),
      uiOutput("crop_selector"),
      style = "background-color: yellow; color: red"
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Nutrient content", 
                 plotOutput("nutrient_plot")
                 ),
        tabPanel("Environmental factors",
                 plotOutput("temp_plot"),
                 plotOutput("rainfall_plot"),
                 plotOutput("ph_plot"),
                 plotOutput("humidity_plot")
        )
        )
      )
  )
  )




# Define server
server <- function(input, output) {
  
  # Define crop selector based on selected crop type
  output$crop_selector <- renderUI({
    crop_type <- input$crop_type
    crop_choices <- switch(crop_type,
                           "Cereals" = c("rice", "maize"),
                           "Pulses" = c("chickpea", "kidneybeans", "pigeonpeas", "mothbeans", "mungbean", "blackgram", "lentil"),
                           "Fruits" = c("pomegranate", "banana", "mango", "grapes", "watermelon", "muskmelon", "apple", "orange", "papaya", "coconut"),
                           "Fiber crops" = c("cotton", "jute"),
                           "Beverages" = "coffee"
    )
    selectInput("crop", "Select a crop:", choices = crop_choices)
  })
  
  # Define nutrient content plot
  output$nutrient_plot <- renderPlot({
    crop_data <- read_excel("Crop.xlsx")
    selected_crop <- crop_data[crop_data$label == input$crop, ]
    nutrient_data <- data.frame(nutrient = c("N", "P", "K"), amount = c(mean(selected_crop$N), mean(selected_crop$P), mean(selected_crop$K)), color = c("blue", "darkgreen", "red"))
    
    nutrient_plot <- ggplot(nutrient_data, aes(x = nutrient, y = amount, fill = color)) + 
      geom_bar(stat = "identity") +
      ggtitle(paste("Nutrients content of", input$crop) %>% toupper()) +
      xlab("Nutrient") +
      ylab("Mg/Kg") +
      scale_fill_manual(values = nutrient_data$color, name = "", labels = c("Nitrogen", "Phosphorus", "Potassium")) +
      theme(plot.title = element_text(size = 20, face = "bold", hjust = 0.5, color = "orange"),
            axis.title = element_text(size = 16, colour = "orange", face = "bold"),
            axis.text = element_text(size = 16, face = "bold"),
            panel.background = element_blank(),
            axis.line = element_line(colour = "black"),
            panel.grid.major.y = element_line(colour = "grey90"),
            panel.grid.minor = element_blank(),
            panel.border = element_blank(),
            legend.text = element_text(size = 12, , face = "bold"),
            legend.position = "bottom")
    
    nutrient_plot
  })
  
  # Define environmental factor plots
  output$temp_plot <- renderPlot({
    crop_data <- read_excel("Crop.xlsx")
    selected_crop <- crop_data[crop_data$label == input$crop, ]
    temp_data <- data.frame(factor = c("Minimum", "Maximum", "Mean"), value = c(min(selected_crop$temperature), max(selected_crop$temperature), mean(selected_crop$temperature)))
    
    # Customize colors
    temp_colors <- c("#ff9900", "#ff6600", "#ffff00")
    
    temp_plot <- ggplot(temp_data, aes(x = factor, y = value, fill = factor)) + 
      geom_bar(stat = "identity") +
      ggtitle(paste("Temperature for", input$crop) %>% toupper()) +
      xlab("Factor") +
      ylab("Celcius") +
      theme_minimal() +
      theme(plot.title = element_text(size = 20, face = "bold", hjust = 0.5, color = "#007bff"),
            axis.line = element_line(colour = "black"),
            axis.title = element_text(size = 16, colour = "#007bff", face = "bold"),
            axis.text = element_text(size = 14, face = "bold"),
            legend.title = element_text(size = 14, colour = "#007bff", face = "bold"),
            legend.text = element_text(size = 12, colour = "red", face = "bold")) +
      scale_fill_manual(values = temp_colors, name = "Temperature", 
                        labels = c("Maximum", "Mean", "Minumum"))
    temp_plot
  })
  
  
  output$rainfall_plot <- renderPlot({
    crop_data <- read_excel("Crop.xlsx")
    selected_crop <- crop_data[crop_data$label == input$crop, ]
    rainfall_data <- data.frame(factor = c("Minimum", "Maximum", "Mean"), value = c(min(selected_crop$rainfall), max(selected_crop$rainfall), mean(selected_crop$rainfall)))
    
    # Customize colors
    rainfall_colors <- c("#000066", "#0000ff", "#9999ff")
    
    rainfall_plot <- ggplot(rainfall_data, aes(x = factor, y = value, fill = factor)) + 
      geom_bar(stat = "identity") +
      ggtitle(paste("Rainfall for", input$crop) %>% toupper()) +
      xlab("Factor") +
      ylab("mm") +
      theme_minimal() +
      theme(plot.title = element_text(size = 20, face = "bold", hjust = 0.5, color = "red"),
            axis.line = element_line(colour = "black"),
            axis.title = element_text(size = 16, colour = "red", face = "bold"),
            axis.text = element_text(size = 14, face = "bold"),
            legend.title = element_text(size = 14, color = "red", face = "bold"),
            legend.text = element_text(size = 12, color = "#007bff", face = "bold")) +
      scale_fill_manual(values = rainfall_colors, name = "Rainfall", 
                        labels = c("Maximum", "Mean", "Minumum"))
    rainfall_plot
  })
  
  output$humidity_plot <- renderPlot({
    crop_data <- read_excel("Crop.xlsx")
    selected_crop <- crop_data[crop_data$label == input$crop, ]
    humidity_data <- data.frame(factor = c("Minimum", "Maximum", "Mean"), value = c(min(selected_crop$humidity), max(selected_crop$humidity), mean(selected_crop$humidity)))
    
    # Customize colors
    humidity_colors <- c("#145214", "#33cc33", "#adebad")
    humidity_plot <- ggplot(humidity_data, aes(x = factor, y = value, fill=factor)) + 
      geom_bar(stat = "identity") +
      ggtitle(paste("Humidity for", input$crop) %>% toupper()) +
      xlab("Factor") +
      ylab("Percentage (%)") +
      theme_minimal() +
      theme(plot.title = element_text(size = 20, face = "bold", hjust = 0.5, color = "maroon"),
            axis.line = element_line(colour = "black"),
            axis.title = element_text(size = 16, colour = "maroon", face = "bold"),
            axis.text = element_text(size = 14, face = "bold"),
            legend.title = element_text(size = 14, color = "maroon", face = "bold"),
            legend.text = element_text(size = 12, color = "orange", face = "bold")) +
      scale_fill_manual(values = humidity_colors, name = "Humidity", 
                        labels = c("Maximum", "Mean", "Minumum"))
    humidity_plot
  })
  
  output$ph_plot <- renderPlot({
    crop_data <- read_excel("Crop.xlsx")
    selected_crop <- crop_data[crop_data$label == input$crop, ]
    ph_data <- data.frame(factor = c("Minimum", "Maximum", "Mean"), value = c(min(selected_crop$ph), max(selected_crop$ph), mean(selected_crop$ph)))
    
    # Customize colors
    ph_colors <- c("blue", "purple", "pink" )
    ph_plot <- ggplot(ph_data, aes(x = factor, y = value, fill = factor)) + 
      geom_bar(stat = "identity") +
      ggtitle(paste("pH for", input$crop) %>% toupper()) +
      xlab("Factor") +
      ylab("pH") +
      theme_minimal() +
      theme(plot.title = element_text(size = 20, face = "bold", hjust = 0.5, color = "green"),
            axis.line = element_line(colour = "black"),
            axis.title = element_text(size = 16, colour = "green", face = "bold"),
            axis.text = element_text(size = 14, face = "bold"),
            legend.title = element_text(size = 14, color = "green", face = "bold"),
            legend.text = element_text(size = 12, colour = "pink", face = "bold")) +
      scale_fill_manual(values = ph_colors, name = "pH", 
                        labels = c("Maximum", "Mean", "Minumum"))
    ph_plot
  })
                                 
}         
                     
# Run the application 
shinyApp(ui = ui, server = server)

