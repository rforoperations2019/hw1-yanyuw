library(shiny)
library(ggplot2)
library(DT)
library(dplyr)
library(tools)
energyData <- read.csv("/Users/yvonne/Desktop/94880/energy-usage-doe-buildings.csv", stringsAsFactors = FALSE)
energy_list <- c("Electricity Demand (KW)", "Electricity Usage (KWH)", 
                "Gas (Therms)", "Total Usage (mmBTUs)", "Steam (mlbs)",
                "Gas Cost ($)", "Electricity Cost ($)")
zone_list <- c("Manhattan", "Bronx", "Brooklyn", "Queens", "Staten Island", "Citywide", "Out of City")

# Define UI for application that draws a histogram
ui <- navbarPage("Energy Use",
   
   # Application title

     tabPanel("Month",
              titlePanel("NYC DOE (Department of Education) Buildings Energy Usage (2009-2012)"),
              sidebarLayout(
                sidebarPanel(
                  textInput(inputId = "plot_title", 
                            label = "Plot title:", 
                            placeholder = "Enter text to be used as plot title"),
                  selectInput(inputId = "x",
                              label = "X-axis:",
                              choices = c("Jan.Ave", "Feb.Ave", "Mar.Ave", "Apr.Ave", 
                                          "May.Ave", "Jun.Ave", "Jul.Ave", "Aug.Ave", 
                                          "Sep.Ave", "Oct.Ave", "Nov.Ave", "Dec.Ave"),
                              selected = "Jul.Ave"),
                  selectInput(inputId = "y",
                              label = "Y-axis:",
                              choices = c("Jan.Ave", "Feb.Ave", "Mar.Ave", "Apr.Ave", 
                                          "May.Ave", "Jun.Ave", "Jul.Ave", "Aug.Ave", 
                                          "Sep.Ave", "Oct.Ave", "Nov.Ave", "Dec.Ave"),
                              selected = "Jan.Ave"),
                  selectInput(inputId = "z",
                              label = "Legend:",
                              choices = c("Borough", "Measurement"),
                              selected = "Borough"),
                  hr(),
            
                  checkboxGroupInput(inputId = "selected_energytype",
                                     label = "Select Energy Type(s):",
                                     choices = energy_list,
                                     selected = "Electricity Demand (KW)"),
                  sliderInput(inputId = "size", 
                              label = "Size:", 
                              min = 0, max = 5, 
                              value = 2),
                  hr(),
                  
                  checkboxInput(inputId = "show_data",
                                label = "Show Data Table",
                                value = TRUE)
                  
                ),
                mainPanel(
                  tabsetPanel(
                    tabPanel("Data Table", DT::dataTableOutput(outputId = "building_table"), hr(), uiOutput(outputId = "n"), hr(),
                             downloadButton(outputId = "downloadFile", label = "Download the File"), hr(),
                             actionButton(inputId = "source_link", label = "Click to View Notes")),
                    tabPanel("Visialize Data", plotOutput(outputId = "ScatterPlot"))
                    )
                  )
              )
     ),
     tabPanel("School",
              titlePanel("NYC DOE (Department of Education) Buildings Energy Usage (2009-2012)"),
              sidebarLayout(
                sidebarPanel(
                  checkboxGroupInput(inputId = "select_BoroughName_Two",
                              label = "Select Interest Zone:",
                              choices = c("Manhattan", "Bronx", "Brooklyn", "Queens", "Staten Island", "Citywide", "Out of City"),
                              selected = zone_list),
                  selectInput(inputId = "select_EnergyType_Two",
                              label = "Select Energy Type:",
                              choices = energy_list,
                              selected = "Electricity Demand (KW)"),
                  
                  selectInput(inputId = "y_two",
                              label = "Selected Month:",
                              choices = c("Jan.Ave", "Feb.Ave", "Mar.Ave", "Apr.Ave", 
                                          "May.Ave", "Jun.Ave", "Jul.Ave", "Aug.Ave", 
                                          "Sep.Ave", "Oct.Ave", "Nov.Ave", "Dec.Ave"),
                              selected = "Jan.Ave"),
                  radioButtons(inputId = "x_two",
                               label = "Catagoized by:",
                               choices = c("School Name" = "School.Name", "Borough"),
                               selected = "Borough")
                ),
              mainPanel(
                tabsetPanel(
                  tabPanel("Data Table", DT::dataTableOutput(outputId = "building_table_two")),
                  tabPanel("Visialize Data", plotOutput(outputId = "BarPlot"), hr(), plotOutput(outputId = "PieChart"))
                  )
               )
            )
        )
   )


# Define server logic required to draw a histogram
server <- function(input, output, session) {
  # First Nav
   buildings_subset <- reactive({
     req(input$selected_energytype)
     filter(energyData, Measurement %in% input$selected_energytype)
   })
   
   customed_plot_title <- reactive({ toTitleCase(input$plot_title) })

   output$ScatterPlot <- renderPlot({
     ggplot(data = buildings_subset(), aes_string(x = input$x, y = input$y, color = input$z)) + 
       geom_point(size = input$size) + 
       labs(x = toTitleCase(input$x),
            y = toTitleCase(input$y),
            color = toTitleCase(input$z),
            title = customed_plot_title()
       )
   })

   output$n <- renderUI({
     types <- buildings_subset()$Measurement %>% 
       factor(levels = input$selected_energytype) 
     counts <- table(types)
     HTML(paste("There are", counts, input$selected_energytype, "DOE buildings in this dataset. <br>"))
   })
   
   # Print data table if checked -------------------------------------
   output$building_table <- DT::renderDataTable(
     if(input$show_data){
       DT::datatable(data = buildings_subset()[, -3:-6], 
                     options = list(pageLength = 10), 
                     rownames = FALSE)
     }
   )
   output$downloadFile <- downloadHandler(
     filename = function(){
       paste("NYC_doe_building", input$selected_energytype, ".csv", sep = "_")
       },
     content = function(file){
         write.csv(buildings_subset(), file, row.names = FALSE)
       }
       )
   observeEvent(input$source_link, {
     print(paste("Source: NYC OPEN DATA https://data.cityofnewyork.us/Environment/Energy-Usage-From-DOE-Buildings/mq6n-s45c")
           )})
   
   # Second Nav
   buildings_subset_two <- reactive({
     req(input$select_BoroughName_Two)
     req(input$select_EnergyType_Two)
     filter(energyData, Measurement == input$select_EnergyType_Two & Borough == input$select_BoroughName_Two)
   })
   
   output$building_table_two <- DT::renderDataTable(
     DT::datatable(data = buildings_subset_two()[, 7:19],
                   rownames = FALSE
                   )
   )
   
   output$BarPlot <- renderPlot({
     ggplot(data = buildings_subset_two(), 
            aes_string(x = input$x_two, y = input$y_two)) + 
       geom_bar(stat = "identity") 
   } + coord_flip())
   
   output$PieChart <- renderPlot({
     ggplot(data = buildings_subset_two(),
             aes_string(x = factor(1), y = input$y_two, fill = input$x_two)) + 
       geom_bar(width = 1, stat = "identity") + coord_polar("y", start = 0) + 
       labs(x = NULL, y = NULL, fill = NULL, title = paste(input$y_two, input$select_EnergyType_Two, "Share", sep = " ")) + 
       theme_classic() + theme(axis.line = element_blank(),
                               axis.text = element_blank(),
                               axis.ticks = element_blank(),
                               plot.title = element_text(hjust = 0.5, color = "#666666"))
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

