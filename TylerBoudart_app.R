# Written by Tyler Boudart

library(shiny)
library(shinydashboard)
library(scales)

## app.R ##

ui <- dashboardPage(
  dashboardHeader(title = "Fatalities Exploration"),
  dashboardSidebar(
      selectInput("dataset", "Choose Qualitative Attribute:",
                  choices = c("Terrorist Organization", 
                              "Attack Location - Region",
                              "Attack Location - Country",
                              "Attack Type", 
                              "Target Type", 
                              "Weapon Type", 
                              "Target Nationality")),
      sliderInput("kills",
                  "Minimum Number of Fatalities:",
                  min = 0,
                  max = 20000, 
                  value = 40),
      sliderInput("year",
                  "Year:",
                  min = 1970,
                  max = 2017, 
                  value = 1970,
                  animate = TRUE,
                  sep = ""),
      actionButton("update", "Update View")
    ),
  dashboardBody(
    fluidRow(
      column(width = 6,
             box(plotOutput("bubblePlot", height = 310), width = NULL),
             box(plotOutput("barChart", height = 310), width = NULL)
      ),
      
      column(width = 6, 
             box(tableOutput("view"), width = NULL))
    )
  ))


server <- function(input, output) {

  datasetInput <- eventReactive(input$update, {switch(input$dataset,
                                                      "Terrorist Organization" = GroupTable,
                                                      "Attack Location - Region" = RegionTable,
                                                      "Attack Location - Country" = countryTable,
                                                      "Attack Type" = attacktypeTable,
                                                      "Target Type" = targtypeTable,
                                                      "Weapon Type" = WeaponTable,
                                                      "Target Nationality" = natltyTable)},
                                ignoreNULL = FALSE)
  
  TitleInput <- eventReactive(input$update, {switch(input$dataset,
                                                    "Terrorist Organization" = "Terrorist Organizations",
                                                    "Attack Location - Region" = "Regions",
                                                    "Attack Location - Country" = "Countries",
                                                    "Attack Type" = "Attack Types",
                                                    "Target Type" = "Target Types",
                                                    "Weapon Type" = "Weapon Types",
                                                    "Target Nationality" = "Target Nationalities")},
                              ignoreNULL = FALSE)
  
  output$bubblePlot <- renderPlot({
    dataset <- datasetInput()
    Title <- TitleInput()
    xmin <- min(dataset$attack_count)
    xmax <- max(dataset$attack_count)
    ymin <- min(dataset$kills_total)
    ymax <- max(dataset$kills_total)
    new_data1 <-dataset[!(dataset$year != input$year | dataset$kills_total < input$kills),]
    ggplot(new_data1, aes(x=attack_count, y=kills_total,color = Title, size=kills_mean)) +
      geom_point(alpha = 0.5) +
      scale_size(range = c(3, 12)) +
      scale_color_viridis_d() +
      scale_y_log10(limits = c(ymin + 1, ymax),
                    labels = scales::comma) +
      scale_x_log10(limits = c(xmin, xmax),
                    labels = scales::comma) +
      ylab("Total Number of Fatalities (Log Scale)") + 
      xlab("Total Number of Attacks (Log Scale)") + 
      theme_bw() +
      labs(title = paste(Title, "Number of Attacks vs Number of Fatalities"),
           size = paste(Title, "with\nMinimum Number of Fatalities:\nAverage Fatalities per Attack")) +
      guides(color = FALSE) +
      theme(
        panel.grid.minor = element_blank(),
        plot.title = element_text(face = "bold", size = 14, hjust = .5),
        axis.title.x = element_text(face = "bold", size = 11, vjust = 0),
        axis.title.y = element_text(face = "bold", size = 11),
        axis.text = element_text(face = "bold"),
        legend.title = element_text(face = "bold", size = 11),
        legend.text = element_text(face = "bold", size = 10)
      )
  })
  
  output$barChart <- renderPlot({
    dataset <- datasetInput()
    Title <- TitleInput()
    new_data2 <- dataset[!(dataset$kills_total < input$kills | dataset$year > input$year),]
    new_data2 <- new_data2 %>% group_by(year) %>% tally() %>% 
      arrange(year) %>% 
      mutate(color = if_else(year - 1 == lag(year), 
                             if_else(n > lag(n), "Increase", 
                                     if_else(n == lag(n), "Same", "Decrease")), "Increase"))
    new_data2$color[1] = "Increase"
    
    new_data2v2 <- dataset[!(dataset$kills_total < input$kills),]
    new_data2v2 <- new_data2v2 %>% group_by(year) %>% tally() 
    
    ymax <- max(new_data2v2$n)
    ggplot(new_data2, aes(x=year, y=n)) + geom_col(color = "#08306b", fill = "#6baed6", width = 1) +
      labs(title = paste(Title, "with Minimum Fatalities")) +
      ylab(paste("Count of", Title)) + 
      xlab("Year") +
      theme_bw() + 
      xlim(1969,2020) +
      ylim(0, ymax) +
      theme(
        panel.grid.minor = element_blank(),
        plot.title = element_text(face = "bold", size = 14, hjust = .5),
        axis.title.x = element_text(face = "bold", size = 12),
        axis.title.y = element_text(face = "bold", size = 12, vjust = 3),
        axis.text = element_text(face = "bold")
      )
  })
  
  output$view <- renderTable({
    dataset <- datasetInput()
    Title <- TitleInput()
    new_data3 <- dataset[!(dataset$year != input$year | dataset$kills_total < input$kills),]
    new_data3 <- new_data3 %>% select(1, attack_count, kills_total, kills_mean) %>% 
      arrange(kills_total, decreasing = TRUE) %>%
      mutate(attack_count = comma(attack_count), 
             kills_total = comma(kills_total), 
             kills_mean = comma(kills_mean))
    colnames(new_data3)[1] <- Title
    colnames(new_data3)[2] <- "Attack Count"
    colnames(new_data3)[3] <- "Total Fatalities"
    colnames(new_data3)[4] <- "Average Fatalities"
    head(new_data3, n = nrow(new_data3))
  })
}

shinyApp(ui, server)