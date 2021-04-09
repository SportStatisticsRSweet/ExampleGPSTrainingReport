
# Load packages
library(shiny)
library(tidyverse)
library(viridis)
library(plotly)
library(bslib)
library(DT)

# Load data
ExampleData <- read.csv("ExampleData.csv")
# Format dates
ExampleData$Date_Formatted <- as.Date(ExampleData$Date, tryFormats = c("%d/%m/%y")) 
# Unite two columns
ExampleData <- ExampleData %>% unite("SessionDate", Date:SessionType, sep = " - ", remove = FALSE)
# Create a pivot_wider table, for DT 
ExampleData_WideFormat <- ExampleData %>% pivot_wider(names_from = "Variable", values_from = "Value")
# Tidy this tibble
ExampleData_WideFormat <- ExampleData_WideFormat %>% select(!(Date:Date_Formatted)) %>% rename(Athlete = PlayerName) %>% rename(Session = SessionDate)



# Define UI for application - can change title below, to add in team name
ui <- navbarPage("Example GPS (or Physical Output) Report",
                 # Add navbar - can change name (to "By Athlete", for example)
                 tabPanel("Athlete Analysis", 
                          # Create a dropdown selection for individual athlete analysis
                          selectInput("AthleteName", label = h3("Select Athlete:"), 
                                                    choices = unique(ExampleData$PlayerName), 
                                                    selected = 1),
                          # Plot output (code below in server)
                          mainPanel(plotOutput("plot1"), width = "100%", height = "100%")),
                 # Repeat for session/ by date analysis
                 tabPanel("Session Analysis", 
                          selectInput("SessionDate1", label = h3("Select Session Date:"), 
                                      choices = unique(ExampleData$SessionDate), 
                                      selected = 1),
                          # Create plot output
                          mainPanel(plotOutput("plot2"), width = "100%", height = "100%")), 
                 # Repeat for session/ by date analysis (interactive)
                 tabPanel("Session Analysis (Interactive)", 
                          selectInput("SessionDate2", label = h3("Select Session Date:"), 
                                      choices = unique(ExampleData$SessionDate), 
                                      selected = 1),
                          # Create plot output
                          mainPanel(plotlyOutput("plot3"), width = "100%", height = "100%")), 
                 # Repeat for session type
                 tabPanel("Session Type Analysis (Interactive)", 
                          selectInput("SessionType", label = h3("Select Session Type:"), 
                                      choices = unique(ExampleData$SessionType), 
                                      selected = 1),
                          # Create plot output
                          mainPanel(plotlyOutput("plot4"), width = "100%", height = "100%")), 
                 # Present all raw data in a table
                 tabPanel("All Data", 
                          DT::dataTableOutput("AllDataTable")),
                 theme = bs_theme(version = 4, bootswatch = "cosmo")
)



# Define server logic required to draw a histogram
server <- function(input, output) {
    output$plot1 <- renderPlot({
            SelectedAthlete <- ExampleData[ExampleData$PlayerName==input$AthleteName,] 
            ggplot(data = SelectedAthlete, aes(x = Date_Formatted, y = Value)) + 
            geom_line(linetype = "dashed", colour = "grey") +
            geom_point(aes(colour = SessionType), size = 4) +
            labs(x = NULL, y = NULL) +
            scale_x_date(breaks = unique(SelectedAthlete$Date_Formatted), date_labels = "%b %d") +
            scale_colour_viridis_d() +
            theme_classic() +
            theme(axis.title.x = element_blank(), 
                  axis.text.x = element_text(size = 9, angle = 45, vjust = 1, hjust = 1, colour = "black"),
                  axis.text.y = element_text(size = 10, colour = "black"), 
                  strip.text.x = element_text(size = 12, face = "bold"),
                  panel.spacing = unit(2, "lines"), 
                  legend.position = "bottom", 
                  legend.title = element_blank()) +
            facet_wrap(~Variable, scales = "free")
    }, height = 900)
    
    output$plot2 <- renderPlot({
        SelectedDate <- ExampleData[ExampleData$SessionDate==input$SessionDate1,] 
        ggplot(data = SelectedDate, aes(x = Variable, y = Value)) + 
            geom_boxplot(alpha = 0.7, colour = "lightgrey", outlier.shape = NA) +
            geom_jitter(aes(colour = PlayerName), size = 4) +
            labs(x = NULL, y = NULL) +
            scale_colour_viridis_d(option = "plasma") +
            theme_classic() +
            theme(axis.title.x = element_blank(), 
                  axis.text.x = element_blank(),
                  axis.ticks.x = element_blank(),
                  axis.text.y = element_text(size = 10, colour = "black"), 
                  strip.text.x = element_text(size = 12, face = "bold"),
                  panel.spacing = unit(2, "lines"), 
                  legend.position = "bottom", 
                  legend.title = element_blank()) +
            facet_wrap(~Variable, scales = "free")
    }, height = 900)
    
    output$plot3 <- renderPlotly({
        SelectedDate2 <- ExampleData[ExampleData$SessionDate==input$SessionDate2,] 
        print(
            ggplotly(
        ggplot(data = SelectedDate2, aes(x = Variable, y = Value)) + 
            geom_boxplot(alpha = 0.7, colour = "lightgrey", outlier.shape = NA) +
            geom_jitter(aes(colour = PlayerName), size = 4) +
            labs(x = NULL, y = NULL) +
            scale_colour_viridis_d(option = "plasma") +
            theme_classic() +
            theme(axis.title.x = element_blank(), 
                  axis.text.x = element_blank(),
                  axis.ticks.x = element_blank(),
                  axis.text.y = element_text(size = 10, colour = "black"), 
                  strip.text.x = element_text(size = 12, face = "bold"),
                  panel.spacing = unit(2, "lines"), 
                  legend.position = "bottom", 
                  legend.title = element_blank()) +
            facet_wrap(~Variable, scales = "free")) %>% layout(height = 1200, width = 2000))
    })
    
    output$plot4 <- renderPlotly({
        SelectedSessionType <- ExampleData[ExampleData$SessionType==input$SessionType,] 
        print(
            ggplotly(
                ggplot(data = SelectedSessionType, aes(x = Variable, y = Value, label = Date)) + 
                    geom_violin(alpha = 0.7) +
                    stat_summary(fun=median, geom="crossbar", size=2, color="darkgrey") +
                    geom_jitter(aes(colour = PlayerName), size = 4) +
                    labs(x = NULL, y = NULL) +
                    scale_colour_viridis_d(option = "plasma") +
                    theme_classic() +
                    theme(axis.title.x = element_blank(), 
                          axis.text.x = element_blank(),
                          axis.ticks.x = element_blank(),
                          axis.text.y = element_text(size = 10, colour = "black"), 
                          strip.text.x = element_text(size = 12, face = "bold"),
                          panel.spacing = unit(2, "lines"), 
                          legend.position = "bottom", 
                          legend.title = element_blank()) +
                    facet_wrap(~Variable, scales = "free")) %>% layout(height = 1200, width = 2000))
    })
    
    output$AllDataTable = DT::renderDataTable({
        ExampleData_WideFormat 
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
