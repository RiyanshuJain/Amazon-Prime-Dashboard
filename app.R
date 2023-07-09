library(shiny)
library(shinydashboard)
library(ggplot2)
library(plotly)
library(shinythemes)

# library("dplyr")
library("knitr")
library("fresh")
library(rworldmap)
library(readxl)
library(shinycssloaders)

# load the data
data <- read.csv("Amazon_Prime_edited.csv")
data2 <- read.csv("amazon_prime_titles_2.csv")
yd <- read.csv("year_rating.csv")
yd2 <- read.csv('year_rating_2.csv')

my_theme = create_theme(
  adminlte_color(
    light_blue = "#00A8E1"
  )
)

# define the UI
ui <- dashboardPage(
  
  dashboardHeader(title = "Amazon Prime Data Analysis"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Summary", tabName = "summary"),
      menuItem("Visuals", tabName = "visuals"),
      menuItem("Data", tabName = "data"),
      menuItem("Geographical Distribution", tabName = "world"),
      valueBox(round(mean(data$IMDb.Rating, na.rm = TRUE), 1), "Average IMDb", color = 'light-blue', icon = icon("imdb"),width = 12)
    )
  ),
  dashboardBody(
    use_theme(my_theme),
    tabItems(
      tabItem(tabName = "summary", 
              fluidRow(
                valueBox('Amazon Prime', "Full Insights Dashboard",color = 'light-blue', icon = icon("amazon"),width = 4),
              
                # box(title = "OTT Dashboard: ",status = 'primary',height = 160, width = 4, solidHeader = TRUE, 
                #     style = "background-color: #4898a8;",
                #     tags$style(type="text/css", "#dashboard{font-size: 25px; font-weight:bold; text-align: center;}"),
                #     verbatimTextOutput("dashboard")),
                valueBox(length(data2$type), "Total Shows",color = 'light-blue', icon = icon("video"),width = 2),
                
                # box(title = "Total shows", width = 2, solidHeader = TRUE, 
                #     tags$style(type="text/css", "#total_shows{font-size: 50px; font-weight:bold; text-align: center;}"),
                #     verbatimTextOutput("total_shows")),

                valueBox(sum(data2$type == "Movie"),paste('Movies (',paste(round(100*sum(data2$type == "Movie")/length(data2$type), 2), "%)")),color = 'light-blue', icon = icon("film"),width = 3),
                
                # box(title = "Movies", width = 2, solidHeader = TRUE,
                #     tags$style(type="text/css", "#movies{font-size: 25px; font-weight:bold; text-align: center;}"),
                #     verbatimTextOutput("movies")),
                
                valueBox(sum(data2$type == "TV Show"),paste('TV Shows (',paste(round(100*sum(data2$type == "TV Show")/length(data2$type), 2), "%)")),color = 'light-blue', icon = icon("tv"),width = 3),
                # valueBox(round(mean(data$IMDb.Rating, na.rm = TRUE), 1), "IMDb", color = 'light-blue', icon = icon("imdb"),width = 2),
                # 
                # box(title = "TV Shows", width = 2, solidHeader = TRUE, 
                #     tags$style(type="text/css", "#tv_shows{font-size: 25px; font-weight:bold; text-align: center;}"),
                #     verbatimTextOutput("tv_shows")),
                
                # box(title = "IMDb score", height=165, width = 2, solidHeader = TRUE, 
                #     tags$style(type="text/css", "#imdb_score{font-size: 50px; font-weight:bold; text-align: center;}"),
                #     verbatimTextOutput("imdb_score"),
                #     tags$style(type="text/css", "#star{text-align: right;}"),
                #     # imageOutput("star")
                #     # tags$img(src = "star.png", height = 50, width = 50)
                #     ),
                
                box(title = "Summary of Ratings", solidHeader = TRUE, 
                    shinycssloaders::withSpinner(plotOutput("rating_summary"))),
                
                box(title = "Summary of Durations", solidHeader = TRUE, 
                    shinycssloaders::withSpinner(plotlyOutput("duration_summary"))),
                
                box(title = "Summary of Age Restrictions", solidHeader = TRUE, 
                    shinycssloaders::withSpinner(plotlyOutput("age_summary"))),
                
                box(title = "Summary of Shows released year wise", solidHeader = TRUE, 
                    shinycssloaders::withSpinner(plotlyOutput("year_summary")))
              )
      ),
      tabItem(tabName = "visuals",
              fluidRow(
                box(title = "Age Ratings by Genre", width=4, solidHeader = TRUE, 
                    selectInput("genre", "Select Genre", choices = c("All", sort(unique(data2$listed_in)))), 
                    shinycssloaders::withSpinner(plotlyOutput("ratings_by_genre"))),
                
                box(title = "Ratings by Year", width=8, solidHeader = TRUE, 
                    sliderInput("year", "Select Year", min = min(yd$year), max = max(yd$year), value = c(min(yd$year), max(yd$year))), 
                    shinycssloaders::withSpinner(plotOutput("ratings_by_year")))
              )
      ),
      tabItem(tabName = "data",
              fluidRow(
                box(title = "Filter Data", width=4, solidHeader = TRUE,
                    sliderInput("rating", "Select Rating", min = 0, max = max(data2$imdb), value = c(0, max(data2$imdb)), step = 0.1),
                    selectInput("genre2", "Select Genre", choices = c("All", sort(unique(data2$listed_in)))),
                    sliderInput("duration", "Select Duration", min = 0, max = max(data2$comp_time), value = c(0, max(data2$comp_time)), step = 10),
                    selectInput("age", "Select Age Restriction", choices = c("All", sort(unique(data2$rating)))),
                    hr(),
                    checkboxInput("top_rated", "Show only top rated movies/series")),
                
                box(title = "Amazon Prime Movies/TV Shows", width=8, solidHeader = TRUE,
                    shinycssloaders::withSpinner(dataTableOutput("prime_data")))
              )
      ),
      tabItem(tabName = "world",
              fluidRow(
                box(title = "Distribution of Movies/Shows with respect to Countries",width=12, solidHeader = TRUE,
                    shinycssloaders::withSpinner(
                      plotOutput("map",height = "600px"))
                    )
                    
              )
      )
    )
  )
)

# define the server
server <- function(input, output) {
  output$icon <- renderUI(
    htmltools::HTML('<i class="fa fa-spinner fa-spin fa-3x fa-fw"></i><span class="sr-only">Loading...</span>')
  )
  # output$star <- renderImage({
  #   width<- "20%"
  #   height<- "10%"
  #   list(src = "star.png",
  #        contentType = "image/png",
  #        width = "auto",
  #        height = height
  #   )
  # }, deleteFile = FALSE)
  
  # summary of ratings
  output$rating_summary <- renderPlot({
    Sys.sleep(0.5)
    ggplot(data, aes(x = IMDb.Rating)) + 
      geom_histogram(fill = "steelblue", alpha = 0.8, bins=20) +
      labs(title = "Summary of Ratings", x = "IMDb Ratings", y = "Frequency")
      # theme_bw()
  })
  
  # summary of durations
  freq_duration <- table(data$duration)
  output$duration_summary <- renderPlotly({
    Sys.sleep(0.5)
    plot_ly(labels = names(freq_duration), values = freq_duration, type = "pie", hole=0.6)
  })
  
  freq_age <- table(data$Maturity.Rating)
  output$age_summary <- renderPlotly({
    Sys.sleep(0.5)
    plot_ly(labels = names(freq_age), values = freq_age, type = "pie")
  })
  
  
  output$map <- renderPlot({
    Sys.sleep(1.0)
    # create country frequency table, match then output map plot
    countries <- data2
    countries <- as.data.frame(table(countries$`country`))
    colnames(countries) <- c("country", "value")
    matched <- joinCountryData2Map(countries, joinCode = "NAME", nameJoinColumn = "country")
    mapCountryData(matched, nameColumnToPlot = "value", mapTitle = "", 
                   catMethod = "pretty", colourPalette = c("lightblue", "darkblue"))
  })
  
  output$year_summary <- renderPlotly({
    Sys.sleep(0.5)
    data_filtered <- subset(data2, release_year >= 2017 & release_year <= 2023)
    data_count <- aggregate(data_filtered, by = list(data_filtered$release_year, data_filtered$type), FUN = length)
    names(data_count) <- c("year", "class", "count")
    
    plot_ly(data_count, x=~year, y=~count, color = ~class, type = "bar") %>%
      layout(barmode='group')
  })
  
  # output$dashboard <- renderText({
  #   paste("Amazon Prime","Full Insights",sep="\n")
  #   # paste((round(mean(data$IMDb.Rating, na.rm = TRUE), 1)), " /10", sep = "")
  # })
  # 
  # output$imdb_score <- renderText({
  #   round(mean(data$IMDb.Rating, na.rm = TRUE), 1)
  #   # paste((round(mean(data$IMDb.Rating, na.rm = TRUE), 1)), " /10", sep = "")
  # })
  # 
  # output$total_shows <- renderText({
  #   length(data2$type)
  # })
  # 
  # output$tv_shows <- renderText({
  #   paste(sum(data2$type == "TV Show"), 
  #         paste(round(100*sum(data2$type == "TV Show")/length(data2$type), 2), "%", sep=" "), 
  #         sep="\n")
  # })
  # 
  # output$movies <- renderText({
  #   paste(sum(data2$type == "Movie"), 
  #         paste(round(100*sum(data2$type == "Movie")/length(data2$type), 2), "%", sep=" "), 
  #         sep="\n")
  # })
  # 
  output$ratings_by_genre <- renderPlotly({
    Sys.sleep(0.5)
    # filter data based on input
    if(input$genre == "All") {
      filtered_data <- table(data2$rating)
    } else {
      filtered_data <- filter(data2, listed_in == input$genre)
      filtered_data <- table(filtered_data$rating)
    }
    # plot ratings by genre
    plot_ly(labels = names(filtered_data), values = filtered_data, type = "pie")
  })

  
  output$ratings_by_year <- renderPlot({
    Sys.sleep(0.5)
    # filter data based on input
    if(input$genre == "All") {
      filtered_data <- filter(yd, year >= input$year[1] & year <= input$year[2])
      ggplot(filtered_data, aes(x = year, y = rating)) +
        geom_line(stat = "identity", size=1, color='#00A8E1') +
        geom_point(size=1, shape=23)
        # theme_bw()
    } else {
      filtered_data <- filter(yd2, year >= input$year[1] & year <= input$year[2])
      filtered_data <- filtered_data[,c("year",input$genre)]
      ggplot(filtered_data, aes(x = year, y = filtered_data[,input$genre])) +
        geom_line(stat = "identity", size=1, color='#00A8E1') +
        geom_point(size=1, shape=23)
        # theme_bw()
    }
    # plot ratings by year
  })

  
  filtered_data <- reactive({
    # filter data based on input
    temp_data <- filter(data2, imdb >= input$rating[1] & imdb <= input$rating[2])
    if(input$genre2 == "All") {
      temp_data <- temp_data
    } else {
      temp_data <- filter(temp_data, listed_in == input$genre2)
    }
    temp_data <- filter(temp_data, comp_time >= input$duration[1] & comp_time <= input$duration[2])
    if(input$age == "All") {
      temp_data <- temp_data
    } else {
      temp_data <- filter(temp_data, rating == input$age)
    }
    if(input$top_rated) {
      temp_data <- arrange(temp_data, desc(imdb))
      temp_data <- head(temp_data, 50)
    }
    return(temp_data[,!(names(temp_data) %in% c("comp_time","type"))])
  })
  output$prime_data <- renderDataTable({
    Sys.sleep(0.5)
    filtered_data()
  },options = list(pageLength = 5))
}

shinyApp(ui, server)