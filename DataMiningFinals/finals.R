library(shiny)
library(bslib)
library(readr)  # for reading CSV
library(dplyr)  # for data manipulation
library(ggplot2)  # for visualization
library(stringr)  # for string manipulation
library(gridExtra)  # for combining plots
library(tidyr)  # for separate_rows

# Print working directory and data path
cat("\n========== NETFLIX DATA LOADING ==========\n")
cat("📂 Working Directory:", getwd(), "\n")
cat("📄 Data File:", file.path(getwd(), "data/NetflixDataKaggle.csv"), "\n")

# Read the Netflix data
netflix_data <- read_csv("data/NetflixDataKaggle.csv", show_col_types = FALSE)

# Print data information
cat("\n=============== DATASET OVERVIEW ================\n")
cat("📊 Total Entries:", nrow(netflix_data), "\n")
cat("🏷️  Total Columns:", ncol(netflix_data), "\n\n")

cat("Column Names and Types:\n")
cat("----------------------\n")
for(col in names(netflix_data)) {
  cat(sprintf("%-20s: %s\n", col, typeof(netflix_data[[col]])))
}

cat("\n========== CONTENT SUMMARY ==========\n")
# Calculate content type counts once
content_types <- table(netflix_data$type)
cat("🎬 Movies:", content_types["Movie"], "\n")
cat("📺 TV Series:", content_types["TV Series"], "\n")

cat("\n========== SAMPLE ENTRIES ==========\n")
cat("First 5 titles in dataset:\n")
cat("-------------------------\n")
sample_data <- head(netflix_data, 5) %>% 
  select(title, type, releaseYear, imdbAverageRating)
for(i in 1:nrow(sample_data)) {
  cat(sprintf("%d. %s (%s, %d) - IMDB: %.1f\n",
              i,
              sample_data$title[i],
              sample_data$type[i],
              sample_data$releaseYear[i],
              sample_data$imdbAverageRating[i]))
}

# Add Genre Analysis
cat("\n========== GENRE DISTRIBUTION SUMMARY ===========\n")

# Movies Top 5 Genres
cat("\n🎬 Top 5 Movie Genres:\n")
cat("---------------------\n")
genre_data <- netflix_data %>% 
  select(type, genres) %>% 
  filter(!is.na(genres)) %>% 
  group_by(type, genres) %>% 
  summarise(count = n(), .groups = 'drop') %>% 
  arrange(type, desc(count))
genre_data %>%
  filter(type == "Movie") %>%
  slice_head(n = 5) %>%
  {
    for(i in 1:nrow(.)) {
      cat(sprintf("%d. %s (%s titles)\n",
                  i,
                  .$genres[i],
                  format(.$count[i], big.mark=",")))
    }
  }

# TV Series Top 5 Genres
cat("\n📺 Top 5 TV Series Genres:\n")
cat("------------------------\n")
genre_data %>%
  filter(type == "TV Series") %>%
  slice_head(n = 5) %>%
  {
    for(i in 1:nrow(.)) {
      cat(sprintf("%d. %s (%s titles)\n",
                  i,
                  .$genres[i],
                  format(.$count[i], big.mark=",")))
    }
  }


# Add IMDb Ratings Analysis
cat("\n============= IMDB RATINGS SUMMARY ==============\n")

# Overall ratings statistics
overall_stats <- netflix_data %>%
  summarise(
    avg_rating = mean(imdbAverageRating, na.rm = TRUE),
    median_rating = median(imdbAverageRating, na.rm = TRUE),
    min_rating = min(imdbAverageRating, na.rm = TRUE),
    max_rating = max(imdbAverageRating, na.rm = TRUE)
  )

cat("\n📊 Overall IMDb Ratings:\n")
cat("------------------------\n")
cat(sprintf("Average Rating: %.2f\n", overall_stats$avg_rating))
cat(sprintf("Median Rating:  %.2f\n", overall_stats$median_rating))
cat(sprintf("Range:         %.1f - %.1f\n", overall_stats$min_rating, overall_stats$max_rating))

# Ratings by content type
cat("\n📈 Ratings by Content Type:\n")
cat("-------------------------")
netflix_data %>%
  group_by(type) %>%
  summarise(
    avg_rating = mean(imdbAverageRating, na.rm = TRUE),
    median_rating = median(imdbAverageRating, na.rm = TRUE)
  ) %>%
  as.data.frame() %>%
  {
    for(i in 1:nrow(.)) {
      cat(sprintf("\n%s:\n", .[i, "type"]))
      cat(sprintf("  Average Rating: %.2f\n", .[i, "avg_rating"]))
      cat(sprintf("  Median Rating:  %.2f\n", .[i, "median_rating"]))
    }
  }

# Add Future Media Analysis
cat("\n============= FUTURE MEDIA SUMMARY =============\n")

# Calculate yearly counts and fit models for prediction
yearly_counts <- netflix_data %>%
  group_by(type, releaseYear) %>%
  summarise(count = n(), .groups = 'drop') %>%
  filter(releaseYear >= 2010)

# Create future years
future_years <- data.frame(
  releaseYear = 2024:2028
)

# Fit linear models
movie_model <- lm(count ~ releaseYear, data = filter(yearly_counts, type == "Movie"))
tv_model <- lm(count ~ releaseYear, data = filter(yearly_counts, type == "TV Series"))

# Generate predictions
future_movies <- predict(movie_model, newdata = future_years)
future_tv <- predict(tv_model, newdata = future_years)

# Print predictions summary
cat("\n📺 Content Trend Analysis:\n")
cat("-------------------------\n")
cat("Movies: Showing declining trend")
cat("\nTV Series: Strong upward trend\n")

cat("\n🔮 Predictions for 2028:\n")
cat("----------------------\n")
cat(sprintf("Movies: %.0f titles\n", future_movies[5]))
cat(sprintf("TV Series: %.0f titles\n", future_tv[5]))

cat("\n📊 Key Finding:\n")
cat("--------------\n")
cat("TV Series are projected to become the dominant content type on Netflix,\n")
cat("reflecting a strategic shift in content strategy and viewer preferences.\n")

# Prepare data for genre analysis
genre_data <- netflix_data %>% 
  select(type, genres) %>% 
  filter(!is.na(genres)) %>% 
  group_by(type, genres) %>% 
  summarise(count = n(), .groups = 'drop') %>% 
  arrange(type, desc(count))

# Get total counts
total_movies <- content_types["Movie"]
total_tv <- content_types["TV Series"]

# Get top 5 genres for each type
top_genres <- genre_data %>% 
  group_by(type) %>% 
  slice_head(n = 5)

# UI Definition
ui <- fluidPage(
  theme = bs_theme(version = 5),
  tags$head(
    tags$style(HTML("
      body { background-color: #221F1F; }
      .container-fluid { background-color: #221F1F; }
      .nav-tabs { border-bottom: 1px solid #E50914; }
      .nav-tabs .nav-link { color: #F5F5F1; }
      .nav-tabs .nav-link.active { 
        color: #E50914; 
        font-weight: bold;
        background-color: #221F1F;
        border-color: #E50914;
        border-bottom: 2px solid #E50914;
      }
      .nav-tabs .nav-link:hover { 
        border-color: #E50914;
        color: #E50914;
      }
      h2, h3, h4 { color: #F5F5F1 !important; }
      p, li { color: #F5F5F1; }
      /* Add styles for verbatimTextOutput */
      pre.shiny-text-output {
        color: #F5F5F1;
        background-color: #1A1A1A;
        border: none;
      }
    "))
  ),
  div(style = "background-color: #000000; padding: 20px 0; margin-bottom: 30px;",
      titlePanel(div(style = "color: #E50914; text-align: center; font-weight: bold;", "Netflix Content Analysis"))
  ),
  fluidRow(
    column(12,
           tabsetPanel(
             # Tab 1
             tabPanel("About Data",
                      fluidRow(
                        column(10, offset = 1,
                               div(style = "padding: 20px;",
                                   # Overview Card
                                   div(style = "background-color: #2D2D2D; padding: 25px; border-radius: 10px; box-shadow: 0 4px 8px rgba(0,0,0,0.3); margin-bottom: 20px;",
                                       h3("Dataset Overview", style = "color: #F5F5F1; border-bottom: 2px solid #E50914; padding-bottom: 10px;"),
                                       p("This comprehensive Netflix dataset contains detailed information about movies and TV shows available on the platform. It includes content added up until 2021, providing insights into Netflix's evolving content library."),
                                       p(strong("Total Entries:"), "14,284 titles")
                                   ),
                                   
                                   # Content Distribution Card
                                   div(style = "background-color: #2D2D2D; padding: 25px; border-radius: 10px; box-shadow: 0 4px 8px rgba(0,0,0,0.3); margin-bottom: 20px;",
                                       h3("Content Distribution", style = "color: #F5F5F1; margin-bottom: 20px; border-bottom: 2px solid #E50914; padding-bottom: 10px;"),
                                       div(style = "display: flex; justify-content: space-around;",
                                           div(
                                             h4("Movies", style = "color: #E50914;"),
                                             p("9,673 titles")
                                           ),
                                           div(
                                             h4("TV Shows", style = "color: #E50914;"),
                                             p("4,611 titles")
                                           )
                                       )
                                   ),
                                   
                                   # Key Features Card
                                   div(style = "background-color: #2D2D2D; padding: 25px; border-radius: 10px; box-shadow: 0 4px 8px rgba(0,0,0,0.3); margin-bottom: 20px;",
                                       h3("Available Information", style = "color: #F5F5F1; margin-bottom: 20px; border-bottom: 2px solid #E50914; padding-bottom: 10px;"),
                                       tags$ul(
                                         tags$li(strong("Content Type: "), "Classification as Movie or TV Show"),
                                         tags$li(strong("Genres: "), "Multiple genres per title"),
                                         tags$li(strong("IMDb Ratings: "), "Average viewer ratings from IMDb"),
                                         tags$li(strong("Release Year: "), "Original release date of content"),
                                         tags$li(strong("Duration: "), "Length in minutes (movies) or seasons (TV shows)")
                                       )
                                   ),
                                   
                                   # Analysis Focus Card
                                   div(style = "background-color: #2D2D2D; padding: 25px; border-radius: 10px; margin-top: 30px;",
                                       h3("Analysis Focus", style = "color: #F5F5F1; margin-bottom: 20px; border-bottom: 2px solid #E50914; padding-bottom: 10px;"),
                                       p("Our analysis explores three main aspects:"),
                                       tags$ul(
                                         tags$li(strong("Genre Distribution: "), "Understanding the most common content categories"),
                                         tags$li(strong("IMDb Ratings Analysis: "), "Examining content quality and audience reception"),
                                         tags$li(strong("Future Media Prediction: "), "Analyzing trends for content strategy insights")
                                       )
                                   )
                               )
                        )
                      )),
             
             # Tab 2
             tabPanel("Question 1: Genre Distribution",
                      fluidRow(
                        column(10, offset = 1,
                               div(style = "padding: 20px;",
                                   h2("What are the Most common Movie and TV Show Genres on Netflix?", 
                                      style = "color: #F5F5F1; text-align: center; margin-bottom: 30px;"),
                                   
                                   # Movie Card
                                   div(style = "background-color: #2D2D2D; padding: 20px; border-radius: 10px; box-shadow: 0 4px 8px rgba(0,0,0,0.3); margin-bottom: 20px;",
                                       h3("Movies Analysis", style = "color: #F5F5F1; margin-bottom: 20px; border-bottom: 2px solid #E50914; padding-bottom: 10px;"),
                                       div(style = "margin-bottom: 15px;",
                                           plotOutput("moviePlot", height = "400px")
                                       ),
                                       div(style = "background-color: #1A1A1A; padding: 15px; border-radius: 5px;",
                                           h4("Top 5 Movie Genres", style = "color: #F5F5F1; margin-bottom: 10px;"),
                                           verbatimTextOutput("movieGenres", placeholder = TRUE)
                                       )
                                   ),
                                   
                                   # TV Series Card
                                   div(style = "background-color: #2D2D2D; padding: 20px; border-radius: 10px; box-shadow: 0 4px 8px rgba(0,0,0,0.3);",
                                       h3("TV Series Analysis", style = "color: #F5F5F1; margin-bottom: 20px; border-bottom: 2px solid #E50914; padding-bottom: 10px;"),
                                       div(style = "margin-bottom: 15px;",
                                           plotOutput("tvPlot", height = "400px")
                                       ),
                                       div(style = "background-color: #1A1A1A; padding: 15px; border-radius: 5px;",
                                           h4("Top 5 TV Series Genres", style = "color: #F5F5F1; margin-bottom: 10px;"),
                                           verbatimTextOutput("tvGenres", placeholder = TRUE)
                                       )
                                   )
                               )
                        )
                      )),
             
             # Tab 3: IMDb Ratings Analysis
             tabPanel("Question 2: IMDb Ratings",
                      fluidRow(
                        column(10, offset = 1,
                               div(style = "padding: 20px;",
                                   h2("How do IMDb Ratings Compare Between Movies and TV Shows?", 
                                      style = "color: #F5F5F1; text-align: center; margin-bottom: 30px;"),
                                   
                                   # Combined Ratings Distribution Card
                                   div(style = "background-color: #2D2D2D; padding: 20px; border-radius: 10px; box-shadow: 0 4px 8px rgba(0,0,0,0.3); margin-bottom: 20px;",
                                       h3("Combined IMDb Ratings Distribution", style = "color: #F5F5F1; margin-bottom: 20px; border-bottom: 2px solid #E50914; padding-bottom: 10px;"),
                                       div(style = "margin-bottom: 15px;",
                                           plotOutput("ratings", height = "400px")
                                       ),
                                       div(style = "text-align: center; color: #F5F5F1;",
                                           verbatimTextOutput("combinedAvgRating")
                                       )
                                   ),
                                   
                                   # Separate Distributions Row
                                   fluidRow(
                                     # Movies Distribution Card
                                     column(6,
                                            div(style = "background-color: #2D2D2D; padding: 20px; border-radius: 10px; box-shadow: 0 4px 8px rgba(0,0,0,0.3);",
                                                h3("Movies Ratings Distribution", style = "color: #F5F5F1; margin-bottom: 20px; border-bottom: 2px solid #E50914; padding-bottom: 10px;"),
                                                div(style = "margin-bottom: 15px;",
                                                    plotOutput("moviesRatings", height = "300px")
                                                ),
                                                div(style = "text-align: center; color: #F5F5F1;",
                                                    verbatimTextOutput("movieAvgRating")
                                                )
                                            )
                                     ),
                                     # TV Series Distribution Card
                                     column(6,
                                            div(style = "background-color: #2D2D2D; padding: 20px; border-radius: 10px; box-shadow: 0 4px 8px rgba(0,0,0,0.3);",
                                                h3("TV Series Ratings Distribution", style = "color: #F5F5F1; margin-bottom: 20px; border-bottom: 2px solid #E50914; padding-bottom: 10px;"),
                                                div(style = "margin-bottom: 15px;",
                                                    plotOutput("tvRatings", height = "300px")
                                                ),
                                                div(style = "text-align: center; color: #F5F5F1;",
                                                    verbatimTextOutput("tvAvgRating")
                                                )
                                            )
                                     )
                                   ),
                                   
                                   # Analysis Description Card
                                   div(style = "background-color: #2D2D2D; padding: 20px; border-radius: 10px; box-shadow: 0 4px 8px rgba(0,0,0,0.3); margin-top: 20px;",
                                       h3("Analysis Results", style = "color: #F5F5F1; margin-bottom: 20px; border-bottom: 2px solid #E50914; padding-bottom: 10px;"),
                                       p("The image presents a visual analysis of IMDb average ratings for movies and TV series. It reveals that while both media types have a similar distribution of ratings, peaking around 6-7, TV series tend to have a slightly higher average rating (7.05) compared to movies (6.31). This suggests that, on average, viewers tend to rate TV series slightly higher than movies on IMDb.",
                                         style = "color: #F5F5F1; line-height: 1.6; font-size: 16px;")
                                   )
                               )
                        )
                      )),
             
             # Tab 4
             tabPanel("Question 3: Future Media",
                      fluidRow(
                        column(10, offset = 1,
                               div(style = "padding: 20px;",
                                   h2("Which type of media will people watch the most in the next 5 years?", 
                                      style = "color: #F5F5F1; text-align: center; margin-bottom: 30px;"),
                                   plotOutput("futurePrediction"),
                                   div(style = "background-color: #2D2D2D; padding: 25px; border-radius: 10px; margin-top: 30px;",
                                       h3("Analysis Results", style = "color: #F5F5F1; border-bottom: 2px solid #E50914; padding-bottom: 10px;"),
                                       p("Based on historical data from 2010 onwards, our analysis reveals several key insights:", 
                                         style = "color: #F5F5F1;"),
                                       tags$ul(
                                         tags$li("While Movies historically dominated Netflix's content library, there's a clear declining trend in movie content additions.", 
                                                style = "color: #F5F5F1;"),
                                         tags$li("TV Series show a strong upward trend, indicating Netflix's strategic shift towards serial content.", 
                                                style = "color: #F5F5F1;"),
                                         tags$li("The convergence of these trends suggests a transformation in Netflix's content strategy, prioritizing TV Series over Movies.", 
                                                style = "color: #F5F5F1;")
                                       ),
                                       h3("Conclusion", style = "color: #F5F5F1; border-bottom: 2px solid #E50914; padding-bottom: 10px; margin-top: 20px;"),
                                       p("Based on our predictive analysis, TV Series will become the dominant content type on Netflix over the next 5 years (2024-2028). The declining trend in movie additions combined with the steady rise in TV Series suggests a clear strategic pivot towards serial content. This shift aligns with modern viewing habits where audiences increasingly prefer long-form, episodic storytelling that allows for deeper character development and more complex narratives.", 
                                         style = "color: #F5F5F1;"),
                                       p("Note: This prediction is based on historical data patterns and assumes current trends will continue. External factors such as market conditions, production challenges, or strategic shifts could impact these projections.", 
                                         style = "color: #F5F5F1; font-style: italic; margin-top: 20px;")
                                   )
                               )
                        )
                      )
             )
           )
    )
  )
)

# Server Definition
server <- function(input, output) {
  # Movies plot
  output$moviePlot <- renderPlot({
    movies_plot <- genre_data %>% 
      filter(type == "Movie") %>% 
      slice_head(n = 10) %>% 
      ggplot(aes(x = reorder(genres, count), y = count)) +
      geom_bar(stat = "identity", fill = "#E50914") +
      coord_flip() +
      labs(title = "Top 10 Movie Genres",
           subtitle = paste("Total Movies:", format(total_movies, big.mark=",")),
           x = "Genre",
           y = "Number of Movies") +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 16, face = "bold", color = "#F5F5F1"),
        plot.subtitle = element_text(size = 12, color = "#F5F5F1"),
        axis.title = element_text(size = 12, color = "#F5F5F1"),
        axis.text = element_text(size = 10, color = "#F5F5F1"),
        panel.grid.major = element_line(color = "#2D2D2D"),
        panel.grid.minor = element_blank(),
        plot.background = element_rect(fill = "#2D2D2D", color = NA),
        panel.background = element_rect(fill = "#2D2D2D", color = NA)
      )
    movies_plot
  })
  
  # TV Series plot
  output$tvPlot <- renderPlot({
    tv_plot <- genre_data %>% 
      filter(type == "TV Series") %>%  
      slice_head(n = 10) %>% 
      ggplot(aes(x = reorder(genres, count), y = count)) +
      geom_bar(stat = "identity", fill = "#E50914") +
      coord_flip() +
      labs(title = "Top 10 TV Series Genres",
           subtitle = paste("Total TV Series:", format(total_tv, big.mark=",")),
           x = "Genre",
           y = "Number of TV Series") +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 16, face = "bold", color = "#F5F5F1"),
        plot.subtitle = element_text(size = 12, color = "#F5F5F1"),
        axis.title = element_text(size = 12, color = "#F5F5F1"),
        axis.text = element_text(size = 10, color = "#F5F5F1"),
        panel.grid.major = element_line(color = "#2D2D2D"),
        panel.grid.minor = element_blank(),
        plot.background = element_rect(fill = "#2D2D2D", color = NA),
        panel.background = element_rect(fill = "#2D2D2D", color = NA)
      )
    tv_plot
  })
  
  output$ratings <- renderPlot({
    # Use all data without filtering
    ratings_data <- netflix_data %>%
      select(type, imdbAverageRating)
    
    ggplot(ratings_data, aes(x = imdbAverageRating, fill = type)) +
      geom_histogram(binwidth = 0.5, alpha = 0.7, position = "identity", color = "white") +
      scale_fill_manual(values = c("Movie" = "#E50914", "TV Series" = "#564D4D")) +
      labs(title = "Distribution of IMDb Ratings for TV Series and Movies",
           subtitle = paste("Total Content:", format(nrow(ratings_data), big.mark = ",")),
           x = "IMDb Average Rating",
           y = NULL,
           fill = "Media Type") +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 16, face = "bold", color = "#F5F5F1"),
        plot.subtitle = element_text(size = 12, color = "#F5F5F1"),
        axis.title = element_text(size = 12, color = "#F5F5F1"),
        axis.text.x = element_text(size = 10, color = "#F5F5F1"),
        axis.text.y = element_blank(),
        panel.grid.major = element_line(color = "#2D2D2D"),
        panel.grid.minor = element_blank(),
        plot.background = element_rect(fill = "#2D2D2D", color = NA),
        panel.background = element_rect(fill = "#2D2D2D", color = NA),
        legend.background = element_rect(fill = "#2D2D2D", color = NA),
        legend.text = element_text(color = "#F5F5F1"),
        legend.title = element_text(color = "#F5F5F1")
      )
  })
  
  # Movies only ratings plot
  output$moviesRatings <- renderPlot({
    movies_data <- netflix_data %>% 
      filter(type == "Movie") %>%
      select(imdbAverageRating)
    
    ggplot(movies_data, aes(x = imdbAverageRating)) +
      geom_histogram(binwidth = 0.5, fill = "#E50914", alpha = 0.7, color = "white") +
      labs(title = "Movies IMDb Ratings Distribution",
           subtitle = paste("Total Movies:", format(nrow(movies_data), big.mark = ",")),
           x = "IMDb Average Rating",
           y = NULL) +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 14, face = "bold", color = "#F5F5F1"),
        plot.subtitle = element_text(size = 10, color = "#F5F5F1"),
        axis.title = element_text(size = 10, color = "#F5F5F1"),
        axis.text.x = element_text(size = 8, color = "#F5F5F1"),
        axis.text.y = element_blank(),
        panel.grid.major = element_line(color = "#2D2D2D"),
        panel.grid.minor = element_blank(),
        plot.background = element_rect(fill = "#2D2D2D", color = NA),
        panel.background = element_rect(fill = "#2D2D2D", color = NA)
      )
  })
  
  # TV Series only ratings plot
  output$tvRatings <- renderPlot({
    tv_data <- netflix_data %>% 
      filter(type == "TV Series") %>%
      select(imdbAverageRating)
    
    ggplot(tv_data, aes(x = imdbAverageRating)) +
      geom_histogram(binwidth = 0.5, fill = "#564D4D", alpha = 0.7, color = "white") +
      labs(title = "TV Series IMDb Ratings Distribution",
           subtitle = paste("Total TV Series:", format(nrow(tv_data), big.mark = ",")),
           x = "IMDb Average Rating",
           y = NULL) +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 14, face = "bold", color = "#F5F5F1"),
        plot.subtitle = element_text(size = 10, color = "#F5F5F1"),
        axis.title = element_text(size = 10, color = "#F5F5F1"),
        axis.text.x = element_text(size = 8, color = "#F5F5F1"),
        axis.text.y = element_blank(),
        panel.grid.major = element_line(color = "#2D2D2D"),
        panel.grid.minor = element_blank(),
        plot.background = element_rect(fill = "#2D2D2D", color = NA),
        panel.background = element_rect(fill = "#2D2D2D", color = NA)
      )
  })
  
  # Movie genres text output
  output$movieGenres <- renderText({
    paste(
      "📽️ Total Movies:", format(total_movies, big.mark=","), "\n\n",
      paste(sprintf("%d. %s (%s movies)", 1:5,
                    top_genres$genres[top_genres$type == "Movie"],
                    format(top_genres$count[top_genres$type == "Movie"], big.mark=",")),
            collapse = "\n")
    )
  })
  
  # TV Series genres text output
  output$tvGenres <- renderText({
    paste(
      "📺 Total TV Series:", format(total_tv, big.mark=","), "\n\n",
      paste(sprintf("%d. %s (%s series)", 1:5,
                    top_genres$genres[top_genres$type == "TV Series"],
                    format(top_genres$count[top_genres$type == "TV Series"], big.mark=",")),
            collapse = "\n")
    )
  })
  
  # Average ratings outputs
  output$combinedAvgRating <- renderText({
    avg_rating <- mean(netflix_data$imdbAverageRating, na.rm = TRUE)
    sprintf("Overall Average IMDb Rating: %.2f", avg_rating)
  })
  
  output$movieAvgRating <- renderText({
    movie_avg <- netflix_data %>%
      filter(type == "Movie") %>%
      summarise(avg = mean(imdbAverageRating, na.rm = TRUE)) %>%
      pull(avg)
    sprintf("Average Movie Rating: %.2f", movie_avg)
  })
  
  output$tvAvgRating <- renderText({
    tv_avg <- netflix_data %>%
      filter(type == "TV Series") %>%
      summarise(avg = mean(imdbAverageRating, na.rm = TRUE)) %>%
      pull(avg)
    sprintf("Average TV Series Rating: %.2f", tv_avg)
  })
  
  # Future prediction plot
  output$futurePrediction <- renderPlot({
    # Calculate yearly counts
    yearly_counts <- netflix_data %>%
      group_by(type, releaseYear) %>%
      summarise(count = n(), .groups = 'drop') %>%
      filter(releaseYear >= 2010)  # Focus on recent years
    
    # Create future years
    future_years <- data.frame(
      releaseYear = 2024:2028
    )
    
    # Fit linear models for each type
    movie_model <- lm(count ~ releaseYear, data = filter(yearly_counts, type == "Movie"))
    tv_model <- lm(count ~ releaseYear, data = filter(yearly_counts, type == "TV Series"))
    
    # Generate predictions
    future_movies <- predict(movie_model, newdata = future_years)
    future_tv <- predict(tv_model, newdata = future_years)
    
    # Combine historical and predicted data
    future_data <- bind_rows(
      yearly_counts,
      data.frame(
        type = rep(c("Movie", "TV Series"), each = 5),
        releaseYear = rep(2024:2028, 2),
        count = c(future_movies, future_tv)
      )
    )
    
    # Create the plot
    ggplot(future_data, aes(x = releaseYear, y = count, color = type, linetype = releaseYear >= 2024)) +
      geom_line(size = 1) +
      scale_color_manual(values = c("Movie" = "#E50914", "TV Series" = "#4CAF50")) +
      scale_linetype_manual(values = c("FALSE" = "solid", "TRUE" = "dashed"), guide = "none") +
      labs(
        x = "Year",
        y = "Number of Titles",
        color = "Content Type",
        title = "Historical and Predicted Content Growth on Netflix"
      ) +
      theme_minimal() +
      theme(
        plot.background = element_rect(fill = "#221F1F"),
        panel.background = element_rect(fill = "#221F1F"),
        text = element_text(color = "#F5F5F1"),
        axis.text = element_text(color = "#F5F5F1"),
        axis.title = element_text(color = "#F5F5F1"),
        panel.grid = element_line(color = "#2D2D2D"),
        legend.background = element_rect(fill = "#221F1F"),
        legend.text = element_text(color = "#F5F5F1"),
        legend.title = element_text(color = "#F5F5F1"),
        plot.title = element_text(color = "#F5F5F1", hjust = 0.5)
      )
  })
}

# Run the application
shinyApp(ui = ui, server = server)
