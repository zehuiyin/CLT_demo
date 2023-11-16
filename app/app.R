library(shiny)
library(bslib)
library(ggplot2)

ui <- fluidPage(
  theme = bslib::bs_theme(bootswatch = "lux"),
  titlePanel(
    h1("Central Limit Theorm", align = "center")
  ),
  div(
  fluidRow(
    column(10,
           align="center",
    span("The sampling distribution of the mean will always be normally distributed, as long as the sample size is large enough.")
    ),
    column(2,
           align="center",
           HTML("<span>Created by <a href='https://zehuiyin.github.io/' target='_blank'>Zehui Yin</a></span>")
    ),
  ),
  fluidRow(
    column(6,
           align="center",
           selectInput("pop_dist", "Data Generating Process", 
                       choices = c(
                         "Normal Distribution" = "norm",
                         "Uniform Distribution" = "unif",
                         "Poisson Distribution" = "poiss",
                         "Chi-squared distribution" = "chiq",
                         "F-distribution" = "F"
                       ),
                       selected = "unif"
                       )
    ),
    column(6,
           align="center",
      sliderInput("sample_size", "Sample Size", min = 0, max = 1000, value = 50)
    )
  ),
  fluidRow(
    column(6,
           align="center",
           uiOutput("para1")
    ),
    column(6,
           align="center",
           uiOutput("para2")
    )
  ),
  fluidRow(
    column(4,
           align="center",
           plotOutput("one_hist_sample")
    ),
    column(4,
           align="center",
      plotOutput("hist_sample_mean")
    ),
    column(4,
           align="center",
      plotOutput("qq_sample_mean")
    )
  ), style = "max-width: 1200px; margin: auto;")
)

server <- function(input, output, session) {
  output$para1 <- renderUI({
    if (input$pop_dist == "norm") {
      sliderInput("normal_mean", "Mean", min = -30, max = 30, value = 0)
    } else if (input$pop_dist == "unif") {
      sliderInput("unif_min", "Min", min = -30, max = 30, value = 0)
    } else if (input$pop_dist == "poiss") {
      sliderInput("poiss_lambda", "Lambda", min = 0, max = 30, value = 5)
    } else if (input$pop_dist == "chiq") {
      sliderInput("chiq_df", "df", min = 0, max = 30, value = 5)
    } else if (input$pop_dist == "F") {
      sliderInput("f_df1", "df1", min = 0, max = 30, value = 5)
    }
  })
  
  output$para2 <- renderUI({
    if (input$pop_dist == "norm") {
      sliderInput("norm_sd", "SD", min = 0, max = 30, value = 5)
    } else if (input$pop_dist == "unif") {
      sliderInput("unif_max", "Max", min = -30, max = 30, value = 10)
    } else if (input$pop_dist == "poiss") {

    } else if (input$pop_dist == "chiq") {

    } else if (input$pop_dist == "F") {
      sliderInput("f_df2", "df2", min = 0, max = 30, value = 5)
    }
  })
  
  samples <- reactive({
    req(input$pop_dist)
    req(input$sample_size)
    
    if (input$pop_dist == "norm") {
      req(input$normal_mean)
      req(input$norm_sd)
      
      data <- data.frame(
        x = sample(rnorm(input$sample_size,
                         mean = input$normal_mean,
                         sd = input$norm_sd))
      )
    }
    
    if (input$pop_dist == "unif") {
      req(input$unif_min)
      req(input$unif_max)
      
      data <- data.frame(
        x = sample(runif(input$sample_size,
                         min = input$unif_min,
                         max = input$unif_max))
      )
    }
    
    if (input$pop_dist == "poiss") {
      req(input$poiss_lambda)
      
      data <- data.frame(
        x = sample(rpois(input$sample_size,
                         lambda = input$poiss_lambda))
      )
    }
    
    if (input$pop_dist == "chiq") {
      req(input$chiq_df)
      
      data <- data.frame(
        x = sample(rchisq(input$sample_size,
                          df = input$chiq_df))
      )
    }
    
    if (input$pop_dist == "F") {
      req(input$f_df1)
      req(input$f_df2)
      
      data <- data.frame(
        x = sample(rf(input$sample_size,
                      df1 = input$f_df1,
                      df2 = input$f_df2))
      )
    }
    
    data
  })
  
  means <- reactive({
    req(input$pop_dist)
    req(input$sample_size)
    
    if (input$pop_dist == "norm") {
      req(input$normal_mean)
      req(input$norm_sd)
      
      data <- data.frame(
        x = replicate(1e4, mean(sample(rnorm(input$sample_size,
                         mean = input$normal_mean,
                         sd = input$norm_sd))))
      )
    }
    
    if (input$pop_dist == "unif") {
      req(input$unif_min)
      req(input$unif_max)
      
      data <- data.frame(
        x = replicate(1e4, mean(sample(runif(input$sample_size,
                         min = input$unif_min,
                         max = input$unif_max))))
      )
    }
    
    if (input$pop_dist == "poiss") {
      req(input$poiss_lambda)
      
      data <- data.frame(
        x = replicate(1e4, mean(sample(rpois(input$sample_size,
                         lambda = input$poiss_lambda))))
      )
    }
    
    if (input$pop_dist == "chiq") {
      req(input$chiq_df)
      
      data <- data.frame(
        x = replicate(1e4, mean(sample(rchisq(input$sample_size,
                          df = input$chiq_df))))
      )
    }
    
    if (input$pop_dist == "F") {
      req(input$f_df1)
      req(input$f_df2)
      
      data <- data.frame(
        x = replicate(1e4, mean(sample(rf(input$sample_size,
                      df1 = input$f_df1,
                      df2 = input$f_df2))))
      )
    }
    
    data
  })
  
  output$one_hist_sample <- renderPlot({
    ggplot(samples(), aes(x = x)) + 
      geom_histogram(bins = round(log(input$sample_size,2)+1)) +
      labs(x = "Data Values",
           y = "Frequencies",
           title = "Histogram of One Sample") +
      theme(panel.background = element_blank(),
            panel.border = element_rect(color = "black", fill = NA),
            plot.title = element_text(hjust = 0.5))
  })
  
  output$hist_sample_mean <- renderPlot({
    ggplot(means(), aes(x = x)) + 
      geom_histogram(bins = round(log(1e4,2)+1)) +
      labs(x = "Sample Means",
           y = "Frequencies",
           title = "Histogram of Sample Means") +
      theme(panel.background = element_blank(),
            panel.border = element_rect(color = "black", fill = NA),
            plot.title = element_text(hjust = 0.5))
  })
  
  output$qq_sample_mean <- renderPlot({
    ggplot(means(), aes(sample = x)) + 
      stat_qq() +stat_qq_line() +
      labs(x = "Theotical Quantiles",
           y = "Sample Quantiles",
           title = "Normal Quantile Plot of Sample Means") +
      theme(panel.background = element_blank(),
            panel.border = element_rect(color = "black", fill = NA),
            plot.title = element_text(hjust = 0.5))
  })
}

shinyApp(ui, server)
