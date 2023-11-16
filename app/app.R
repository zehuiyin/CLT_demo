library(shiny)
library(bslib)
library(ggplot2)

ui <- fluidPage(
  theme = bslib::bs_theme(bootswatch = "lux"),
  titlePanel(
    h1("Central Limit Theorm", align = "center")
  ),
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
      sliderInput("sample_size", "Sample Size", min = 0, max = 500, value = 50)
    )
  ),
  fluidRow(
    column(6,
           align="center",
           sliderInput("para1", "Min/Mean/lambda/df1", min = 0, max = 30, value = 5)
    ),
    column(6,
           align="center",
           sliderInput("para2", "Max/SD/df2", min = 0, max = 30, value = 10)
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
  )
)

server <- function(input, output, session) {
  samples <- reactive({
    req(input$pop_dist)
    
    if (input$pop_dist == "norm") {
      data <- data.frame(
        x = sample(rnorm(input$sample_size,
                         mean = input$para1,
                         sd = input$para2))
      )
    }
    
    if (input$pop_dist == "unif") {
      data <- data.frame(
        x = sample(runif(input$sample_size,
                         min = input$para1,
                         max = input$para2))
      )
    }
    
    if (input$pop_dist == "poiss") {
      data <- data.frame(
        x = sample(rpois(input$sample_size,
                         lambda = input$para1))
      )
    }
    
    if (input$pop_dist == "chiq") {
      data <- data.frame(
        x = sample(rchisq(input$sample_size,
                          df = input$para1))
      )
    }
    
    if (input$pop_dist == "F") {
      data <- data.frame(
        x = sample(rf(input$sample_size,
                      df1 = input$para1,
                      df2 = input$para2))
      )
    }
    
    data
  })
  
  means <- reactive({
    req(input$pop_dist)
    
    if (input$pop_dist == "norm") {
      data <- data.frame(
        x = replicate(1e4, mean(sample(rnorm(input$sample_size,
                         mean = input$para1,
                         sd = input$para2))))
      )
    }
    
    if (input$pop_dist == "unif") {
      data <- data.frame(
        x = replicate(1e4, mean(sample(runif(input$sample_size,
                         min = input$para1,
                         max = input$para2))))
      )
    }
    
    if (input$pop_dist == "poiss") {
      data <- data.frame(
        x = replicate(1e4, mean(sample(rpois(input$sample_size,
                         lambda = input$para1))))
      )
    }
    
    if (input$pop_dist == "chiq") {
      data <- data.frame(
        x = replicate(1e4, mean(sample(rchisq(input$sample_size,
                          df = input$para1))))
      )
    }
    
    if (input$pop_dist == "F") {
      data <- data.frame(
        x = replicate(1e4, mean(sample(rf(input$sample_size,
                      df1 = input$para1,
                      df2 = input$para2))))
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
           title = "Normal Quantile Plot") +
      theme(panel.background = element_blank(),
            panel.border = element_rect(color = "black", fill = NA),
            plot.title = element_text(hjust = 0.5))
  })
}

shinyApp(ui, server)
