library(shiny)
library(popbio)
library(ggplot2)
library(Rage)
library(DiagrammeR)

ui <- fluidPage(
  
  titlePanel("Population Projection using popbio"),
  
  sidebarLayout(
    sidebarPanel(
      sliderInput("juv_to_adult", 
                  "Juvenile to Adult Transition Rate:", 
                  min = 0, max = 1, value = 0.2, step = 0.05),
      
      sliderInput("adult_fertility", 
                  "Adult Fertility:", 
                  min = 0, max = 5, value = 2, step = 0.1),
      
      sliderInput("adult_survival", 
                  "Adult Survival Rate:", 
                  min = 0, max = 1, value = 0.2, step = 0.05)
    ),
    
    mainPanel(
      h3("Projection Matrix"),
      tableOutput("matrixTable"),
      
      h3("Life Cycle Graph"),
      fluidPage(grVizOutput("life_cycle_plot")),
      
      h3("Population Metrics"),
      tableOutput("metricsTable"),
      
      h3("Stable Stage Distribution"),
      tableOutput("stableStageTable"),
      
      h3("Population Projection Plot"),
      plotOutput("projectionPlot")
    )
  )
)

server <- function(input, output) {
  # Reactive expression to calculate the projection matrix
  projectionMatrix <- reactive({
    matrix(c(0, input$adult_fertility,
             input$juv_to_adult, input$adult_survival),
           nrow = 2, byrow = TRUE,
           dimnames = list(c("Juvenile", "Adult"), c("Juvenile", "Adult")))
  })
  
  # Reactive expression to perform population analysis
  populationAnalysis <- reactive({
    A <- projectionMatrix()
    lambda <- eigen(A)$values[1]
    net_reproductive_rate <- popbio::net.reproductive.rate(A)
    generation_time <- popbio::generation.time(A)
    stable_stage <- popbio::stable.stage(A)
    
    list(
      lambda = Re(lambda),
      net_reproductive_rate = net_reproductive_rate,
      generation_time = generation_time,
      stable_stage = stable_stage
    )
  })
  
  # Reactive expression to project population growth
  populationProjection <- reactive({
    A <- projectionMatrix()
    initial_population <- c(2, 2)
    projection <- popbio::pop.projection(A, initial_population, iterations = 20)
    # if (is.null(projection) || is.null(projection$pop.sizes)) {
    #   print("DEBUG: projection or projection$pop.sizes is NULL")
    # } else {
    #   print("DEBUG: projection$pop.sizes is not NULL")
    # }
    projection
  })
  
  # Output the projection matrix
  output$matrixTable <- renderTable({
    projectionMatrix()
  }, rownames = TRUE, colnames = TRUE)
  
  # Output population metrics
  output$metricsTable <- renderTable({
    analysis <- populationAnalysis()
    data.frame(
      Metric = c("Lambda (Growth Rate)", "Net Reproductive Rate", "Generation Time"),
      Value = c(analysis$lambda, analysis$net_reproductive_rate, analysis$generation_time)
    )
  })
  
  # Output stable stage distribution
  output$stableStageTable <- renderTable({
    stable_stage <- populationAnalysis()$stable_stage
    data.frame(
      Stage = c("Juvenile", "Adult"),
      Proportion = stable_stage
    )
  })
  
  output$life_cycle_plot <- renderGrViz({  # Ensure this name matches the UI
    mat <- projectionMatrix()  # Get the current matrix
    plot_life_cycle(mat)  # Generate the life cycle graph
  })
  
  # Output population projection plot
  output$projectionPlot <- renderPlot({
    projection <- populationProjection()
    if (!is.null(projection$pop.sizes)) {
      pop_data <- data.frame(
        Time = 1:length(projection$pop.sizes),
        Population = projection$pop.sizes
      )
      
      ggplot(pop_data, aes(x = Time, y = Population)) +
        geom_line(linewidth = 1.2, color = "blue") +
        labs(
          title = "Population Growth Projection",
          x = "Time Steps",
          y = "Total Population Size"
        ) +
        theme_minimal()
    } else {
      ggplot() +
        annotate("text", x = 1, y = 1, label = "Error: Unable to generate population projection", size = 6, color = "red") +
        theme_void()
    }
  })
}

shinyApp(ui = ui, server = server)