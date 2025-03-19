# Bayesian Statistics Teaching App with Rocklets
library(shiny)
library(ggplot2)
library(DT)

ui <- fluidPage(
  titlePanel("Estadística Bayesiana"),
  
  sidebarLayout(
    # Left panel for navigation and controls
    sidebarPanel(
      sliderInput("n_values", 
                  "Valores discretos:", 
                  min = 2, 
                  max = 20, 
                  value = 11,
                  step = 1),
      
      numericInput("cant", "N = ", value = 5, min = 0, step = 1),
      
      numericInput("cant_color", "Y = ", value = 5, min = 0, step = 1),
      
      width = 2
    ),
    
    # Main panel with tabset
    mainPanel(
      tabsetPanel(
        id = "tabs",
        
        # First Tab - Prior (A & B)
        tabPanel("Prior", value = "tab1",
                 fluidRow(
                   column(4, 
                          h3("Creencia inicial"),
                          plotOutput("plotA", height = "300px")
                   ),
                 ),
                 fluidRow(
                   column(4,
                          h3("Creencia inicial"),
                          DTOutput("tableB"),
                          actionButton("resetPrior", "Hacer uniforme"),
                          actionButton("normalizePrior", "Normalizar")
                   ),
                 )
        ),
        
        # Second Tab - Likelihood (A, B, C & D)
        tabPanel("Likelihood", value = "tab2",
                 fluidRow(
                   column(4, 
                          h3("Creencia inicial"),
                          plotOutput("plotA2", height = "300px")
                   ),
                   column(4, 
                          h3("Verosimilitud"),
                          plotOutput("plotC", height = "300px")
                   )
                 ),
                 fluidRow(
                   column(4,
                          h3("Prior Values"),
                          DTOutput("tableB2")
                   ),
                   column(4,
                          h3("Likelihood Values"),
                          DTOutput("tableD")
                   )
                 )
        ),
        
        # Third Tab - Posterior (A, B, C, D, E & F)
        tabPanel("Posterior", value = "tab3",
                 fluidRow(
                   column(4, 
                          h3("Creencia inicial"),
                          plotOutput("plotA3", height = "300px")
                   ),
                   column(4, 
                          h3("Verosimilitud"),
                          plotOutput("plotC2", height = "300px")
                   ),
                   column(4, 
                          h3("Creencia final"),
                          plotOutput("plotE", height = "300px")
                   )
                 ),
                 fluidRow(
                   column(4,
                          h3("Creencia inicial"),
                          DTOutput("tableB3")
                   ),
                   column(4,
                          h3("Verosimilitud"),
                          DTOutput("tableD2")
                   ),
                   column(4,
                          h3("Creencia final"),
                          DTOutput("tableF")
                   )
                 )
        )
      ),
      width = 9
    )
  )
)

server <- function(input, output, session) {
  
  # Reactive value to hold user input for table B
  priorValues <- reactiveVal(NULL)
  
  # Function to normalize values
  normalize_values <- function(df) {
    df$prior <- as.numeric(df$prior)
    df$prior <- df$prior / sum(df$prior)
    df$prior <- round(df$prior, 3)
    return(df)
  }
  
  # Reactive function to generate theta values based on N
  theta_values <- reactive({
    N <- input$n_values
    df <- data.frame(
      theta = round(seq(0, 1, length.out = N), 3),
      prior = rep(1/N, N)
    )
    df$prior <- round(df$prior, 3)
    return(df)
  })
  
  
  # Initialize prior values when N changes
  observeEvent(input$n_values, {
    priorValues(theta_values())
  }, priority = 1)
  
  # Ensure priorValues is initialized
  observe({
    if (is.null(priorValues())) {
      priorValues(theta_values())
    }
  })
  
  # Reset prior values to uniform
  observeEvent(input$resetPrior, {
    priorValues(theta_values())
  })
  
  # Normalize prior values
  observeEvent(input$normalizePrior, {
    df <- priorValues()
    df <- normalize_values(df)
    priorValues(df)
  })
  
  # Table B (editable)
  output$tableB <- renderDT({
    df <- priorValues()
    df$theta <- sprintf("%.3f", df$theta)
    
    datatable(
      df,
      editable = list(target = "column", disable = list(columns = 1)),
      colnames = c("π", "p(π)"),
      options = list(dom = "t", ordering = FALSE, paging = FALSE)
    )
  })
  
  # Handle cell edits in table B
  observeEvent(input$tableB_cell_edit, {
    info <- input$tableB_cell_edit
    df <- priorValues()

    # El evento se dispara cuando se edita cualquier columna, 
    # aunque se haya marcado como no editable.
    # Pero como no son editables, los valores siempre vienen iguales, 
    # y por lo tanto no hay cambio.
    # Dicho eso, igualmente se evalúa el condicional 
    # para evitar actualizaciones innecesarias en `priorValues()`.
    column_idx <- info$col[[1]] # El valor es constante.
    if (column_idx == 2) {
      new_value <- suppressWarnings(as.numeric(info$value))
      if (all(!is.na(new_value))) {
        df[, column_idx] <- new_value
        # Save the updated data
        priorValues(df)
      } else {
        showNotification("Todos los valores deben ser numéricos.", type = "error")
      }
    }
  })
  
  # Non-editable versions of table B for other tabs
  output$tableB2 <- renderDT({
    df <- priorValues()
    df$theta <- sprintf("%.3f", df$theta)
    datatable(df, colnames = c("π", "p(π)"),
              options = list(dom = 't', ordering = FALSE, paging = FALSE))
  })
  
  output$tableB3 <- renderDT({
    df <- priorValues()
    df$theta <- sprintf("%.3f", df$theta)
    datatable(df, colnames = c("π", "p(π)"),
              options = list(dom = 't', ordering = FALSE, paging = FALSE))
  })
  


  
  # Likelihood data
  likelihood_values <- reactive({
    df <- priorValues()
    # Likelihood
    df$likelihood <- dbinom(input$cant_color, input$cant, df$theta)
    df$likelihood <- round(df$likelihood, 3)
    return(df)
  })
  
  # Table D - Likelihood
  output$tableD <- renderDT({
    df <- likelihood_values()[, c("theta", "likelihood")]
    df$theta <- sprintf("%.3f", df$theta)
    datatable(df, colnames = c("π", "p(y|π)"),
              options = list(dom = 't', ordering = FALSE, paging = FALSE))
  })
  
  output$tableD2 <- renderDT({
    df <- likelihood_values()[, c("theta", "likelihood")]
    df$theta <- sprintf("%.3f", df$theta)
    datatable(df, colnames = c("π", "p(y|π)"),
              options = list(dom = 't', ordering = FALSE, paging = FALSE))
  })
  
  # Posterior calculation
  posterior_values <- reactive({
    df <- likelihood_values()
    # Calculate posterior (prior * likelihood)
    df$posterior_ <- df$prior * df$likelihood
    df$posterior_ <- round(df$posterior_, 3)
    # Normalize
    df$posterior <- df$posterior_ / sum(df$posterior_)
    df$posterior <- round(df$posterior, 3)
    return(df)
  })
  
  # Table F - Posterior
  output$tableF <- renderDT({
    df <- posterior_values()[, c("theta", "posterior_", "posterior")]
    df$theta <- sprintf("%.3f", df$theta)
    datatable(df, colnames = c("π", "p(π) × p(y|π)", "p(π|y)"),
              options = list(dom = 't', ordering = FALSE, paging = FALSE))
  })
  
  # Plot A - Prior
  output$plotA <- renderPlot({
    df <- priorValues()
    ggplot(df) +
      geom_segment(aes(x = theta, xend = theta, y = 0, yend = prior), color = "#56CBF9", linewidth = 0.8) +
      geom_point(aes(x = theta, y = prior), color = "#56CBF9", size = 2.4) +
      labs(x = "π", y = "") +
      scale_x_continuous(breaks = priorValues()$theta) +
      scale_y_continuous(limits = c(0,1.05)) +
      ggtitle("Creencia inicial") +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5))
  })
  
  # Duplicated plots for other tabs
  output$plotA2 <- renderPlot({
    df <- priorValues()
    ggplot(df) +
      geom_segment(aes(x = theta, xend = theta, y = 0, yend = prior), color = "#56CBF9", linewidth = 0.8) +
      geom_point(aes(x = theta, y = prior), color = "#56CBF9", size = 2.4) +
      labs(x = "π", y = "") +
      scale_x_continuous(breaks = priorValues()$theta) +
      scale_y_continuous(limits = c(0,1.05)) +
      ggtitle("Creencia inicial") +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5))
  })
  
  output$plotA3 <- renderPlot({
    df <- priorValues()
    ggplot(df) +
      geom_segment(aes(x = theta, xend = theta, y = 0, yend = prior), color = "#56CBF9", linewidth = 0.8) +
      geom_point(aes(x = theta, y = prior), color = "#56CBF9", size = 2.4) +
      labs(x = "π", y = "") +
      scale_x_continuous(breaks = priorValues()$theta) +
      scale_y_continuous(limits = c(0,1.05)) +
      ggtitle("Creencia inicial") +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5))
  })
  
  # Plot C - Likelihood
  output$plotC <- renderPlot({
    df <- likelihood_values()
    ggplot(df, aes(x = theta, y = likelihood)) +
      geom_segment(aes(x = theta, xend = theta, y = 0, yend = likelihood), color = "#21dbbc", linewidth = 0.8) +
      geom_point(aes(x = theta, y = likelihood), color = "#21dbbc", size = 2.4) +
      labs(x = "π (pi)", y = "") +
      scale_x_continuous(breaks = priorValues()$theta) +
      scale_y_continuous(limits = c(0,1.05)) +
      ggtitle("Verosimilitud") +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5))
  })
  
  output$plotC2 <- renderPlot({
    df <- likelihood_values()
    ggplot(df, aes(x = theta, y = likelihood)) +
      geom_segment(aes(x = theta, xend = theta, y = 0, yend = likelihood), color = "#21dbbc", linewidth = 0.8) +
      geom_point(aes(x = theta, y = likelihood), color = "#21dbbc", size = 2.4) +
      labs(x = "π (pi)", y = "") +
      scale_x_continuous(breaks = priorValues()$theta) +
      scale_y_continuous(limits = c(0,1.05)) +
      ggtitle("Verosimilitud") +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5))
  })
  
  # Plot E - Posterior
  output$plotE <- renderPlot({
    df <- posterior_values()
    ggplot(df, aes(x = theta, y = posterior)) +
      geom_segment(aes(x = theta, xend = theta, y = 0, yend = posterior), color = "#FF729F", linewidth = 0.8) +
      geom_point(aes(x = theta, y = posterior), color = "#FF729F", size = 2.4) +
      labs(x = "π (pi)", y = "") +
      scale_x_continuous(breaks = priorValues()$theta) +
      scale_y_continuous(limits = c(0,1.05)) +
      ggtitle("Creencia final") +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5))
  })
  
}

shinyApp(ui, server)
