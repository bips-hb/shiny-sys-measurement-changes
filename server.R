#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(shinyjs) #enable input dynamically
library(ggplot2)
library(dplyr)

# Preprocessing ----------------------------------------------------------------

## load data
res <- readRDS("data/res.Rds")

## define labels
# algorithm
algo_labels <- c(
  "alg_arima" = "ARIMA",
  "alg_flsa" = "FLSA",
  "alg_gam" = "GAM",
  "alg_lowess" = "LOWESS",
  "alg_moving_avg" = "MA",
  "alg_pelt" = "PELT",
  "alg_piecewise_reg" = "PR"
)
# from scico palette "batlow"
# sample(scico::scico(7, palette = "batlow"))
alg_col <- c(
  "#D29243",
  "#818231",
  "#FDAC9C",
  "#134D61",
  "#001959",
  "#3B6C55",
  "#F9CCF9"
)

# estimands
estimand_labels <- c(
  "range" = "Range", 
  "var" = "Variance", 
  "madm" = "Mean absolute deviation around the median", 
  "n_cpts" = "Number of change points"
)


# Server logic -----------------------------------------------------------------

function(input, output, session) {
  
  ## force boxplot for "combined" pattern
  plot_type <- reactive({
    if (input$pattern == "combined") {
      "boxplot"
    } else {
      input$plot_type
    }
  })
  
  ## update dmaxf choices & selections depending on pattern
  observeEvent(input$pattern, {
    if (input$pattern == "notrend" || input$pattern == "combined") {
      
      # hide dmaxf by clearing selection
      updateSelectizeInput(session, 
                           "dmaxf", 
                           choices = NULL, 
                           selected = NULL)
    } else {
      valid_choices <- c(0.25, 0.5, 1, 2)
      current_sel <- input$dmaxf
      
      # preserve selections if valid; otherwise, set defaults
      new_sel <- if (!is.null(current_sel) && length(current_sel) > 0) {
        intersect(current_sel, valid_choices)
      } else {
        c(2)
      }
      updateSelectizeInput(session, "dmaxf",
                           choices = valid_choices,
                           selected = new_sel)
    }
  }, ignoreNULL = FALSE) # run once at start as well
  
  ## reactive subset of the data based on user input
  filtered_data <- reactive({
    req(input$distribution, 
        input$pattern, 
        input$estimand)
    
    if (input$pattern == "combined"){
      data <- res %>%
        filter(
          distribution == input$distribution,
          algorithm %in% input$algorithms
        )
      if(input$combined_individual_snr){
        req(input$combined_snr)
        data <- data %>% filter(snr %in% input$combined_snr)
      }
      
    } else {
      req(input$snr)
      data <- res %>%
        filter(
          distribution == input$distribution,
          pattern == input$pattern,
          snr %in% input$snr
        )
      # dmaxf filtering only applies when pattern is not "nochange"
      if (input$pattern != "nochange") {
        req(input$dmaxf)
        data <- data %>% filter(dmaxf %in% input$dmaxf)
      }
    }
    
    ## algorithm filtering
    if (length(input$algorithms) > 0) {
      data <- data %>%
        filter(algorithm %in% input$algorithms) %>%
        mutate(
          algorithm = factor(algorithm, 
                             levels = names(algo_labels), 
                             labels = algo_labels)
        )
    } else {
      data <- data[0, ]  # return empty dataset
    }
    
    return(data)
  })
  
  ## plot
  output$simPlot <- renderPlot({
    data <- filtered_data()
    
    if (nrow(data) == 0) {
      plot.new()
      title("No data to display. Please adjust your selections.")
      return()
    }
    
    current_plot_type <- plot_type()
    
    # define names
    est_col <- paste0(input$estimand, "_est")
    dif_col <- paste0(input$estimand, "_dif")
    true_col <- input$estimand     
    estimand_label <- estimand_labels[input$estimand]
    
    # determine number of facets
    if (input$pattern == "combined"){
      num_snr <- 0
      num_dmaxf <- 0
    } else {
      num_snr <- length(unique(data$snr))
      num_dmaxf <- if ("dmaxf" %in% names(data)) length(unique(data$dmaxf)) else 0
    }
    
    if (current_plot_type == "loess") {
      p <- ggplot(data, 
                  aes(x = nobs,
                      y = .data[[est_col]],
                      color = algorithm,
                      fill=algorithm)) +
           geom_smooth(method = "loess", 
                       span = input$span, 
                       se = input$show_ci, 
                       alpha = 0.2) +
           geom_line(aes(y = .data[[true_col]]), 
                     linetype = "dashed", 
                     color = "black") +
           scale_color_manual(values = alg_col) + 
           scale_fill_manual(values = alg_col) +
           theme_bw() +
           labs(
              title = paste0("Estimated ", 
                             tolower(estimand_label), 
                             " over sample size"),
              subtitle = paste0("(Dashed line indicates the true ", 
                                tolower(estimand_label), 
                                ")"),
              x = "Sample size",
              y = paste("Estimated", 
                        tolower(estimand_label)),
              color = "Algorithm",
              fill = "Algorithm"
           )
    } else if (current_plot_type == "gam") {
      p <- ggplot(data, 
                  aes(x = nobs, 
                      y = .data[[est_col]], 
                      color = algorithm, 
                      fill = algorithm)) +
        geom_smooth(method = "gam", 
                    formula = y ~ s(x, bs = "cs"), 
                    se = input$show_ci, 
                    alpha = 0.2) +
        geom_line(aes(y = .data[[true_col]]), 
                  linetype = "dashed", 
                  color = "black") +
        scale_color_manual(values = alg_col) +
        scale_fill_manual(values = alg_col) +
        theme_bw() +
        labs(
          title = paste0("Estimated ", 
                         tolower(estimand_label),
                         " over sample size"),
          subtitle = paste0("(Dashed line indicates the true ", 
                            tolower(estimand_label), 
                            ")"),
          x = "Sample size",
          y = paste("Estimated", 
                    tolower(estimand_label)),
          color = "Algorithm",
          fill = "Algorithm"
        )
    } else if (current_plot_type == "boxplot") {
      p <- ggplot(data, 
                  aes(x = nobs_cat, 
                      y = .data[[dif_col]], 
                      color = algorithm)) +
           geom_boxplot(outlier.shape = 16, 
                        outlier.size = 0.5) +
           geom_hline(yintercept = 0, 
                      linetype = "dashed", 
                      color = "darkgray") +
           scale_color_manual(values = alg_col) + 
           theme_bw() +
           labs(
              title = paste0("Boxplot of ", 
                             tolower(estimand_label), 
                             " difference over sample size"),
              x = "Sample size category",
              y = paste(estimand_label, 
                        "(estimated - true)"),
              color = "Algorithm"
           ) +
           theme(axis.text.x = element_text(angle = 45, hjust = 1))
    }
    
    ## optionally add raw data points
    if (current_plot_type != "boxplot" & input$show_points) {
      p <- p + geom_point(alpha = 0.4, size = 1.5)
    }
    
    ## conditionally add facets
    if (input$pattern == "combined" && input$combined_individual_snr) {
      unique_snr <- unique(data$snr)
      if (length(unique_snr) > 1) {
        p <- p + facet_wrap(~ snr, 
                            labeller = label_both)
      }
    } else if (input$pattern != "combined"){
      if (num_snr > 1 && num_dmaxf > 1) {
        p <- p + facet_grid(snr ~ dmaxf, 
                            labeller = label_both)
      } else if (num_snr > 1) {
        p <- p + facet_grid(rows = vars(snr), 
                            labeller = label_both)
      } else if (num_dmaxf > 1) {
        p <- p + facet_grid(cols = vars(dmaxf), 
                            labeller = label_both)
      }
    }
    
    print(p)
    
  })

}
