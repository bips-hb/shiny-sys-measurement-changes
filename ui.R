#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(shinyjs)

# Define UI for application that draws a histogram
fluidPage(
    useShinyjs(),

    # Application title
    titlePanel("Results of the simulation study"),

    # Sidebar with an input for the required parameters
    sidebarLayout(
        sidebarPanel(
          selectInput("distribution", 
                      "Distribution:", 
                      c("Normal" = "norm", 
                        "Log-normal" = "lognorm")),
          selectInput("estimand", 
                      "Estimand:", 
                      c("Range" = "range", 
                        "Variance" = "var", 
                        "Mean absolute deviation around the median" = "madm", 
                        "Number of change points" = "n_cpts")),
          selectInput("pattern", 
                      "Systematic error pattern:", 
                      c("No change" = "nochange", 
                        "Jump" = "jump", 
                        "Linear" = "linear", 
                        "Quadratic" = "quadratic", 
                        "Recurrent" = "recurrent", 
                        "-All patterns combined-" = "combined")),
          conditionalPanel(
            condition = "input.pattern == 'combined'",
            checkboxInput("combined_individual_snr", 
                          "Display by signal-to-noise ratio (SNR)", 
                          value = FALSE)
          ),
          conditionalPanel(
            condition = "input.pattern == 'combined' && input.combined_individual_snr == true",
            selectizeInput(
              inputId = "combined_snr",
              label = "Signal-to-noise ratio (SNR):",
              choices = c(0.33, 0.66, 1, 2),
              selected = c(2),
              multiple = TRUE,
              options = list(plugins = list("remove_button"))
            )
          ),
          conditionalPanel(
            condition = "input.pattern != 'combined' & input.pattern != 'notrend'",
            selectizeInput(
              inputId = "dmaxf",
              label = "Factor for the maximum magnitude of systematic change (dmaxf):",
              choices = c(0.25, 0.5, 1, 2),
              multiple = TRUE,
              options = list(plugins = list('remove_button'))
            )
          ),
          conditionalPanel(
            condition = "input.pattern != 'combined'", 
            selectizeInput(
              inputId = "snr",
              label = "Signal-to-noise ratio (SNR):",
              choices = c(0.33, 0.66, 1, 2),
              selected = c(2),
              multiple = TRUE,
              options = list(plugins = list('remove_button'))
            ),
            radioButtons("plot_type", 
                         "Plot type:", 
                         c("Loess" = "loess",
                           "Boxplot" = "boxplot"))
          ),
          conditionalPanel(
            condition = "input.pattern != 'combined' && input.plot_type != 'boxplot'",
            checkboxInput("show_ci", 
                          "Show confidence intervals", 
                          value = FALSE),
            checkboxInput("show_points", 
                          "Show raw data", 
                          value = FALSE),
          ),
          conditionalPanel(
            condition = "input.pattern != 'combined' && input.plot_type == 'loess'",
            sliderInput(
              inputId = "span",
              label = "Span:",
              min = 0.05,
              max = 1.5,
              value = 0.75,
              step = 0.05)
            ),
          selectizeInput("algorithms", 
                         "Algorithms:", 
                         c("ARIMA" = "alg_arima",
                           "FLSA" = "alg_flsa",
                           "GAM" = "alg_gam",
                           "LOWESS" = "alg_lowess",
                           "MA" = "alg_moving_avg",
                           "PELT" = "alg_pelt",
                           "PR" = "alg_piecewise_reg"),
            selected = c("alg_arima", 
                         "alg_lowess_aicc"),  # optional: pre-select a few
            multiple = TRUE,
            options = list(plugins = list('remove_button'))
          )
        ),

    # Show a plot of the generated distribution
        mainPanel(
            plotOutput("simPlot")
        )
    )
)
