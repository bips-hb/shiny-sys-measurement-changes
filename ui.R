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
library(bslib)
library(shinyBS)

ui <- page_sidebar(
  theme = bs_theme(
    version = 5,
    bootswatch = "flatly",
    base_font = font_google("Roboto"),
    heading_font = font_google("Roboto Slab"),
    primary = "#001959"
  ),
  
  useShinyjs(),
  
  title = div(
    h2("Systematic changes in measurements"),
    h4(HTML('Results from the paper <i>"Assessing systematic changes in measurements: A simulation study"</i>'))
  ),
  
  sidebar = sidebar(
    width = 350,
    
    h5("Simulation setting"),
    
    selectInput(
      "distribution",
      "Data distribution",
      choices = c(
        "Normal" = "norm",
        "Log-normal" = "lognorm"
      )
    ),
    
    selectInput(
      "pattern",
      "Systematic change pattern",
      choices = c(
        "No change" = "nochange",
        "Jump" = "jump",
        "Linear trend" = "linear",
        "Quadratic trend" = "quadratic",
        "Recurrent pattern" = "recurrent",
        "All patterns combined" = "combined"
      )
    ),
    
    conditionalPanel(
      condition = "input.pattern == 'combined'",
      checkboxInput(
        "combined_individual_snr",
        "Stratify combined results by signal-to-noise ratio (SNR)",
        value = FALSE
      )
    ),
    
    conditionalPanel(
      condition = "input.pattern == 'combined' && input.combined_individual_snr == true",
      selectizeInput(
        "combined_snr",
        "Signal-to-noise ratio (SNR)",
        choices = c(0.33, 0.66, 1, 2),
        selected = 2,
        multiple = TRUE,
        options = list(plugins = list("remove_button"))
      )
    ),
    
    conditionalPanel(
      condition = "input.pattern != 'combined'",
      selectizeInput(
        "snr",
        "Signal-to-noise ratio (SNR)",
        choices = c(0.33, 0.66, 1, 2),
        selected = 2,
        multiple = TRUE,
        options = list(plugins = list("remove_button"))
      )
    ),
    
    conditionalPanel(
      condition = "input.pattern != 'combined' && input.pattern != 'nochange'",
      selectizeInput(
        "dmaxf",
        "Factor for the maximum magnitude of systematic change (dmaxf)",
        choices = c(0.25, 0.5, 1, 2),
        multiple = TRUE,
        options = list(plugins = list("remove_button"))
      )
    ),
    
    hr(),
    
    h5("Estimand and Methods"),
    
    selectInput(
      "estimand",
      "Estimand",
      choices = c(
        "Range" = "range",
        "Variance" = "var",
        "Mean absolute deviation around the median" = "madm",
        "Number of change points" = "n_cpts"
      )),
    
    selectizeInput(
      "algorithms",
      "Methods",
      choices = c(
        "ARIMA" = "alg_arima",
        "FLSA" = "alg_flsa",
        "GAM" = "alg_gam",
        "LOWESS" = "alg_lowess",
        "MA" = "alg_moving_avg",
        "PELT" = "alg_pelt",
        "PR" = "alg_piecewise_reg"
      ),
      selected = c("alg_arima", "alg_lowess"),
      multiple = TRUE,
      options = list(plugins = list("remove_button"))
    ),
    
    hr(),
    
    h5("Visualisation"),
    
    conditionalPanel(
      condition = "input.pattern != 'combined'",
      radioButtons(
        "plot_type",
        "Plot type",
        choices = c(
          "Estimated values (LOESS)" = "loess",
          "Differences true vs. estimated (Boxplot)" = "boxplot"
        )
      )
    ),
    
    conditionalPanel(
      condition = "input.pattern != 'combined' && input.plot_type == 'loess'",
      sliderInput(
        "span",
        "Span for LOESS",
        min = 0.05,
        max = 1.5,
        value = 0.75,
        step = 0.05
      ),
      checkboxInput("show_ci", "Show confidence intervals", FALSE),
      checkboxInput("show_points", "Show raw simulation results", FALSE)
    )
  ),
  
  card(
    card_header("Results"),
    plotOutput("simPlot", height = "600px")
    ),
  
  accordion(
    accordion_panel(
      "About",
      HTML(
        '<p>This application allows you to interactively visualise the results of the 
      simulation study from the paper <i>"Assessing systematic changes in measurements: 
      A simulation study"</i>.</p>
      
      <p>The study compared different statistical methods for quantifying systematic changes 
      in measurements across various settings.</p>
      
      <p>Use the controls on the left to select the desired estimand, simulation setting, 
      and statistical methods for the visualisation.</p>'
      )
    )
  )
)