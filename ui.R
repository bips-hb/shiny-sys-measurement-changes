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
    
    # distribution
    div(
      style = "display: flex; align-items: center; gap: 6px;",
      tags$label("Distribution"),
      
      popover(
        trigger = actionLink(
          "distribution_info",
          label = "",
          icon = icon("info-circle"),
          class = "btn-info",
          style = "color: #001959;"
        ),
        HTML(
          "Distribution used to simulate the measurements."
        ),
        title = "Distribution",
        placement = "right",
        options = list(trigger = "focus")
      )
    ),
    
    selectInput(
      "distribution",
      label = NULL,
      choices = c(
        "Normal" = "norm",
        "Log-normal" = "lognorm"
      )
    ),
    
    # pattern
    div(
      style = "display: flex; align-items: center; gap: 6px;",
      tags$label("Systematic change pattern"),
      
      popover(
        trigger = actionLink(
          "pattern_info",
          label = "",
          icon = icon("info-circle"),
          class = "btn-info",
          style = "color: #001959;"
        ),
        HTML(
          "Systamtic change pattern used to simulate the measurements."
        ),
        title = "Systematic change pattern",
        placement = "right",
        options = list(trigger = "focus")
      )
    ),
    
    selectInput(
      "pattern",
      label = NULL,
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
      
      div(
        class = "form-check d-flex align-items-center gap-2",
        
        tags$input(
          type = "checkbox",
          class = "form-check-input",
          id = "combined_individual_snr"
        ),
        
        tags$label(
          class = "form-check-label",
          `for` = "combined_individual_snr",
          "Stratify results by signal-to-noise ratio"
        ),
        
        popover(
          trigger = actionLink(
            "combined_snr_info",
            label = "",
            icon = icon("info-circle"),
            class = "info-icon",
            style = "color: #001959;"
          ),
          "Show results separately for each signal-to-noise ratio (SNR) selected below.",
          title = "Stratification by signal-to-noise ratio",
          placement = "right",
          options = list(trigger = "focus")
        )
      )
    ),
    
    # snr
    conditionalPanel(
      condition = "input.pattern == 'combined' && input.combined_individual_snr == true",

      div(
        style = "display: flex; align-items: center; gap: 6px; margin-bottom: 4px;",
        
        tags$label(
          "Signal-to-noise ratio (SNR)",
          style = "margin-bottom: 0; font-weight: normal;"
        ),
        
        popover(
          trigger = actionLink(
            "combined_snr_info",
            label = "",
            icon = icon("info-circle"),
            class = "info-icon",
            style = "color: #001959;"
          ),
          "SNR used to simulate the measurements. Multiple ratios can be selected.
          Each selected SNR is shown in an individual subplot.",
          title = "Signal-to-noise ratio (SNR)",
          placement = "right",
          options = list(trigger = "focus")
        )
      ),
      
      selectizeInput(
        "combined_snr",
        label = NULL,
        choices = c(0.33, 0.66, 1, 2),
        selected = 2,
        multiple = TRUE,
        options = list(plugins = list("remove_button"))
      )
    ),
    
    conditionalPanel(
      condition = "input.pattern != 'combined'",
      
      div(
        style = "display: flex; align-items: center; gap: 6px; margin-bottom: 4px;",
        
        tags$label(
          "Signal-to-noise ratio (SNR)",
          style = "margin-bottom: 0; font-weight: normal;"
        ),
        
        popover(
          trigger = actionLink(
            "snr_info",
            label = "",
            icon = icon("info-circle"),
            class = "info-icon",
            style = "color: #001959;"
          ),
          "SNR used to simulate the measurements. Multiple ratios can be selected. 
          Each selected SNR is shown in an individual subplot.",
          title = "Signal-to-noise ratio (SNR)",
          placement = "right",
          options = list(trigger = "focus")
        )
      ),
      
      selectizeInput(
        "snr",
        label = NULL,
        choices = c(0.33, 0.66, 1, 2),
        selected = 2,
        multiple = TRUE,
        options = list(plugins = list("remove_button"))
      )
    ),
    
    # dmaxf
    conditionalPanel(
      condition = "input.pattern != 'combined' && input.pattern != 'nochange'",
      
      div(
        style = "display: flex; align-items: center; gap: 6px; margin-bottom: 4px;",
        
        tags$label(
          "Factor for the maximum magnitude of systematic change (dmaxf)",
          style = "margin-bottom: 0; font-weight: normal;"
        ),
        
        popover(
          trigger = actionLink(
            "dmaxf_info",
            label = "",
            icon = icon("info-circle"),
            class = "info-icon",
            style = "color: #001959;"
          ),
          HTML("Factor dmaxf used to simulate the measurements. The factor was multiplied
          with the measurement distribution's standard deviation (on the normal or
          log scale) to obtain the maximum magnitude of systematic change 
          <i>d<sub>max</sub></i>. 
          Multiple factors can be selected. Each selected dmaxf is shown in an 
          individual subplot."),
          title = "Factor for the maximum magnitude of systematic change (dmaxf)",
          placement = "right",
          options = list(trigger = "focus")
        )
      ),
      
      selectizeInput(
        "dmaxf",
        label = NULL,
        choices = c(0.25, 0.5, 1, 2),
        multiple = TRUE,
        options = list(plugins = list("remove_button"))
      )
    ),
    
    hr(),
    
    h5("Estimand and Method"),
    
    # estimand
    div(
      style = "display: flex; align-items: center; gap: 6px;",
      tags$label("Estimand"),
      
      popover(
        trigger = actionLink(
          "estimand_info",
          label = "",
          icon = icon("info-circle"),
          class = "btn-info",
          style = "color: #001959;"
        ),
        HTML(
          "Target estimand to quantify systematic changes in measurements."
        ),
        title = "Estimand",
        placement = "right",
        options = list(trigger = "focus")
      )
    ),
    
    selectInput(
      "estimand",
      label = NULL,
      choices = c(        
        "Range" = "range",
        "Variance" = "var",
        "Mean absolute deviation around the median" = "madm",
        "Number of change points" = "n_cpts"
      )
    ),
    
    # method
    div(
      style = "display: flex; align-items: center; gap: 6px;",
      tags$label("Method"),
      
      popover(
        trigger = actionLink(
          "method_info",
          label = "",
          icon = icon("info-circle"),
          class = "btn-info",
          style = "color: #001959;"
        ),
        HTML(
          "Statistical method to quantify systmatic changes in measurements: 
          ARIMA (autoregressive integrated moving average), FLSA (fused lasso 
          signal approximator), GAM (Generalized additive model), LOWESS (locally
          weighted scatterplot smoothing), MA (moving average), PELT (pruned exact
          linear time), PR (piecewise regression). Multiple methods can be selected. 
          The selected methods are shown in the same plot using different colors."
        ),
        title = "Method",
        placement = "right",
        options = list(trigger = "focus")
      )
    ),
    
    selectizeInput(
      "algorithms",
      label = NULL,
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
    
    # plot type
    conditionalPanel(
      condition = "input.pattern != 'combined'",
      
      div(
        style = "display: flex; align-items: center; gap: 6px; margin-bottom: 4px;",
        
        tags$label(
          "Plot type",
          style = "margin-bottom: 0; font-weight: normal;"
        ),
        
        popover(
          trigger = actionLink(
            "plot_type_info",
            label = "",
            icon = icon("info-circle"),
            class = "info-icon",
            style = "color: #001959;"
          ),
          "Choose whether to display estimated values over the sample size using LOESS smoothing or 
          differences between true and estimated values over the five sample size 
          categories (30-50, 51-100, 101-200, 201-500, 501-1000) using boxplots.",
          title = "Plot type",
          placement = "right",
          options = list(trigger = "focus")
        )
      ),
      
      radioButtons(
        "plot_type",
        label = NULL,
        choices = c(
          "Estimated values (LOESS)" = "loess",
          "Differences true vs. estimated (Boxplot)" = "boxplot"
        )
      )
    ),
    
    # Additional loess options
    conditionalPanel(
      condition = "input.pattern != 'combined' && input.plot_type == 'loess'",
      
      # span for loess
      div(
        style = "display: flex; align-items: center; gap: 6px; margin-bottom: 4px;",
        tags$label("Span for LOESS", style = "margin-bottom: 0; font-weight: normal;"),
        popover(
          trigger = actionLink(
            "span_info",
            label = "",
            icon = icon("info-circle"),
            class = "info-icon",
            style = "color: #001959;"
          ),
          "Controls the smoothing parameter for the LOESS fit. Smaller values 
          follow the data more closely whereas larger values produce smoother curves.",
          title = "Span for LOESS",
          placement = "right",
          options = list(trigger = "focus")
        )
      ),
      sliderInput(
        "span",
        label = NULL,
        min = 0.05,
        max = 1.5,
        value = 0.75,
        step = 0.05
      ),
      
      # show confidence intervals
      div(
        style = "display: flex; align-items: center; gap: 6px; margin-bottom: 2px;",
        
        tags$input(
          type = "checkbox",
          id = "show_ci",
          style = "margin: 0; width: auto; height: auto;"
        ),
        
        tags$label(
          "Show confidence intervals",
          `for` = "show_ci",
          style = "margin-bottom: 0; font-weight: normal; flex: 1;"
        ),
        
        popover(
          trigger = actionLink(
            "show_ci_info",
            label = "",
            icon = icon("info-circle"),
            class = "info-icon",
            style = "color: #001959;"
          ),
          "Additionally display the confidence intervals around the LOESS fit. 
          Helpful to see the uncertainty in the estimated curve.",
          title = "Confidence intervals",
          placement = "right",
          options = list(trigger = "focus")
        )
      ),
      
      # show raw points
      div(
        style = "display: flex; align-items: center; gap: 6px; margin-bottom: 2px;",
        
        tags$input(
          type = "checkbox",
          id = "show_points",
          style = "margin: 0; width: auto; height: auto;"
        ),
        
        tags$label(
          "Show raw data",
          `for` = "show_points",
          style = "margin-bottom: 0; font-weight: normal; flex: 1;"
        ),
        
        popover(
          trigger = actionLink(
            "show_points_info",
            label = "",
            icon = icon("info-circle"),
            class = "info-icon",
            style = "color: #001959;"
          ),
          "Additionally display the raw data points.",
          title = "Raw data",
          placement = "right",
          options = list(trigger = "focus")
        )
      )
    )
  ),
  
  card(
    card_header("Results"),
    plotOutput("simPlot", height = "600px"),
    div(
      style = "margin-top: 6px; text-align: right;",
      downloadButton("download_plot", "Download figure", class = "btn-sm btn-primary")
    )
  ),
  
  accordion(
    accordion_panel(
      "About",
      HTML(
        '<p>This application allows you to interactively visualise the results of the 
      simulation study from the paper <i>"Assessing systematic changes in measurements: 
      A simulation study"</i>. The study compared different statistical methods for quantifying systematic changes 
      in measurements across various settings.</p>
      <p>Use the controls on the left to select the desired estimand, simulation setting, 
      and statistical methods for the visualisation. The resulting figure can be 
      downloaded by clicking on the download button below the figure.</p>'
      )
    )
  )
)