library(shiny)
library(shinyBS)
ui <- fluidPage(
    tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"),
        tags$title("Power Analysis for Two Mediators")
    ),

    fluidRow(
        column(12, align = "center",
               h3("Monte Carlo Power Analysis for Interventional Indirect Effects with Two Mediators"),
               p("Developed by Jasmine Zeng, Xiao Liu")
        )
    ),

    tabsetPanel(
        tabPanel("Run Simulation",

                 fluidRow(
                     column(12, align = "center",
                            radioButtons("objective", "Select Objective:",
                                         choices = c("Calculate Power" = "power", "Estimate Sample Size" = "samplesize"),
                                         selected = "power",
                                         inline = TRUE)
                     )
                 ),

                 fluidRow(
                     column(4,
                            bsCollapse(id = "collapse_panels", multiple = TRUE,

                                       bsCollapsePanel("Simulation Settings",
                                                       conditionalPanel(
                                                           condition = "input.objective == 'power'",
                                                           numericInput("n", "Sample Size", value = 50)
                                                       ),
                                                       conditionalPanel(
                                                           condition = "input.objective == 'samplesize'",
                                                           tagList(
                                                               numericInput("TarPow", "Target Power", value = 0.8),
                                                               numericInput("n", "Starting Sample Size", value = 50),
                                                               numericInput("steps", "Sample Size Step", value = 20),
                                                               numericInput("max_n", "Max Sample Size", value = 200),
                                                               checkboxGroupInput("sig.adjust", "Significance Adjustment Method:", choices = c("no_adjust", "bonferroni", "modified_bon1", "modified_bon2"), selected = c("no_adjust", "modified_bon1")),
                                                           checkboxGroupInput("mediation", "Mediation Effects:", choices = c("IIE_M1", "IIE_M2"), selected = c("IIE_M1", "IIE_M2")),
                                                       checkboxGroupInput("power", "Power Type:", choices = c("familywise", "per-test"), selected = c("per-test"))
                                                           )
                                                       ),
                                                       numericInput("nsims", "Number of Simulations", value = 5),
                                                       numericInput("n.draws", "Monte Carlo Replications", value = 1000),
                                                       numericInput("nboot", "Bootstrap Replications", value = 1000),
                                                       numericInput("seed", "Random Seed", value = 1234),
                                                       numericInput("conf", "Confidence Level (%)", value = 95, step = 1),
                                                       numericInput("mc.cores", "Number of Cores", value = 1, min = 1, max = parallel::detectCores() - 1)
                                       ),

                                       bsCollapsePanel("Treatment & Covariates",
                                                       numericInput("num_x", "Number of Covariates (X)", value = 2, min = 0, max = 10),
                                                       numericInput("treat.prop", "Proportion of Treatment (A)", value = 0.3, min = 0, max = 1, step = 0.01),
                                                       checkboxInput("treat.randomized", "Randomized Treatment Assignment", value = FALSE),
                                                       checkboxInput("M1_binary", "Binary Mediator 1", value = TRUE),
                                                       checkboxInput("M2_binary", "Binary Mediator 2", value = FALSE),
                                                       checkboxInput("Y_binary", "Binary Outcome", value = FALSE),
                                                       selectInput("coef_type", "Coefficient Input Type:",
                                                                   choices = c("Standardized Coefficient" = "standard", "Raw Coefficient" = "raw"),
                                                                   selected = "raw")
                                       ),

                                       bsCollapsePanel("Exposure Model Parameters",
                                                       conditionalPanel(
                                                           condition = "input.coef_type == 'raw'",
                                                           numericInput("a_on_x", "Raw Coefficient: X → A", value = sqrt(0.03))
                                                       ),
                                                       conditionalPanel(
                                                           condition = "input.coef_type == 'standard'",
                                                           numericInput("R2.ax", "R-squared: X → A", value = 0.1)
                                                       )
                                       ),

                                       bsCollapsePanel("Mediators Model Parameters",
                                                       conditionalPanel(
                                                           condition = "input.coef_type == 'raw'",
                                                           tagList(
                                                               numericInput("m1_on_a", "Raw Coefficient: A → M₁", value = 0.5),
                                                               numericInput("m1_on_x", "Raw Coefficient: X → M₁", value = sqrt(0.13)),
                                                               numericInput("m2_on_a", "Raw Coefficient: A → M₂", value = 0.5),
                                                               numericInput("m2_on_x", "Raw Coefficient: X → M₂", value = sqrt(0.13))
                                                           )
                                                       ),
                                                       conditionalPanel(
                                                           condition = "input.coef_type == 'standard'",
                                                           tagList(
                                                               numericInput("R2.mx", "R-squared: X → M", value = 0.1),
                                                               numericInput("std.m1_on_a", "Standardized Coefficient: A → M₁", value = 0.36),
                                                               numericInput("std.m2_on_a", "Standardized Coefficient: A → M₂", value = 0.36)
                                                           )
                                                       ),
                                                       numericInput("em_corr", "Residual Corr(M₁, M₂)", value = 0.03)
                                       ),

                                       bsCollapsePanel("Outcome Model Parameters",
                                                       conditionalPanel(
                                                           condition = "input.coef_type == 'raw'",
                                                           tagList(
                                                               numericInput("y_on_x", "Raw Coefficient: X → Y", value = sqrt(0.02)),
                                                               numericInput("y_on_a", "Raw Coefficient: A → Y", value = 0.01),
                                                               numericInput("y_on_m1", "Raw Coefficient: M₁ → Y", value = 0.5),
                                                               numericInput("y_on_m2", "Raw Coefficient: M₂ → Y", value = 0.5),
                                                               numericInput("y_on_am1_2way", "Interaction: A × M₁ → Y", value = 0.1),
                                                               numericInput("y_on_am2_2way", "Interaction: A × M₂ → Y", value = 0.1),
                                                               numericInput("y_on_m_2way", "Interaction: M₁ × M₂ → Y", value = 0.1),
                                                               numericInput("y_on_am_3way", "Interaction: A × M₁ × M₂ → Y", value = 0.03)
                                                           )
                                                       ),
                                                       conditionalPanel(
                                                           condition = "input.coef_type == 'standard'",
                                                           tagList(
                                                               numericInput("R2.yx", "R-squared: X → Y", value = 0.1),
                                                               numericInput("std.y_on_a", "Standardized Coefficient: A → Y", value = 0.14),
                                                               numericInput("std.y_on_m1", "Standardized Coefficient: M₁ → Y", value = 0.04),
                                                               numericInput("std.y_on_m2", "Standardized Coefficient: M₂ → Y", value = 0.04),
                                                               numericInput("std.y_on_am1_2way", "Standardized Interaction: A × M₁ → Y", value = 0),
                                                               numericInput("std.y_on_am2_2way", "Standardized Interaction: A × M₂ → Y", value = 0.36),
                                                               numericInput("std.y_on_m_2way", "Standardized Interaction: M₁ × M₂ → Y", value = 0),
                                                               numericInput("std.y_on_am_3way", "Standardized Interaction: A × M₁ × M₂ → Y", value = 0.36)
                                                           )
                                                       )
                                       )
                            )
                     ),

                     column(8,
                            wellPanel(
                                actionButton("run", label = textOutput("run_label"), class = "btn-primary", width = "100%"),
                                br(), br(),
                                DT::dataTableOutput("power_table"),
                                br(),
                                conditionalPanel(
                                    condition = "input.objective == 'samplesize'",
                                    plotOutput("power_plot", height = "500px"),
                                    br(),
                                    downloadButton("download_plot", "Download Power Curve Plot")
                                ),
                                downloadButton("download_table", "Download Results Table"),
                            )
                     )
                 )
        ),

        tabPanel("Help",
                 wellPanel(
                     tags$h4("Instructions"),

                     tags$ol(
                         tags$li("Set simulation and model parameters using the collapsible sections."),
                         tags$li("Choose input method: Standardized Coefficients or R-squared."),
                         tags$li("Select your objective: calculate power or estimate required sample size."),
                         tags$li("Click 'Run' to perform the simulation and view/download results.")
                     ),

                     tags$br(),

                     tags$p("This Shiny app was designed for conducting power analysis and sample size estimation in two-mediator causal inference models.
                   It supports both binary under different multiple testing correction methods."),

                     tags$br(),

                     tags$img(src = "DAG.png", height = "400px",
                              style = "display: block; margin-left: auto; margin-right: auto;"),

                     tags$figcaption("Figure: Causal diagram with two correlated mediators.",
                                     style = "text-align: center; font-style: italic;"),

                     tags$br(),

                     tags$h5("References"),

                     tags$p(
                         "Schoemann, A. M., Boulton, A. J., & Short, S. D. (2017). ",
                         tags$i("Determining Power and Sample Size for Simple and Complex Mediation Models."),
                         " Social Psychological and Personality Science, 8(4), 379–386. ",
                         tags$a(href = "https://doi.org/10.1177/1948550617715068",
                                target = "_blank",
                                "https://doi.org/10.1177/1948550617715068"),
                         " (Original work published 2017)."
                     )
                 )
        )
    )
)


