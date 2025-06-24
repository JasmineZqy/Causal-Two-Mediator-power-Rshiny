library(shiny)
library(shinyBS)

# Core tidyverse packages: includes dplyr, ggplot2, purr.
library(tidyverse)

# Source all R scripts inside the "function/" directory
function_files <- list.files("function", pattern = "\\.R$", full.names = TRUE)
sapply(function_files, source)

# Additional required packages
library(glue)
library(cubature)
library(DT)
library(mvtnorm)     # For multivariate normal functions
library(future)      # For asynchronous processing
library(promises)    # For working with futures
library(parallel)    # For parallel computing


server <- function(input, output, session) {

    result <- reactiveVal(NULL)
    plot_result <- reactiveVal(NULL)

    output$run_label <- renderText({
        if (input$objective == "samplesize") "Estimate Sample Size" else "Calculate Power"
    })


    observeEvent(input$run, {
        result(NULL)
        plot_result(NULL)

        progress <- Progress$new(session, min = 0, max = 1)
        on.exit(progress$close())
        progress$set(message = "Running simulation...", value = 0.1)


        args <- list(
            seed_user = input$seed,
            num_x = input$num_x,
            treat.prop = input$treat.prop,
            treat.randomized = input$treat.randomized,
            em_corr = input$em_corr,
            M_binary = c(input$M1_binary, input$M2_binary),
            Y_binary = input$Y_binary,
            nboot = input$nboot,
            n.draws = input$n.draws,
            sig.level = input$conf / 100,
            nsims = input$nsims,
            mc.cores = input$mc.cores,
            n = input$n
        )

        if (input$coef_type == "standard") {
            args <- c(args, list(
                R2.ax = input$R2.ax,
                std.m_on_a = c(input$std.m1_on_a, input$std.m2_on_a),
                R2.mx = input$R2.mx,
                std.y_on_a = input$std.y_on_a,
                std.y_on_m = c(input$std.y_on_m1, input$std.y_on_m2),
                std.y_on_am_2way = c(input$std.y_on_am1_2way, input$std.y_on_am2_2way),
                std.y_on_m_2way = input$std.y_on_m_2way,
                std.y_on_am_3way = input$std.y_on_am_3way,
                R2.yx = input$R2.yx
            ))
        } else {
            args <- c(args, list(
                a_on_x = input$a_on_x,
                m_on_a = c(input$m1_on_a, input$m2_on_a),
                m_on_x = c(input$m1_on_x, input$m2_on_x),
                y_on_a = input$y_on_a,
                y_on_m = c(input$y_on_m1, input$y_on_m2),
                y_on_am_2way = c(input$y_on_am1_2way, input$y_on_am2_2way),
                y_on_m_2way = input$y_on_m_2way,
                y_on_am_3way = input$y_on_am_3way,
                y_on_x = input$y_on_x
            ))
        }

        if (input$objective == "power") {
            tryCatch({
                res <- do.call(runPower, args)
                result(res)
            }, error = function(e) {
                showNotification(paste("Error in runPower:", e$message), type = "error")
            })
        } else {
            args <- c(args, list(
                steps = input$steps,
                TarPow = input$TarPow,
                max_n = input$max_n,
                sig.adjust = input$sig.adjust,
                mediation = input$mediation,
                power = input$power
            ))
            tryCatch({
                res <- do.call(runSampsize, args)
                result(res$target_power_table)
                # result(res$power_df)
                plot_result(res$plot)
            }, error = function(e) {
                showNotification(paste("Error in runSampsize:", e$message), type = "error")
            })
        }

        progress$set(value = 1)
    })

    output$power_table <- DT::renderDataTable({
        req(result())
        DT::datatable(result(), options = list(scrollX = TRUE, scrollY = TRUE))
    })

    output$power_plot <- renderPlot({
        req(plot_result())
        print(plot_result())
    })

    output$download_table <- downloadHandler(
        filename = function() paste0("Power_Table_", Sys.Date(), ".csv"),
        content = function(file) {
            write.csv(result(), file, row.names = FALSE)
        }
    )

    output$download_plot <- downloadHandler(
        filename = function() paste0("Power_Curve_", Sys.Date(), ".png"),
        content = function(file) {
            ggsave(file, plot = plot_result(), width = 7, height = 5)
        }
    )
}



# Run Apps
# rsconnect::deployApp()
# Launch the app
# shinyApp(ui = ui, server = server)

