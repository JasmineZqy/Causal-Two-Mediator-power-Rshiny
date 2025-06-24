library(mvtnorm)
library(tidyverse)
library(glue)
library(shiny)
library(shinyBS, quietly = TRUE, warn.conflicts = FALSE)


# rsconnect::deployApp()
# Source UI and server
source("ui.R")
source("server.R")

# Source all R scripts inside the "function/" directory
function_files <- list.files("function", pattern = "\\.R$", full.names = TRUE)
sapply(function_files, source)

# Launch the app
shinyApp(ui = ui, server = server)
