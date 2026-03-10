#####################################
## @Description: 
## @version: 
## @Author: Li Kangguo
## @Date: 2026-03-10 10:26:49
## @LastEditors: Li Kangguo
## @LastEditTime: 2026-03-10 10:26:50
#####################################

library(shiny)
library(bslib)
library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)
library(readr)
library(lubridate)
library(DT)
library(htmltools)
library(scales)

get_app_dir <- function() {
  frame_files <- vapply(
    sys.frames(),
    function(env) {
      file <- env$ofile
      if (is.null(file)) {
        return(NA_character_)
      }
      as.character(file)
    },
    character(1)
  )

  frame_files <- frame_files[!is.na(frame_files)]
  if (length(frame_files) > 0) {
    return(dirname(normalizePath(frame_files[[length(frame_files)]], winslash = "/", mustWork = TRUE)))
  }

  normalizePath(getwd(), winslash = "/", mustWork = TRUE)
}

app_dir <- get_app_dir()
app_env <- new.env(parent = environment())
app_env$app_dir <- app_dir

source_app_file <- function(file_name) {
  sys.source(file.path(app_dir, "R", file_name), envir = app_env)
}

source_app_file("helpers.R")
source_app_file("data.R")
source_app_file("plots.R")
source_app_file("components.R")
source_app_file("ui_theme.R")
source_app_file("ui_overview.R")
source_app_file("ui_recovery.R")
source_app_file("ui_timeseries.R")
source_app_file("ui_seasonal.R")
source_app_file("ui_methods.R")
source_app_file("ui.R")
source_app_file("server_overview.R")
source_app_file("server_recovery_helpers.R")
source_app_file("server_recovery.R")
source_app_file("server_seasonal_helpers.R")
source_app_file("server_seasonal.R")
source_app_file("server_timeseries.R")
source_app_file("server_methods.R")
source_app_file("server.R")

shinyApp(ui = app_env$ui, server = app_env$server)
