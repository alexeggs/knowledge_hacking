load_app_libraries <- function() {
  
  ## list of packages to load
  list.of.packages <- c("shiny", "shinyjs",
                        "wordcloud2", "tm",
                        "SnowballC", "dplyr",
                        "httr", "jsonlite",
                        "shinysky",
                        "udpipe",
                        "googlesheets",
                        "remotes",
                        "shinydashboard",
                        "rclipboard",
                        "rsconnect")
  
  ## check if packages already exist
  new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
  
  ## install new packages
  if(length(new.packages)) install.packages(new.packages)
  
  ## load packages
  library(shiny)
  library(shinyjs)
  library(shinydashboard)
  library(wordcloud)
  library(wordcloud2)
  library(tm)
  library(SnowballC)
  library(dplyr)
  library(httr)
  library(jsonlite)
  library(udpipe)
  library(googlesheets)
  library(remotes)
  library(rclipboard)
  library(rsconnect)
  
  ## list of packages to load
  list.of.packages <- c("emitanaka/shinycustomloader")

  ## check if packages already exist
  new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
  
  ## install new packages
  if(length(new.packages)) devtools::install_github(new.packages)
  
  library(shinycustomloader)
  
  source("src/tools/shinyapps_creds.R")
  
}