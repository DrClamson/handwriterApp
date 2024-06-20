## Load Libraries
library(shiny)
library(shinyjs)
library(shinyBS)
library(ggplot2)
library(bslib)
library(bsicons)
library(shinycssloaders)
library(randomForest)
library(dplyr)
library(DT)

## Load handwriter
library(handwriter)

## Config
options(shiny.maxRequestSize = 30*1024^2)
addResourcePath("images", "images")


# SERVER ------------------------------------------------------------------
server <- function(input, output, session) {
  
  innerServer('inner1')
  
}