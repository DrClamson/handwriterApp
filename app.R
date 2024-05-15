# Shiny handwriter

library(bslib)
library(bsicons)
library(ggplot2)
library(shiny)
library(shinyBS)
library(shinycssloaders)
library(shinyjs)
library(shinyscreenshot)

print(paste0('working in: ', getwd()))
addResourcePath("images", "images")
source('shinyUI.R', local = TRUE)
source('shinyServer.R')

shinyApp(ui, server)
