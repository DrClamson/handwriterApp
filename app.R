# Shiny handwriter

library(magick)
library(shiny)
library(shinyjs)
library(shinybusy)
library(shinyBS)
library(shinyFiles)
library(DT)
library(stringr)
library(dplyr)
library(ggplot2)

print(paste0('working in: ', getwd()))

source('shinyUI.R', local = TRUE)
source('shinyServer.R')

shinyApp(ui, server)
