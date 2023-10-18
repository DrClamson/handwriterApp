# Shiny handwriter

library(dplyr)
library(DT)
library(ggplot2)
library(handwriter)
library(magick)
library(shiny)
library(shinyBS)
library(shinybusy)
library(shinyFiles)
library(shinyjs)
library(stringr)

print(paste0('working in: ', getwd()))

source('shinyUI.R', local = TRUE)
source('shinyServer.R')

shinyApp(ui, server)
