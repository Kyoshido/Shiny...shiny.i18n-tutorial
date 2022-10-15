# ClinRTools modularized demo
# 2021-03-06 by Ed Parsadanyan
#
# Globals: libraries, modules etc.
# 
# TODO:
# + translations functionality via i18n package
# + Intro page
# - Find hosting solution


# Module dependencies are stored within Server function of the module. No need to attach libraries here
library(shiny)
# library(shiny.router)
library(shinymeta)
library(shinyjs)
library(shiny.i18n)
# library(shinydashboard)


default_UI_lang <- "en"


# Add common utilities
source("modules/common_utils.R")


# Add individual modules here
source("modules/BE_samplesize.R")
source("modules/Randomize.R")
