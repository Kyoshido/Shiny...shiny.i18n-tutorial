###############################################################################
###############################################################################
###############################################################################

# Packages --------------------------------------------------------------------
library(shiny)
library(shiny.i18n)

# Translations ----------------------------------------------------------------
i18n <- Translator$new(translation_csvs_path = "translations/",
                       separator_csv = ";") # translation file
i18n$set_translation_language("en")

# -----------------------------------------------------------------------------

###############################################################################

ui <- fluidPage(
  
  # Translations
  shiny.i18n::usei18n(i18n),
  div(style = "float: right;",
      selectInput('selected_language',
                  i18n$t("Change language"),
                  choices = i18n$get_languages(),
                  selected = i18n$get_key_translation())
  ),
  
  #############################################################################
  # UI ------------------------------------------------------------------------
  #############################################################################
  
  # Application title
  titlePanel(i18n$t("Hello Shiny!"), windowTitle = NULL),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      sliderInput("bins",
                  i18n$t("Number of bins:"), # you use i18n object as always
                  min = 1,
                  max = 50,
                  value = 30),
      uiOutput("selectInput_sex")
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("distPlot"),
      p(i18n$t("This is description of the plot.")),
      textOutput("text_sex")
    )
  )
  
  #############################################################################
  # ---------------------------------------------------------------------------
  #############################################################################
  
) # End of ui()

###############################################################################
###############################################################################
###############################################################################