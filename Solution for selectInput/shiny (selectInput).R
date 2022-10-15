###############################################################################
###############################################################################
###############################################################################

# Packages --------------------------------------------------------------------
library(shiny)
library(shiny.i18n)
library(shinyWidgets)

# Translations ----------------------------------------------------------------
i18n <- Translator$new(translation_csvs_path = "translations - kopie/",
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
  ), br(),
  
  #############################################################################
  # UI ------------------------------------------------------------------------
  #############################################################################
  
  tabsetPanel(
    id = "tabs",
    type = "tabs",
    tabPanel(
      "Browser side translation",
      value = 1,
      pickerInput(
        inputId = "browser_translation",
        label = "", 
        choices = c("pdregion"),
        width = "500px",
        choicesOpt = list(
          content = c(as.character(i18n$t("Population density of the region")))
        )
      )
    ),
    tabPanel(
      "Server-side translation",
      value = 2,
      uiOutput("tab_content"),
      textOutput("selected_var")
    )
  )
  
  #############################################################################
  # ---------------------------------------------------------------------------
  #############################################################################
  
) # End of ui()

###############################################################################

server <- function(input, output, session) {
  
  observeEvent(input$selected_language, {   # Update language in session
    shiny.i18n::update_lang(session, input$selected_language)
  })
  
  #############################################################################
  # Server --------------------------------------------------------------------
  #############################################################################
  
  output$tab_content <- renderUI({
    selectInput(
      inputId = "server_translation",
      label = "",
      width = "500px",
      choices = setNames(c('pdregion',"pdregion2"), 
                         c(i18n$t("Population density of the region"),
                           i18n$t("Population density of the city")
                         )
      )
      
    )
  })
  
  output$selected_var <- renderText({ 
    paste("You have selected", input$server_translation)
  })
  
  #############################################################################
  # ---------------------------------------------------------------------------
  #############################################################################
  
} # End of server()

###############################################################################

# Run the application 

shinyApp(ui = ui, 
         server = server)

###############################################################################
###############################################################################
###############################################################################