###############################################################################
###############################################################################
###############################################################################

# Packages --------------------------------------------------------------------

library(shiny)
library(shiny.i18n)

# -----------------------------------------------------------------------------

###############################################################################

server <- function(input, output, session) {
  
  # Translations --------------------------------------------------------------
  i18n <- Translator$new(translation_csvs_path = "translations/",
                         separator_csv = ";") # translation file
  i18n$set_translation_language("en")
  
  #############################################################################
  # Server --------------------------------------------------------------------
  #############################################################################
  
  # Parametres ----------------------------------------------------------------
  
  bins <- mod_slider_server("mod_slider")
  sex <-  reactive({ input$sex })
  
  text <- reactive({
    validate(need(input$sex, FALSE))
           if( input$sex == 1){ i18n$t("Males") 
    } else if( input$sex == 2){ i18n$t("Females") 
    } else if( input$sex == 0){ i18n$t("Both") 
    }   
  })
  
  # Translator ----------------------------------------------------------------
  i18n_r <- reactive({
    selected <- input$selected_language
    if (length(selected) > 0 && selected %in% i18n$get_languages()) {
      i18n$set_translation_language(selected)
    }
    i18n
  })

  # Update language in session
  observeEvent(i18n_r(), {
    shiny.i18n::update_lang(session, input$selected_language)
  })

  # Main ----------------------------------------------------------------------
  
  mod_slider_server("mod_selectInput")
  
  mod_selectInput_server("mod_selectInput",
                         i18n_r # Parameters
  )

  mod_text_server("mod_text",
                  i18n_r, sex, text # Parameters
  )
                              
  mod_plot_server("mod_plot",
                  bins, text, i18n_r # Parameters
  )
  
  #############################################################################
  # ---------------------------------------------------------------------------
  #############################################################################
  
} # End of server()

###############################################################################
###############################################################################
###############################################################################