#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # Your application server logic

  # calling the translator sent as a golem option
  i18n <- golem::get_golem_options(which = "translator")
  i18n$set_translation_language("en")

  #############################################################################
  # Server --------------------------------------------------------------------
  #############################################################################

  # Parametres ----------------------------------------------------------------

  bins <- mod_slider_server("slider_1")
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

  mod_slider_server("slider_1")

  mod_selectInput_server("selectInput_1",
                         i18n_r # Parameters
  )

  mod_text_server("text_1",
                  i18n_r, sex, text # Parameters
  )

  mod_plot_server("plot_1",
                  bins, text, i18n_r # Parameters
  )


  #############################################################################
  # ---------------------------------------------------------------------------
  #############################################################################
}
