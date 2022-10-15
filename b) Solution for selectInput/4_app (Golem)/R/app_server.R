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

  mod_plot_server("plot_1",
                  bins, i18n_r
  )

  #############################################################################
  # ---------------------------------------------------------------------------
  #############################################################################
}
