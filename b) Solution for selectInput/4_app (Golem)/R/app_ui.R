#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {

  # calling the translator sent as a golem option
  i18n <- golem::get_golem_options(which = "translator")
  i18n$set_translation_language("en")

  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic

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

    fluidPage(

      # Application title
      titlePanel(i18n$t("Hello Shiny!"), windowTitle = NULL),

      # Sidebar with a slider input for number of bins
      sidebarLayout(
        sidebarPanel(

          mod_slider_ui("slider_1",
                        i18n # Parameters
          )

        ), # End of sidebarPanel()

        # Show a plot of the generated distribution
        mainPanel(

          mod_plot_ui("plot_1",
                      i18n # Parameters
          )

        ) # End of mainPanel()
      ) # End of sidebarLayout¨()

    ) # End of fluidPage()
    #############################################################################
    # ---------------------------------------------------------------------------
    #############################################################################

  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "app"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
