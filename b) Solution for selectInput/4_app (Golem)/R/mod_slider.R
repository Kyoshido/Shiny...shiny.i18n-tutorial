#' slider UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_slider_ui <- function(id,
                          # Parameters
                          i18n
) {
  ns <- NS(id)
  tagList(

    ###########################################################################
    # UI ----------------------------------------------------------------------
    ###########################################################################

    sliderInput(ns("bins"),
                i18n$t("Number of bins:"), # you use i18n object as always
                min = 1,
                max = 50,
                value = 30
    )

    ###########################################################################
    # -------------------------------------------------------------------------
    ###########################################################################

  )
}

#' slider Server Functions
#'
#' @noRd
mod_slider_server <- function(id
){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    ###########################################################################
    # Server ------------------------------------------------------------------
    ###########################################################################

    bins <- reactive({ input$bins })
    return(bins)

    ###########################################################################
    # -------------------------------------------------------------------------
    ###########################################################################

  })
}

## To be copied in the UI
# mod_slider_ui("slider_1")

## To be copied in the server
# mod_slider_server("slider_1")
