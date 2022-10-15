#' plot UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_plot_ui <- function(id,
                        # Parameters
                        i18n
) {
  ns <- NS(id)
  tagList(

    ###########################################################################
    # UI ----------------------------------------------------------------------
    ###########################################################################

    plotOutput(ns("distPlot")),

    p(i18n$t("This is description of the plot."))

    ###########################################################################
    # -------------------------------------------------------------------------
    ###########################################################################

  )
}

#' plot Server Functions
#'
#' @noRd
mod_plot_server <- function(id,
                            # Parameters
                            bins, i18n_r
){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    ###########################################################################
    # Server ------------------------------------------------------------------
    ###########################################################################

    output$distPlot <- renderPlot({

      # Reactive parametres ...................................................
      bins <- bins()
      i18n_r <- i18n_r()

      # generate bins based on input$bins from ui.R
      x    <- faithful[, 2]
      bins <- seq(min(x),
                  max(x),
                  length.out = bins + 1)

      # draw the histogram with the specified number of bins
      hist(x,
           breaks = bins,
           col = "darkgray",
           border = "white",
           main = i18n_r$t("Histogram of x"),
           ylab = i18n_r$t("Frequency"))
    })

    ###########################################################################
    # -------------------------------------------------------------------------
    ###########################################################################
  })
}

## To be copied in the UI
# mod_plot_ui("plot_1")

## To be copied in the server
# mod_plot_server("plot_1")
