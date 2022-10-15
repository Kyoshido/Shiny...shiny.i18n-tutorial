# Sample size calculation
# Modularized version
# 2021-03-07 by Ed Parsadanyan
# 
# TODO
# - split main interface into module-specific left panel and generic outputs for main panel

#####################
#      UI PART      #
#####################

UI_BE_samplesize <- function(id, i18n) {
  
  ns <- NS(id)
  
  fluidPage(
  titlePanel(title = i18n$t("ui_BE_title")),
  sidebarPanel(width = 4,
               numericInput(ns("CV"),   label = i18n$t("ui_BE_input_cv"),  min=0, max=100, step=0.01, value=0.25),
               numericInput(ns("RATIO"),label = i18n$t("ui_BE_input_tr"), min=0, max=2,   step=0.01, value=0.95),
               radioButtons(ns("DESIGN"),i18n$t("ui_BE_input_design"),
                            c("2x1 (Parallel 2 groups)" = "parallel",
                              "2x2 (Simple crossover)" = "2x2",
                              "2x2x4 (Full replicate)" = "2x2x4"
                            )
                            , selected="2x2" ),
               radioButtons(ns("BELIMITS"),i18n$t("ui_BE_input_limits"),
                            c("80% - 125%" = "80_125",
                              "90% - 111.11%" = "90_111",
                              "Widened BE limits" = "BE_expanded"
                            )
               ),
               radioButtons(ns("POWER"),i18n$t("ui_BE_input_power"),
                            c("80%" = 0.8,
                              "90%" = 0.9
                            ) ),
               helpText(i18n$t("ui_BE_input_pleaseclick")),
               actionButton(ns("btnCalculate"),i18n$t("ui_BE_input_calculate"))
  ),
  mainPanel(wideth = 8,
            
            tabsetPanel(type = "tabs",
                        tabPanel(i18n$t("ui_app_common_outpanel"), 
                                 tags$p(),
                                 verbatimTextOutput(ns('ex_out')) #,
                        ),
                        UI_Rcode_environment(id, i18n=i18n)

            )
  )
  )
}





#####################
#    SERVER PART    #
#####################

SERVER_BE_samplesize <- function(id, i18n_r, lang) {
  moduleServer(id, function(input, output, session) {

    library_code <- quote({
      library(PowerTOST)
    })
    
    eval(library_code)
    
    # Some UI elements should be updated on the Server side (e.g. radiobuttons). Prepare translations:
    # i18n_r <- reactive({
    #   i18n_r
    # })
    
    # Update Radiobuttons when language is changed
    observeEvent(lang(), {
      i18n_r()$set_translation_language(lang())
      
        updateRadioButtons(session, "DESIGN", label = i18n_r()$t("ui_BE_input_design"),
                           choices =
                             setNames(c("parallel","2x2","2x2x4"),
                                      i18n_r()$t(c("ui_BE_input_2x1","ui_BE_input_2x2","ui_BE_input_2x2x4")) ),
                           selected=input$DESIGN)
        
        updateRadioButtons(session, "BELIMITS", label = i18n_r()$t("ui_BE_input_limits"),
                           choices =
                             setNames(c("80_125","90_111","BE_expanded"),
                                      c("80% - 125%","90% - 111.11%", i18n_r()$t("ui_BE_input_widened")) ),
                           selected=input$BELIMITS)
    })
    

    # Main app functionality
    result <- metaReactive2 (varname="result",{ # reactive | metaReactive
      req(input$btnCalculate)
      isolate({
        
        if(input$BELIMITS == "90_111") {
          theta1_=0.9
          theta2_=1.1111
        } else if(input$BELIMITS == "80_125") {
          theta1_=0.8
          theta2_=1/theta1_
        } else if(input$BELIMITS == "BE_expanded") {
          BElim <- scABEL(input$CV, regulator="EMA")
          theta1_=round(BElim[1],4)
          theta2_=round(BElim[2],4)
        }        
        
        validate(
          need(input$CV >0, i18n_r()$t("ui_BE_validate_cv")),
          need(input$RATIO > theta1_ & input$RATIO < theta2_,
               as.character(sprintf(i18n_r()$t("ui_BE_validate_tr"), as.character(theta1_), as.character(theta2_)))
               ),
          need((input$BELIMITS == "BE_expanded" & input$DESIGN == "2x2x4" & input$CV >0.3 ) | input$BELIMITS != "BE_expanded",
               i18n_r()$t("ui_BE_validate_widened"))
        )
        
        if(input$BELIMITS == "BE_expanded") {
          metaExpr(bindToReturn = TRUE,{
            CVintra <- ..(input$CV)
            BElim <- scABEL(CVintra, regulator="EMA")
            sampleN.scABEL(CV=CVintra, theta0=..(input$RATIO), design = ..(input$DESIGN),
                           theta1=round(BElim[1],4), theta2=round(BElim[2],4), targetpower=..(as.double(input$POWER)),
                           print=TRUE)
          })          
          
        } else {
          metaExpr(bindToReturn = TRUE,{
            sampleN.TOST(CV=..(input$CV), theta0=..(input$RATIO), design = ..(input$DESIGN),
                         theta1=..(theta1_), theta2=..(theta2_), targetpower=..(as.double(input$POWER)),
                         print=TRUE)
          })          
        }

        
      })
    })
    

    app_version <- eventReactive(input$btnCalculate,{
      cat(version$version.string," / ",citation("shiny")$title," (",citation("shiny")$note,")","\n -",citation("PowerTOST")$title,". ", citation("PowerTOST")$note,
          sep="")
    })
    
    
    output$ex_out <- renderPrint({
      invisible(result())
    })
    output$code_out <- metaRender(renderPrint,{
      expandChain(library_code,
                  invisible(result())
      )
    })
    output$version_out <- renderPrint({
      app_version()
    })
    # SERVER_Rcode_environment(id)
    
    
  })
}






