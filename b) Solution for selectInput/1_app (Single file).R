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
                  value = 30
                  ),
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

server <- function(input, output, session) {
  
  observeEvent(input$selected_language, {   # Update language in session
    shiny.i18n::update_lang(session, input$selected_language)
  })
  
  #############################################################################
  # Server --------------------------------------------------------------------
  #############################################################################
  
  output$selectInput_sex <- renderUI({ # >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    selectInput(
      inputId = "sex",
      label = i18n$t("Select sex:"),
      width = "500px",
      choices = setNames(c(1,2,0), 
                         c(i18n$t("Males"),
                           i18n$t("Females"),
                           i18n$t("Both")
                         )
      )
    )
  }) # End of selectInput_sex()
  
  text <- reactive({
    validate(need(input$sex, FALSE))
             if( input$sex == 1){ i18n$t("Males") 
      } else if( input$sex == 2){ i18n$t("Females") 
      } else if( input$sex == 0){ i18n$t("Both") 
      }   
  })
  
  output$text_sex <- renderText({ # >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    
    paste(i18n$t("You have selected"), 
          text(),
          i18n$t("with code"), 
          input$sex)
  }) # End of text_sex()
  
  output$distPlot <- renderPlot({ # >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    # generate bins based on input$bins from ui.R
    x    <- faithful[, 2]
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    
    # draw the histogram with the specified number of bins
    hist(x,
         breaks = bins,
         col = "darkgray", 
         border = "white",
         main = paste(i18n$t("Histogram of x"),
                      i18n$t("for"),
                      text()
                      ), 
         ylab = i18n$t("Frequency"))
  }) # Emd of distPlot()
  
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