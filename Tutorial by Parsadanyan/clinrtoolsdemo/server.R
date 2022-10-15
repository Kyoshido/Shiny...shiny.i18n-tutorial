# ClinRTools modularized demo
# 2021-03-06 by Ed Parsadanyan
# 
# Not for production, but to test a couple of modules:
# 
# Dashboard sources all modules
# Each module has it's own UI and Server part
# Additionally, commou module UI is called for each module (output, source code, system version)
# 

# Add all server functions from each module here
server <- function(input, output, session) {

  # URLs are not yet implemented
  # router$server(input, output, session)

  # Load translations
  # setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
  i18n <- Translator$new(translation_csvs_path = paste0("data/translations"),
                         separator_csv="|")
  i18n$set_translation_language(default_UI_lang)
  

  # Language picker
  observeEvent(input$lang_pick, {
    update_lang(session, input$lang_pick)
    # i18n$set_translation_language(input$lang_pick)
    
    # Refresh readme file
    removeUI(selector ="#readmediv", immediate = TRUE)
    insertUI(immediate = TRUE,
             selector = '#readmehere', session=session,
             ui = div(id="readmediv",
                      includeHTML(
                        as.character(i18n$get_translations()["ui_mainpage_readmefile",input$lang_pick])
                                  )
                      )
    )
    
  })
  
  
  # Pass language selection into the module for Server-side translations
  # If not done, some UI elements will not be updated upon language change
  SERVER_BE_samplesize("BE_samplesize",i18n_r = reactive(i18n), lang = reactive(input$lang_pick))
  SERVER_Randomize("Randomize"        ,i18n_r = reactive(i18n), lang = reactive(input$lang_pick))
}

