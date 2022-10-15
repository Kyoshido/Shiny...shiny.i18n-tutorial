# Common module elements
# 2021-03-07 by Ed Parsadanyan
# 

#####################
#      UI PART      #
#####################


# Reproducible R code + environment options.
# This part of UI should be implemented via tabsetPanel()

UI_Rcode_environment <- function(id, i18n) {
  
  ns <- NS(id)
  
  tabPanel(i18n$t("ui_app_common_rcodepanel"), 
           tags$p(),
           tags$head(tags$style(paste0("#",ns('code_out'),"{white-space: pre-wrap; word-break: keep-all;}"))),
           verbatimTextOutput(ns('code_out')),
           
           helpText(i18n$t("ui_app_common_envirpanel")),
           tags$head(tags$style(paste0("#",ns('version_out'),"{white-space: pre-wrap; word-break: keep-all;}"))),
           verbatimTextOutput(ns('version_out'))
  )                        
  
}








#####################
#    SERVER PART    #
#####################

# SERVER_Rcode_environment <- function(id) {
#   # moduleServer(id, function(input, output, session) {
#     
#     output$code_out <- metaRender(renderPrint,{
#       expandChain(library_code,
#                   invisible(result())
#       )
#     })
#     output$version_out <- renderPrint({
#       app_version()
#     })
#     
#   # })
# }
