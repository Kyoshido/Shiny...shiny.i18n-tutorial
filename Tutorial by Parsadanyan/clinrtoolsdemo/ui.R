# ClinRTools modularized demo
# 2021-03-06 by Ed Parsadanyan
#
# UI "skeleton" of the whole app.
# Individual module UI is attached via UI_<module_name> functions
# 


# Translation is moved to UI section in order to isolate language selections for different users
i18n <- Translator$new(translation_csvs_path = paste0("data/translations"),
                       separator_csv="|")
active_ui_lang <- grep("ui",i18n$get_languages(), invert = TRUE, value = TRUE)
i18n$set_translation_language(default_UI_lang)


# Main page panel. Language picker is stored here due to limitations of the navbarPage
ui_mainpage <- fluidPage(
  shiny.i18n::usei18n(i18n),
  selectInput("lang_pick", i18n$t("ui_mainpage_lang"),
              choices=setNames(active_ui_lang,active_ui_lang),
              selected = default_UI_lang),
  tags$div(id = 'readmehere',
           div(id="readmediv",
               tags$h4(i18n$t("ui_mainpage_loading")))
           )
)



ui <- navbarPage("ClinRTools Demo", #position = "fixed-top",
                 # numericInput("CV",
                 #              label = "Enter CV intra",
                 #              min=0, max=100, step=0.01, value=0.25),
                 
                 tabPanel(i18n$t("ui_nav_page_main"),
                          ui_mainpage
                 ),
                 
                 
                 # navbarMenu("Sample size",
                 #            tabPanel("Bioequivalence trials"
                 #                     ,UI_BE_samplesize("BE_samplesize")
                 #                     ),
                 #            tabPanel("App 2"
                 #                      ,fluidPage()
                 #                     )
                 # ),
                 tabPanel(i18n$t("ui_nav_app_be"),
                          UI_BE_samplesize("BE_samplesize", i18n=i18n)
                 ),
                 tabPanel(i18n$t("ui_nav_app_rand")
                          ,UI_Randomize("Randomize", i18n=i18n)
                 ) #,
                 # tabPanel(i18n$t("ui_nav_page_help")
                 #          ,fluidPage()
                 # )

)

# ui <- dashboardPage(
#   dashboardHeader(
#     title = "MY TITLE",
#     tags$li(class = "dropdown",     
#             radioButtons(inputId = "language",
#                          label = "",
#                          choices = c("日文" = "cn", "English" = "en"),
#                          selected = "jp")
#     )
#   ),
#   dashboardSidebar(),
#   dashboardBody()
#   
# )
