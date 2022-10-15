# Randomization
# Modularized version
# 2021-03-07 by Ed Parsadanyan
# 
# TODO
# - split main interface into module-specific left panel and generic outputs for main panel
# - add stratified randomization

#####################
#      UI PART      #
#####################

UI_Randomize <- function(id, i18n) {
  
  ns <- NS(id)
  
  # parameter_tabs <- tabsetPanel(
  #   id = ns("params")
  #   ,type = "hidden"
  #   ,tabPanel(ns("simple"), )
  #   ,tabPanel(ns("block"),
  #            numericInput(ns("Blocksize"),    label = "Enter randomisation block size",    min=1,  max=1000,    step=1, value=4)
  #   )
  #   # Stratified randomisation is not yet implemented
  #   # ,tabPanel("strata_block",
  #   #          # textInput("STRATA_1", "Strata 1 label", value = "Strata name: Category 1, Category 2"),
  #   #          tags$div(id = ns('NewStrata')),
  #   #          actionButton(ns("btnAddStrat"),"Add new strata",icon=icon("plus")),  )
  # )
  
  fluidPage(
    useShinyjs(),
    tags$head(
      tags$style(HTML("hr {border-top: 1px solid #000000;}"))
    ),
    titlePanel(title = i18n$t("ui_RAND_title")),
    sidebarPanel(width = 4,
                 radioButtons(ns("DESIGN"),i18n$t("ui_RAND_input_method"),
                              c("Simple" = "simple"
                                ,"Blocked" = "block"
                                # Stratified randomisation is not yet implemented
                                # ,"Stratified balanced" = "strata_block"
                              ), selected="simple" ),
                 numericInput(ns("Nsubj"),   label = i18n$t("ui_RAND_input_nsubjects"),  min=10, max=1000, step=1, value=24),
                 helpText(i18n$t("ui_RAND_input_armlabelinfo")),
                 splitLayout(cellWidths = c("75%", "25%"),
                             textInput(ns("ARM_1_label"), "Arm 1 label", value = "Treatment"),
                             numericInput(ns("ARM_1_n"), "size", min=1, value = 1)
                 ), 
                 splitLayout(cellWidths = c("75%", "25%"),
                             textInput(ns("ARM_2_label"), "Arm 2 label", value = "Control"),
                             numericInput(ns("ARM_2_n"), "size", min=1, value = 1)
                 ), tags$div(id = ns('NewArms')),
                 actionButton(ns("btnAddArm"),i18n$t("ui_RAND_input_addarm"),icon=icon("plus")),
                 hr(),
                 
                 # parameter_tabs
                 tabsetPanel(
                   id = ns("params")
                   ,type = "hidden"
                   ,tabPanel("simple", )
                   ,tabPanel("block",
                             numericInput(ns("Blocksize"),    label = i18n$t("ui_RAND_input_blocksize"),    min=1,  max=1000,    step=1, value=4)
                   )
                 # Stratified randomisation is not yet implemented
                 # ,tabPanel("strata_block",
                 #          # textInput("STRATA_1", "Strata 1 label", value = "Strata name: Category 1, Category 2"),
                 #          tags$div(id = ns('NewStrata')),
                 #          actionButton(ns("btnAddStrat"),"Add new strata",icon=icon("plus")),  )
                 )
                 
                 ,helpText(i18n$t("ui_RAND_input_pleaseclick")),
                 
                 actionButton(ns("btnCalculate"),i18n$t("ui_RAND_input_calculate")), p(),
                 disabled(downloadButton(ns("getCSV"),i18n$t("ui_RAND_input_savecsv")))
                 
    ),
    mainPanel(wideth = 8,
              
              tabsetPanel(type = "tabs",
                          tabPanel(i18n$t("ui_app_common_outpanel"), 
                                   helpText(i18n$t("ui_RAND_input_preview")),
                                   tableOutput(ns('table_out'))
                                   
                          ),
                          UI_Rcode_environment(id, i18n=i18n)
              )            
    )
  )
  
}







#####################
#    SERVER PART    #
#####################

SERVER_Randomize <- function(id, i18n_r, lang) {
  moduleServer(id, function(input, output, session) {
    
    # !!! session$ns is needed to properly address reactive UI elements from the Server function
    ns <- session$ns

    library_code <- quote({
      library(randomizeR)
    })
    
    eval(library_code)
    
    counter <- reactiveValues(ARMn = 2,Stratn=0, curseed=0, outdf = NULL)

    
    # Some code for UI observers, randomisation code goes under "Main app functionality"
    
    # Update left side elements visibility
    observeEvent(input$DESIGN, {
      updateTabsetPanel(session, "params", selected = input$DESIGN)
    })
    
    # Add new TRT arms
    observeEvent(input$btnAddArm, {
      if (counter$ARMn<7) {
        counter$ARMn <- counter$ARMn + 1
        insertUI(
          selector = paste0("#",ns('NewArms')), session=session,
          ui = splitLayout(cellWidths = c("75%", "25%"),
                           textInput(ns(paste0("ARM_",as.character(counter$ARMn),"_label")),
                                     as.character(sprintf(i18n_r()$t("ui_RAND_input_armlabel"), as.character(counter$ARMn))), value = ""),
                           numericInput(inputId=ns(paste0("ARM_",as.character(counter$ARMn),"_n")),
                                        label=i18n_r()$t("ui_RAND_input_armsize"), min=1, value = 1) # , min=1, value = 1
          )
        )
      }
      if (counter$ARMn>=7) { shinyjs::disable("btnAddArm")}
    })
    
    # Add new Stratas
    # Stratified randomisation is not yet implemented
    # observeEvent(input$btnAddStrat, {
    #   if (counter$Stratn<4) {
    #     counter$Stratn <- counter$Stratn + 1
    #     insertUI(
    #       selector = '#NewStrata',
    #       ui = textInput(paste0("STRATA_",as.character(counter$Stratn)),
    #                      paste0("Strata ",as.character(counter$Stratn)," name and values"),
    #                      value = paste0("Strata ",as.character(counter$Stratn)," name: Category 1, Category 2"))
    #     )
    #   }
    #   if (counter$Stratn>=4) {shinyjs::disable("btnAddStrat")}
    # })
    
    # Generate random number only at button click to avoid re-generation during the script execution
    observeEvent(input$btnCalculate, {
      
      disable("getCSV")
      
      counter$curseed <- as.integer(runif(1)*1000000)
      counter$outdf <- randomize_proc()
      
      if (is.data.frame(counter$outdf)) { enable("getCSV") }
    })
    

    # Some UI elements should be updated on the Server side:
    # Update Radiobuttons and arm text values when language is changed
    observeEvent(lang(), {
      i18n_r()$set_translation_language(lang())
      
      updateRadioButtons(session, "DESIGN", label = i18n_r()$t("ui_RAND_input_method"),
                         choices =
                           setNames(c("simple","block"),
                                    i18n_r()$t(c("ui_RAND_input_methodsimple","ui_RAND_input_methodblocked")) ),
                         selected=input$DESIGN)

      for (i in (1:counter$ARMn)) {
        updateTextInput(session, paste0("ARM_",as.character(i),"_label"),
                        label = as.character(sprintf(i18n_r()$t("ui_RAND_input_armlabel"), as.character(i))))
        
        updateNumericInput(session, paste0("ARM_",as.character(i),"_n"),
                           label = i18n_r()$t("ui_RAND_input_armsize"))
      }
      
    })
    
    
    ##########################
    # Main app functionality #
    ##########################
    
    randomize_proc <- metaReactive2 (varname="randtable",{ # reactive | metaReactive
      req(input$btnCalculate)
      isolate({
        

        # Reset output
        counter$outdf <- NULL
        
        # Get all treatment arms and size values
        arm_labels <- c()
        arm_size   <- c()
        for (i in (1:counter$ARMn)){ # use only non-missing labels
          cur_label <- input[[paste0("ARM_",as.character(i),"_label")]]
          cur_size <-  input[[paste0("ARM_",as.character(i),"_n")]] #,"_n"
          
          # cat(i,":",cur_label,cur_size,"\n")
          if (cur_label!="")  {
            arm_labels <- c(arm_labels,cur_label)
            arm_size   <- c(arm_size,cur_size)
          }
        }
        
        # Check the block size to avoid errors
        #   Some other inconsistent conditions don't require checks as the randomization
        #   procedure doesn't fail on them.
        validate(
          need(((input$Blocksize%%sum(arm_size)==0) & (input$DESIGN == "block")) | (input$DESIGN != "block"),
               as.character(sprintf(i18n_r()$t("ui_RAND_validate_blocksize"), as.character(sum(arm_size))))),
          need((min(arm_size%%1)!=0 | min(as.integer(arm_size)>=1)!=0),
               as.character(sprintf(i18n_r()$t("ui_RAND_validate_armsize"), as.character(1)))),
          need(as.integer(input$Nsubj) == input$Nsubj & input$Nsubj > 1,
               as.character(sprintf(i18n_r()$t("ui_RAND_validate_nsubj"), as.character(2+abs(as.integer(input$Nsubj)))))),
          need(!length(which(duplicated(arm_labels))),
               as.character(i18n_r()$t("ui_RAND_validate_armduplicated")) )
        )
        
        # Pick parameters according to the randomization type
        if(input$DESIGN == "simple") {
          rand_options <- metaReactive(bindToReturn = TRUE,{
            # ..get_arms()
            params <- crPar(..(input$Nsubj), K = ..(length(arm_labels)), ratio = ..(arm_size), groups=..(arm_labels))
            new_seed <- ..(counter$curseed)
            set.seed(new_seed, kind = "Mersenne-Twister", normal.kind = "Inversion", sample.kind="Rejection")
            RandSeq_ <- genSeq(params, seed=new_seed)
            # data.frame(SEED=RandSeq_$seed)
            
            # print(RandSeq_$seed)
            # ..(rand_generate())
            # print(RandSeq_)
            
            Rlist <- t(getRandList(RandSeq_))
            Seq_  <- (1:length(Rlist))
            data.frame(Sequence = Seq_,
                       Randnum = paste0("R",sprintf("%04d",Seq_)),
                       Arm     = Rlist
            )
          })
        }
        else if(input$DESIGN == "block") {
          rand_options <- metaReactive(bindToReturn = TRUE,{
            block_size  <- ..(input$Blocksize)
            rand_blocks <- rep(block_size, ceiling(..(input$Nsubj)/block_size))
            params <- pbrPar(rand_blocks, K = ..(length(arm_labels)), ratio = ..(arm_size), groups=..(arm_labels))
            # Randseed <- ..(.Random.seed)
            new_seed <- ..(counter$curseed)
            set.seed(new_seed, kind = "Mersenne-Twister", normal.kind = "Inversion", sample.kind="Rejection")
            RandSeq_ <- genSeq(params, seed=new_seed)
            
            Rlist <- t(getRandList(RandSeq_))
            Seq_  <- (1:length(Rlist))
            # data.frame(c(1:20))
            data.frame(Sequence = Seq_,
                       Randnum = paste0("R",sprintf("%04d",Seq_)),
                       Blocknum= rep(1:length(rand_blocks),each=block_size),
                       Arm     = Rlist
            )
          })
        }         
        # else {
        #   rand_options <- metaReactive(bindToReturn = TRUE,{
        #     data.frame(iris)
        #   })
        # }
        
        metaExpr(bindToReturn = TRUE,{
          ..(rand_options())
        })
        
      })
      
    })
    

    app_version <- eventReactive(input$btnCalculate,{
      cat(version$version.string," / ",citation("shiny")$title," (",citation("shiny")$note,")","\n -",citation("randomizeR")$title," (version ", as.character(packageVersion("randomizeR")),").",
          sep="")
    })
    
    # Download button
    output$getCSV <- downloadHandler(
      filename = function() {
        paste0(input$DESIGN,"_", counter$curseed ,".csv")
      },
      content = function(file) {
        write.csv(counter$outdf, file, row.names=FALSE)
      }
    )
    

    
    output$code_out <- metaRender(renderPrint,{
      req(input$btnCalculate)
      isolate(
        expandChain(library_code,
                    invisible(randomize_proc())
        )
      )
    })
    
    output$table_out <- metaRender(renderTable,{
      req(input$btnCalculate)
      isolate(
        head(randomize_proc(),50)
      )
    })

    output$version_out <- renderPrint({
      app_version()
    })    
    
    
  })
}






