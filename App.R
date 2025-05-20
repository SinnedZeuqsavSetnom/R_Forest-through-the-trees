# - - - - - - - - - - Load packages from R library (Install if required) - - - - - - - - - - - - - - - - - - - - - - - - - - #
library("party")
library("haven"); library("readxl")
library("shinyjs"); library("shinythemes")


# - - - App User Interface and Server code - - -
ui <- fluidPage(
  tags$head(
    tags$style( HTML( ".form-group {margin-bottom: 0 !important;}")     )     ),
  # Some HTML functions
  fluidRow(column(4, offset=8,
         tags$img(height=40, width=40,src="HJD_Streetscape_and_Lobby.jpg"),
         tags$button(id = 'close', type = "button", class = "btn action-button",
                     onclick = "setTimeout(function(){window.close();},500);", "Close window"), align="right")),
  theme = shinytheme("cyborg"),  # Select theme before individual tabs
  tabsetPanel(
  # Browser broken into panel tabs
  tabPanel("",title = "R-shiny: Party ctree GUI", titlePanel("Load Data and Run Trees"),
    sidebarLayout( # Sidebar layout with side and main panel ----
      sidebarPanel(  # Side Panel for input file and options ----
      fileInput(inputId = "FolderPathLoadData", "Input Folder Location of Database", accept = c('.sav','.xlsx')),
        uiOutput("ComboBox_Y"),    # Dynamic input - prior input generates an input item in server
        uiOutput("ComboBox_X"),    # Dynamic input - prior input generates an input item in server
      actionButton("RunTree_Button", "Run ctree"),
      div(style='margin: 20px 0px 0px 0px;',uiOutput("CheckBox_PlotT")),
       
      div(style="margin: 0px;display:inline-block; width: 120px;vertical-align:middle;",uiOutput("CheckBox_Split")),
      div(style="margin: 0px;display:inline-block; width: 60px;vertical-align:middle;",uiOutput("Input_Split")),
       
      div(style="margin: 0px;display:inline-block; width: 120px;vertical-align:middle;",uiOutput("CheckBox_Alpha")),
      div(style="margin: 0px;display:inline-block; width: 60px;vertical-align:middle;",uiOutput("Input_Alpha")),
       
      div(style="margin: 0px;display:inline-block; width: 120px;vertical-align:middle;",uiOutput("CheckBox_MxLvl")),
      div(style="margin: 0px;display:inline-block; width: 60px;vertical-align:middle;",uiOutput("Input_MxLvl"))
      ),
      mainPanel(  # Main panel for displaying outputs ----
        textOutput(outputId = "Msg_InProgress"),
        plotOutput(outputId = "ctree"),
        textOutput(outputId = "Msg_Tree")
      ),
    fluid=TRUE)),
  tabPanel("", title = "Preparing Data", titlePanel("Modify Variables"),
          uiOutput("CheckBox_VariablesIncluded")
           )
  ))


server<-function(input, output, session) {
  options(shiny.maxRequestSize = 100*1024^2)
  #This function is repsonsible for loading in the selected file
  filedata <- reactive({
    infile <- input$FolderPathLoadData
    if (is.null(infile)) {return(NULL)}
    read_sav(infile$datapath)
    })
  #This function is repsonsible for Predictor variable split db
  filedata_split <- reactive({
    df<-filedata()
    Chk_Split <- input$Chk_Split
    if (is.null(df)) {return(NULL)}
    if (!Chk_Split) {return(NULL)} else {
      CutOffVal <- input$Split
      Predictor <- input$X
      df$CutOffVar <- as.factor(ifelse(df[Predictor]>CutOffVal,paste(">",CutOffVal),paste("<=",CutOffVal)))
      df}
    })
 
  #The following set of functions populate the column selectors
  output$ComboBox_Y <- renderUI({
    df <-filedata(); if (is.null(df)) return(NULL)
    items=names(df); names(items)=items
    selectInput("Y", "Select Y variable (Target)",items)  })
  output$ComboBox_X <- renderUI({
    df <-filedata(); if (is.null(df)) return(NULL)
    items=names(df); names(items)=items
    selectInput("X", "Select X variable (Predictor)",items)  })
  output$CheckBox_PlotT <- renderUI({ checkboxInput(inputId = "Chk_PlotT", label = "Simple plot (Descriptive)", value = TRUE)  })
  output$CheckBox_Split <- renderUI({ checkboxInput(inputId = "Chk_Split", label = "Force Split (at)", value = FALSE)  })
  output$Input_Split <- renderUI({ textInput(inputId = "Split", label = NULL)  })
  output$CheckBox_Alpha <- renderUI({ checkboxInput(inputId = "Chk_Alpha", label = "1 - alpha:", value = FALSE)  })
  output$Input_Alpha <- renderUI({ textInput(inputId = "Alpha", label = NULL)  })
  output$CheckBox_MxLvl <- renderUI({ checkboxInput(inputId = "Chk_MxLvl", label = "Max Levels:", value = FALSE)  })
  output$Input_MxLvl <- renderUI({ textInput(inputId = "MxLvl", label = NULL)  })
  
  output$CheckBox_VariablesIncluded <- renderUI({
    df <-filedata(); if (is.null(df)) return(NULL)
    #items=names(df); names(items)=items
    checkboxGroupInput("VarList", "Check all variables to be included", choices=names(df), selected = names(df))  })
  
  # Run ctree with Dynamic Input, activates with action button
  Run_Tree <- eventReactive(input$RunTree_Button, {
    Target <- input$Y
    Predictor <- input$X
    Chk_Split <- input$Chk_Split
    if (!Chk_Split) {   
    df <- filedata()
    CondInfTree <- ctree(as.formula(paste(Target,"~",Predictor,sep="")),
                    data=subset(df,!is.na(df[Target])),
                    controls = ctree_control(mincriterion=as.numeric(Significance$Alpha), maxdepth = as.integer(TreeDepth$MxLvl))) }
    else {
    df <- filedata_split()
    CondInfTree <- ctree(as.formula(paste(Target,"~CutOffVar",sep="")),
                    data=subset(df,!is.na(df[Target])),
                    controls = ctree_control(mincriterion=as.numeric(Significance$Alpha), maxdepth = as.integer(TreeDepth$MxLvl))) }
      })
  
  # ctree Plot parameters
  PlotType<-reactiveValues(PlotT="simple")     #Resets PLotType if checkbox=TRUE *default value: "simple"
  Significance<-reactiveValues(Alpha=.95)    #Resets Alpha if checkbox=TRUE *default value: 0.05
  TreeDepth<-reactiveValues(MxLvl=0)          #Resets Max Levels if checkbox=TRUE *default value: 0 --> unlimited
  observeEvent(input$Chk_PlotT, { if (input$Chk_PlotT) {PlotType$PlotT<-"simple"} else {PlotType$PlotT<-"extended"}  })
  observe({ input$X                           #Resets Chk_ForceSplit if input$x AKA Predictor changes
            updateCheckboxInput(session, inputId = "Chk_Split", value = FALSE)  })
  observeEvent({input$Chk_Alpha
                input$Alpha }, { if (input$Chk_Alpha) {Significance$Alpha<-input$Alpha} else {Significance$Alpha<-.95} })
  observeEvent({input$Chk_MxLvl
                input$MxLvl }, { if (input$Chk_MxLvl) {TreeDepth$MxLvl<-input$MxLvl} else {TreeDepth$MxLvl<-0} })

  
  PrintMsg<-eventReactive(input$RunTree_Button, {colnames(filedata_split())})
  output$Msg_Tree <- renderPrint(PrintMsg())
    #output$Msg_Tree <- renderPrint(print(Run_Tree()))
  output$ctree<-renderPlot({ plot(Run_Tree(), type=PlotType$PlotT, main=input$X)  })

    
  #Exit Session and Browser
  observe({if (input$close>0) stopApp()})  
  }
 

shinyApp(ui=ui,server=server)
 # Currently following along example from:
#Link<-"https://gist.github.com/psychemedia/9737637"