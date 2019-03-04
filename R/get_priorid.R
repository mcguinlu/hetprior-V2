#'Informative bayesian prior lookup for heterogenity in meta-analyses
#'
#'@imports shiny
#'@imports miniUI
#'
#'@export

get_priorid <- function() {
  ui <- miniUI::miniPage(
    gadgetTitleBar("Prior ID Selection Tool"),
    miniContentPanel(
      scrollable = FALSE,
      p("Please complete the boxes below, and select submit to view the relevant prior."),
      p("Once you are happy with your selection, click 'Done' to export the Prior ID to R for use with", em("hetprior")),
      p("Note: choices are updated based on your input into the previous boxes."),

      fillRow(
      fillCol(
      h3("Conditions:"),
      selectizeInput("hetstat", label = "Heterogeneity statistic:",
                       choices = c(unique(as.character(hetdata$Heterogeneity.statistic))),
                       options = list(placeholder = 'Choose',
                                      onInitialize = I('function() { this.setValue(""); }'))),
      uiOutput("datatypeui"),
      uiOutput("effectmeasureui"),
      uiOutput("distformui"),
      uiOutput("interventiontypeui"),
      uiOutput("natureoutcomeui"),
      uiOutput("medicalareaui"),
      uiOutput("samplesizeui"),
      uiOutput("actiondoui"),
      actionButton("clear","Clear")),

      fillCol(
      h3("Results:"),
      uiOutput("prioridui"),
      uiOutput("meanui"),
      uiOutput("sdui"),
      height = "40%")
      )
    )
  )

  server <- function(input, output, session) {
    v <- reactiveValues(data = NULL, buttonclick = 0)
    observeEvent(input$hetstat,{
      v$buttonclick <- 0
      updateSelectInput(session,'datatype',
                        choices = c("", unique(as.character(hetdata$Data.Type[hetdata$Heterogeneity.statistic==input$hetstat]))))
    })

    observeEvent(input$datatype,{
      v$buttonclick <- 0
      updateSelectInput(session,'effectmeasure',
                        choices = c("", unique(as.character(hetdata$Effect.Measure[hetdata$Heterogeneity.statistic==input$hetstat &
                                                                                     hetdata$Data.Type==input$datatype]))))
    })

    observeEvent(input$effectmeasure,{
      v$buttonclick <- 0
      updateSelectInput(session,'distributionform',
                        choices = c("", unique(as.character(hetdata$Distribution.form[hetdata$Heterogeneity.statistic==input$hetstat &
                                                                                        hetdata$Data.Type==input$datatype &
                                                                                        hetdata$Effect.Measure==input$effectmeasure]))))
    })

    observeEvent(input$distributionform,{
      v$buttonclick <- 0
      updateSelectInput(session,'interventiontype',
                        choices = c("", unique(as.character(hetdata$Type.of.Intervention[hetdata$Heterogeneity.statistic==input$hetstat &
                                                                                           hetdata$Data.Type==input$datatype &
                                                                                           hetdata$Effect.Measure==input$effectmeasure &
                                                                                           hetdata$Distribution.form==input$distributionform]))))
    })

    observeEvent(input$interventiontype,{
      v$buttonclick <- 0
      updateSelectInput(session,'natureoutcome',
                        choices = c("", unique(as.character(hetdata$Nature.of.Outcome[hetdata$Heterogeneity.statistic==input$hetstat &
                                                                                        hetdata$Data.Type==input$datatype &
                                                                                        hetdata$Effect.Measure==input$effectmeasure &
                                                                                        hetdata$Distribution.form==input$distributionform &
                                                                                        hetdata$Type.of.Intervention==input$interventiontype]))))
    })

    observeEvent(input$natureoutcome,{
      v$buttonclick <- 0
      updateSelectInput(session,'medicalarea',
                        choices = c("", unique(as.character(hetdata$Medical.area[hetdata$Heterogeneity.statistic==input$hetstat &
                                                                                   hetdata$Data.Type==input$datatype &
                                                                                   hetdata$Effect.Measure==input$effectmeasure &
                                                                                   hetdata$Distribution.form==input$distributionform &
                                                                                   hetdata$Type.of.Intervention==input$interventiontype &
                                                                                   hetdata$Nature.of.Outcome==input$natureoutcome
                                                                                 ]))))
    })

    observeEvent(input$medicalarea,{
      v$buttonclick <- 0
      updateSelectInput(session,'samplesize',
                        choices = c("", unique(as.character(hetdata$Average.Sample.Size[hetdata$Heterogeneity.statistic==input$hetstat &
                                                                                          hetdata$Data.Type==input$datatype &
                                                                                          hetdata$Effect.Measure==input$effectmeasure &
                                                                                          hetdata$Distribution.form==input$distributionform &
                                                                                          hetdata$Type.of.Intervention==input$interventiontype &
                                                                                          hetdata$Nature.of.Outcome==input$natureoutcome &
                                                                                          hetdata$Medical.area==input$medicalarea
                                                                                        ]))))
    })

    output$datatypeui <-
      renderUI({
        if (is.null(input$hetstat)){}else{
          if (input$hetstat!=""){
            selectizeInput("datatype", label = "Data type:",
                           choices = NULL,
                           options = list(placeholder = 'Choose',
                                          onInitialize = I('function() { this.setValue(""); }')))
          }}})

    output$effectmeasureui <-
      renderUI({
        if (is.null(input$datatype)){}else{
          if (input$hetstat!="" & input$datatype!=""){
            selectizeInput("effectmeasure", label = "Effect measure:",
                           choices = NULL,
                           options = list(placeholder = 'Choose',
                                          onInitialize = I('function() { this.setValue(""); }')))
          }}})

    output$distformui <-
      renderUI({
        if (is.null(input$effectmeasure)){}else{
          if (input$hetstat!="" & input$datatype!="" & input$effectmeasure!=""){
            selectizeInput("distributionform", label = "Distribution form:",
                           choices = NULL,
                           options = list(placeholder = 'Choose',
                                          onInitialize = I('function() { this.setValue(""); }')))
          }}})

    output$interventiontypeui <-
      renderUI({
        if (is.null(input$distributionform)){}else{
          if (input$hetstat!="" & input$datatype!="" & input$effectmeasure!="" & input$distributionform!=""){
            selectizeInput("interventiontype", label = "Type of intervention:",
                           choices = NULL,
                           options = list(placeholder = 'Choose',
                                          onInitialize = I('function() { this.setValue(""); }')))
          }}})

    output$natureoutcomeui <-
      renderUI({
        if (is.null(input$interventiontype)){}else{
          if (input$hetstat!="" & input$datatype!="" & input$effectmeasure!="" & input$distributionform!="" & input$interventiontype!=""){
            selectizeInput("natureoutcome", label = "Nature of Outcome:",
                           choices = NULL,
                           options = list(placeholder = 'Choose',
                                          onInitialize = I('function() { this.setValue(""); }')))
          }}})

    output$medicalareaui <-
      renderUI({
        if (is.null(input$natureoutcome)){}else{
          if (input$hetstat!="" &
              input$datatype!="" &
              input$effectmeasure!="" &
              input$distributionform!="" &
              input$interventiontype!="" &
              input$natureoutcome!=""
              ){
            selectizeInput("medicalarea", label = "Medical area:",
                           choices = NULL,
                           options = list(placeholder = 'Choose',
                                          onInitialize = I('function() { this.setValue(""); }')))
          }}})

    output$samplesizeui <-
      renderUI({
        if (is.null(input$natureoutcome)){}else{
          if (input$hetstat!="" &
              input$datatype!="" &
              input$effectmeasure!="" &
              input$distributionform!="" &
              input$interventiontype!="" &
              input$natureoutcome!=""
              ){
            selectizeInput("samplesize", label = "Average sample size:",
                           choices = NULL,
                           options = list(placeholder = 'Choose',
                                          onInitialize = I('function() { this.setValue(""); }')))
          }}})

    output$actiondoui <-
      renderUI({
        if (is.null(input$medicalarea) |is.null(input$samplesize)){}else{
          if (input$hetstat!="" &
              input$datatype!="" &
              input$effectmeasure!="" &
              input$distributionform!="" &
              input$interventiontype!="" &
              input$natureoutcome!="" &
              input$medicalarea!=""&
              input$samplesize!=""
          ){
            actionButton("do", "Submit")
            }}})


observeEvent(input$do, {
      v$data <- subset(hetdata,
                       Heterogeneity.statistic == input$hetstat &
                         Data.Type == input$datatype &
                         Effect.Measure==input$effectmeasure &
                         Distribution.form==input$distributionform &
                         Type.of.Intervention==input$interventiontype &
                         Nature.of.Outcome==input$natureoutcome &
                         Medical.area==input$medicalarea &
                         Average.Sample.Size==input$samplesize
      )
      v$priorid <- as.numeric(v$data[1, 1])
      v$mean <- as.numeric(v$data[1, 10])
      v$sd <- as.numeric(v$data[1, 11])
      v$median <- v$data[1, 12]
      v$lowquant <- v$data[1, 13]
      v$highquant <- v$data[1, 14]
      v$notes1 <- v$data[1, 15]
      v$reference <- v$data [1, 16]
      v$buttonclick <- 1
      })

observeEvent(input$clear, {

      v$buttonclick <- 0
      v$data <- NULL
      updateTextInput(session, "hetstat", value = "")
      v$priorid <- NULL
      v$mean <- NULL
      v$sd <- NULL
      v$median <- NULL
      v$lowquant <- NULL
      v$highquant <- NULL
      v$notes1 <- NULL
      v$reference <- NULL
    })

output$priorid <- renderText(v$priorid)
output$mean <- renderText(v$mean)
output$sd <- renderText(v$sd)
output$median <- renderText(as.character(v$median))
output$reference <- renderText(as.character(v$reference))
output$lowquant <- renderText(as.character(v$lowquant))
output$highquant <- renderText(as.character(v$highquant))
output$notes1 <- renderText(as.character(v$notes1))

output$prioridui <-
  renderUI({p(em("Prior ID:"), textOutput("priorid"))})

output$meanui <-
  renderUI({
    p(em("Mean/Shape:"), textOutput("mean"))})

output$sdui <-
  renderUI({p(em("Standard deviation/scale:"), textOutput("sd"))})


    observeEvent(input$done, {
      stopApp(v$priorid)
    })
    observeEvent(input$cancel, {
      stopApp(stop("No prior id.", call. = FALSE))
    })
  }

  runGadget(ui, server, viewer = dialogViewer("Prior ID", width = 800, height = 900))
}
