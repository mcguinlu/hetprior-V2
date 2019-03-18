#'Informative bayesian prior lookup for heterogenity in meta-analyses
#'
#'@imports shiny
#'@imports miniUI
#'
#'@export

get_priorid <- function() {
  ui <- miniUI::miniPage(
    miniUI::gadgetTitleBar("Prior ID Selection Tool"),
    miniUI::miniContentPanel(
      scrollable = TRUE,
      shiny::p("Please complete the boxes on the left below, and select submit to view the relevant prior."),
      shiny::p("Once you are happy with your selection, click 'Done' to export the Prior ID to R for use with", shiny::em("hetprior")),
      shiny::fillRow(
      shiny::fillCol(
      shiny::h3("Conditions:"),
      shiny::fillRow(
      em("Heterogeneity statistic:"),
      shiny::selectizeInput("hetstat",
                       choices = c(unique(as.character(hetdata$Heterogeneity.statistic))),
                       label = "",
                       options = list(placeholder = 'Heterogeneity statistic:',
                                      onInitialize = I('function() { this.setValue(""); }')))),
      fillRow(
      em("Data type:"),
      shiny::uiOutput("datatypeui")),

      fillRow(
      em("Effect measure:          "), shiny::uiOutput("effectmeasureui")),

      fillRow(
      em("Distribution form:       "),
      shiny::uiOutput("distformui")),

      shiny::uiOutput("interventiontypeui"),

      shiny::uiOutput("natureoutcomeui"),

      shiny::uiOutput("medicalareaui"),

      shiny::uiOutput("samplesizeui"),

      shiny::uiOutput("actiondoui"),

      height = "90%",
      width = "80%"
      ),

      shiny::fillCol(
      shiny::h3("Results:"),
      shiny::uiOutput("prioridui"),
      shiny::uiOutput("meanui"),
      shiny::uiOutput("sdui"),
      shiny::uiOutput("clearui"),
      height = "45%",
      width = "50%"
      )
      ), flex = c(3, 2)

    )
  )

server <- function(input, output, session) {
    v <- reactiveValues(data = NULL, buttonclick = 0)
    shiny::observeEvent(input$hetstat,{
      v$buttonclick <- 0
      v$priorid <- NULL
      shiny::updateSelectInput(session,'datatype',
                        choices = c("", unique(as.character(hetdata$Data.Type[hetdata$Heterogeneity.statistic==input$hetstat]))))
    })

    shiny::observeEvent(input$datatype,{
      v$buttonclick <- 0
      v$priorid <- NULL
      shiny::updateSelectInput(session,'effectmeasure',
                        choices = c("", unique(as.character(hetdata$Effect.Measure[hetdata$Heterogeneity.statistic==input$hetstat &
                                                                                     hetdata$Data.Type==input$datatype]))))
    })

    shiny::observeEvent(input$effectmeasure,{
      v$buttonclick <- 0
      v$priorid <- NULL
      shiny::updateSelectInput(session,'distributionform',
                        choices = c("", unique(as.character(hetdata$Distribution.form[hetdata$Heterogeneity.statistic==input$hetstat &
                                                                                        hetdata$Data.Type==input$datatype &
                                                                                        hetdata$Effect.Measure==input$effectmeasure]))))
    })

    shiny::observeEvent(input$distributionform,{
      v$buttonclick <- 0
      v$priorid <- NULL
      shiny::updateSelectInput(session,'interventiontype',
                        choices = c("", unique(as.character(hetdata$Type.of.Intervention[hetdata$Heterogeneity.statistic==input$hetstat &
                                                                                           hetdata$Data.Type==input$datatype &
                                                                                           hetdata$Effect.Measure==input$effectmeasure &
                                                                                           hetdata$Distribution.form==input$distributionform]))))
    })

    shiny::observeEvent(input$interventiontype,{
      v$buttonclick <- 0
      v$priorid <- NULL
      shiny::updateSelectInput(session,'natureoutcome',
                        choices = c("", unique(as.character(hetdata$Nature.of.Outcome[hetdata$Heterogeneity.statistic==input$hetstat &
                                                                                        hetdata$Data.Type==input$datatype &
                                                                                        hetdata$Effect.Measure==input$effectmeasure &
                                                                                        hetdata$Distribution.form==input$distributionform &
                                                                                        hetdata$Type.of.Intervention==input$interventiontype]))))
    })

    shiny::observeEvent(input$natureoutcome,{
      v$buttonclick <- 0
      v$priorid <- NULL
      shiny::updateSelectInput(session,'medicalarea',
                        choices = c("", unique(as.character(hetdata$Medical.area[hetdata$Heterogeneity.statistic==input$hetstat &
                                                                                   hetdata$Data.Type==input$datatype &
                                                                                   hetdata$Effect.Measure==input$effectmeasure &
                                                                                   hetdata$Distribution.form==input$distributionform &
                                                                                   hetdata$Type.of.Intervention==input$interventiontype &
                                                                                   hetdata$Nature.of.Outcome==input$natureoutcome
                                                                                 ]))))
    })

    shiny::observeEvent(input$medicalarea,{
      v$buttonclick <- 0
      v$priorid <- NULL
      shiny::updateSelectInput(session,'samplesize',
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
      shiny::renderUI({
        if (is.null(input$hetstat)){}else{
          if (input$hetstat!=""){
            shiny::selectizeInput("datatype", label = "",
                           choices = NULL,
                           options = list(placeholder = 'Data type:',
                                          onInitialize = I('function() { this.setValue(""); }')))
          }}})

    output$effectmeasureui <-
      shiny::renderUI({
        if (is.null(input$datatype)){}else{
          if (input$hetstat!="" & input$datatype!=""){
            shiny::selectizeInput("effectmeasure", label = "",
                           choices = NULL,
                           options = list(placeholder = 'Effect measure:',
                                          onInitialize = I('function() { this.setValue(""); }')))
          }}})

    output$distformui <-
      shiny::renderUI({
        if (is.null(input$effectmeasure)){}else{
          if (input$hetstat!="" & input$datatype!="" & input$effectmeasure!=""){
            shiny::selectizeInput("distributionform", label = "",
                           choices = NULL,
                           options = list(placeholder = 'Distribution form:',
                                          onInitialize = I('function() { this.setValue(""); }')))
          }}})

    output$interventiontypeui <-
      shiny::renderUI({
        if (is.null(input$distributionform)){}else{
          if (input$hetstat!="" & input$datatype!="" & input$effectmeasure!="" & input$distributionform!=""){
            shiny::selectizeInput("interventiontype", label = "",
                           choices = NULL,
                           options = list(placeholder = 'Type of intervention:',
                                          onInitialize = I('function() { this.setValue(""); }')))
          }}})

    output$natureoutcomeui <-
      shiny::renderUI({
        if (is.null(input$interventiontype)){}else{
          if (input$hetstat!="" & input$datatype!="" & input$effectmeasure!="" & input$distributionform!="" & input$interventiontype!=""){
            shiny::selectizeInput("natureoutcome", label = "",
                           choices = NULL,
                           options = list(placeholder = 'Nature of Outcome:',
                                          onInitialize = I('function() { this.setValue(""); }')))
          }}})

    output$medicalareaui <-
      shiny::renderUI({
        if (is.null(input$natureoutcome)){}else{
          if (input$hetstat!="" &
              input$datatype!="" &
              input$effectmeasure!="" &
              input$distributionform!="" &
              input$interventiontype!="" &
              input$natureoutcome!=""
              ){
            shiny::selectizeInput("medicalarea", label = "",
                           choices = NULL,
                           options = list(placeholder = 'Medical area:',
                                          onInitialize = I('function() { this.setValue(""); }')))
          }}})

    output$samplesizeui <-
      shiny::renderUI({
        if (is.null(input$natureoutcome)){}else{
          if (input$hetstat!="" &
              input$datatype!="" &
              input$effectmeasure!="" &
              input$distributionform!="" &
              input$interventiontype!="" &
              input$natureoutcome!=""
              ){
            p("Average sample size")
            shiny::selectizeInput("samplesize", label = "",
                           choices = NULL,
                           options = list(placeholder = 'Average sample size:',
                                          onInitialize = I('function() { this.setValue(""); }')))
          }}})

    output$actiondoui <-
      shiny::renderUI({
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
            shiny::actionButton("do", "Submit")
            }}})


shiny::observeEvent(input$do, {
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

shiny::observeEvent(input$clear, {

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

output$priorid <- shiny::renderText(v$priorid)
output$mean <- shiny::renderText(v$mean)
output$sd <- shiny::renderText(v$sd)
output$median <- shiny::renderText(as.character(v$median))
output$reference <- shiny::renderText(as.character(v$reference))
output$lowquant <- shiny::renderText(as.character(v$lowquant))
output$highquant <- shiny::renderText(as.character(v$highquant))
output$notes1 <- shiny::renderText(as.character(v$notes1))

 output$prioridui <-
 shiny::renderUI({if(v$buttonclick!=0){shiny::p(shiny::em("Prior ID:"), shiny::textOutput("priorid"))}})

 output$meanui <-
 shiny::renderUI({if(v$buttonclick!=0){shiny::p(shiny::em("Mean/Shape:"), shiny::textOutput("mean"))}})

 output$sdui <-
 shiny::renderUI({if(v$buttonclick!=0){shiny::p(shiny::em("Standard deviation/scale:"), shiny::textOutput("sd"))}})

 output$clearui <-
 shiny::renderUI({if(v$buttonclick!=0){shiny::actionButton("clear","Clear")}})


    shiny::observeEvent(input$done, {
      shiny::stopApp(v$priorid)
    })
    shiny::observeEvent(input$cancel, {
      shiny::stopApp(stop("No prior id.", call. = FALSE))
    })
  }

  shiny::runGadget(ui, server, viewer = shiny::dialogViewer("Prior ID", width = 900, height = 800))
}
