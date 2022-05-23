#' One_Way UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_One_Way_ui <- function(id){
  ns <- NS(id)

  fluidPage(

    sidebarLayout(
      sidebarPanel(          
        
        h3("One-way ANOVA"),
        br(), 
selectizeInput(ns("numvar"), label = "Select a response variable (y)", 
                choices = names(mtcars), multiple = FALSE), 
selectizeInput(ns("catvar"), label = "Select an factor", 
               choices = names(mtcars), multiple = FALSE), 
radioButtons(
  inputId = ns("oneway"),
  label = "Types of analysis",
  choices = c(
    "Descriptive statistics and plots" = "descriptive",
    "Model fit" = "modelfit",
    "Analysis of residuals" = "residuals",
    "Post hoc multiple comparisons" = "posthoc"
  )
),
conditionalPanel(
  condition = "input.oneway == 'descriptive'",
  selectizeInput(ns("descriptive"), label = "Data summary", 
                 choices = c("Summmary statistics" = "summary",
                             "Boxplot with jitter" = "boxplot",
                             "Histogram" = "histplot1")
                           ), 
  checkboxInput( ns("bygroup"), "by the selected factor", TRUE),
  conditionalPanel(
    condition = "input.descriptive == 'histplot1'", 
    sliderInput(ns("bins"), "Number of bins:",
                min = 1,   max = 50,   value = 5),
           ns=ns),
  ns=ns
),
conditionalPanel(
  condition = "input.oneway == 'modelfit'",
  selectizeInput(ns("modelfit"), label = "One-way ANOVA results", 
                 choices = c("Model fit summary" = "fitsummary",
                             "ANOVA table" = "anova")
                    ),
  ns=ns
),
conditionalPanel(
  condition = "input.oneway == 'residuals'",
  selectizeInput(ns("residuals"), label = "Check model assumptions", 
                 choices = c("Histogram of residuals" = "histplot2",
                             "QQ plot of residuals" = "normalityplot",
                             "Test normality of residuals" = "normalitytest",
          "Residual plot (check independence and constant variance)" = "residualplot",
          "Test of constant variance" = "variancetest"
                 )),
  conditionalPanel(
    condition = "input.residuals == 'variancetest'",
    radioButtons(
      inputId = ns("variancetest"),
      label = "Choose a test",
      choices = c(
        "Bartlett's test" = "variancetest1",
        "Levene's test" = "variancetest2",
        "Fligner-Killeen test " = "variancetest3"
      )
    ), ns=ns),
  ns=ns
),
conditionalPanel(
  condition = "input.oneway == 'posthoc'", 
  radioButtons(
    inputId = ns("posthoc"),
    label = "Choose a test",
    choices = c(
      "Bonferroni test" = "mtest1",
      "Holm procedure" = "mtest2",
      "Tukey's test " = "mtest3"
    )
  ),
conditionalPanel(
    condition = "input.posthoc == 'mtest3'",
sliderInput(ns("alpha"), "Significance level \\(\\alpha = \\)", 
            min=0.001, max=0.2, step=0.01, value=0.05),
ns=ns),
ns=ns
)
        
),    
      
mainPanel(  
  
  htmlOutput(ns("selected_variable")),
  br(),
  conditionalPanel(
    condition = "input.oneway=='descriptive' & input.descriptive=='summary'",
    verbatimTextOutput(ns("summary")), ns=ns
  ),
  conditionalPanel(
    condition = "input.oneway=='descriptive' & input.descriptive=='boxplot'",
    plotOutput(ns("boxplot"), height="320px"), ns=ns
  ),
  conditionalPanel(
    condition = "input.oneway=='descriptive' & input.descriptive=='histplot1'",
    plotOutput(ns("histplot1"), height="320px"), ns=ns
  ),  
  conditionalPanel(
    condition = "input.oneway=='modelfit' & input.modelfit=='fitsummary'",
    verbatimTextOutput(ns("fitsummary")), ns=ns
  ), 
  conditionalPanel(
    condition = "input.oneway=='modelfit' & input.modelfit=='anova'",
    verbatimTextOutput(ns("anova")), ns=ns
  ),   
  conditionalPanel(
    condition = "input.oneway=='residuals' & input.residuals=='histplot2'",
    plotOutput(ns("histplot2"), height="320px"), ns=ns
  ), 
  conditionalPanel(
    condition = "input.oneway=='residuals' & input.residuals=='normalityplot'",
    plotOutput(ns("normalityplot"), height="320px"), ns=ns
  ),  
  conditionalPanel(
    condition = "input.oneway=='residuals' & input.residuals=='normalitytest'",
    verbatimTextOutput(ns("normalitytest")), ns=ns
  ), 
  conditionalPanel(
    condition = "input.oneway=='residuals' & input.residuals=='residualplot'",
    plotOutput(ns("residualplot"), height="320px"), ns=ns
  ), 
  conditionalPanel(
    condition = "input.oneway=='residuals' & input.residuals=='variancetest' & input.variancetest=='variancetest1'",
    verbatimTextOutput(ns("variancetest1")), ns=ns
  ),
  conditionalPanel(
    condition = "input.oneway=='residuals' & input.residuals=='variancetest' & input.variancetest=='variancetest2'",
    verbatimTextOutput(ns("variancetest2")), ns=ns
  ),
  conditionalPanel(
    condition = "input.oneway=='residuals' & input.residuals=='variancetest' & input.variancetest=='variancetest3'",
    verbatimTextOutput(ns("variancetest3")), ns=ns
  ),
  conditionalPanel(
    condition = "input.oneway=='posthoc' & input.posthoc=='mtest1' ",
    verbatimTextOutput(ns("mtest1")), ns=ns
  ),
  conditionalPanel(
    condition = "input.oneway=='posthoc' & input.posthoc=='mtest2' ",
    verbatimTextOutput(ns("mtest2")), ns=ns
  ),
  conditionalPanel(
    condition = "input.oneway=='posthoc' & input.posthoc=='mtest3' ",
    verbatimTextOutput(ns("mtest3")), ns=ns
  ),
  rclipboard::rclipboardSetup(),
  uiOutput(ns("clip")),  
  shinyAce::aceEditor(ns("code_oneway"), "", mode = "r", readOnly = TRUE, 
            height = "600px", fontSize = 16, wordWrap = TRUE,
            theme = "chrome")
  
)   
    )
  )
}

    
#' One_Way Server Functions
#'
#' @noRd 
mod_One_Way_server <- function(id, Data, userdir){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    R.numericnames <- reactive({
      if (is.null(Data())) return(NULL)
      return(numericNames(Data()))
    })  
    R.categoricnames <- reactive({
      if (is.null(Data())) return(NULL)
      return(categoricNames(Data()))
    })     
    
    observe({
      updateSelectizeInput(session, "numvar", 
                           choices = c("None" = "none", R.numericnames())  )
      updateSelectizeInput(session, "catvar", 
                           choices = c("None" = "none", R.categoricnames()  ))
    })    
  
   
output$selected_variable <- renderText({
  if (is.null(Data())) return(NULL)
  return(paste0("<B>Selected Response Variable: </B>", input$numvar, "; ",
                    " <B>Selected Factor: </B>",  input$catvar, "."  ))
    })   

output$summary <- renderPrint({
  Data=Data()
  if (input$numvar=="none" | input$catvar=="none") {return(NULL)}
  else{
    if (input$bygroup==TRUE){
interpolate( ~(datasummary= tapply(df$var1, df$var2,
                             function(x) format(summary(x) )) ), 
             df = quote(Data), var1=input$numvar, var2= input$catvar,
             file = "code_oneway.R", 
             mydir = userdir, append = FALSE, save_result = TRUE, eval = TRUE) 
  if( anyNA(Data[,input$numvar]) | anyNA(Data[,input$catvar]) )
  {
interpolate(~( meanvar<- df%>%select(var1, var2)%>%na.omit()%>%group_by(var2)%>%
            summarize(mean=mean(var1), var=var(var1))%>%as.data.frame() ), 
            df = quote(Data), var1=as.name(input$numvar),  
            var2=as.name(input$catvar), file = "code_oneway.R", 
            mydir = userdir, append = FALSE, save_result = TRUE, eval = TRUE)
     } else{
interpolate(~( meanvar<- df%>%group_by(var2)%>%
          summarize(mean=mean(var1), var=var(var1))%>%as.data.frame() ), 
           df = quote(Data), var1=as.name(input$numvar),  
           var2=as.name(input$catvar), file = "code_oneway.R", 
           mydir = userdir, append = FALSE, save_result = TRUE, eval = TRUE)
    }
interpolate( ~( list(datasummary, meanvar) ), 
       file = "code_oneway.R", 
       mydir = userdir, append = FALSE, save_result = TRUE, eval = TRUE)     
    } else{
interpolate( ~(datasummary= summary(df$var1) ), 
           df = quote(Data), var1=input$numvar, file = "code_oneway.R", 
           mydir = userdir, append = FALSE, save_result = TRUE, eval = TRUE) 
if( anyNA(Data[,input$numvar]) ){
interpolate(~( meanvar<- df%>%select(var1)%>%na.omit()%>%
          summarize(mean=mean(var1), var=var(var1))%>%as.data.frame() ), 
          df = quote(Data), var1=as.name(input$numvar), file = "code_oneway.R", 
          mydir = userdir, append = FALSE, save_result = TRUE, eval = TRUE)
} else{
interpolate(~( meanvar<- df%>%summarize(mean=mean(var1), var=var(var1))%>%as.data.frame() ), 
          df = quote(Data), var1=as.name(input$numvar), file = "code_oneway.R", 
          mydir = userdir, append = FALSE, save_result = TRUE, eval = TRUE)
  }
    interpolate( ~( list(datasummary, meanvar) ), 
    file = "code_oneway.R", 
    mydir = userdir, append = FALSE, save_result = TRUE, eval = TRUE)
      }
  }
})

output$boxplot<- renderPlot({
  if (input$numvar=="none" | input$catvar=="none") {return(NULL)}
  else{
    Data=Data()
    if (input$bygroup==TRUE){
interpolate(~(  df%>%ggplot(aes(y=var1,x=var2))+ 
                  geom_boxplot()+
                  geom_jitter(aes(color=var2)) ), 
                df = quote(Data), var1=as.name(input$numvar),  
                var2=as.name(input$catvar), file = "code_oneway.R", 
        mydir = userdir, append = FALSE, save_result = TRUE, eval = TRUE)
    } else{
interpolate(~(  df%>%ggplot(aes(y=var1,x=""))+ 
                geom_boxplot()+
                geom_jitter(color="blue") ), 
              df = quote(Data), var1=as.name(input$numvar), file = "code_oneway.R", 
              mydir = userdir, append = FALSE, save_result = TRUE, eval = TRUE)      
    }
  }
})

output$histplot1<- renderPlot({
  if (input$numvar=="none" | input$catvar=="none") {return(NULL)}
  else{
    Data=Data()
    if (input$bygroup==TRUE)
    {
interpolate(~( df%>%ggplot(aes(x=var1))+ 
             geom_histogram(aes(fill=var2),bins=bins)+facet_wrap(~var2) ), 
          df = quote(Data), var1=as.name(input$numvar),var2=as.name(input$catvar), 
          bins=input$bins, file = "code_oneway.R", 
          mydir = userdir, append = FALSE, save_result = TRUE, eval = TRUE)      
    } else
    {
interpolate(~( df%>%ggplot(aes(x=var1))+ 
           geom_histogram(fill="blue",bins=bins) ), 
          df = quote(Data), var1=as.name(input$numvar), 
          bins=input$bins, file = "code_oneway.R", 
          mydir = userdir, append = FALSE, save_result = TRUE, eval = TRUE)
    }
  }
})

output$fitsummary <- renderPrint({
  if (input$numvar=="none" | input$catvar=="none") {return(NULL)}
  else{
    Data=Data()
    interpolate( ~( fit=lm(var1 ~ var2, data= df) ), 
                 df = quote(Data), var1=as.name(input$numvar), var2=as.name(input$catvar),
                 file = "code_oneway.R", 
                 mydir = userdir, append = FALSE, save_result = TRUE, eval = TRUE) 
    interpolate( ~( summary(fit) ), 
              file = "code_oneway.R", 
              mydir = userdir, append = FALSE, save_result = TRUE, eval = TRUE) 
  }
})

fit <- reactive ({ 
  if (input$numvar=="none" | input$catvar=="none") {return(NULL)}
  else{
    Data=Data()  
    interpolate2(~( fit=lm(var1~var2, data=df) ), 
      df = quote(Data), var1=as.name(input$numvar), var2= as.name(input$catvar) )
    return(fit)
  }
})

output$anova <- renderPrint({
  if ( is.null(fit()) ) {return(NULL)}
  else{
    fit=fit()
    interpolate( ~( anova(fit) ), 
                 file = "code_oneway.R", 
                 mydir = userdir, append = FALSE, save_result = TRUE, eval = TRUE)
    }
})

output$histplot2 <- renderPlot({
  fit=fit()
  if (is.null(fit)) { return(NULL)} 
  else {
    interpolate(~(  hist(fit$residuals)  ), 
                file = "code_oneway.R", 
                mydir = userdir, append = FALSE, save_result = TRUE, eval = TRUE) 
  }
})

output$normalityplot <- renderPlot({
  fit=fit()
  if (is.null(fit)) { return(NULL)} 
  else {
    interpolate(~(library(car)), file = "code_all.R", 
                mydir = userdir, append = TRUE, nodupes = TRUE)
    interpolate(~(  car::qqPlot(fit$residuals)  ), 
                file = "code_oneway.R", 
                mydir = userdir, append = FALSE, save_result = TRUE, eval = TRUE) 
  }
})

output$normalitytest <- renderPrint({
  fit=fit()
  if (is.null(fit)) { return(NULL)} 
  else {
    interpolate(~( shapiro.test(fit$residuals)  ), 
                file = "code_oneway.R", 
                mydir = userdir, append = FALSE, save_result = TRUE, eval = TRUE)
  }
})

output$residualplot <- renderPlot({
  fit=fit()
  if (is.null(fit)) { return(NULL)} 
  else {
    interpolate(~(  plot(fit$fitted.values, fit$residuals)  ), 
                file = "code_oneway.R", 
                mydir = userdir, append = FALSE, save_result = TRUE, eval = TRUE) 
  }
})

output$variancetest1 <- renderPrint({
  if (is.null(fit())) { return(NULL)} 
  else {
    Data=Data()
    interpolate(~( bartlett.test(var1 ~ var2, data = df) ), 
                df = quote(Data), var1=as.name(input$numvar), 
                var2= as.name(input$catvar), file = "code_oneway.R", 
                mydir = userdir, append = FALSE, save_result = TRUE, eval = TRUE)
  }
})

output$variancetest2 <- renderPrint({
  if (is.null(fit())) { return(NULL)} 
  else {
    Data=Data()
    interpolate(~( car::leveneTest(var1 ~ var2, data = df) ), 
    df = quote(Data), var1=as.name(input$numvar), 
    var2= as.name(input$catvar), file = "code_oneway.R", 
    mydir = userdir, append = FALSE, save_result = TRUE, eval = TRUE)
  }
})

output$variancetest3 <- renderPrint({
  if (is.null(fit())) { return(NULL)} 
  else {
    Data=Data()
    interpolate(~( fligner.test(var1 ~ var2, data = df) ), 
                df = quote(Data), var1=as.name(input$numvar), 
                var2= as.name(input$catvar), file = "code_oneway.R", 
                mydir = userdir, append = FALSE, save_result = TRUE, eval = TRUE)
  }
})

output$mtest1 <- renderPrint({
  if (is.null(fit())) { return(NULL)} 
  else {
    Data=Data()
    interpolate(~( pairwise.t.test(x=df$var1, g=df$var2, p.adj = "bonf") ), 
                df = quote(Data), var1=input$numvar, 
                var2= input$catvar, file = "code_oneway.R", 
                mydir = userdir, append = FALSE, save_result = TRUE, eval = TRUE)
  }
})

output$mtest2 <- renderPrint({
  if (is.null(fit())) { return(NULL)} 
  else {
    Data=Data()
    interpolate(~( pairwise.t.test(x=df$var1, g=df$var2, p.adj = "holm") ), 
                df = quote(Data), var1=input$numvar, 
                var2= input$catvar, file = "code_oneway.R", 
                mydir = userdir, append = FALSE, save_result = TRUE, eval = TRUE)
  }
})

output$mtest3 <- renderPrint({
  if (is.null(fit())) { return(NULL)} 
  else {
    fit=fit()
    interpolate(~( TukeyHSD(aov(fit), conf.level = clevel) ), 
                clevel=1-input$alpha, file = "code_oneway.R", 
                mydir = userdir, append = FALSE, save_result = TRUE, eval = TRUE)
  }
})

code <- reactiveFileReader(5000, session, file.path(userdir, "code_all.R"), clean_readlines)
observe({    
  updateAceEditor(session, "code_oneway", 
          value = paste(code(), collapse = "\n"))
}) 
output$clip <- renderUI({
  rclipButton("clipbtn", "Copy-R-code-to-clipboard", input$code_oneway, icon=icon("clipboard", class = "btn btn-success"))
})

  })
}
    
