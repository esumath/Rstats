#' SLR_Model UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_SLR_Model_ui <- function(id){
  ns <- NS(id)

  fluidPage(
    withMathJax(),
    sidebarLayout(
      sidebarPanel(          
        
        h3("Simple Linear Regression Analysis"),
        br(),  
selectizeInput(ns("numvar1"), label = "Select a response variable (y)", 
              choices = names(mtcars), multiple = FALSE), 
selectizeInput(ns("numvar2"), label = "Select an explanatory variable (x)", 
            choices = names(mtcars), multiple = FALSE), 
radioButtons(
  inputId = ns("SLR"),
  label = "Types of analysis",
  choices = c(
    "Correlation" = "correlation",
    "Scatter plot" = "scatterplot",
    "Model fit" = "slrfit",
    "Analysis of residuals" = "residuals"
  )
),
conditionalPanel(
  condition = "input.SLR == 'correlation'",
  selectizeInput(ns("corrtype"), label = "Type of correlation", 
                 choices = c("Pearson" = "pearson",
                             "Spearman rank" = "spearman" 
                 )),   ns=ns
),
conditionalPanel(
  condition = "input.SLR == 'scatterplot'",
  checkboxInput( ns("bygroup"), "Scatter plot by a grouping variable", FALSE),
  conditionalPanel(
    condition = "input.bygroup == true ",
    selectizeInput(ns("grouping"), label = "Select a grouping variable", 
                   choices = names(mtcars), multiple = FALSE), ns=ns
  ),  ns=ns
),
conditionalPanel(
  condition = "input.SLR == 'slrfit'",
  selectizeInput(ns("fittype"), label = "SLR analysis results", 
                 choices = c("Model fit summary" = "modelfit",
                             "CI of the parameters" = "parests", 
                             "ANOVA table" = "anova",
                  "Prediction of the response for future observations" = "prediction"
                                )), 
  conditionalPanel(
    condition = "input.fittype == 'prediction'",
    textInput(ns("newx"), "Enter the future value(s) of the explanatroy variable x:", 
              value = "20, 30", 
              placeholder = "Enter values separated by a comma with decimals as points, e.g. 21, 32.5  etc."),
    radioButtons(
      inputId = ns("predictiontype"),
      label = "Prediction type",
      choices = c(
        "Confidence interval of the mean response" = "confidence",
        "Prediction confidence interval of an individual response" = "prediction"
      )
    ),  ns=ns
  ),
  ns=ns
),
conditionalPanel(
  condition = "input.SLR == 'residuals'",
  selectizeInput(ns("residuals"), label = "Check model assumptions", 
                 choices = c("Histogram of residuals" = "histplot",
                   "QQ plot of residuals" = "normalityplot",
                    "Test normality of residuals" = "normalitytest",
     "Residual plot (check independence and constant variance)" = "residualplot"
                 )),
  ns=ns
),
conditionalPanel(
  condition = "input.SLR == 'scatterplot' | (input.SLR == 'slrfit' & (input.fittype == 'parests' | input.fittype =='prediction')) ",
  sliderInput(ns("alpha"), "Significance level \\(\\alpha = \\)", 
        min=0.001, max=0.2, step=0.01, value=0.05),
  ns=ns
)
        
      ),   

      
 mainPanel(        
        
   htmlOutput(ns("selected_variable")),
   br(), 
   conditionalPanel(
     condition = "input.SLR=='correlation'",
     verbatimTextOutput(ns("correlation")), ns=ns
   ),
   conditionalPanel(
     condition = "input.SLR=='scatterplot'",
     plotOutput(ns("scatterplot"), height="320px"), ns=ns
   ),        
   conditionalPanel(
     condition = "input.SLR=='slrfit' & input.fittype=='modelfit' ",
     verbatimTextOutput(ns("modelfit")), ns=ns
   ),
   conditionalPanel(
     condition = "input.SLR=='slrfit' & input.fittype=='parests' ",
     verbatimTextOutput(ns("parests")), ns=ns
   ), 
   conditionalPanel(
     condition = "input.SLR=='slrfit' & input.fittype=='anova' ",
     verbatimTextOutput(ns("anova")), ns=ns
   ),
   conditionalPanel(
     condition = "input.SLR=='slrfit' & input.fittype=='prediction' ",
     verbatimTextOutput(ns("CI_prediction")), ns=ns
   ),
   conditionalPanel(
     condition = "input.SLR=='residuals' & input.residuals=='histplot' ",
     plotOutput(ns("histplot"), height="320px"), ns=ns
   ),
   conditionalPanel(
     condition = "input.SLR=='residuals' & input.residuals=='normalityplot' ",
     plotOutput(ns("normalityplot"), height="320px"), ns=ns
   ),
   conditionalPanel(
     condition = "input.SLR=='residuals' & input.residuals=='normalitytest' ",
     verbatimTextOutput(ns("normalitytest")), ns=ns
   ),
   conditionalPanel(
     condition = "input.SLR=='residuals' & input.residuals=='residualplot' ",
     plotOutput(ns("residualplot"), height="320px"), ns=ns
   ),
   rclipboard::rclipboardSetup(),
   uiOutput(ns("clip")),
   shinyAce::aceEditor(ns("code_slr"), "", mode = "r", readOnly = TRUE, 
             height = "600px", fontSize = 16, wordWrap = TRUE,
             theme = "chrome")
   
 )    
 
 
    )
  )
}
    
#' SLR_Model Server Functions
#'
#' @noRd 
mod_SLR_Model_server <- function(id, Data, userdir){
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
  updateSelectizeInput(session, "numvar1", 
                choices = c("None" = "none", R.numericnames())  )
  updateSelectizeInput(session, "numvar2", 
                choices = c("None" = "none", R.numericnames())  )
  updateSelectizeInput(session, "grouping", 
                choices = c("None" = "none", R.categoricnames()  ))
})    
    
    
output$selected_variable <- renderText({
  if (is.null(Data())) return(NULL)
  return(paste0("<B>Selected Response Variable: </B>", input$numvar1, "; ",
                " <B>Selected Explanatory Variable: </B>",  input$numvar2, "."  ))
})   
    
output$correlation <- renderPrint({
  Data=Data()
  if (input$numvar1=="none" | input$numvar2=="none") {return(NULL)}
  else if( anyNA(Data[,input$numvar1]) | anyNA(Data[,input$numvar2]) ){
   interpolate(~(df2=df%>%select(var1, var2)%>%na.omit()), 
          df = quote(Data), var1=input$numvar1, var2=input$numvar2, 
          file = "code_slr.R", mydir = userdir, 
          append = FALSE, save_result = TRUE, eval = TRUE)
  interpolate(~(  cor(y=df2$var1, x=df2$var2, method = md)  ), 
        var1=input$numvar1, var2=input$numvar2,
        md=input$corrtype,  file = "code_slr.R", 
        mydir = userdir, append = FALSE, 
        save_result = TRUE, eval = TRUE)
  } else{
  interpolate(~(  cor(y=df$var1, x=df$var2, method = md)  ), 
          df = quote(Data), var1=input$numvar1, var2=input$numvar2,
          md=input$corrtype,  file = "code_slr.R", 
          mydir = userdir, append = FALSE, save_result = TRUE, eval = TRUE)
    }
})


output$scatterplot<- renderPlot({
  if (input$numvar1=="none" | input$numvar2=="none") {return(NULL)}
  else{
    Data=Data() 
    if (input$bygroup==FALSE | input$grouping=="none"){
interpolate(~( ggplot(df, aes(y=var1, x=var2)) + geom_point()+
          geom_smooth(method=lm, formula = y~x, se=TRUE, level=level)+
            labs(title="Plot of fit with confidence band") ), 
     df = quote(Data), var1=as.name(input$numvar1), var2=as.name(input$numvar2), 
      level=1-input$alpha,  file = "code_slr.R", 
    mydir = userdir, append = FALSE, save_result = TRUE, eval = TRUE)
    } else{
interpolate(~( ggplot(df, aes(y=var1, x=var2, color=var3)) + geom_point()+
               geom_smooth(aes(group=1),method=lm, formula = y~x, se=TRUE, level=level)+
               labs(title="Plot of fit with confidence band") ), 
        df = quote(Data), var1=as.name(input$numvar1), var2=as.name(input$numvar2), 
             var3=as.name(input$grouping), level=1-input$alpha,  file = "code_slr.R", 
              mydir = userdir, append = FALSE, save_result = TRUE, eval = TRUE)      
    }
  }
})

output$modelfit <- renderPrint({
  if (input$numvar1=="none" | input$numvar2=="none") {return(NULL)}
  else{
  Data=Data() 
  interpolate(~(  fit=lm(var1~var2, data=df)  ), 
              df = quote(Data), var1=as.name(input$numvar1), var2=as.name(input$numvar2),
              file = "code_slr.R", 
              mydir = userdir, append = FALSE, save_result = TRUE, eval = TRUE)
  interpolate(~( summary(fit)  ), 
              file = "code_slr.R", 
              mydir = userdir, append = FALSE, save_result = TRUE, eval = TRUE)
          }
})


fit <- reactive ({ 
  if (input$numvar1=="none" | input$numvar2=="none") {return(NULL)}
  else{
  Data=Data() 
  interpolate2(~( fit=lm(var1~var2, data=df) ), 
          df = quote(Data), var1=as.name(input$numvar1), 
          var2=as.name(input$numvar2)
            )
  }
})


output$parests <- renderPrint({
  fit=fit()
  if (is.null(fit)) { return(NULL)} 
  else {
  interpolate(~( confint(fit, level=lvl)  ), 
              lvl=1-input$alpha, file = "code_slr.R", 
              mydir = userdir, append = FALSE, save_result = TRUE, eval = TRUE)
  }
})

output$anova <- renderPrint({
  fit=fit()
  if (is.null(fit)) { return(NULL)} 
  else {
  interpolate(~( anova(fit)  ), 
               file = "code_slr.R", 
              mydir = userdir, append = FALSE, save_result = TRUE, eval = TRUE)
  }
})


output$CI_prediction <- renderPrint({
  fit=fit()
  if (is.null(fit)) { return(NULL)} 
  else {
  interpolate(~(  x=x0  ), 
              x=as.name(input$numvar2),
              x0=as.numeric(unlist(strsplit(input$newx,","))),  file = "code_slr.R", 
              mydir = userdir, append = FALSE, save_result = TRUE, eval = TRUE)
  interpolate(~(  new=data.frame(x)  ), 
              x=as.name(input$numvar2), file = "code_slr.R", 
              mydir = userdir, append = FALSE, save_result = TRUE, eval = TRUE) 
  interpolate(~( predict(fit, newdata=new, interval=intervaltype, level=lvl)  ),
              intervaltype= input$predictiontype,
               lvl=1-input$alpha, file = "code_slr.R", 
              mydir = userdir, append = FALSE, save_result = TRUE, eval = TRUE)
  }
})




output$histplot <- renderPlot({
  fit=fit()
  if (is.null(fit)) { return(NULL)} 
  else {
  interpolate(~(  hist(fit$residuals)  ), 
          file = "code_slr.R", 
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
              file = "code_slr.R", 
              mydir = userdir, append = FALSE, save_result = TRUE, eval = TRUE) 
  }
})

output$normalitytest <- renderPrint({
  fit=fit()
  if (is.null(fit)) { return(NULL)} 
  else {
  interpolate(~( shapiro.test(fit$residuals)  ), 
               file = "code_slr.R", 
              mydir = userdir, append = FALSE, save_result = TRUE, eval = TRUE)
  }
})

output$residualplot <- renderPlot({
  fit=fit()
  if (is.null(fit)) { return(NULL)} 
  else {
  interpolate(~(  plot(fit$fitted.values, fit$residuals)  ), 
              file = "code_slr.R", 
              mydir = userdir, append = FALSE, save_result = TRUE, eval = TRUE) 
  }
})


code <- reactiveFileReader(3000, session, file.path(userdir, "code_all.R"), clean_readlines)
observe({    
  updateAceEditor(session, "code_slr", 
                  value = paste(code(), 
                                collapse = "\n"))
}) 
output$clip <- renderUI({
  rclipButton("clipbtn", "Copy-R-code-to-clipboard", input$code_slr, icon=icon("clipboard", class = "btn btn-success"))
})
 
   
    
 
  })
}
    
