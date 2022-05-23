#' MLR_Model UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_MLR_Model_ui <- function(id){
  ns <- NS(id)

  fluidPage(
    withMathJax(),
    sidebarLayout(
      sidebarPanel(        
        
        h3("Multiple Linear Regression Analysis"),
        br(),  
        radioButtons(
          inputId = ns("models"),
          label = "Choose a multiple linear regression model",
          choices = c(
            "Polynomial regression" = "MLR1",
            "General multiple linear regression" = "MLR2",
            "Logistic regression" = "MLR3"
          )
        ),
      conditionalPanel(
          condition = "input.models == 'MLR1' | input.models == 'MLR2'",
selectizeInput(ns("numvar0"), label = "Select a response variable (y)", 
                       choices = names(mtcars), multiple = FALSE),
ns=ns),
conditionalPanel(
  condition = "input.models == 'MLR1'",
  selectizeInput(ns("numvar1"), label = "Select an independent variable (x)", 
                 choices = names(mtcars), multiple = FALSE),
textInput(ns("degree"), "Enter a degree for the polynomial regression model", 
            value = "2", 
            placeholder = "Enter an integer greater than 1"),
ns=ns),
conditionalPanel(
  condition = "input.models == 'MLR3'",
  selectizeInput(ns("binvar"), label = "Select a binary response variable", 
                 choices = NULL,  multiple = FALSE),
  ns=ns
),
conditionalPanel(
  condition = "input.models == 'MLR2' | input.models == 'MLR3'",
  selectizeInput(ns("numvar2"), label = "Select independent variables (x's)", 
                 choices = names(mtcars), multiple = TRUE),
  ns=ns
),
conditionalPanel(
condition = "(input.models != 'MLR3' & input.MLR == 'scatterplot') | (input.models == 'MLR3' & input.analysis == 'scatterplot')",
selectizeInput(ns("grouping1"), label = "Select a grouping variable for the scatter plot (matrix)", 
                 choices = names(mtcars), selected="none", multiple = FALSE),
  ns=ns),
conditionalPanel(
  condition = "(input.models == 'MLR2' & (input.MLR =='mlrfit' | input.MLR =='residuals')) | (input.models == 'MLR3' & (input.analysis == 'modelfit' | input.analysis == 'predictions'))",
  selectizeInput(ns("grouping2"), label = "Select grouping variables", 
          choices = names(mtcars), selected="none", multiple = TRUE),
  ns=ns
),
conditionalPanel(
  condition = "input.models == 'MLR1' | input.models == 'MLR2'",
radioButtons(
  inputId = ns("MLR"),
  label = "Types of analysis",
  choices = c(
    "Correlations" = "correlation",
    "Scatter plot" = "scatterplot",
    "Model fit" = "mlrfit",
    "Analysis of residuals" = "residuals"
  )
),ns=ns
),
conditionalPanel( 
  condition = "input.models == 'MLR3'",  
  radioButtons(
    inputId = ns("analysis"),
    label = "Types of analysis",
    choices = c(
      "Scatter plot" = "scatterplot",
      "Model fit" = "modelfit",
      "Predictions" = "predictions"
    ) ),  ns=ns
),
conditionalPanel(
  condition = "input.MLR == 'correlation' & input.models != 'MLR3'",
  selectizeInput(ns("corrtype"), label = "Type of correlation", 
                 choices = c("Pearson" = "pearson",
                             "Spearman rank" = "spearman" 
                 )),   ns=ns
),
conditionalPanel(
  condition = "input.models=='MLR1' & input.MLR == 'mlrfit'",
  selectizeInput(ns("fittype1"), label = "Polynomial regression analysis results", 
                 choices = c("Model fit summary" = "modelfit1",
                             "CI of the parameters" = "parests1", 
                             "ANOVA table" = "anova1",
              "Prediction of the response for a future observation" = "prediction1"
                 )), 
  conditionalPanel(
    condition = "input.fittype1 == 'CI_mean1' | input.fittype1 == 'prediction1' ",
    textInput(ns("newx1"), "Enter the future value(s) of the explanatroy variable x:", 
              value = "20, 30", 
              placeholder = "Enter values separated by a comma with decimals as points, e.g. 21, 32.5  etc."),
    radioButtons(
      inputId = ns("predictiontype1"),
      label = "Prediction type",
      choices = c(
        "Confidence interval of the mean response" = "confidence",
        "Prediction confidence interval of an individual response" = "prediction"
               )
         ),
    ns=ns
  ),
  ns=ns
),
conditionalPanel(
  condition = "input.models=='MLR2' & input.MLR == 'mlrfit'",
  selectizeInput(ns("fittype2"), label = "MLR analysis results", 
                 choices = c("Model fit summary" = "modelfit2",
                             "CI of the parameters" = "parests2", 
                             "ANOVA table" = "anova2",
       "Prediction of the response for future observations" = "prediction2"
                                     )), 
  conditionalPanel(
    condition = "input.fittype2 =='prediction2' ",
    textInput(ns("newx2"), "Enter a vector for the future value of the explanatroy variables x's:", 
        value = "20, 30", 
        placeholder = "Enter values separated by a comma with decimals as points, e.g. 21, 32.5  etc."),
    radioButtons(
      inputId = ns("predictiontype2"),
      label = "Prediction type",
      choices = c(
        "Confidence interval of the mean response" = "confidence",
        "Prediction confidence interval of an individual response" = "prediction"
           )
    ),    ns=ns
  ),
  ns=ns
),
conditionalPanel(
  condition = "input.models == 'MLR3' & input.analysis == 'modelfit'",
  selectizeInput(ns("estimations"), label = "Fit the logistic regression model", 
                 choices = c("Model fit summary" = "summary",
                             "CI of the parameters" = "parests"
                 )),   
  ns=ns 
),
conditionalPanel(
  condition = "(input.models == 'MLR2' & input.MLR == 'mlrfit') | (input.models == 'MLR3' & (input.analysis == 'modelfit' | input.analysis =='predictions'))",
checkboxInput( ns("factorasx"), "Add the grouping variable(s) to the model", FALSE),
ns=ns
),
conditionalPanel(
  condition = "input.models=='MLR1' & input.MLR == 'residuals'",
  selectizeInput(ns("residuals1"), label = "Check model assumptions", 
                 choices = c("Histogram of residuals" = "histplot1",
                             "QQ plot of residuals" = "normalityplot1",
                      "Test normality of residuals" = "normalitytest1",
              "Residual plot (check independence and constant variance)" = "residualplot1"
                 )),
  ns=ns
),
conditionalPanel(
  condition = "input.models=='MLR2' & input.MLR == 'residuals'",
  selectizeInput(ns("residuals2"), label = "Check model assumptions", 
                 choices = c("Histogram of residuals" = "histplot2",
                             "QQ plot of residuals" = "normalityplot2",
                      "Test normality of residuals" = "normalitytest2",
              "Residual plot (check independence and constant variance)" = "residualplot2"
                 )),
  ns=ns
),
conditionalPanel(
  condition = "input.models=='MLR3' & input.analysis == 'predictions'",
  textInput(ns("newx3"), "Enter a vector for the future value of the explanatroy variables x's:", 
            value = "20, 30", 
            placeholder = "Enter values separated by a comma with decimals as points, e.g. 21, 32.5 etc."),
  ns=ns
),
conditionalPanel(
condition = "(input.models=='MLR1' & input.MLR=='scatterplot') | (input.models=='MLR1' & (input.MLR == 'mlrfit' & (input.fittype1 == 'parests1' | input.fittype1 == 'prediction1')))  | (input.models=='MLR2' & (input.MLR == 'mlrfit' & (input.fittype2 == 'parests2' | input.fittype2 == 'prediction2'))) | (input.models=='MLR3' & (input.analysis == 'scatterplot' |input.analysis == 'predictions' | (input.analysis == 'modelfit' & input.estimations=='parests')))",
   sliderInput(ns("alpha"), "Significance level \\(\\alpha = \\)", 
              min=0.001, max=0.2, step=0.01, value=0.05),
  ns=ns
)
      ),   

      
  mainPanel( 
        
conditionalPanel(
  condition = "input.models=='MLR1'",
  htmlOutput(ns("selected_variable1")), ns=ns
        ),
conditionalPanel(
  condition = "input.models=='MLR2'",
  htmlOutput(ns("selected_variable2")), ns=ns
),
conditionalPanel(
  condition = "input.models=='MLR3'",
  htmlOutput(ns("selected_variable3")), ns=ns
),
conditionalPanel(
  condition = "input.models=='MLR1' & input.MLR=='correlation'",
  verbatimTextOutput(ns("correlation1")), ns=ns
),
conditionalPanel(
  condition = "input.models=='MLR1' & input.MLR=='scatterplot'",
  plotOutput(ns("scatterplot1"), height="320px"), ns=ns
),
conditionalPanel(
  condition = "input.models=='MLR1' & input.MLR=='mlrfit' & input.fittype1=='modelfit1' ",
  verbatimTextOutput(ns("modelfit1")), ns=ns
),
conditionalPanel(
  condition = "input.models=='MLR1' & input.MLR=='mlrfit' & input.fittype1=='parests1' ",
  verbatimTextOutput(ns("parests1")), ns=ns
),
conditionalPanel(
  condition = "input.models=='MLR1' & input.MLR=='mlrfit' & input.fittype1=='anova1' ",
  verbatimTextOutput(ns("anova1")), ns=ns
),
conditionalPanel(
  condition = "input.models=='MLR1' & input.MLR=='mlrfit' & input.fittype1=='prediction1' ",
  verbatimTextOutput(ns("CI_prediction1")), ns=ns
),
conditionalPanel(
  condition = "input.models=='MLR1' & input.MLR=='residuals' & input.residuals1=='histplot1' ",
  plotOutput(ns("histplot1"), height="320px"), ns=ns
),
conditionalPanel(
  condition = "input.models=='MLR1' & input.MLR=='residuals' & input.residuals1=='normalityplot1' ",
  plotOutput(ns("normalityplot1"), height="320px"), ns=ns
),
conditionalPanel(
  condition = "input.models=='MLR1' & input.MLR=='residuals' & input.residuals1=='normalitytest1' ",
  verbatimTextOutput(ns("normalitytest1")), ns=ns
),
conditionalPanel(
  condition = "input.models=='MLR1' & input.MLR=='residuals' & input.residuals1=='residualplot1' ",
  plotOutput(ns("residualplot1"), height="320px"), ns=ns
),
conditionalPanel(
  condition = "input.models=='MLR2' & input.MLR=='correlation'",
  verbatimTextOutput(ns("correlation2")), ns=ns
),
conditionalPanel(
  condition = "input.models=='MLR2' & input.MLR=='scatterplot'",
  plotOutput(ns("scatterplot2"), height="320px"), ns=ns
),
conditionalPanel(
  condition = "input.models=='MLR3' & input.analysis=='scatterplot'",
  plotOutput(ns("scatterplot3"), height="320px"), ns=ns
),
conditionalPanel(
  condition = "input.models=='MLR2' & input.MLR=='mlrfit' & input.fittype2=='modelfit2' ",
  verbatimTextOutput(ns("modelfit2")), ns=ns
),
conditionalPanel(
  condition = "input.models=='MLR2' & input.MLR=='mlrfit' & input.fittype2=='parests2' ",
  verbatimTextOutput(ns("parests2")), ns=ns
),
conditionalPanel(
  condition = "input.models=='MLR3' & input.analysis=='modelfit' & input.estimations=='summary'",
  verbatimTextOutput(ns("summary3")), ns=ns
),
conditionalPanel(
  condition = "input.models=='MLR3' & input.analysis=='modelfit' & input.estimations=='parests'",
  verbatimTextOutput(ns("parests3")), ns=ns
),
conditionalPanel(
  condition = "input.models=='MLR2' & input.MLR=='mlrfit' & input.fittype2=='anova2' ",
  verbatimTextOutput(ns("anova2")), ns=ns
),
conditionalPanel(
  condition = "input.models=='MLR2' & input.MLR=='mlrfit' & input.fittype2=='prediction2' ",
  verbatimTextOutput(ns("CI_prediction2")), ns=ns
),
conditionalPanel(
  condition = "input.models=='MLR2' & input.MLR=='residuals' & input.residuals2=='histplot2' ",
  plotOutput(ns("histplot2"), height="320px"), ns=ns
),
conditionalPanel(
  condition = "input.models=='MLR2' & input.MLR=='residuals' & input.residuals2=='normalityplot2' ",
  plotOutput(ns("normalityplot2"), height="320px"), ns=ns
),
conditionalPanel(
  condition = "input.models=='MLR2' & input.MLR=='residuals' & input.residuals2=='normalitytest2' ",
  verbatimTextOutput(ns("normalitytest2")), ns=ns
),
conditionalPanel(
  condition = "input.models=='MLR2' & input.MLR=='residuals' & input.residuals2=='residualplot2' ",
  plotOutput(ns("residualplot2"), height="320px"), ns=ns
),
conditionalPanel(
  condition = "input.models=='MLR3' & input.analysis=='predictions'",
  verbatimTextOutput(ns("respest")), ns=ns
),
rclipboard::rclipboardSetup(),
uiOutput(ns("clip")),              
shinyAce::aceEditor(ns("code_mlr"), "", mode = "r", readOnly = TRUE, 
                  height = "800px", fontSize = 16, wordWrap = TRUE,
                  theme = "chrome")
        
      )   
 
    )
  )
}
  

    
#' MLR_Model Server Functions
#'
#' @noRd 
mod_MLR_Model_server <- function(id, Data, userdir){
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
      updateSelectizeInput(session, "numvar0", 
                           choices = c("None" = "none", R.numericnames())  )
  updateSelectizeInput(session, "binvar", 
                       choices = c("None" = "none", R.categoricnames()  ))
      updateSelectizeInput(session, "numvar1", 
                           choices = c("None" = "none", R.numericnames())  )
      updateSelectizeInput(session, "numvar2", 
                           choices = c( R.numericnames() )  )
  updateSelectizeInput(session, "grouping1", 
            choices = c("None" = "none", R.categoricnames()  ))
  updateSelectizeInput(session, "grouping2",   
            choices = c( R.categoricnames()  ))
    })   
   
output$selected_variable1 <- renderText({
      if (is.null(Data())) return(NULL)
      return(paste0("<B>Selected Response Variable: </B>", input$numvar0, "; ",
                 " <B>Selected Explanatory Variable: </B>",  input$numvar1, "."  ))
    }) 
output$selected_variable2 <- renderText({
    if (is.null(Data())) return(NULL)
    return(paste0("<B>Selected Response Variable: </B>", input$numvar0, "."  ))
  })
output$selected_variable3 <- renderText({
  Data=Data()
  if (is.null(Data()) | input$binvar=="none") {return(NULL)} 
  else if (length( levels(Data[,input$binvar]) ) !=2) {
    return(paste0("<B>The levels of the response variable is not binary! </B>"))
  } else
  {return(paste0("<B>Selected Binary Response Variable: </B>", input$binvar,  "."  ))}
}) 


output$correlation1 <- renderPrint({
  Data=Data()
  if (input$numvar0=="none" | input$numvar1=="none") {return(NULL)}
  else if( anyNA(Data[,input$numvar0]) | anyNA(Data[,input$numvar1]) )
  {
  interpolate(~(df2=df%>%select(var1, var2)%>%na.omit()), 
        df = quote(Data), var1=input$numvar0, var2=input$numvar1, 
        file = "code_mlr.R", mydir = userdir, 
        append = FALSE, save_result = TRUE, eval = TRUE)
  interpolate(~(  cor(y=df2$var1, x=df2$var2, method = md)  ), 
        var1=input$numvar0, var2=input$numvar1,
        md=input$corrtype,  file = "code_mlr.R", 
        mydir = userdir, append = FALSE, save_result = TRUE, eval = TRUE)
    }else{
  interpolate(~(  cor(y=df$var1, x=df$var2, method = md)  ), 
        df = quote(Data), var1=input$numvar0, var2=input$numvar1,
        md=input$corrtype,  file = "code_mlr.R", 
        mydir = userdir, append = FALSE, save_result = TRUE, eval = TRUE)
  }
})

output$correlation2 <- renderPrint({
  Data=Data() 
  if (input$numvar0=="none" | is.null(input$numvar2) ) {return(NULL)}
  else{
    interpolate(~(  dataset=Data[,names(Data) %in% var1 | names(Data) %in% var2]  ), 
                var1=input$numvar0, var2=input$numvar2,  
                file = "code_mlr.R", 
                mydir = userdir, append = FALSE, save_result = TRUE, eval = TRUE)  
  if (anyNA(dataset))
  {
interpolate(~(dataset=dataset%>%na.omit()), 
      file = "code_mlr.R", mydir = userdir, 
      append = FALSE, save_result = TRUE, eval = TRUE)
interpolate(~(  cor( dataset, method = md)  ), 
      md=input$corrtype,  file = "code_mlr.R", 
      mydir = userdir, append = FALSE, save_result = TRUE, eval = TRUE)
  }else{
interpolate(~(  cor( dataset, method = md)  ), 
      md=input$corrtype,  file = "code_mlr.R", 
      mydir = userdir, append = FALSE, save_result = TRUE, eval = TRUE)
     }
         }
})


output$scatterplot1<- renderPlot({
  if (input$numvar0=="none" | input$numvar1=="none") {return(NULL)}
  else {  Data=Data() 
  if (input$grouping1=="none"){
    interpolate(~( ggplot(df, aes(y=var1, x=var2)) + geom_point()+
              geom_smooth(method='lm',formula=y~poly(x,degree), se=TRUE, level=lvl)+
               labs(title="Plot fit with confidence band") ), 
                df = quote(Data), var1=as.name(input$numvar0), var2=as.name(input$numvar1), 
                degree=as.numeric(input$degree), lvl=1-input$alpha, file = "code_mlr.R", 
                mydir = userdir, append = FALSE, save_result = TRUE, eval = TRUE)
  } else{
    interpolate(~( ggplot(df, aes(y=var1, x=var2, color=var3)) + geom_point()+
                     geom_smooth(aes(group=1),method='lm',formula=y~poly(x,degree), se=TRUE, level=lvl)+
                     labs(title="PLot fit with confidence band") ), 
                df = quote(Data), var1=as.name(input$numvar0), var2=as.name(input$numvar1), 
                var3=as.name(input$grouping1), degree=as.numeric(input$degree), 
                lvl=1-input$alpha, file = "code_mlr.R", 
                mydir = userdir, append = FALSE, save_result = TRUE, eval = TRUE)      
  }
  }
})
  

output$scatterplot2<- renderPlot({
  if (input$numvar0=="none" | is.null(input$numvar2) ) {return(NULL)}
  else{
  Data=Data() 
  interpolate(~(  dataset=Data[,names(Data) %in% var1 | names(Data) %in% var2]  ), 
                var1=input$numvar0, var2=input$numvar2,  
                file = "code_mlr.R", 
                mydir = userdir, append = FALSE, save_result = TRUE, eval = TRUE)  
    if (input$grouping1 =="none")
  {
    interpolate(~( pairs(dataset, lower.panel = NULL) ), 
                file = "code_mlr.R", 
                mydir = userdir, append = FALSE, save_result = TRUE, eval = TRUE)
  } else
  { 
  interpolate(~( k=length( levels(df$var) ) ), 
              df=quote(Data), var=input$grouping1, file = "code_mlr.R",
              mydir = userdir, append = FALSE, save_result = TRUE, eval = TRUE)
  interpolate(~( pairs(dataset, lower.panel = NULL, col=c(2:(k+1))[df$var]) ), 
                df=quote(Data), var=input$grouping1, file = "code_mlr.R", 
                mydir = userdir, append = FALSE, save_result = TRUE, eval = TRUE)
  }
}
})

output$scatterplot3<- renderPlot({
  Data=Data()
  if( input$binvar=="none" | is.null(input$numvar2)  ) {return(NULL)}
  else if (length( levels(Data[,input$binvar]) ) !=2 ) {return(NULL)}
  else{  
    interpolate(~( df$var=as.numeric(I(df$var==levels(df$var)[2]))  ), 
                df = quote(Data), var=as.name(input$binvar),  
                file = "code_mlr.R", 
                mydir = userdir, append = FALSE, save_result = TRUE, eval = TRUE)
    if ( input$grouping1=="none" | input$grouping1==input$binvar)
    {
      interpolate(~( ggplot(df, aes(y=var1, x=var2)) + geom_point()+
                       geom_smooth(method=glm, formula=y~x, se=TRUE, level=level,
                                   method.args = list(family=binomial) ) +
                       labs(title="Plot of fit with confidence band") ), 
                  df = quote(Data), var1=as.name(input$binvar), var2=as.name(input$numvar2), 
                  level=1-input$alpha,  file = "code_mlr.R", 
                  mydir = userdir, append = FALSE, save_result = TRUE, eval = TRUE)
    } else
    {
      interpolate(~( ggplot(df, aes(y=var1, x=var2, color=var3)) + geom_point()+
                       geom_smooth(aes(group=1), method=glm, formula=y~x, se=TRUE, level=level,
                                   method.args = list(family=binomial) ) +
                       labs(title="Plot of fit with confidence band") ), 
                  df = quote(Data), var1=as.name(input$binvar), 
                  var2=as.name(input$numvar2),  var3=as.name(input$grouping1),
                  level=1-input$alpha,  file = "code_mlr.R", 
                  mydir = userdir, append = FALSE, save_result = TRUE, eval = TRUE)  
    }
  }
  
})

output$modelfit1 <- renderPrint({
  if (input$numvar0=="none" | input$numvar1=="none") {return(NULL)}
  else{ Data=Data() 
  {
    interpolate(~(  fit1=lm(var1~poly(var2, degree, raw=T), data=df)  ), 
                df = quote(Data), var1=as.name(input$numvar0), var2=as.name(input$numvar1),
                degree=as.numeric(input$degree), file = "code_mlr.R", 
                mydir = userdir, append = FALSE, save_result = TRUE, eval = TRUE)
    interpolate(~( summary(fit1)  ), 
                file = "code_mlr.R", 
                mydir = userdir, append = FALSE, save_result = TRUE, eval = TRUE)
}
  }
})


output$modelfit2 <- renderPrint({
  
  if (input$numvar0=="none" | is.null(input$numvar2) ) {return(NULL)}
  else{
  Data=Data()  
  if ( input$factorasx==FALSE | is.null(input$grouping2) )
   {
interpolate(~(  formula2=as.formula( paste0(var1, " ~ ", paste0(var2, collapse = " + ")) )  ), 
                 df = quote(Data), var1=input$numvar0, var2=input$numvar2,
                 file = "code_mlr.R", 
                 mydir = userdir, append = FALSE, save_result = TRUE, eval = TRUE)
    } else {  
   interpolate(~(  formula2=as.formula( paste0(var1, " ~ ", paste0(var2, collapse = "+"),
                                                  "+", paste0(var3, collapse = "+")         ) )  ), 
                  df = quote(Data), var1=input$numvar0, var2=input$numvar2, 
                  var3=input$grouping2,     file = "code_mlr.R", 
                  mydir = userdir, append = FALSE, save_result = TRUE, eval = TRUE)
      
    }  
 interpolate(~(  formula2  ), 
              file = "code_mlr.R", 
              mydir = userdir, append = FALSE, save_result = TRUE, eval = TRUE)
 interpolate(~(  fit2=lm(formula=formula2, data=df)  ), 
                df = quote(Data),  file = "code_mlr.R", 
                mydir = userdir, append = FALSE, save_result = TRUE, eval = TRUE) 
 interpolate(~( summary(fit2)  ), 
            file = "code_mlr.R", 
            mydir = userdir, append = FALSE, save_result = TRUE, eval = TRUE)
}
 })

output$summary3 <- renderPrint({
  Data=Data() 
  if( input$binvar=="none" | is.null(input$numvar2)  ) {return(NULL)}
  else if (length( levels(Data[,input$binvar]) ) !=2) {return(NULL)}
  else{
    if ( input$factorasx==FALSE | is.null(input$grouping2) )
    {
      interpolate(~(  formula3=as.formula( paste0(var1, " ~ ", paste0(var2, collapse = " + ")) )  ), 
                  df = quote(Data), var1=input$binvar, var2=input$numvar2,
                  file = "code_mlr.R", 
                  mydir = userdir, append = FALSE, save_result = TRUE, eval = TRUE) 
      interpolate(~(  fit3=glm(formula3, data=df, family=binomial)  ), 
                  df = quote(Data),  file = "code_mlr.R", 
                  mydir = userdir, append = FALSE, save_result = TRUE, eval = TRUE)
      interpolate(~( summary(fit3)  ), 
                  file = "code_logistic.R", 
                  mydir = userdir, append = FALSE, save_result = TRUE, eval = TRUE)
    } else{   
      if(input$binvar %in% input$grouping2){paste0("Please choose a grouping variable different from the response variable!")}
      else{interpolate(~(  formula3=as.formula( paste0(var1, " ~ ", paste0(var2, collapse = " + "), 
                                                       "+", paste0(var3, collapse = "+")  ) )  ), 
                       df = quote(Data), var1=input$binvar, var2=input$numvar2,
                       var3=input$grouping2,  file = "code_mlr.R", 
                       mydir = userdir, append = FALSE, save_result = TRUE, eval = TRUE) 
        interpolate(~(  fit3=glm(formula3, data=df, family=binomial)  ), 
                    df = quote(Data),  file = "code_logistic.R", 
                    mydir = userdir, append = FALSE, save_result = TRUE, eval = TRUE)
        interpolate(~( summary(fit3)  ), 
                    file = "code_mlr.R", 
                    mydir = userdir, append = FALSE, save_result = TRUE, eval = TRUE)
      }
    }
  }
})

fit1 <- reactive ({
  if (input$numvar0=="none" | input$numvar1=="none") {return(NULL)}
  else{  Data=Data()  
  interpolate2(~( lm(var1~poly(var2, degree, raw=T), data=df) ), 
               df = quote(Data), degree=as.numeric(input$degree),
               var1=as.name(input$numvar0), 
               var2=as.name(input$numvar1)
               )
  }
})

fit2 <- reactive ({ 
  if (input$numvar0=="none" | is.null(input$numvar2) ) {return(NULL)}
  else{
  Data=Data()  
  if ( input$factorasx==FALSE | is.null(input$grouping2) )
  {
    formula2=as.formula( paste0(input$numvar0, " ~ ", 
                          paste0(input$numvar2, collapse = " + ")) )
  } else {   
      formula2=as.formula( paste0(input$numvar0, " ~ ", 
                        paste0(input$numvar2, collapse = "+"),
                          "+", paste0(input$grouping2, collapse = "+")  ) )
    }
 fit2<- lm(formula2, data=Data)
 return(fit2)
  }
})

fit3 <- reactive ({ 
  Data=Data() 
  if( input$binvar=="none" | is.null(input$numvar2)  ) {return(NULL)}
  else if (length( levels(Data[,input$binvar]) ) !=2) {return(NULL)}
  else{
    if (input$factorasx==FALSE | is.null(input$grouping2) | (input$binvar %in% input$grouping2) )
    {
      formula3=as.formula( paste0(input$binvar, " ~ ", 
                                  paste0(input$numvar2, collapse = " + ")) )
      fit3= glm(formula3, data=Data, family=binomial)
    } else 
    {
      formula3=as.formula( paste0(input$binvar, " ~ ", 
                                  paste0(input$numvar2, collapse = " + "),
                                  "+", paste0(input$grouping2, collapse = "+")    ) )
      fit3= glm(formula3, data=Data, family=binomial)
    }
    return(fit3) 
  }
})

output$parests1 <- renderPrint({
  fit1=fit1()
  if ( is.null(fit1)) {return(NULL)}
  else { 
  interpolate(~( confint(fit1, level=lvl)  ), 
              lvl=1-input$alpha, file = "code_mlr.R", 
              mydir = userdir, append = FALSE, save_result = TRUE, eval = TRUE)
  }
})

output$parests2 <- renderPrint({
  fit2=fit2()
  if ( is.null(fit2)) {return(NULL)}
  else {
  interpolate(~( confint(fit2, level=lvl)  ), 
              lvl=1-input$alpha, file = "code_mlr.R", 
              mydir = userdir, append = FALSE, save_result = TRUE, eval = TRUE)
  }
})

output$parests3 <- renderPrint({
  fit3=fit3()
  if (is.null(fit3) ) {return(NULL)}  
  else{  
    #confint returns profile confidence intervals
    #confint.default returns  Wald confidence intervals
    interpolate(~( confint.default(fit3, level=lvl)  ), 
                lvl=1-input$alpha, file = "code_logistic.R", 
                mydir = userdir, append = FALSE, save_result = TRUE, eval = TRUE)
  }
})


output$anova1 <- renderPrint({
  fit1=fit1()
  if ( is.null(fit1)) {return(NULL)}
  else {
  interpolate(~( anova(fit1) ), 
      file = "code_mlr.R", 
      mydir = userdir, append = FALSE, save_result = TRUE, eval = TRUE)
  }
})

output$anova2 <- renderPrint({
  fit2=fit2()
  if ( is.null(fit2)) {return(NULL)}
  else {
  interpolate(~( anova(fit2) ), 
              file = "code_mlr.R", 
              mydir = userdir, append = FALSE, save_result = TRUE, eval = TRUE)
  }
})

  
output$CI_prediction1 <- renderPrint({
  fit1=fit1()
  if ( is.null(fit1)) {return(NULL)}
  else {
  interpolate(~(  x=x0  ), 
              x=as.name(input$numvar1),
              x0=as.numeric(unlist(strsplit(input$newx1,","))),  file = "code_mlr.R", 
              mydir = userdir, append = FALSE, save_result = TRUE, eval = TRUE)
  interpolate(~(  new=data.frame(x)  ), 
              x=as.name(input$numvar1), file = "code_mlr.R", 
              mydir = userdir, append = FALSE, save_result = TRUE, eval = TRUE) 
  interpolate(~( predict(fit1, newdata=new, interval=intervaltype, level=lvl)  ), 
              intervaltype= input$predictiontype1,
              lvl=1-input$alpha, file = "code_mlr.R", 
              mydir = userdir, append = FALSE, save_result = TRUE, eval = TRUE)
  }
})

output$CI_prediction2 <- renderPrint({
  fit2=fit2()
  if ( is.null(fit2)) {return(NULL)}
  else {
  interpolate(~(  new=data.frame(rbind(x0))  ), 
              x0=as.numeric(unlist(strsplit(input$newx2,","))),  file = "code_mlr.R", 
              mydir = userdir, append = FALSE, save_result = TRUE, eval = TRUE)
  
  if ( input$factorasx==FALSE | is.null(input$grouping2) )
  {
  if (length(input$numvar2) != dim(new)[2]) {"Number of values must be equal to the number of independent variables!"} 
   else{
     interpolate(~(  colnames(new)=varinput  ), 
                 varinput=input$numvar2, file = "code_mlr.R", 
                 mydir = userdir, append = FALSE, save_result = TRUE, eval = TRUE) 
     interpolate(~( predict(fit2, newdata=new, interval=intervaltype, level=lvl)  ), 
                 intervaltype= input$predictiontype2,
                 lvl=1-input$alpha, file = "code_mlr.R", 
                 mydir = userdir, append = FALSE, save_result = TRUE, eval = TRUE)
   } 
    
  } else {   
      if (length(input$numvar2)+ length(input$grouping2) != dim(new)[2]) {"Number of values must be equal to the number of independent variables!"} 
     else{
       interpolate(~(  colnames(new)=c(paste(c(varinput1,varinput2), sep=","))  ), 
                   varinput1=input$numvar2, varinput2=input$grouping2,
                   file = "code_mlr.R", 
                   mydir = userdir, append = FALSE, save_result = TRUE, eval = TRUE)
       interpolate(~(  new[, varinput2]=factor(new[, varinput2])  ), 
                   varinput2=input$grouping2, file = "code_mlr.R", 
                   mydir = userdir, append = FALSE, save_result = TRUE, eval = TRUE)
       
       interpolate(~( predict(fit2, newdata=new, interval=intervaltype, level=lvl)  ), 
                   intervaltype= input$predictiontype2,
                   lvl=1-input$alpha, file = "code_mlr.R", 
                   mydir = userdir, append = FALSE, save_result = TRUE, eval = TRUE)
     } 
   
    }
    
}

  
})


output$histplot1 <- renderPlot({
  fit1=fit1()
  if ( is.null(fit1)) {return(NULL)}
  else {
  interpolate(~(  hist(fit1$residuals)  ), 
              file = "code_mlr.R", 
              mydir = userdir, append = FALSE, save_result = TRUE, eval = TRUE)
  }
})

output$histplot2 <- renderPlot({
  fit2=fit2()
  if ( is.null(fit2)) {return(NULL)}
  else {
  interpolate(~(  hist(fit2$residuals)  ), 
              file = "code_mlr.R", 
              mydir = userdir, append = FALSE, save_result = TRUE, eval = TRUE)
  }
})

output$normalityplot1 <- renderPlot({
  fit1=fit1()
  if ( is.null(fit1)) {return(NULL)}
  else {
  interpolate(~(library(car)), file = "code_all.R", 
              mydir = userdir, append = TRUE, nodupes = TRUE)
  interpolate(~(  car::qqPlot(fit1$residuals)  ), 
              file = "code_mlr.R", 
              mydir = userdir, append = FALSE, save_result = TRUE, eval = TRUE)
  }
})

output$normalityplot2 <- renderPlot({
  fit2=fit2()
  if ( is.null(fit2)) {return(NULL)}
  else {
  interpolate(~(library(car)), file = "code_all.R", 
              mydir = userdir, append = TRUE, nodupes = TRUE)
  interpolate(~(  car::qqPlot(fit2$residuals)  ), 
              file = "code_mlr.R", 
              mydir = userdir, append = FALSE, save_result = TRUE, eval = TRUE)
  }
})

output$normalitytest1 <- renderPrint({
  fit1=fit1()
  if ( is.null(fit1)) {return(NULL)}
  else {
  interpolate(~( shapiro.test(fit1$residuals)  ), 
              file = "code_mlr.R", 
              mydir = userdir, append = FALSE, save_result = TRUE, eval = TRUE)
  }
})

output$normalitytest2 <- renderPrint({
  fit2=fit2()
  if ( is.null(fit2)) {return(NULL)}
  else {
    interpolate(~( shapiro.test(fit2$residuals)  ), 
              file = "code_mlr.R", 
              mydir = userdir, append = FALSE, save_result = TRUE, eval = TRUE)
  }
})

output$residualplot1 <- renderPlot({
  fit1=fit1()
  if ( is.null(fit1)) {return(NULL)}
  else {
  interpolate(~(  plot(fit1$fitted.values, fit1$residuals)  ), 
              file = "code_mlr.R", 
              mydir = userdir, append = FALSE, save_result = TRUE, eval = TRUE)
  }
})

output$residualplot2 <- renderPlot({
  fit2=fit2()
  if ( is.null(fit2)) {return(NULL)}
  else {
  interpolate(~(  plot(fit2$fitted.values, fit2$residuals)  ), 
              file = "code_mlr.R", 
              mydir = userdir, append = FALSE, save_result = TRUE, eval = TRUE)
  }
})

output$respest <- renderPrint({
  fit3=fit3()
  if (is.null(fit3) ) {return(NULL)}  
  else{
    interpolate(~(  new=data.frame( rbind(x0))  ), 
      x0=as.numeric(unlist(strsplit(input$newx3,","))),  file = "code_mlr.R", 
        mydir = userdir, append = FALSE, save_result = TRUE, eval = TRUE)
    
    if ( input$factorasx==FALSE | is.null(input$grouping2) )
    {
      if (length(input$numvar2) != dim(new)[2]) {paste0("Number of values must be equal to the number of independent variables!")} 
      else{
        interpolate(~(  colnames(new)=varinput  ), 
                    varinput=input$numvar2, file = "code_mlr.R", 
                    mydir = userdir, append = FALSE, save_result = TRUE, eval = TRUE)
        interpolate(~( preds=predict(fit3, newdata=new, se=TRUE)  ), 
                    file = "code_mlr.R", 
                    mydir = userdir, append = FALSE, save_result = TRUE, eval = TRUE) 
        interpolate(~( pfit=exp(preds$fit)/(1+exp(preds$fit))  ), 
                    file = "code_mlr.R", 
                    mydir = userdir, append = FALSE, save_result = TRUE, eval = TRUE)
        interpolate(~( se.bands.logit = cbind(preds$fit-qnorm(sig, lower.tail = FALSE)*preds$se.fit,
                                              preds$fit+qnorm(sig, lower.tail = FALSE)*preds$se.fit)  ), 
                    file = "code_mlr.R", sig=input$alpha/2,
                    mydir = userdir, append = FALSE, save_result = TRUE, eval = TRUE)
        interpolate(~( se.bands = exp(se.bands.logit)/(1+exp(se.bands.logit))   ), 
                    file = "code_mlr.R", 
                    mydir = userdir, append = FALSE, save_result = TRUE, eval = TRUE)
        interpolate(~( return(list( Point_estimate = as.numeric(pfit), Confidence_interval = se.bands))  ), 
                    file = "code_mlr.R", 
                    mydir = userdir, append = FALSE, save_result = TRUE, eval = TRUE)
      }
    } else
    {
      if (length(input$numvar2)+ length(input$grouping2) != dim(new)[2]) {paste0("Number of values must be equal to the number of independent variables!")} 
      else{
        # predict(fit3, newdata=new, type = "response")
        interpolate(~(  colnames(new)=c(paste(c(varinput1,varinput2), sep=","))  ), 
                    varinput1=input$numvar2, varinput2=input$grouping2,
                    file = "code_mlr.R", 
                    mydir = userdir, append = FALSE, save_result = TRUE, eval = TRUE)
        interpolate(~(  new[, varinput2]=factor(new[, varinput2])  ), 
                    varinput2=input$grouping2, file = "code_logistic.R", 
                    mydir = userdir, append = FALSE, save_result = TRUE, eval = TRUE)  
        interpolate(~( preds=predict(fit3, newdata=new, se=TRUE)  ), 
                    file = "code_mlr.R", 
                    mydir = userdir, append = FALSE, save_result = TRUE, eval = TRUE) 
        interpolate(~( pfit=exp(preds$fit)/(1+exp(preds$fit))  ), 
                    file = "code_mlr.R", 
                    mydir = userdir, append = FALSE, save_result = TRUE, eval = TRUE)
        interpolate(~( se.bands.logit = cbind(preds$fit-qnorm(sig, lower.tail = FALSE)*preds$se.fit,
                                              preds$fit+qnorm(sig, lower.tail = FALSE)*preds$se.fit)  ), 
                    file = "code_mlr.R", sig=input$alpha/2,
                    mydir = userdir, append = FALSE, save_result = TRUE, eval = TRUE)
        interpolate(~( se.bands = exp(se.bands.logit)/(1+exp(se.bands.logit))   ), 
                    file = "code_mlr.R", 
                    mydir = userdir, append = FALSE, save_result = TRUE, eval = TRUE)
        interpolate(~( list( Point_estimate = as.numeric(pfit), Confidence_interval = se.bands)  ), 
                    file = "code_mlr.R", 
                    mydir = userdir, append = FALSE, save_result = TRUE, eval = TRUE)
        
      }
    }
  }
   
})


code <- reactiveFileReader(3000, session, file.path(userdir, "code_all.R"), clean_readlines)
observe({    
  updateAceEditor(session, "code_mlr", 
                      value = paste(code(), 
                                    collapse = "\n"))
    }) 
output$clip <- renderUI({
  rclipButton("clipbtn", "Copy-R-code-to-clipboard", input$code_mlr, icon=icon("clipboard", class = "btn btn-success"))
})
    
  
  })
}
    
