#' Numerical UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_Numerical_ui <- function(id){
  ns <- NS(id)
  
  fluidPage(
                withMathJax(),
    sidebarLayout(
      sidebarPanel(          
h3("Univariate Numerical Data Analysis"),
        br(),
        
selectizeInput(ns("numvar"), label = "Select a numerical variable", 
               choices = names(mtcars), multiple = FALSE),

radioButtons(
  inputId = ns("descriptive"),
  label = "Descriptive Statistics and Inferences",
  choices = c(
    "Numerical summary" = "summary",
    "Plots and Normality test" = "plot",
    "Inference about population mean(s)" = "inference1",
    "Inference about population variance(s)" = "inference2"
  )
),

conditionalPanel(
  condition = "input.descriptive == 'summary'",
  selectizeInput(ns("statistics"), label = "Summary Statistics", 
                 choices = c("Summary" = "summary",
                             "Mean" = "mean",
                             "Variance" = "variance",
                             "Standard deviation" = "sd" 
                 )),   ns=ns
),
conditionalPanel(
  condition = "input.descriptive == 'plot'",
  selectizeInput(ns("plottype"), label = "Plot Type", 
                 choices = c("Histogram" = "histogram",
                             "Boxplot" = "boxplot",
                "Normal Quantile Plot" = "qqplot",
                "Shapiro-Wilk test of normality"="normality" 
                )), 
  conditionalPanel(
    condition = "input.plottype == 'histogram'",
    sliderInput(ns("bins"), "Number of bins:",
       min = 1,   max = 50,   value = 5),
    ns=ns
  ),  ns=ns),

conditionalPanel(
  condition = "input.descriptive == 'summary' | input.descriptive == 'plot' | input.tests == 'twovar' ",
  selectizeInput(ns("grouping"), label = "By a grouping variable, please choose one", 
                 choices = names(mtcars), multiple = FALSE),
  ns=ns),
conditionalPanel(
  condition = "input.descriptive == 'inference1' | input.descriptive == 'inference2'",
  selectizeInput(ns("tests"), label = "Type", 
                 choices=c("One Sample" = "onevar", 
                           "Two Samples" = "twovar")),
  conditionalPanel(
    condition = "input.tests == 'twovar'",
    selectizeInput(ns("level1"), label = "Select a level of the grouping variable", 
                   choices = NULL),
    selectizeInput(ns("level2"), "Select another level of the grouping variable",
                   choices = NULL),
    ns=ns
  ),ns=ns
),
conditionalPanel(      
  condition = "input.descriptive == 'inference1'",
  numericInput(ns("h0_1"), "Hypothesized Value \\( H_0: \\mu_0 = \\)", 
               value = 0),
  conditionalPanel(
    condition = "input.tests == 'onevar'",
    radioButtons(ns("alternativemean1"), "Alternative Hypothesis \\( H_1:\\)", 
            choices=c("\\( \\mu \\neq \\mu_0 \\)" = "two.sided",
                   "\\( \\mu  < \\mu_0 \\)" = "less",
                   "\\( \\mu   > \\mu_0\\)" = "greater")
                 ),
    ns=ns
  ),
  conditionalPanel(
    condition = "input.tests == 'twovar'",
    radioButtons(ns("alternativemean2"), "Alternative Hypothesis \\( H_1:\\)", 
  choices=c("\\( \\mu_1 - \\mu_2  \\neq \\mu_0\\)" = "two.sided",
                  "\\( \\mu_1 - \\mu_2 < \\mu_0 \\)" = "less",
                  "\\( \\mu_1 - \\mu_2  > \\mu_0\\)" = "greater"
                  )),
checkboxInput( ns("var.equal"), "Variances of the populations are equal", FALSE),
checkboxInput( ns("paired"), "Conduct paired t-test", FALSE),
    ns=ns
  ),
  ns=ns),
conditionalPanel(      
  condition = "input.descriptive == 'inference2'",
    conditionalPanel(
    condition = "input.tests == 'onevar'",
    numericInput(ns("h0_2"), "Hypothesized Value \\( H_0: \\sigma^2 = \\sigma_0^2=\\)", 
                 value = 1),
    radioButtons(ns("alternativevar1"), "Alternative Hypothesis \\( H_1:\\)", 
      choices=c("\\( \\sigma^2 \\neq \\sigma_0^2 \\)" = "two.sided",
                   "\\( \\sigma^2  < \\sigma_0^2 \\)" = "less",
                   "\\( \\sigma^2   > \\sigma_0^2\\)" = "greater")
                 ),
    ns=ns
  ),
  conditionalPanel(
    condition = "input.tests == 'twovar'",
    sprintf("\\( H_0 : \\sigma^2_1 = \\sigma^2_2 \\)"),
    radioButtons( ns("alternativevar2"), "Alternative Hypothesis \\( H_1:\\)", 
        choices = c(
         "\\( \\sigma^2_1 \\neq \\sigma^2_2 \\)" = "two.sided",
            "\\( \\sigma^2_1 < \\sigma^2_2 \\)" = "less",
            "\\( \\sigma^2_1 > \\sigma^2_2 \\)" = "greater")
                   ),
    ns=ns),
  ns=ns
),
conditionalPanel(
  condition = "input.descriptive == 'inference1' | input.descriptive == 'inference2'",
  sliderInput(ns("alpha"), "Significance level \\(\\alpha = \\)", 
              min=0.001, max=0.2, step=0.01, value=0.05),
  ns=ns
)

  ),      
      
   mainPanel(
       
   htmlOutput(ns("selected_variable")),
    br(),
    conditionalPanel(
     condition = "input.descriptive=='summary' & input.statistics == 'summary'",
        verbatimTextOutput(ns("summary")), ns=ns
    ),
  conditionalPanel(
    condition = "input.descriptive=='summary' & input.statistics == 'mean'",
     verbatimTextOutput(ns("mean")), ns=ns
    ),
   conditionalPanel(
     condition = "input.descriptive=='summary' & input.statistics == 'variance'",
      verbatimTextOutput(ns("variance")), ns=ns
    ),
    conditionalPanel(
      condition = "input.descriptive=='summary' & input.statistics == 'sd'",
      verbatimTextOutput(ns("sd")), ns=ns
   ),
   conditionalPanel(
     condition = "input.descriptive=='plot' & input.plottype == 'histogram'",
     plotOutput(ns("histogram")), ns=ns
   ),
  conditionalPanel(
    condition = "input.descriptive=='plot' & input.plottype == 'boxplot'",
     plotOutput(ns("boxplot")), ns=ns
   ),
   conditionalPanel(
    condition = "input.descriptive=='plot' & input.plottype == 'qqplot'",
    plotOutput(ns("qqplot")), ns=ns
     ),
conditionalPanel(
         condition = "input.descriptive=='plot' & input.plottype == 'normality'",
         verbatimTextOutput(ns("normaltest")), ns=ns
       ),
conditionalPanel(
 condition = "input.descriptive=='inference1' & input.tests == 'onevar'",
 verbatimTextOutput(ns("onemeant")), ns=ns
       ),
conditionalPanel(
  condition = "input.descriptive=='inference1' & input.tests == 'twovar'",
  verbatimTextOutput(ns("twomeant")), ns=ns
),
conditionalPanel(
 condition = "input.descriptive=='inference2' & input.tests == 'onevar'",
 verbatimTextOutput(ns("onevariance")), ns=ns
       ),       
conditionalPanel(
  condition = "input.descriptive=='inference2' & input.tests == 'twovar'",
  verbatimTextOutput(ns("twovariance")), ns=ns
), 
rclipboard::rclipboardSetup(),
uiOutput(ns("clip")),
shinyAce::aceEditor(ns("code_numerical"), "", mode = "r", readOnly = TRUE, 
                 height = "800px", fontSize = 19, wordWrap = TRUE,
                 theme = "chrome")
        
      )    
      
      
    )
  )
}
    
#' Numerical Server Functions
#'
#' @noRd 
mod_Numerical_server <- function(id, Data, userdir){
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
  updateSelectizeInput(session, "grouping", 
            choices = c("None" = "none", 
                        R.categoricnames()  ))
})

observe({
  var=  Data()[[input$grouping]]
  lvl <- levels(var)  
  updateSelectInput(session, "level1", choices = lvl, selected =lvl[1])
  updateSelectInput(session, "level2", choices = lvl, selected =lvl[2])
})

output$selected_variable <- renderText({
  if (is.null(Data())) return(NULL)
  return(paste0("<B>Selected Numerical Variable: </B>", input$numvar, ";", "  ",
                "<B>Selected Categorical Variable: </B>",input$grouping, "." ))
})   

output$summary <- renderPrint({
  if (input$numvar=="none") {return(NULL)} else{
  Data=Data()
  if (input$grouping == "none"){
    interpolate(~(  summary(df$var)  ), 
      df = quote(Data), var=input$numvar, file = "code_numerical.R", 
      mydir = userdir, append = FALSE, save_result = TRUE, eval = TRUE)}
  else
  {
    interpolate( ~(tapply(df$var1, df$var2,
                         function(x) format(summary(x) )) ), 
      df = quote(Data), var1=input$numvar, 
      var2= input$grouping,
      file = "code_numerical.R", 
      mydir = userdir, append = FALSE, save_result = TRUE, eval = TRUE)
  }
  }

})
  
output$mean <- renderPrint({
  Data=Data()
  if (input$numvar=="none") {return(NULL)} 
  else{
    if (input$grouping == "none"){
      if (anyNA(Data[,input$numvar])){ 
        interpolate(~(df%>%select(var)%>%na.omit()%>%summarise(mean=mean(var)) ), 
                    df = quote(Data), var=as.name(input$numvar), file = "code_numerical.R", 
                    mydir = userdir, append = FALSE, save_result = TRUE, eval = TRUE)
      } else{
        interpolate(~(mean(df$var) ), 
                    df = quote(Data), var=as.name(input$numvar), file = "code_numerical.R", 
                    mydir = userdir, append = FALSE, save_result = TRUE, eval = TRUE)
      } 
    } else{
    if (anyNA(Data[,input$numvar]) | anyNA(Data[,input$grouping])) {
      interpolate(~(  df%>%select(var1, var2)%>%na.omit()%>%group_by(var2)%>%summarize(mean=mean(var1))%>%as.data.frame() ),  
            df = quote(Data), var1=as.name(input$numvar),var2=as.name(input$grouping),  
            file = "code_numerical.R",  mydir = userdir, 
            append = FALSE, save_result = TRUE, eval = TRUE)
    }else{
      interpolate(~( df%>%select(var1, var2)%>%group_by(var2)%>%summarize(mean=mean(var1))%>%as.data.frame() ),  
            df = quote(Data), var1=as.name(input$numvar), var2=as.name(input$grouping), file = "code_numerical.R", 
            mydir = userdir, append = FALSE, save_result = TRUE, eval = TRUE) 
          } 
           }
            }
})

output$variance <- renderPrint({
  Data=Data()
  if (input$numvar=="none") {return(NULL)} else{
  if (input$grouping == "none"){
    if (anyNA(Data[,input$numvar])){
interpolate(~(df%>%select(var1)%>%na.omit()%>%summarise(var=var(var1))), 
        df = quote(Data), var1=as.name(input$numvar), file = "code_numerical.R", 
        mydir = userdir, append = FALSE, save_result = TRUE, eval = TRUE) 
    } else{
interpolate(~(var(df$var1)), 
       df = quote(Data), var1=as.name(input$numvar), file = "code_numerical.R", 
       mydir = userdir, append = FALSE, save_result = TRUE, eval = TRUE)
       } 
    } else{
    if ( anyNA(Data[,input$numvar]) | anyNA(Data[,input$grouping]) ) 
        {interpolate(~( df%>%select(var1, var2)%>%na.omit()%>%group_by(var2)%>%summarize( var=var(var1) )%>%as.data.frame() ), 
      df = quote(Data), var1=as.name(input$numvar),  var2=as.name(input$grouping), file = "code_numerical.R", 
      mydir = userdir, append = FALSE, save_result = TRUE, eval = TRUE)}
  else {interpolate(~( df%>%select(var1, var2)%>%group_by(var2)%>%summarize( var=var(var1) )%>%as.data.frame() ), 
      df = quote(Data), var1=as.name(input$numvar),  var2=as.name(input$grouping),  file = "code_numerical.R", 
      mydir = userdir, append = FALSE, save_result = TRUE, eval = TRUE)  
  }
    }
   }
})

output$sd <- renderPrint({
  Data=Data()
  if (input$numvar=="none") {return(NULL)} else{
    if (input$grouping == "none"){
    if (anyNA(Data[,input$numvar])) interpolate(~(df%>%select(var1)%>%na.omit()%>%summarise(sd=sd(var1))), 
      df = quote(Data), var1=as.name(input$numvar), file = "code_numerical.R", 
      mydir = userdir, append = FALSE, save_result = TRUE, eval = TRUE)
    else  interpolate(~(sd(df$var1)), 
      df = quote(Data), var1=as.name(input$numvar), file = "code_numerical.R", 
      mydir = userdir, append = FALSE, save_result = TRUE, eval = TRUE)
  } else
  {
  if (anyNA(Data[,input$numvar]) | anyNA(Data[,input$grouping])) interpolate(~(  df%>%select(var1, var2)%>%na.omit()%>%group_by(var2)%>%summarize(sd =sd(var1))%>%as.data.frame()  ), 
        df = quote(Data), var1=as.name(input$numvar),  var2=as.name(input$grouping), file = "code_numerical.R", 
        mydir = userdir, append = FALSE, save_result = TRUE, eval = TRUE)
  else  interpolate(~(  df%>%select(var1, var2)%>%group_by(var2)%>%summarize(sd =sd(var1))%>%as.data.frame()  ), 
        df = quote(Data), var1=as.name(input$numvar),  var2=as.name(input$grouping), file = "code_numerical.R", 
        mydir = userdir, append = FALSE, save_result = TRUE, eval = TRUE)
  }
     }
})
  

output$histogram<- renderPlot({
  if (input$numvar=="none") {return(NULL)} else{
  Data=Data()
  if (input$grouping == "none"){ 
    interpolate(~( ggplot(df, aes(x=var)) + 
                     geom_histogram(bins=bins, fill="blue")+
                     labs(x = paste("Histogram of", var3))   ), 
                df = quote(Data), var=as.name(input$numvar), var3=input$numvar, 
                bins=input$bins,  file = "code_numerical.R", 
                mydir = userdir, append = FALSE, save_result = TRUE, eval = TRUE)
  } else
  {
    interpolate(~( ggplot(df, aes(x=var1)) + 
                     geom_histogram(aes(fill=var2),bins=bins)+
                     facet_wrap(~var2)+
                     labs( x = paste("Histogram of", var3) )   ), 
                df = quote(Data), var1=as.name(input$numvar),var2=as.name(input$grouping),
                var3=input$numvar, bins=input$bins, file = "code_numerical.R", 
                mydir = userdir, append = FALSE, save_result = TRUE, eval = TRUE)   
  }
    }

})

output$boxplot<- renderPlot({
  if (input$numvar=="none") {return(NULL)} else{
  Data=Data()
  if (input$grouping == "none"){ 
    interpolate(~( boxplot(df$var, main=paste("Box plot of", var),
                           xlab=var)   ), 
                df = quote(Data), var=input$numvar, file = "code_numerical.R", 
                mydir = userdir, append = FALSE, save_result = TRUE, eval = TRUE)
      } else
  {
    interpolate(~( boxplot(df$var1~df$var2, main=paste("Box plot of", var1, "by", var2),
                           xlab=var1)   ), 
      df = quote(Data), var1=input$numvar, var2=input$grouping,file = "code_numerical.R", 
      mydir = userdir, append = FALSE, save_result = TRUE, eval = TRUE) 
    }
  }
})

output$qqplot<- renderPlot({
  if (input$numvar=="none") {return(NULL)} else{
  Data=Data()
  interpolate(~(library(car)), file = "code_all.R", 
              mydir = userdir, append = TRUE, nodupes = TRUE) 
  if (input$grouping == "none"){ 
    interpolate(~( car::qqPlot(df$var)   ), 
        df = quote(Data), var=input$numvar,  file = "code_numerical.R", 
        mydir = userdir, append = FALSE, save_result = TRUE, eval = TRUE)
  } else
  {
    interpolate(~(  car::qqPlot(var1~var2, data=df)   ), 
        df = quote(Data), var1=as.name(input$numvar), 
        var2=as.name(input$grouping), file = "code_numerical.R", 
        mydir = userdir, append = FALSE, save_result = TRUE, eval = TRUE) 
  } 
  }
})

output$normaltest<- renderPrint({
  if (input$numvar=="none") {return(NULL)} else{
  Data=Data()
  if (input$grouping == "none"){ 
    interpolate(~( shapiro.test(df$var)   ), 
      df = quote(Data), var=input$numvar, file = "code_numerical.R", 
      mydir = userdir, append = FALSE, save_result = TRUE, eval = TRUE)
  } else
  {
interpolate(~(library(rstatix)), file = "code_all.R", 
                mydir = userdir, append = TRUE, nodupes = TRUE) 
interpolate(~( df%>%select(var1,var2)%>%group_by(var2)%>%rstatix::shapiro_test(var1)%>%as.data.frame()  ), 
            df = quote(Data), var1=as.name(input$numvar), var2= as.name(input$grouping),
            file = "code_numerical.R", 
            mydir = userdir, append = FALSE, save_result = TRUE, eval = TRUE)
  } 
  }
})

output$onemeant <- renderPrint({
  if (input$numvar=="none") {return(NULL)} else{
  Data=Data()
  interpolate(~( t.test(df$var, mu=mu0, alternative=ha, conf.level=clevel) ), 
          df = quote(Data), var=input$numvar, 
          mu0=input$h0_1, ha=input$alternativemean1, clevel=1-input$alpha,
          file = "code_numerical.R", 
          mydir = userdir, append = FALSE, save_result = TRUE, eval = TRUE)
  }
})

output$twomeant <- renderPrint({
  if (input$numvar=="none") {return(NULL)} else if (input$grouping == "none") {
    paste0("Please choose a grouping variable!")} else{
  Data=Data()
  if (input$paired==TRUE)
  {
    freq = Data%>%count(Data[,input$grouping])
    count1=freq[freq[, 1]==input$level1, ]
    n1=count1[,2]
    count2=freq[freq[, 1]==input$level2, ]
    n2=count2[,2]
if (n1 != n2){paste0("The sample sizes are not equal!  The data are certainly not paired!")} else
    {
interpolate(~( t.test(x=df[df$var2==level1,]$var1, y=df[df$var2==level2,]$var1, 
         mu=mu0, paired = TRUE, alternative=ha, conf.level=clevel) ), 
         df = quote(Data), var1=input$numvar, var2=input$grouping,
         level1=input$level1, level2=input$level2,
         mu0=input$h0_1, ha=input$alternativemean2, clevel=1-input$alpha,
         file = "code_numerical.R", 
         mydir = userdir, append = FALSE, save_result = TRUE, eval = TRUE) 
    }
  }
  else{
  interpolate(~( t.test(x=df[df$var2==level1,]$var1, y=df[df$var2==level2,]$var1, 
                        mu=mu0, var.equal=varequal,
                        alternative=ha, conf.level=clevel) ), 
              df = quote(Data), var1=input$numvar, var2=input$grouping,
         varequal = input$var.equal,level1=input$level1, level2=input$level2,
         mu0=input$h0_1, ha=input$alternativemean2, clevel=1-input$alpha,
         file = "code_numerical.R", 
         mydir = userdir, append = FALSE, save_result = TRUE, eval = TRUE)
  }
   }
})

output$onevariance <- renderPrint({
  if (input$numvar=="none" | input$h0_2<0 ) {return(NULL)} else{
  Data=Data()
  interpolate(~(library(EnvStats)), file = "code_all.R", 
              mydir = userdir, append = TRUE, nodupes = TRUE)
  
  interpolate(~( varTest(df$var, sigma.squared=h0,alternative=ha, conf.level=clevel) ), 
              df = quote(Data), var=input$numvar, 
              h0=input$h0_2, ha=input$alternativevar1, clevel=1-input$alpha,
              file = "code_numerical.R", 
              mydir = userdir, append = FALSE, save_result = TRUE, eval = TRUE)
  }
})

output$twovariance <- renderPrint({
  if (input$numvar=="none") {return(NULL)} else if (input$grouping == "none") {
    paste0("Please choose a grouping variable!")} else{
  Data=Data()
  interpolate(~( var.test(x=df[df$var2==level1,]$var1, y=df[df$var2==level2,]$var1, 
                       alternative=ha, conf.level=clevel) ), 
              df = quote(Data), var1=input$numvar, var2=input$grouping,
              level1=input$level1, level2=input$level2,
              ha=input$alternativevar2, clevel=1-input$alpha,
              file = "code_numerical.R", 
              mydir = userdir, append = FALSE, save_result = TRUE, eval = TRUE) 
  }
  })


code <- reactiveFileReader(3000, session, file.path(userdir, "code_all.R"), clean_readlines)
observe({    
  updateAceEditor(session, "code_numerical", 
                  value = paste(code(), 
                                collapse = "\n"))
})
output$clip <- renderUI({
  rclipButton("clipbtn", "Copy-R-code-to-clipboard", input$code_numerical, icon=icon("clipboard", class = "btn btn-success"))
})


  })
}
    
