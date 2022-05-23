#' Categorical UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_Categorical_ui <- function(id){
  ns <- NS(id)

  fluidPage(
    withMathJax(),
    sidebarLayout(
      sidebarPanel(          
        
  h3("Univariate Categorical Data Analysis"),
  br(),
        
  selectizeInput(ns("catvar"), label = "Select a categorical variable", 
                       choices = names(mtcars), multiple = FALSE), 
    radioButtons(
    inputId = ns("descriptive"),
    label = "Descriptive Statistics and Inferences",
    choices = c(
      "Summary statistics and plots" = "summary",
      "Inference about population proportion(s)" = "inference"
    )
  ),
  conditionalPanel(
    condition = "input.descriptive == 'summary'",
    selectizeInput(ns("statistics"), label = "Summary statistics and plots", 
                   choices = c("Frequency table" = "freqtable",
                               "Bar chart" = "bar1",
                               "Pie chart" = "pie1" 
                   )),   ns=ns
  ),
  conditionalPanel(
    condition = "input.descriptive == 'inference'",
    selectizeInput(ns("inference"), label = "Inference about", 
             choices = c("One population proportion" = "oneprop",
                     "Two population proportions" = "twoprop",
                     "Three or more population proportions" = "multiprop",
                     "Goodness-of-fit" = "goodfit" 
                 )),   ns=ns
    ),
  conditionalPanel( 
  condition = "input.descriptive == 'inference'",
  conditionalPanel(
  condition = "input.inference == 'oneprop'",
  selectizeInput(ns("level0"), label = "Select a population (level):", 
                 choices = NULL, multiple = FALSE),
numericInput(ns("h0_1"), "Hypothesized Value under \\( H_0: p = p_0= \\)", 
                     value = 0.5),
  radioButtons(ns("alternativep1"), "Alternative Hypothesis \\( H_1:\\)", 
    choices=c("\\( p \\neq p_0 \\)" = "two.sided",
                 "\\( p  < p_0 \\)" = "less",
                "\\( p   > p_0 \\)" = "greater"
                   )),
        ns=ns
      ),
  conditionalPanel(
        condition = "input.inference == 'twoprop'",
        selectizeInput(ns("level1"), label = "Select a level of the categorical variable", 
                       choices = NULL),
        selectizeInput(ns("level2"), "Select another level of the categorical variable",
                       choices = NULL),
        sprintf("Null hypothesis \\( H_0 : p_1 = p_2 \\)"),
        radioButtons(ns("alternativep2"), "Alternative Hypothesis \\(H_1:\\)", 
                     choices=c("\\( p_1 \\neq p_2 \\)" = "two.sided",
                       "\\( p_1  < p_2 \\)" = "less",
                               "\\( p_1   > p_2 \\)" = "greater"
                      )),
          ns=ns
      ),
      conditionalPanel(
        condition = "input.inference == 'multiprop'",
        sprintf("\\( H_0:\\) All the following population proportions are equal."),
        selectizeInput(ns("levels"), label = "Select the populations (levels) you want to compare", 
                       choices = NULL, multiple = TRUE),
        sprintf("\\( H_1:\\) Not all population proportions are equal."),
        ns=ns
       ),
      conditionalPanel(
        condition = "input.inference == 'goodfit'",
        sprintf("\\( H_0 : \\) Population probabilities are equal to the following specified values:"),
        textInput(ns("h0_good"), "Probabilities under \\( H_0\\)", 
                      value = "0.2, 0.5, 0.3", 
      placeholder = "Enter values separated by a comma with decimals as points, e.g. 0.2, 0.5, 0.3, etc."),
        sprintf("\\( H_1 : \\) Not all probabilities are equal to the specified values."),
        ns=ns
          ),
          ns=ns
        ),
        conditionalPanel(
          condition = "input.descriptive == 'inference' ",
          sliderInput(ns("alpha"), "Significance level \\(\\alpha = \\)", 
                      min=0.001, max=0.2, step=0.01, value=0.05),
          ns=ns
        )
      ),   

      mainPanel(
        
        htmlOutput(ns("selected_variable")),
        br(),
conditionalPanel(
  condition = "input.descriptive=='summary' & input.statistics == 'freqtable'",
  verbatimTextOutput(ns("ftable")), ns=ns
),
conditionalPanel(
  condition = "input.descriptive=='summary' & input.statistics == 'bar1'",
  plotOutput(ns("barchart1")), ns=ns
),
conditionalPanel(
  condition = "input.descriptive=='summary' & input.statistics == 'pie1'",
  plotOutput(ns("piechart1")), ns=ns
),
conditionalPanel(
  condition = "input.descriptive=='inference' & input.inference == 'oneprop'",
  verbatimTextOutput(ns("oneprop")), ns=ns
), 
conditionalPanel(
  condition = "input.descriptive=='inference' & input.inference == 'twoprop'",
  verbatimTextOutput(ns("twoprop")), ns=ns
), 
conditionalPanel(
  condition = "input.descriptive=='inference' & input.inference == 'multiprop'",
  verbatimTextOutput(ns("multiprop")), ns=ns
),
conditionalPanel(
  condition = "input.descriptive=='inference' & input.inference == 'goodfit'",
  verbatimTextOutput(ns("goodfit")), ns=ns
), 
rclipboard::rclipboardSetup(),
uiOutput(ns("clip")),
shinyAce::aceEditor(ns("code_categorical"), "", mode = "r", readOnly = TRUE, 
          height = "700px", fontSize = 16, wordWrap = TRUE,
          theme = "chrome")

      ) 
  
    ))
}
    
#' Categorical Server Functions
#'
#' @noRd 
mod_Categorical_server <- function(id, Data, userdir){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    R.categoricnames <- reactive({
      if (is.null(Data())) return(NULL)
      return(categoricNames(Data()))
    }) 
    observe({
      updateSelectizeInput(session, "catvar", 
      choices = c("None" = "none", R.categoricnames()  ))
    })  
    
    observe({
      var=  Data()[[input$catvar]]
      lvl <- levels(var) 
      updateSelectInput(session, "level0", choices = lvl)
      updateSelectInput(session, "level1", choices = lvl)
      updateSelectInput(session, "level2", choices = lvl)
      updateSelectInput(session, "levels", choices = lvl)
    })   
    
output$selected_variable <- renderText({
      if (is.null(Data()) | input$catvar=="none" ) return(NULL)
      return(paste0("<B>Selected Categorical Variable: </B>", input$catvar, "; ",
                    " <B>Number of levels: </B>",  length( levels(Data()[,input$catvar]) ), "."  ))
    }) 
  
output$ftable <- renderPrint({
  if (input$catvar=="none") {return(NULL)} else{
    Data=Data()
    if (anyNA(Data[,input$catvar])){
  interpolate(~( table=df%>%select(var)%>%na.omit()%>%group_by(var)%>%summarise(ni=n())%>%mutate(phat=ni/sum(ni))%>%as.data.frame() ),
              df = quote(Data), var=as.name(input$catvar), file = "code_categorical.R", 
              mydir = userdir, append = FALSE, save_result = TRUE, eval = TRUE)
         }else{
  interpolate(~( table=df%>%select(var)%>%group_by(var)%>%summarise(ni=n())%>%mutate(phat=ni/sum(ni))%>%as.data.frame() ),
            df = quote(Data), var=as.name(input$catvar), file = "code_categorical.R", 
            mydir = userdir, append = FALSE, save_result = TRUE, eval = TRUE)
     }
    interpolate(~(  table  ), 
      df = quote(Data), var=as.name(input$catvar), file = "code_categorical.R", 
      mydir = userdir, append = FALSE, save_result = TRUE, eval = TRUE)
     }
})
    

table <- reactive ({
if (input$catvar=="none") {return(NULL)} else{
    Data=Data()
    if (anyNA(Data[,input$catvar])){
interpolate2(~( table=df%>%select(var)%>%na.omit()%>%group_by(var)%>%summarise(ni=n())%>%mutate(phat=ni/sum(ni))%>%as.data.frame()  ), df = quote(Data), var=as.name(input$catvar) )
    }else{
interpolate2(~( table=df%>%select(var)%>%group_by(var)%>%summarise(ni=n())%>%mutate(phat=ni/sum(ni))%>%as.data.frame()  ), df = quote(Data), var=as.name(input$catvar) )
    }
  return(table)
  }
})

output$barchart1<- renderPlot({
  if (input$catvar=="none") {return(NULL)} else{
  Data=Data()
interpolate(~(  ggplot(df, aes(x="", fill=var)) +geom_bar(position=position_dodge())  ), 
              df = quote(Data), var=as.name(input$catvar), file = "code_categorical.R", 
              mydir = userdir, append = FALSE, save_result = TRUE, eval = TRUE)
  }
})

output$piechart1<- renderPlot({
  if (input$catvar=="none") {return(NULL)} else{
  Data=Data()
  interpolate(~(  ggplot(df, aes(x="", fill=var)) +geom_bar()+coord_polar("y")  ), 
              df = quote(Data), var=as.name(input$catvar), file = "code_categorical.R", 
              mydir = userdir, append = FALSE, save_result = TRUE, eval = TRUE)
  }
})

output$oneprop <- renderPrint({
  table=table()
  if (is.null(table)) {return(NULL)} else{
  interpolate(~(  n0= sum(table[,2])  ), 
              file = "code_categorical.R", 
              mydir = userdir, append = FALSE, save_result = TRUE, eval = TRUE) 
  interpolate(~(  x0= table1[table1$var==level,2]  ), 
              file = "code_categorical.R", table1=quote(table),
              var=input$catvar, level=input$level0,
              mydir = userdir, append = FALSE, save_result = TRUE, eval = TRUE)
  interpolate(~(  prop.test(x=x0, n=n0, p = p0,alternative = ha,
                            conf.level = clevel, correct = FALSE)  ), 
              p0=input$h0_1, ha=input$alternativep1, clevel=1-input$alpha,
              file = "code_categorical.R", 
              mydir = userdir, append = FALSE, save_result = TRUE, eval = TRUE)
  }
})

output$twoprop <- renderPrint({
  table=table()
  if (is.null(table)) {return(NULL)} else{
  interpolate(~(  n= sum(table[,2])  ), 
              file = "code_categorical.R", 
              mydir = userdir, append = FALSE, save_result = TRUE, eval = TRUE)
  interpolate(~(  x1= table1[table1$var==level,2]  ), 
              file = "code_categorical.R", table1=quote(table),
              var=input$catvar, level=input$level1,
              mydir = userdir, append = FALSE, save_result = TRUE, eval = TRUE)
  interpolate(~(  x2= table1[table1$var==level,2]  ), 
              file = "code_categorical.R", table1=quote(table),
              var=input$catvar, level=input$level2,
              mydir = userdir, append = FALSE, save_result = TRUE, eval = TRUE)
  interpolate(~(  prop.test(x=c(x1,x2), n=c(n,n), alternative = ha,
                            conf.level = clevel, correct = FALSE)  ), 
              ha=input$alternativep2, clevel=1-input$alpha,
              file = "code_categorical.R", 
              mydir = userdir, append = FALSE, save_result = TRUE, eval = TRUE)
  }
})

output$multiprop <- renderPrint({
  table=table()
  if (is.null(table)) {return(NULL)} else{
  interpolate(~(  n= sum(table[,2])  ), 
              file = "code_categorical.R", 
              mydir = userdir, append = FALSE, save_result = TRUE, eval = TRUE) 
  interpolate(~(  k=length(levs)   ), 
              levs=input$levels,file = "code_categorical.R", 
              mydir = userdir, append = FALSE, save_result = TRUE, eval = TRUE)
  interpolate(~(  x= integer(k)  ), 
              file = "code_categorical.R", 
              mydir = userdir, append = FALSE, save_result = TRUE, eval = TRUE)
  if (k <= 2) {cat("Please choose three or more levels!!!  ")} else
  {
    i=1
    interpolate(~( for (i in 1:k) { x[i]= table1[table1$var==levs[i],2]}  ), 
                file = "code_categorical.R", table1=quote(table),
                var=input$catvar, levs=input$levels,
                mydir = userdir, append = FALSE, save_result = TRUE, eval = TRUE)
    interpolate(~(  prop.test(x=x, n=rep(n,k), alternative = "two.sided",
                              conf.level = clevel, correct = FALSE)  ), 
                clevel=1-input$alpha,  file = "code_categorical.R", 
                mydir = userdir, append = FALSE, save_result = TRUE, eval = TRUE)
  }
  }
})

output$goodfit <- renderPrint({
  if (is.null(table())) {return(NULL)} else{
    table=table()
  interpolate(~(  p=p0  ), 
        p0=as.numeric(unlist(strsplit(input$h0_good,","))),  file = "code_categorical.R", 
        mydir = userdir, append = FALSE, save_result = TRUE, eval = TRUE) 
  interpolate(~( x=table[, 2]  ), 
        file = "code_categorical.R", 
        mydir = userdir, append = FALSE, save_result = TRUE, eval = TRUE)
  interpolate(~(  chisq.test(x=x, p=p,  correct = FALSE)  ), 
         file = "code_categorical.R", 
         mydir = userdir, append = FALSE, save_result = TRUE, eval = TRUE)
  }
})
    
code <- reactiveFileReader(3000, session, file.path(userdir, "code_all.R"), clean_readlines)
observe({    
  updateAceEditor(session, "code_categorical", 
                  value = paste(code(), collapse = "\n"))
}) 
output$clip <- renderUI({
  rclipButton("clipbtn", "Copy-R-code-to-clipboard", input$code_categorical, icon=icon("clipboard", class = "btn btn-success"))
})

  })
}
