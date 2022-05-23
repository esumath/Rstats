#' Data UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
#' 

mod_Data_ui <- function(id){
  ns <- NS(id)  
  
  fluidPage(
    
    sidebarLayout(
      
sidebarPanel(          
  
  h3("Data import/export and transformation"),
  hr(),
  conditionalPanel(
    condition = "input.tabs_data == 'DataView'",
conditionalPanel(
    condition = "input.own == false",
    selectInput(ns("data"), label = "Choose a Dataset", 
                choices = c(
                  "mpg" = "mpg",
                  "mtcars" = "mtcars", 
                  "iris" = "iris",
                  "marketing data set in package datarium"= "marketing",
                  "Weight Data By Gende in package datarium"="genderweight",
                  "Chicken Weights by Feed Type" = "chickwts", 
                  "The Effect of Vitamin C on Tooth Growth in Guinea Pigs" = "ToothGrowth"
                ), selected= "mtcars"), ns=ns
  ),
  conditionalPanel(
    condition = "input.own == true",
     fileInput(ns('data_own'), 'Choose a CSV(comma-separated-values) File',
              accept=c('text/csv', 
                       'text/comma-separated-values,text/plain', 
                       '.csv')),
     checkboxInput(ns('header'), 'The first row is header', TRUE),
       ns=ns
             ),
  checkboxInput(ns("own"), "Upload dataset"),
  hr(),
conditionalPanel(
  condition = "input.own == false",
actionButton(ns("jumpToEntry"), 'Enter data manaully', icon = icon("long-arrow-alt-right"), class ="btn btn-info"),
ns=ns
),
br(),
actionButton(ns("jumpToTransformation"), 'Data transformation', icon = icon("long-arrow-alt-right"), class ="btn btn-info"),
hr(),
downloadButton(ns("downloaddata"), "Download data", class = "btn btn-primary"),
hr(),
actionButton(ns("reset"), "Reset data", icon = icon("refresh"), class = "btn btn-primary btn-lg"),
ns=ns),
conditionalPanel(
  condition = "input.tabs_data == 'DataEntry'",
  shinyjs::useShinyjs(),
  numericInput( ns("ncols"), "Number of variables (columns), up to 5 only", 
                value = 2, min = 1, max=5, step = 1),
  conditionalPanel(
    condition = "input.ncols == 1",
    checkboxInput(ns("freq_table"), "Frequency Table (frequencies must be integers)", FALSE),
    ns=ns
  ),
  column(
    5, actionButton(ns("btn1"), "Set columns", icon = icon("hammer"),  class ="btn btn-info") ),
  column(
    5, actionButton(ns("btn2"), "Reset columns", icon = icon("refresh"), class ="btn btn-danger") ),
  br(),
  br(),
  h4("The initial number of rows is 10; you can add or delete rows as in Excel."),
  br(),
   column(
    10,
  actionButton(ns("btn3"), "Submit", icon = icon("save"), class = "btn btn-primary btn-lg")
       ),
  br(),
  br(),
  h4("You must click this button to save the entered data!"),
  ns=ns
),
conditionalPanel(
  condition = "input.tabs_data == 'Transformation'",
  selectizeInput(ns("trans"), label = "Choose Transformation", 
                 choices = c("Power" = "power", 
                             "Logarithm (for a positive variable)" = "log",
                             "To Categorical" = "categorical", 
                             "To Numeric" = "numeric"), 
                 multiple = FALSE),
  selectizeInput(ns("var_trans"), label = "Select a variable", 
                 choices = numericNames(mtcars),
                 multiple = FALSE),
  conditionalPanel(
    condition = "input.trans == 'power'",
    numericInput( ns("power"), "Power", 
                  value = 1, min = -100, max = 100, step = 0.1),
    ns=ns
  ),
  conditionalPanel(
    condition = "input.trans == 'log'",
    selectizeInput(ns("log_base"), "Log Base", 
                   choices = c("e" = 2.718282, "2" = 2, "10" = 10)),
    ns=ns
  ),
  conditionalPanel(
    condition = "input.trans == 'categorical'",
    selectizeInput(ns("categorical_method"), "Method", 
                   choices = c("Direct" = "direct", "Binning" = "binning")),
    conditionalPanel(condition = "input.categorical_method == 'binning'",
                     numericInput(ns("categorical_intervals"), 
                                  "Number of Intervals", value = 4, min = 2),
                     ns=ns), ns=ns
  ), 
  br(),
actionButton(ns("savetrans"), label="Save transformation", icon = icon("save"), class = "btn btn-primary"),
ns=ns
 )

),   

mainPanel(
  

tabsetPanel( id = ns("tabs_data"), type = "tabs",
           tabPanel("DataView", DT::dataTableOutput(ns("dataview")),
                    conditionalPanel(
                      condition = "input.own == true",
                      uiOutput(ns("nocsv")), 
                      uiOutput(ns("codeinf")),
                      ns = ns)
                        ),
tabPanel("DataEntry", hr(), tags$b("Manually enter numerical data, up to 5 columns:"), br(), br(), rhandsontable::rHandsontableOutput(ns("my_datatable")) ),
             tabPanel("Transformation", 
                      textOutput(ns("var_trans_text")),
                      tags$b("Original Data"),
                      plotOutput(ns("var_plot"), height="320px"),                                        
                      tags$b("Transformed Data"),
                      plotOutput(ns("trans_plot"), height="320px")
                      )     
  ),
rclipboard::rclipboardSetup(),
uiOutput(ns("clip")),
shinyAce::aceEditor(ns("code_data"), "", mode = "r", readOnly = TRUE, 
            height = "400px", fontSize = 16, wordWrap = TRUE, 
            showLineNumbers = TRUE, theme = "chrome")
  
  
)   
    )
  )
}
    
#' Data Server Functions
#'
#' @noRd 

mod_Data_server <- function(id, userdir){
  moduleServer( id, function(input, output, session){
    #ns <- session$ns   #?
    
#options(shiny.maxRequestSize = 10*1024^2)
values <- reactiveValues(dataset = mtcars, datatemp=NULL)

read.csv <- function (path, header) {
  return(utils::read.csv(input$data_own[,"datapath"], 
                  header=header,
                  sep = ",", stringsAsFactors = TRUE))
}

data.module <- function(inFile, dataset, own) { 
  cat("\n", file = file.path(userdir, "code_all.R"), append = TRUE)
      if (is.null(inFile) | !own) {
        interpolate(~(Data <- get(dat)%>%as.data.frame()), 
                    dat = dataset, file = "code_data.R", 
                    mydir = userdir, append = FALSE, save_result = TRUE)
      } else {
        interpolate(~(Data <- read.csv(data, header) ),
                 data = input$data_own$name,
                 header = input$header,
                file = "code_data.R", 
                mydir = userdir, append = FALSE, save_result = TRUE)
      }
  return(Data)  
    }

interpolate(~(library(dplyr)), file = "code_all.R", 
            mydir = userdir, append = TRUE, nodupes = TRUE)  
interpolate(~(library(ggplot2)), file = "code_all.R", 
            mydir = userdir, append = TRUE, nodupes = TRUE)
interpolate(~(library(datarium)), file = "code_all.R", 
            mydir = userdir, append = TRUE, nodupes = TRUE)
interpolate(~(mtcars$cyl=as.factor(mtcars$cyl)), file = "code_all.R", 
            mydir = userdir, append = TRUE, nodupes = TRUE)
interpolate(~(mtcars$vs=as.factor(mtcars$vs)), file = "code_all.R", 
            mydir = userdir, append = TRUE, nodupes = TRUE)
interpolate(~(mtcars$am=as.factor(mtcars$am)), file = "code_all.R", 
            mydir = userdir, append = TRUE, nodupes = TRUE)

observe({
  values$dataset <- data.module(input$data_own, input$data, input$own)
}) 

#########
observeEvent(input$jumpToEntry, {
  updateTabsetPanel(session, "tabs_data",
                    selected = "DataEntry")
})
observeEvent(input$jumpToTransformation, {
  updateTabsetPanel(session, "tabs_data",
                    selected = "Transformation")
})
observeEvent(input$reset, {
  values$dataset=mtcars
})


observe({
  if (is.na(input$ncols) | is.null(input$ncols) | input$ncols<1 | input$ncols>5) updateNumericInput(session, "ncols", value=1)
  else if (input$ncols %% 1 != 0) updateNumericInput(session, "ncols", value=round(input$ncols)) 
  })

observe({
  values$datatemp<- if (is.na(input$ncols) | is.null(input$ncols) | input$ncols<1 | input$ncols>5) matrix(0, nrow=10, ncol=1)%>%as.data.frame()  else if (input$ncols==1 & input$freq_table==TRUE) data.frame(cbind(rep(0,10),rep(1,10))) else matrix(0, nrow=10, ncol=input$ncols)%>%as.data.frame() 
})

observe({
  varnames<-if (is.na(input$ncols) | is.null(input$ncols) | input$ncols<1 | input$ncols>5) character(length=1) else character(length=ifelse((input$ncols==1 & input$freq_table==TRUE), 2,input$ncols)     )
  
  if (is.na(input$ncols) | is.null(input$ncols) | input$ncols<1 | input$ncols>5){
    varnames[1] = "Y"
  } else if (input$ncols==1 & input$freq_table==FALSE){
    varnames[1] = "Y"} 
  else if (input$ncols==1 & input$freq_table==TRUE)
  {varnames[1] = "Y"
  varnames[2] = "Frequency"
  } else if (input$ncols==2)
  {varnames[1] = "Y"
   varnames[2] = "X"
  } else{
      varnames[1] = "Y"  
    for (i in 2:input$ncols) {varnames[i]=paste0("X", i-1)}
  } 
  colnames(values$datatemp)<-varnames
})

observeEvent(input$btn1, {
  shinyjs::disable("ncols")
})
observe({
  input$btn2
  values$datatemp<- if (is.na(input$ncols) | is.null(input$ncols) | input$ncols<1 | input$ncols>5) matrix(0, nrow=10, ncol=1)%>%as.data.frame()  else if (input$ncols==1 & input$freq_table==TRUE) data.frame(cbind(rep(0,10),rep(1,10))) else matrix(0, nrow=10, ncol=input$ncols)%>%as.data.frame() 
})
observeEvent(input$btn2, {
  shinyjs::enable("ncols")
})
output$my_datatable <- rhandsontable::renderRHandsontable({
  rhandsontable::rhandsontable(values$datatemp)
})
observeEvent(input$my_datatable,{
  df <- hot_to_r(input$my_datatable)
  df <- as.data.frame(df)
  values$datatemp <- df
})
##Code of manual data entry:
entry_data <- function(data, cols, freq){
 if (cols == 1 & freq == TRUE){
    interpolate(~(Data <- data.frame(Y=rep(y, f))   ), 
                y=as.numeric( unlist(strsplit((paste(as.character(data[,1]), collapse=", ")),",")) ),
                f=as.numeric( unlist(strsplit((paste(as.character(data[,2]), collapse=", ")),",")) ),  
                mydir = userdir,  file = "code_data.R")
  } else if (cols==1 & freq==FALSE)
  {  interpolate(~(Data <- data.frame(Y=y)),
                 y=as.numeric( unlist(strsplit((paste(as.character(data[,1]), collapse=", ")),",")) ),
                 mydir = userdir,file = "code_data.R")
  } else if (cols==2)
  { interpolate(~(Data=data.frame(Y=y, X=x)),
                y=as.numeric( unlist(strsplit((paste(as.character(data[,1]), collapse=", ")),",")) ),
                x=as.numeric( unlist(strsplit((paste(as.character(data[,2]), collapse=", ")),",")) ),
                mydir = userdir,file = "code_data.R")   
    
  }  else if (cols==3)
  { interpolate(~(Data=data.frame(Y=y, X1=x1, X2=x2)),
                y=as.numeric( unlist(strsplit((paste(as.character(data[,1]), collapse=", ")),",")) ),
                x1=as.numeric( unlist(strsplit((paste(as.character(data[,2]), collapse=", ")),",")) ),
                x2=as.numeric( unlist(strsplit((paste(as.character(data[,3]), collapse=", ")),",")) ),
                mydir = userdir,file = "code_data.R")   
    
  }  else if (cols==4)
  { interpolate(~(Data=data.frame(Y=y, X1=x1, X2=x2, X3=x3)),
                y=as.numeric( unlist(strsplit((paste(as.character(data[,1]), collapse=", ")),",")) ),
                x1=as.numeric( unlist(strsplit((paste(as.character(data[,2]), collapse=", ")),",")) ),
                x2=as.numeric( unlist(strsplit((paste(as.character(data[,3]), collapse=", ")),",")) ),
                x3=as.numeric( unlist(strsplit((paste(as.character(data[,4]), collapse=", ")),",")) ),
                mydir = userdir,file = "code_data.R")   
    
  }  else if (cols==5)
  { interpolate(~(Data=data.frame(Y=y, X1=x1, X2=x2, X3=x3, X4=x4)),
                y=as.numeric( unlist(strsplit((paste(as.character(data[,1]), collapse=", ")),",")) ),
                x1=as.numeric( unlist(strsplit((paste(as.character(data[,2]), collapse=", ")),",")) ),
                x2=as.numeric( unlist(strsplit((paste(as.character(data[,3]), collapse=", ")),",")) ),
                x3=as.numeric( unlist(strsplit((paste(as.character(data[,4]), collapse=", ")),",")) ),
                x4=as.numeric( unlist(strsplit((paste(as.character(data[,5]), collapse=", ")),",")) ),
                mydir = userdir,file = "code_data.R")   
    
  } else{ 
    Data=data
    #paste("Please enter an integer between 1 and 5.")
    }
    return(Data) 
}

Data.Entry <- reactive({
  Data=as.data.frame( entry_data(data=values$datatemp, cols=input$ncols, freq=input$freq_table)  )
  return(Data)
})

observeEvent(input$btn3, {
 #values$dataset <- if (input$ncols==1 & input$freq_table==TRUE) data.frame(Y=rep(values$datatemp[,1], values$datatemp[,2])) else values$datatemp 
values$dataset <- Data.Entry()
cat(paste0("\n", paste(c(readLines(file.path(userdir, "code_data.R")), "\n"), 
        collapse = "\n")), file = file.path(userdir, "code_all.R"), append = TRUE)
})
#########

R.numericnames <- reactive({
  if (is.null(Data())) return(NULL) 
  return(numericNames(Data()))
})
R.categoricnames <- reactive({
  if (is.null(Data())) return(NULL) 
  return(categoricNames(Data()))
})
output$nocsv <- renderUI({ 
      req(input$data_own)  
      validate(
        need(tools::file_ext(input$data_own$datapath) %in% c('text/csv','csv'),
             "Data was not recognized. Please use a CSV file!"))
            })
output$dataview<-  DT::renderDataTable({  Data()  })

############ reactive data
Data <- reactive({
  Data <- values$dataset%>%as.data.frame()
  return(Data)
})
###########


observe({
  updateSelectizeInput(session, "trans", 
                       choices = ( if(is.null(R.numericnames())) c("To Numeric" = "numeric") else if (is.null(R.categoricnames())) c("Power" = "power", "Logarithm (for a positive variable)" = "log", "To Categorical" = "categorical") else c("Power" = "power", "Logarithm (for a positive variable)" = "log", "To Categorical" = "categorical", "To Numeric" = "numeric") ))
}) 
observe({
  updateSelectizeInput(session, "var_trans", 
                       choices = c( if(input$trans %in% c("power", "log", "categorical")) R.numericnames() else R.categoricnames()  ) )
})

transform_data <- function(Data, trans, var_trans, colname, 
                           method = "direct", intervals = 4, 
                           power = 1, logbase = exp(1)){
  colname =  case_when(
    input$trans == "power" ~ paste(var_trans, sub("\\.", "", power), sep = "_"),
    input$trans == "log" ~ paste(var_trans, sub("\\.", "", "log"),  sep = "_"),
    input$trans == "categorical"~paste(var_trans, "Categorical", sep = "_"),
    input$trans == "numeric"~paste(var_trans, "Numeric", sep = "_")
  )  
  
  if (trans == "power" || trans == "log" && var_trans %in% numericNames(Data)){
    if (!is.numeric(power) ) {power <- 1} 
    if (trans == "power" )
    {interpolate(~(trans_x <- (df$var)^power), 
                 df = quote(Data), var = var_trans, power = power,
                 mydir = userdir,  file = "code_data.R")
    } else {
      interpolate(~(trans_x <- log(df$var, base = bs)), 
                 df = quote(Data), var = var_trans, 
                 bs = as.numeric(logbase), mydir = userdir, 
                 file = "code_data.R")    
    } 
    if (all(!is.infinite(trans_x))) interpolate(~(df$col <- trans_x), 
              df = quote(Data), col = colname, 
              mydir = userdir, file = "code_data.R", append = TRUE)
  } 
  else if ( trans %in% c("categorical", "numeric") ){
    if (trans == "categorical") {     
      if (method == "direct") interpolate(~(trans_x<-as.factor(df$var) ), df = quote(Data), var = var_trans,  mydir = userdir, file = "code_data.R")
      else if (method == "binning") interpolate(~(trans_x<-cut(as.numeric(df$var), breaks = intervals)), df = quote(Data), var = var_trans, intervals = intervals, mydir = userdir, file = "code_data.R")
    } else {   
      interpolate(~(trans_x <- as.numeric(type.convert(as.character(df$var)))), df = quote(Data), var = var_trans, mydir = userdir, file = "code_data.R")     
    } 
    interpolate(~(df$col <- trans_x), 
                df = quote(Data), col = colname, mydir = userdir, 
                file = "code_data.R", append = TRUE)  
  } 
  return(Data)
}

R.transform <- reactive({
  if (is.null(Data())) {return(NULL)}
  else{
    colname0 =  case_when(
      input$trans == "power" ~ paste(input$var_trans, sub("\\.", "", input$power), sep = "_"),
      input$trans == "log" ~ paste(input$var_trans,  sub("\\.", "", "log"),  sep = "_"),
      input$trans == "categorical"~paste(input$var_trans, "Categorical", sep = "_"),
      input$trans == "numeric"~paste(input$var_trans, "Numeric", sep = "_")
    )
    Data=Data()
    R_trans <- as.data.frame( transform_data( Data, 
                                              trans=input$trans, var_trans=input$var_trans, 
                                              colname = colname0,  method = input$categorical_method, 
                                              intervals = input$categorical_intervals, 
                                              power = input$power, logbase = input$log_base) )
    R_trans$R_var <-  R_trans[, input$var_trans]
    R_trans$R_trans_var<-R_trans[, colname0]
    return(R_trans)  
  }
})

output$var_trans_text <- renderText({
  return(paste0("Selected Variable: ", input$var_trans))
}) 

output$var_plot<- renderPlot({
  Data=Data()
  if ( input$trans!='numeric') {  
    hist(Data[, input$var_trans], breaks = 6, col = 'blue', main="",
         xlab=paste("Histogram of origial variable"), border = 'white') 
    box()
  } else{
    counts <- table(Data[, input$var_trans])
    barplot(counts) 
  } 
})
output$trans_plot<-renderPlot({
  dataset = R.transform()
  if ( input$trans !='categorical') {  
    hist(dataset$R_trans_var, breaks = 6, col = 'blue', main="",
         xlab=paste("Histogram of transformed variable"), border = 'white')
    box()}
  else{
    counts <- table(dataset$R_trans_var)
    barplot(counts)
    
  }
})  

observeEvent(input$savetrans, {
  values$dataset <- R.transform()%>%dplyr::select(-R_var, -R_trans_var)#%>%as.data.frame()
  cat(paste0("\n", paste(c(readLines(file.path(userdir, "code_data.R")), "\n"), 
                         collapse = "\n")), file = file.path(userdir, "code_all.R"), append = TRUE)
})

output$codeinf <- renderUI({ 
  str1 <- paste("The app cannot tell the path of the file you just uploaded.")
  str2 <- paste("To upload the file to an R console, please specify the path 
                to the file.  For example (for windows users), ")
  str3 <- paste0("Data=", "read.csv","(", '"','C',':','/','Users','/',
  'Downloads','/','mtcars.csv','"',',',  ' ', 'header=TRUE',')')
  HTML(paste(str1, str2, str3, sep = '<br/>'))
})

code <- reactiveFileReader(5000, session,  file.path(userdir, "code_all.R"), clean_readlines)  
observe({    
  updateAceEditor(session, "code_data", 
          value = paste(code(), 
          collapse = "\n"))
})
output$clip <- renderUI({
  rclipButton("clipbtn", "Copy-R-code-to-clipboard", input$code_data, icon=icon("clipboard", class = "btn btn-success"))
})

output$downloaddata <- downloadHandler(
  filename = function() { 
    paste("data_", Sys.Date(), ".csv", sep="")
  },
  content = function(file) {
    write.csv(Data(), file)
  }
)

# Reactive data to other modules
return(reactive(Data()))
 
  })
}
