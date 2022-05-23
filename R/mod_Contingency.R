#' Contingency UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_Contingency_ui <- function(id){
  ns <- NS(id)

  fluidPage(
    withMathJax(),
    sidebarLayout(
      sidebarPanel(          
        
  h3("Contingency Analysis of Two Categorical Variables"),
  br(), 
selectizeInput(ns("catvar1"), label = "Select a categorical variable", 
               choices = names(mtcars),  multiple = FALSE),
selectizeInput(ns("catvar2"), label = "Select another categorical variable", 
               choices = names(mtcars),  multiple = FALSE),
radioButtons(
  inputId = ns("analysis"),
  label = "Types of analysis",
  choices = c(
    "Contingency table" = "table",
    "Statistical inference" = "inference"
  )
),
conditionalPanel(
  condition = "input.analysis == 'inference' ",
  sprintf("\\( H_0: \\) The two classifications are independent."),
  br(),
  sprintf("\\( H_1: \\) The two classifications are not independent."),
  br(),
  selectizeInput(ns("types"), label = "Inference types", 
                 choices = c("Chi-Square test" = "chisquare",
                             "Fisher Exact test" = "fisher")
                 ),
  ns=ns
),
conditionalPanel(
  condition = "input.analysis == 'inference' & input.types=='chisquare' ",
checkboxInput( ns("correct"), "Apply continuity correction", FALSE),
ns=ns
)

    ),      
      
mainPanel(   
  
  htmlOutput(ns("selected_variable")), 
  
  conditionalPanel(
    condition = "input.analysis=='table'",
    verbatimTextOutput(ns("table")), ns=ns
  ), 
  conditionalPanel(
    condition = "input.analysis=='inference' & input.types=='chisquare'",
    verbatimTextOutput(ns("inference1")), ns=ns
  ), 
  conditionalPanel(
    condition = "input.analysis=='inference' & input.types=='fisher'",
    verbatimTextOutput(ns("inference2")), ns=ns
  ), 
  rclipboard::rclipboardSetup(),
  uiOutput(ns("clip")),
  shinyAce::aceEditor(ns("code_Contingency"), "", mode = "r", readOnly = TRUE, 
            height = "500px", fontSize = 16, wordWrap = TRUE,
            theme = "chrome")        
  
)   
    )
  )
}
    
#' Contingency Server Functions
#'
#' @noRd 
mod_Contingency_server <- function(id, Data, userdir){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
  R.categoricnames <- reactive({
    if (is.null(Data())) return(NULL)
    return(categoricNames(Data()))
  })
    
  observe({
    updateSelectizeInput(session, "catvar1", 
                         choices = c("None" = "none", R.categoricnames()  ))
    updateSelectizeInput(session, "catvar2", 
                         choices = c("None" = "none", R.categoricnames()  ))
  }) 
  
output$selected_variable <- renderText({
    if (is.null(Data())) return(NULL)
 return(paste0("<B>The first selected variable: </B>", input$catvar1,  "; ",
               "<B>The second selected variable: </B>", input$catvar2,  ". "  
               ) )
  }) 

output$table <- renderPrint({
  if ( input$catvar1=="none" | input$catvar2=="none" ) {return(NULL)}  
  else{
    Data=Data()
  interpolate(~(  tab=table(df$var1, df$var2)  ), 
              df = quote(Data), var1=as.name(input$catvar1), 
              var2=as.name(input$catvar2),
              file = "code_Contingency.R", 
              mydir = userdir, append = FALSE, save_result = TRUE, eval = TRUE) 
  interpolate(~(  tab  ), 
              file = "code_Contingency.R", 
              mydir = userdir, append = FALSE, save_result = TRUE, eval = TRUE)
  }
 })
  
tab <- reactive ({ 
  if ( input$catvar1=="none" | input$catvar2=="none" ) {return(NULL)}  
  else{
    Data=Data()
    tab=table(Data[,input$catvar1], Data[,input$catvar2])
    return(tab)
  } 
  })
  
output$inference1 <- renderPrint({
  tab=tab()
  if (is.null(tab) ) {return(NULL)}  
  else{
    Data=Data()
    interpolate(~(  test1=chisq.test(tab, correct = correction)  ), 
                file = "code_Contingency.R", correction=input$correct,
                mydir = userdir, append = FALSE, save_result = TRUE, eval = TRUE)
    interpolate(~(  list(test=test1,observed=test1$observed,expected=test1$expected)  ), 
                file = "code_Contingency.R", 
                mydir = userdir, append = FALSE, save_result = TRUE, eval = TRUE)
  }
})

output$inference2 <- renderPrint({
  tab=tab()
  if (is.null(tab) ) {return(NULL)}
  else{
    Data=Data()
    tab=tab()
    interpolate(~(  fisher.test(tab)  ), 
                file = "code_Contingency.R", correction=input$correct,
                mydir = userdir, append = FALSE, save_result = TRUE, eval = TRUE)
  }
}) 
  
code <- reactiveFileReader(5000, session, file.path(userdir, "code_all.R"), clean_readlines)
observe({    
  updateAceEditor(session, "code_Contingency", 
                  value = paste(code(),  collapse = "\n"))
}) 
output$clip <- renderUI({
  rclipButton("clipbtn", "Copy-R-code-to-clipboard", input$code_Contingency, icon=icon("clipboard", class = "btn btn-success"))
})

  })
}
