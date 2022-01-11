#' About UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_About_ui <- function(id){
  ns <- NS(id)
  
  col_10 <- function(...){
    column(10, ...)
  }
  
  tagList(
    column(1),
    col_10(
    includeMarkdown(
      system.file("app/www/about.md", package = "Rstats")
    )
    ),
    column(1)
  )
}
    
#' About Server Functions
#'
#' @noRd 
mod_About_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
  })
}