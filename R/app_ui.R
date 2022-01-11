#' The application User-Interface
#' 
#' @param request Internal parameter for `{shiny}`. 
#'     DO NOT REMOVE.
#' @import shiny
## @noRd
## app_ui <- function() {
app_ui <- function(request) {
  
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    
    # Your application UI logic 
    navbarPage("Rstats", theme = 
                 shinythemes::shinytheme("united"),
               navbarMenu("Distributions", 
                          tabPanel("Probability Calculation",
                                   mod_Probability_Calculation_ui("Probability_Calculation_ui_1")
                          ),
                          tabPanel("Quantile Calculation",
                                   mod_Quantile_Calculation_ui("Quantile_Calculation_ui_1")
                          ) 
               ),  
               navbarMenu("Inferences",
                          tabPanel("Statistics",
                                   mod_Summary_Inferences_ui("Summary_Inferences_ui_1") ),
                          tabPanel("Data",
                                   mod_Data_Inferences_ui("Data_Inferences_ui_1") )
               ),  
               tabPanel("Data Import", mod_Data_ui("Data_ui_1") ),
               navbarMenu("Univariate",
                          tabPanel("Numerical Data Analysis", mod_Numerical_ui("Numerical_ui_1")),
                          tabPanel("Categorical Data Analysis",mod_Categorical_ui("Categorical_ui_1")) 
               ),
               navbarMenu("Multivariate",
                          tabPanel("SLR Model", mod_SLR_Model_ui("SLR_Model_ui_1") ),
                          tabPanel("MLR Model", mod_MLR_Model_ui("MLR_Model_ui_1") ),
                          tabPanel("Contingency Analysis", mod_Contingency_ui("Contingency_ui_1"))
               ),
               navbarMenu("ANOVA",
                          tabPanel("One-way ANOVA", mod_One_Way_ui("One_Way_ui_1") ),
                          tabPanel("Two-way ANOVA", mod_Two_Way_ui("Two_Way_ui_1")  ) ),
               tabPanel("About", mod_About_ui("About_ui_1") )      
    ),     
    
  tags$head(includeScript("inst/app/www/google-analytics.js"))
    
  )
}

#' Add external Resources to the Application
#' 
#' This function is internally used to add external 
#' resources inside the Shiny application. 
#' 
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function(){
  
  add_resource_path(
    'www', app_sys('app/www')
  )
  
  
  tags$head(
    favicon(ext = 'png'),
    bundle_resources(
      path = app_sys('app/www'),
      app_title = 'Web.R.PASSHE'
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert() 
  )
}