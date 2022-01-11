#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function( input, output, session ) {
  userdir <- tempfile()
  dir.create(userdir, recursive = TRUE)
  sapply(file.path(userdir, dir(userdir)[grep("code_", dir(userdir))]), file.remove)
  # Your application server logic:
  mod_Probability_Calculation_server("Probability_Calculation_ui_1") 
  mod_Quantile_Calculation_server("Quantile_Calculation_ui_1")
  mod_Summary_Inferences_server("Summary_Inferences_ui_1")
  mod_Data_Inferences_server("Data_Inferences_ui_1")
  Data<-mod_Data_server("Data_ui_1", userdir)
  mod_Numerical_server("Numerical_ui_1", Data, userdir)
  mod_Categorical_server("Categorical_ui_1", Data, userdir)
  mod_SLR_Model_server("SLR_Model_ui_1", Data, userdir)
  mod_MLR_Model_server("MLR_Model_ui_1", Data, userdir)
  mod_Contingency_server("Contingency_ui_1", Data, userdir)
  mod_One_Way_server("One_Way_ui_1", Data, userdir)
  mod_Two_Way_server("Two_Way_ui_1", Data, userdir)
  ############### Update directory:
  newuserdir <- tempfile()
  dir.create(newuserdir, recursive = TRUE)
  sapply(file.path(newuserdir, dir(newuserdir)[grep("code_", dir(newuserdir))]), file.remove)
  file.copy(file.path(userdir, "code_all.R"), newuserdir)
  userdir <- newuserdir
  dir.create(file.path(userdir, "Data"))
  ###############
  mod_About_server("About_ui_1")
}