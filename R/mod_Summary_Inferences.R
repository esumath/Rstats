#' Summary_Inferences UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_Summary_Inferences_ui <- function(id){
  ns <- NS(id)

inferencetypes <- list(
    "One Population Mean - T Test/Interval"="OneMeanT",
    "One Population Proportion - Z Test/Interval"="OneProp",
    "Two Population Means - T Test/Interval"="TwoMeanT",
    "Two Population Proportions - Z Test/Interval"="TwoProp",
    "One Population Variance"="OneVariance", 
    "Two Population Variances"="TwoVariance",
    "Goodness-of-Fit Test"="FreqTable",
    "Independence between Two Classifications"="ConTable",
    "One Population Mean - Z Test/Interval"="OneMeanZ",
    "Two Population Means - Z Test/Interval"="TwoMeanZ")
  
 
  fluidPage(
    
    withMathJax(),
    sidebarLayout(
      
      sidebarPanel(          
        
  h4("Inference about population parameters when sample statistics are given:"),
        br(),
        selectInput(
          inputId = ns("inference"),
          label = "Inference about:",
          choices = inferencetypes,
          multiple = FALSE,
          selected = "one mean"
        ),
conditionalPanel(
condition = "input.inference == 'OneMeanZ' | input.inference == 'OneMeanT' | input.inference == 'OneProp' | input.inference == 'OneVariance'",
    numericInput(ns("n0"), "Sample size \\( n = \\)",
                 value = 31, min = 2, step = 1
      ), ns=ns
),
conditionalPanel(
  condition = "input.inference == 'OneMeanZ' | input.inference == 'OneMeanT'",
  numericInput(ns("mean_onemean"), "Sample mean \\( \\bar{x} = \\)",
               value = 490,  step = 1
  ),ns=ns
),
  conditionalPanel(
    condition = "input.inference == 'OneMeanZ'",
    numericInput(ns("sigma2_onemeanz"), "Population variance \\( \\sigma^2 = \\)",
                 value = 1000, min = 0, step = 1
    ), ns=ns
    ),
  conditionalPanel(
    condition = "input.inference == 'OneMeanT'",
      numericInput(ns("s2_onemeant"), "Sample variance \\( s^2 = \\)",
                   value = 1000, min = 0, step = 1
      ),ns=ns
  ),
    conditionalPanel(
      condition = "input.inference == 'OneProp'",
      numericInput(ns("x_oneprop"), " Number of successes \\(x = \\)",
                   value = 10, min = 0, step = 1
      ),
    ns=ns
 ),
conditionalPanel(
  condition = "input.inference == 'TwoMeanZ' | input.inference == 'TwoMeanT' | input.inference == 'TwoProp' | input.inference == 'TwoVariance' ",
  numericInput(ns("n1_two"), "Sample 1 size \\( n_1 = \\)",
               value = 20, min = 2, step = 1
  ),
  conditionalPanel(
    condition ="input.inference != 'TwoProp' & input.inference != 'TwoVariance'",
    numericInput(ns("mean1_twomean"), "Sample 1 mean \\( \\bar{x_1} = \\)",
                 value = 490,  step = 1
    ),    ns=ns 
  ),
  conditionalPanel(
    condition = "input.inference == 'TwoProp'",
    numericInput(ns("x1_twoprop"), "Number of successes in sample 1 \\(x_1 = \\)",
                 value = 10, min = 0, step = 1
    ),  ns=ns
  ),
  ns=ns
),
conditionalPanel(
  condition = "input.inference == 'TwoMeanZ'",
  numericInput(ns("sigma21_twomeanz"), "Population 1 variance \\( \\sigma_1^2 = \\)",
               value = 2000, min = 0, step = 1
  ),
  ns=ns
),
conditionalPanel(
  condition = "input.inference == 'TwoMeanT'",
  numericInput(ns("s21_twomeant"), "Sample 1 variance \\( s_1^2 = \\)",
               value = 2200, min = 0, step = 1
  ),
  ns=ns
),
conditionalPanel(
  condition = "input.inference == 'TwoMeanZ' | input.inference == 'TwoMeanT' | input.inference == 'TwoProp' | input.inference == 'TwoVariance' ",
  numericInput(ns("n2_two"), "Sample 2 size \\( n_2 = \\)",
               value = 40, min = 2, step = 1
  ),
  conditionalPanel(
    condition ="input.inference != 'TwoProp' &  input.inference != 'TwoVariance'",
    numericInput(ns("mean2_twomean"), "Sample 2 mean \\( \\bar{x_2} = \\)",
                 value = 470,  step = 1
    ),    ns=ns 
  ),
  conditionalPanel(
    condition = "input.inference == 'TwoProp'",
    numericInput(ns("x2_twoprop"), "Number of successes in sample 2 \\(x_2 = \\)",
                 value = 12, min = 0, step = 1
    ),    ns=ns
  ),
 ns=ns 
),
conditionalPanel(
      condition = "input.inference == 'TwoMeanZ'",
      numericInput(ns("sigma22_twomeanz"), "Population 2 variance \\( \\sigma_2^2 = \\)",
                   value = 2500, min = 0, step = 1
      ),
    ns=ns
  ),
  conditionalPanel(
      condition = "input.inference == 'TwoMeanT'",
      numericInput(ns("s22_twomeant"), "Sample 2 variance \\( s_2^2 = \\)",
                   value = 2500, min = 0, step = 1
      ),  
    checkboxInput( ns("var.equal"), "Variances of the populations are equal", FALSE),
          ns=ns
),
conditionalPanel(
        condition = "input.inference == 'OneVariance'",
        numericInput(ns("s2_onevariance"), "Sample variance \\( s^2 = \\)",
                     value = 10.5, min = 0, step = 1
        ),
    ns=ns),
   conditionalPanel(
    condition = "input.inference == 'TwoVariance'",
      numericInput(ns("s21_twovariance"), "Sample  1 variance \\( s^2_1 = \\)",
                   value = 5, min = 0, step = 1
      ),
      numericInput(ns("s22_twovariance"), "Sample  2 variance \\( s^2_2 = \\)",
                   value = 6, min = 0, step = 1
      ),
  ns=ns),
  conditionalPanel(
    condition = "input.inference == 'FreqTable'",
    textInput(ns("freq_freqtable"), "Enter corresponding frequencies of the classification:", 
              value = "15, 20, 30, 18, 12", 
              placeholder = "Enter integer values separated by a comma, e.g. 12, 20, 8, 25, etc."),
    ns=ns),
  conditionalPanel(
    condition = "input.inference == 'ConTable'",
    radioButtons(
      inputId = ns("nrows"), 
  label = "Number of rows of the contingency table (all rows must have the same number of counts,up to 4 rows for this app):",
      choices = c(
        "Number of rows of the contingency table = 2" = "2",
        "Number of rows of the contingency table = 3" = "3",
        "Number of rows of the contingency table = 4" = "4"
      )
    ),
    conditionalPanel(
      condition = "input.nrows == '2'",
      textInput(ns("row21"), "Frequencies in the first row", 
                value = "15, 20, 30, 18, 12", 
                placeholder = "Enter integer values separated by a comma, e.g. 12, 20, 8, 25, etc."),
      textInput(ns("row22"), "Frequencies in the second row", 
                value = "10, 18, 22, 11, 23", 
                placeholder = "Enter integer values separated by a comma, e.g. 12, 20, 8, 25, etc."),
      ns=ns
    ),
    conditionalPanel(
      condition = "input.nrows == '3'",
      textInput(ns("row31"), "Frequencies in the first row", 
                value = "15, 20, 30, 18, 12", 
                placeholder = "Enter integer values separated by a comma, e.g. 12, 20, 8, 25, etc."),
      textInput(ns("row32"), "Frequencies in the second row", 
                value = "10, 25, 22, 11, 27", 
                placeholder = "Enter integer values separated by a comma, e.g. 12, 20, 8, 25, etc."),
      textInput(ns("row33"), "Frequencies in the third row", 
                value = "10, 15, 12, 21, 14", 
                placeholder = "Enter integer values separated by a comma, e.g. 12, 20, 8, 25, etc."),
            ns=ns
    ),
    conditionalPanel(
      condition = "input.nrows == '4'",
      textInput(ns("row41"), "Frequencies in the first row", 
                value = "15, 20, 30, 18, 12", 
                placeholder = "Enter integer values separated by a comma, e.g. 12, 20, 8, 25, etc."),
      textInput(ns("row42"), "Frequencies in the second row", 
                value = "10, 25, 22, 11, 35", 
                placeholder = "Enter integer values separated by a comma, e.g. 12, 20, 8, 25, etc."),
      textInput(ns("row43"), "Frequencies in the third row", 
                value = "10, 15, 18, 13, 14", 
                placeholder = "Enter integer values separated by a comma, e.g. 12, 20, 8, 25, etc."),
      textInput(ns("row44"), "Frequencies in the fourth row", 
                value = "16, 12, 19, 31, 27", 
                placeholder = "Enter integer values separated by a comma, e.g. 12, 20, 8, 25, etc."),
            ns=ns
    ), 
  ns=ns 
  ),        
        tags$b("Null hypothesis"),
  conditionalPanel(
      condition = "input.inference == 'OneMeanZ' | input.inference == 'OneMeanT' ",
          sprintf("\\( H_0 : \\mu = \\mu_0 =  \\)"), ns=ns
        ),
  conditionalPanel(
    condition = "input.inference == 'OneProp'",
    sprintf("\\( H_0 : p = p_0 =  \\)"), ns=ns
  ),  
        conditionalPanel(
          condition = "input.inference == 'TwoMeanZ' | input.inference == 'TwoMeanT'",
          sprintf("\\( H_0 : \\mu_1 - \\mu_2 = \\)"), ns=ns
        ),
        conditionalPanel(
          condition = "input.inference == 'TwoProp'",
          sprintf("\\( H_0 : p_1 - p_2 = 0\\)"), ns=ns
        ),
        conditionalPanel(
          condition = "input.inference == 'OneVariance'",
          sprintf("\\( H_0 : \\sigma^2 = \\sigma_0^2=\\)"), ns=ns
        ),
        conditionalPanel(
          condition = "input.inference == 'TwoVariance'",
          sprintf("\\( H_0 : \\sigma^2_1 = \\sigma^2_2 \\)"), ns=ns
        ), 
  conditionalPanel(
    condition = "input.inference == 'ConTable'",
    sprintf("\\( H_0: \\) The two classifications are independent"), ns=ns
  ),
conditionalPanel( 
   condition = " input.inference == 'OneMeanZ' | input.inference == 'OneMeanT' | input.inference == 'TwoMeanZ' | input.inference == 'TwoMeanT'",
          numericInput(ns("h0_mean"),     label = NULL,
                       value = 480, step = 1
          ), ns=ns   
        ),  
  conditionalPanel( 
    condition = " input.inference == 'OneProp'",
    numericInput(ns("h0_oneprop"),
                 label = NULL,
                 value = 0.2, step = 0.1
    ), ns=ns   
  ),
  conditionalPanel( 
    condition = " input.inference == 'OneVariance'",
    numericInput(ns("h0_onevariance"),
                 label = NULL,
                 value = 9.0, step = 0.1
    ), ns=ns   
  ),
  conditionalPanel( 
    condition = " input.inference == 'FreqTable'",
    textInput(ns("h0_freqtable"), "Probabilities under \\( H_0\\)", 
              value = "0.2, 0.2, 0.2, 0.2, 0.2", 
              placeholder = "Enter values separated by a comma with decimals as points, e.g. 4.2, 4.4, 5, 5.03, etc."),
         ns=ns   
  ),  
  conditionalPanel(
    condition = "input.inference == 'OneMeanZ' | input.inference == 'OneMeanT' ",
    radioButtons(
            inputId = ns("alternative_onemean"),
            label = "Alternative \\( H_1 : \\)",
            choices = c(
              "\\( \\mu \\neq \\mu_0 \\)" = "two.sided",
              "\\( \\mu < \\mu_0 \\)" = "less",
              "\\( \\mu > \\mu_0 \\)" = "greater"
            )
          ), ns=ns
        ), 
  conditionalPanel(
    condition = "input.inference == 'OneProp' ",
    radioButtons(
      inputId = ns("alternative_oneprop"),
      label = "Alternative \\( H_1 : \\)",
      choices = c(
        "\\( p \\neq p_0 \\)" = "two.sided",
        "\\( p < p_0 \\)" = "less",
        "\\( p > p_0 \\)" = "greater"
              )
    ), ns=ns
  ),
  conditionalPanel(
    condition = "input.inference == 'TwoMeanZ' |  input.inference == 'TwoMeanT'",
    radioButtons(
      inputId = ns("alternative_twomean"),
      label = "Alternative \\( H_1 : \\)",
      choices = c(
        "\\( \\mu_1 - \\mu_2 \\neq \\)" = "two.sided",
        "\\( \\mu_1 - \\mu_2< \\)" = "less",
        "\\( \\mu_1 - \\mu_2 > \\)" = "greater"
              )
    ), ns=ns
  ),
  conditionalPanel(
    condition = "input.inference == 'TwoProp' ",
    radioButtons(
      inputId = ns("alternative_twoprop"),
      label = "Alternative \\( H_1 : \\)",
      choices = c(
        "\\( p_1 - p_2 \\neq 0 \\)" = "two.sided",
        "\\( p_1 - p_2 < 0 \\)" = "less",
        "\\( p_1 - p_2 > 0 \\)" = "greater"
              )
    ), 
   ns=ns
  ),
  conditionalPanel(
    condition = "input.inference == 'OneVariance' ",
    radioButtons(
      inputId = ns("alternative_onevariance"),
      label = "Alternative \\( H_1 : \\)",
      choices = c(
        "\\( \\sigma^2 \\neq \\sigma_0^2 \\)" = "two.sided",
        "\\( \\sigma^2 < \\sigma_0^2 \\)" = "less",
        "\\( \\sigma^2 > \\sigma_0^2 \\)" = "greater"
              )
    ), ns=ns
  ),
  conditionalPanel(
          condition = "input.inference == 'TwoVariance'",
          radioButtons(
            inputId = ns("alternative_twovariance"),
            label = "Alternative \\( H_1 : \\)",
            choices = c(
              "\\( \\sigma^2_1 \\neq \\sigma^2_2 \\)" = "two.sided",
              "\\( \\sigma^2_1 < \\sigma^2_2 \\)" = "less",
              "\\( \\sigma^2_1 > \\sigma^2_2 \\)" = "greater"
                          )
          ), ns=ns
        ),            
  conditionalPanel(
    condition = "input.inference == 'FreqTable'",
    sprintf("\\( H_1 : \\) Not all probabilities are equal to the specified values."),
    ns=ns
  ),
  conditionalPanel(
    tags$b("Alternative hypothesis"),
    br(),
    condition = "input.inference == 'ConTable'",
    sprintf("\\( H_1 : \\) The two classifications are not independent."),
    ns=ns
  ),
      sliderInput(ns("alpha"),
                    "Significance level \\(\\alpha = \\)",
                    min = 0.001,
                    max = 0.20,
                    step=0.001,
                    value = 0.05
        )
        
        
      ),    
      
      
      
  mainPanel(
       
   
    conditionalPanel(
      condition = "input.inference == 'OneMeanZ'",
      uiOutput(ns("theory_onemeanz")), ns = ns
    ),
    conditionalPanel(
      condition = "input.inference == 'OneMeanT'",
      uiOutput(ns("theory_onemeant")), ns = ns
    ),
    conditionalPanel(
      condition = "input.inference == 'OneProp'",
      uiOutput(ns("theory_oneprop")), ns = ns
    ),
    conditionalPanel(
      condition = "input.inference == 'TwoMeanZ'",
      uiOutput(ns("theory_twomeanz")), ns = ns
    ),
    conditionalPanel(
      condition = "input.inference == 'TwoMeanT'",
      uiOutput(ns("theory_twomeant")), ns = ns
    ),
    conditionalPanel(
      condition = "input.inference == 'TwoProp'",
      uiOutput(ns("theory_twoprop")), ns = ns
    ),
    conditionalPanel(
      condition = "input.inference == 'OneVariance'",
      uiOutput(ns("theory_onevariance")), ns = ns
    ),
    conditionalPanel(
      condition = "input.inference == 'TwoVariance'",
      uiOutput(ns("theory_twovariance")), ns = ns
    ),
    conditionalPanel(
      condition = "input.inference == 'FreqTable'",
      uiOutput(ns("theory_freqtable")), ns = ns
    ),
    conditionalPanel(
      condition = "input.inference == 'ConTable'  ",
      uiOutput(ns("theory_contable")), ns = ns
    ),
    br(),
    conditionalPanel(
      condition = "input.inference == 'OneMeanZ'",
      plotOutput(ns("plot_onemeanz"), height="270px"), ns = ns
    ),
    conditionalPanel(
      condition = "input.inference == 'OneMeanT'",
      plotOutput(ns("plot_onemeant"), height="270px"), ns = ns
    ),
    conditionalPanel(
      condition = "input.inference == 'OneProp'",
      plotOutput(ns("plot_oneprop"), height="270px"), ns = ns
    ),
    conditionalPanel(
      condition = "input.inference == 'TwoMeanZ'",
      plotOutput(ns("plot_twomeanz"), height="270px"), ns = ns
    ),
    conditionalPanel(
      condition = "input.inference == 'TwoMeanT'",
      plotOutput(ns("plot_twomeant"), height="270px"), ns = ns
    ),
    conditionalPanel(
      condition = "input.inference == 'TwoProp'",
      plotOutput(ns("plot_twoprop"), height="270px"), ns = ns
    ),
    conditionalPanel(
      condition = "input.inference == 'OneVariance'",
      plotOutput(ns("plot_onevariance"), height="270px"), ns = ns
    ),
    conditionalPanel(
      condition = "input.inference == 'TwoVariance'",
      plotOutput(ns("plot_twovariance"), height="270px"), ns = ns
    ),
    conditionalPanel(
      condition = "input.inference == 'FreqTable'",
      plotOutput(ns("plot_freqtable"), height="270px"), ns = ns
    ),
    conditionalPanel(
      condition = "input.inference == 'ConTable'  ",
    plotOutput(ns("plot_contable"), height="270px"), ns = ns
    ),
       tags$b("R output:"),  
    conditionalPanel(
      condition = "input.inference == 'OneMeanZ'",
      verbatimTextOutput(ns("results_onemeanz")), ns = ns
    ),
    conditionalPanel(
      condition = "input.inference == 'OneMeanT'",
      verbatimTextOutput(ns("results_onemeant")), ns = ns
    ),
    conditionalPanel(
      condition = "input.inference == 'OneProp'",
      verbatimTextOutput(ns("results_oneprop")), ns = ns
    ),
    conditionalPanel(
      condition = "input.inference == 'TwoMeanZ'",
      verbatimTextOutput(ns("results_twomeanz")), ns = ns
    ),
    conditionalPanel(
      condition = "input.inference == 'TwoMeanT'",
      verbatimTextOutput(ns("results_twomeant")), ns = ns
    ),
    conditionalPanel(
      condition = "input.inference == 'TwoProp'",
      verbatimTextOutput(ns("results_twoprop")), ns = ns
    ),
    conditionalPanel(
      condition = "input.inference == 'OneVariance'",
      verbatimTextOutput(ns("results_onevariance")), ns = ns
    ),
    conditionalPanel(
      condition = "input.inference == 'TwoVariance'",
      verbatimTextOutput(ns("results_twovariance")), ns = ns
    ),
    conditionalPanel(
      condition = "input.inference == 'FreqTable'",
      verbatimTextOutput(ns("results_freqtable")), ns = ns
    ),
    conditionalPanel(
      condition = "input.inference == 'ConTable' && input.nrows=='2' ",
      verbatimTextOutput(ns("results_contable2")), ns = ns
    ),
    conditionalPanel(
      condition = "input.inference == 'ConTable' && input.nrows=='3' ",
      verbatimTextOutput(ns("results_contable3")), ns = ns
    ),
    conditionalPanel(
      condition = "input.inference == 'ConTable' && input.nrows=='4' ",
      verbatimTextOutput(ns("results_contable4")), ns = ns
    ),
        tags$b("R code:"),    
    conditionalPanel(
      condition = "input.inference == 'OneMeanZ'",
      verbatimTextOutput(ns("code_onemeanz")), ns = ns
    ),
    conditionalPanel(
      condition = "input.inference == 'OneMeanT'",
      verbatimTextOutput(ns("code_onemeant")), ns = ns
    ),
    conditionalPanel(
      condition = "input.inference == 'OneProp'",
      verbatimTextOutput(ns("code_oneprop")), ns = ns
    ),
    conditionalPanel(
      condition = "input.inference == 'TwoMeanZ'",
      verbatimTextOutput(ns("code_twomeanz")), ns = ns
    ),
    conditionalPanel(
      condition = "input.inference == 'TwoMeanT'",
      verbatimTextOutput(ns("code_twomeant")), ns = ns
    ),
    conditionalPanel(
      condition = "input.inference == 'TwoProp'",
      verbatimTextOutput(ns("code_twoprop")), ns = ns
    ),
    conditionalPanel(
      condition = "input.inference == 'OneVariance'",
      verbatimTextOutput(ns("code_onevariance")), ns = ns
    ),
    conditionalPanel(
      condition = "input.inference == 'TwoVariance'",
      verbatimTextOutput(ns("code_twovariance")), ns = ns
    ),
    conditionalPanel(
      condition = "input.inference == 'FreqTable'",
      verbatimTextOutput(ns("code_freqtable")), ns = ns
    ),
    conditionalPanel(
      condition = "input.inference == 'ConTable' && input.nrows=='2' ",
      verbatimTextOutput(ns("code_contable2")), ns = ns
    ),
    conditionalPanel(
      condition = "input.inference == 'ConTable' && input.nrows=='3' ",
      verbatimTextOutput(ns("code_contable3")), ns = ns
    ),
    conditionalPanel(
      condition = "input.inference == 'ConTable' && input.nrows=='4' ",
      verbatimTextOutput(ns("code_contable4")), ns = ns
    )
    
  
    )       
  )
  )    
}

  
    
#' Summary_Inferences Server Functions
#'
#' @noRd 
mod_Summary_Inferences_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns 
    
output$theory_onemeanz <- renderUI({ 
  if (any(is.na(input$n0), is.na(input$mean_onemean), is.na(input$sigma2_onemeanz), is.na(input$h0_mean)))
  {
paste("Please enter all required statistics and the parameter value under the null hypothesis.")
  }else{
C.level = 1 - (input$alpha) 
n = input$n0 
xbar= input$mean_onemean 
sigma2= input$sigma2_onemeanz 
test=zsum.test(mean.x=xbar, sigma.x = sqrt(sigma2), n.x=n,
               alternative=input$alternative_onemean,
               mu = input$h0_mean, conf.level=C.level) 

withMathJax(
  tags$b("Confidence interval"),
  br(),
  case_when(  
    input$alternative_onemean=="less" ~ 
      paste0(
        C.level * 100, "% CI for \\(\\mu = (-\\infty, \\bar{x} + \\)", 
        "\\( Z_{",  
        input$alpha,
        ", n-1}",
        " \\dfrac{\\sigma}{\\sqrt{n}} )= \\) ",
        "\\( ( -\\infty,  \\) ", xbar,"\\( + \\)", 
        round(qnorm(input$alpha, lower.tail = FALSE), 6), 
        " * ", round(sqrt(sigma2), 6), " / ", 
        round(sqrt(n), 6), "\\( ) \\) ", "\\( = \\) ",
        "(", round(test$conf.int[1], 6), ",  ", 
        round(test$conf.int[2], 6), ")"
      ),
    input$alternative_onemean=="greater" ~ 
      paste0(
        C.level * 100, "% CI for \\(\\mu = ( \\bar{x} - \\)", 
        "\\( Z_{",  
        input$alpha,
        ", n-1}",
        " \\dfrac{\\sigma}{\\sqrt{n}} ,  \\infty)  = \\) ",
        "\\( (   \\) ", xbar, "\\( - \\)",
        round(qnorm(input$alpha, lower.tail = FALSE), 6), 
        " * ", round(sqrt(sigma2), 6), " / ", 
        round(sqrt(n), 6), ",  ",
        "\\(  \\infty)  \\) ",  "\\( = \\) ",
        "(", round(test$conf.int[1], 6), ",  ", 
        round(test$conf.int[2], 6), ")"
      ),
    input$alternative_onemean == "two.sided" ~
      paste0(
        C.level * 100, "% CI for \\(\\mu = \\bar{x} \\pm \\)", 
        "\\( Z_{",  
        input$alpha/2,
        ", n-1}",
        " \\dfrac{\\sigma}{\\sqrt{n}} = \\) ",
        round(test$estimate, 6), "  \\( \\pm \\) ", "\\( ( \\)", 
        round(qnorm(input$alpha/2, lower.tail = FALSE), 6), 
        " * ", round(sqrt(sigma2), 6), " / ", 
        round(sqrt(n), 6), "\\( ) \\) ", "\\( = \\) ",
        "(", round(test$conf.int[1], 6), ",  ", 
        round(test$conf.int[2], 6), ")"
      )
  ),  
      br(),
  tags$b("Hypothesis test"),
  br(),
  paste0("1. \\(H_0 : \\mu = \\) ", test$null.value, 
         " and \\(H_1 : \\mu \\) ", ifelse(input$alternative_onemean == "two.sided", 
          "\\( \\neq \\) ", ifelse(input$alternative_onemean == "greater", "\\( > \\) ", "\\( < \\) ")), test$null.value),
  br(),
  paste0(
    "2. Test statistic : \\(z_{obs} = \\dfrac{\\bar{x} - \\mu_0}{ \\sigma / \\sqrt{n}} = \\) ",
    "(", round(test$estimate, 6), ifelse(test$null.value >= 0, 
          paste0(" - ", test$null.value), paste0(" + ", abs(test$null.value))), ") / ", 
    round(sqrt(sigma2)/sqrt(n), 6), " \\( = \\) ",
    round(test$statistic, 6)
  ),
  br(),
  paste0(
    "3. p-value : ", 
    case_when( 
      input$alternative_onemean == "less" ~ 
        paste0("P\\( (Z \\)  ",  "\\( \\leq  z_{obs} )  \\) = ", 
               round(test$p.value,6)),
      input$alternative_onemean == "greater" ~ 
        paste0("P\\( (Z \\)  ",  "\\( \\geq  z_{obs} )  \\) = ", 
               round(test$p.value,6)),
      input$alternative_onemean == "two.sided"~ 
        paste0("2*min ", "(P\\( (Z \\)  ",  "\\( \\leq  z_{obs} ),  \\) ",
               "P\\( (Z \\) ",  "\\( \\geq  z_{obs} )  \\) ) =", round(test$p.value,6))
    )
  ),
  br(),
  paste0("4. Conclusion : ", ifelse(test$p.value <= input$alpha, 
         "Reject \\(H_0\\)", "Do not reject \\(H_0\\)")),
  br(),
  tags$b("Interpretation"),
  br(),
  paste0("At the ", input$alpha * 100, "% significance level, ", 
         ifelse(test$p.value <= input$alpha, 
                "we reject the null hypothesis", "we do not reject the null hypothesis"), 
         " \\((p\\)-value ",  round(test$p.value, 6), ")", ".")
)
}

})   

output$plot_onemeanz <- renderPlot({
  if (any(is.na(input$n0), is.na(input$mean_onemean), is.na(input$sigma2_onemeanz), is.na(input$h0_mean)))
  { return(NULL)}else{
  C.level = 1 - (input$alpha) 
  n = input$n0 
  xbar= input$mean_onemean 
  sigma2= input$sigma2_onemeanz 
  test=BSDA::zsum.test(mean.x=xbar, sigma.x = sqrt(sigma2), n.x=n,
                 alternative=input$alternative_onemean,
                 mu = input$h0_mean, conf.level=C.level) 
  
  zstat=test$statistic 
  abszstat=abs(zstat) 
  range1=qnorm(0.9999, lower.tail = FALSE)  
  range2=qnorm(0.9999, lower.tail = TRUE) 
  p= ggplot(data.frame(x = c(range1, range2)), aes(x = x))
  if (input$alternative_onemean == "less") {
    p <- p +
      geom_area(stat = "function", fun = dnorm, args = list(mean = 0, sd = 1), 
                xlim = c(range1,zstat),fill="blue",alpha = 0.4) +
      geom_area(stat = "function", fun = dnorm,args = list(mean = 0, sd = 1), 
                xlim = c(zstat, range2),fill="grey",alpha = 0.2) 
  } else if (input$alternative_onemean == "greater"){
    p <- p +
      geom_area(stat = "function", fun = dnorm, args = list(mean = 0, sd = 1), 
                xlim = c(range1,zstat),fill="grey",alpha = 0.2) +
      geom_area(stat = "function", fun = dnorm,args = list(mean = 0, sd = 1), 
                xlim = c(zstat, range2),fill="blue",alpha = 0.4) 
  } else  
    {
    p <- p +
      geom_area(stat = "function", fun = dnorm, args = list(mean = 0, sd = 1), 
                xlim = c(range1,-abszstat),fill="blue",alpha = 0.4) +
      geom_area(stat = "function", fun = dnorm,args = list(mean = 0, sd = 1), 
                xlim = c(-abszstat, abszstat),fill="grey",alpha = 0.2) +
      geom_area(stat = "function", fun = dnorm,args = list(mean = 0, sd = 1), 
                xlim = c(abszstat, range2),fill="blue",alpha = 0.4)+
      geom_vline(xintercept = -zstat, color = "red")
    }
  p<-p+theme_minimal()+
    geom_vline(xintercept = zstat, color = "red") +
    geom_text(aes(x = zstat, 
                  label = paste0("Test statistic = ", round(test$statistic, 6)), y = 0.2), 
              colour = "red", angle = 90, vjust = 1.3, text = element_text(size = 11)) +
    ggtitle(paste0("Standard Normal distribution", " N(0, 1)", 
                   ", ",  "p-value is indicated by the shaded area")) +
    theme(plot.title = element_text(face = "bold", hjust = 0.5)) +
    ylab("Density") +
    xlab("x")
  p
  }
})

 
output$results_onemeanz<- metaRender2( 
  renderPrint,
  { 
    if ( any(is.na(input$n0), is.na(input$mean_onemean), is.na(input$sigma2_onemeanz), is.na(input$h0_mean) ) )
    {
      metaExpr( paste("Please enter all required statistics and the parameter value under the null hypothesis."))
    }else{
    metaExpr({
    test=zsum.test(mean.x=..(input$mean_onemean), sigma.x = sqrt(..(input$sigma2_onemeanz)), n.x=..(input$n0),
                         alternative=..(input$alternative_onemean),
                         mu = ..(input$h0_mean), conf.level=1-..(input$alpha))
    CI= test$conf.int
    cat("Confidence interval", "of level", 1-..(input$alpha), "is :", CI)
    test  
    })
    } 
  }
)     

 
output$theory_onemeant <- renderUI({ 
  if ( any(is.na(input$n0), is.na(input$mean_onemean), is.na(input$sigma2_onemeanz), is.na(input$h0_mean) ) )
  {
    paste("Please enter all required statistics and the parameter value under the null hypothesis.")
  }else{
  C.level = 1 - (input$alpha) 
  n = input$n0 
  xbar= input$mean_onemean 
  s2=input$s2_onemeant 
  test<-tsum.test(mean.x=xbar, s.x = sqrt(s2), n.x=n,
                  alternative=input$alternative_onemean,
                  mu = input$h0_mean, conf.level=C.level) 
  
  withMathJax(
    
    tags$b("Confidence interval"),
    br(),
    case_when(  
      input$alternative_onemean=="less" ~ 
        paste0(
          C.level * 100, "% CI for \\(\\mu = (-\\infty, \\bar{x} + \\)", 
          "\\( t_{",  
          input$alpha,
          ", n-1}",
          " \\dfrac{s}{\\sqrt{n}} )= \\) ",
          "\\( ( -\\infty,  \\) ", xbar,"\\( + \\)", 
          round(qt(input$alpha, df = test$parameter, lower.tail = FALSE), 6), 
          " * ", round( sqrt(s2), 6), " / ", 
          round(sqrt(n), 6), "\\( ) \\) ", "\\( = \\) ",
          "(", round(test$conf.int[1], 6), ",  ", 
          round(test$conf.int[2], 6), ")"
        ),
      input$alternative_onemean=="greater" ~ 
        paste0(
          C.level * 100, "% CI for \\(\\mu = ( \\bar{x} - \\)", 
          "\\( t_{",  
          input$alpha,
          ", n-1}",
          " \\dfrac{s}{\\sqrt{n}} ,  \\infty)  = \\) ",
          "\\( (   \\) ", xbar, "\\( - \\)",
          round(qt(input$alpha, df = test$parameter, lower.tail = FALSE), 6), 
          " * ", round( sqrt(s2), 6), " / ", 
          round(sqrt(n), 6), ",  ",
          "\\(  \\infty)  \\) ",  "\\( = \\) ",
          "(", round(test$conf.int[1], 6), ",  ", 
          round(test$conf.int[2], 6), ")"
        ),
      input$alternative_onemean == "two.sided" ~
        paste0(
          C.level * 100, "% CI for \\(\\mu = \\bar{x} \\pm \\)", 
          "\\( t_{",  
          input$alpha/2,
          ", n-1}",
          " \\dfrac{s}{\\sqrt{n}} = \\) ",
          round(test$estimate, 6), "  \\( \\pm \\) ", "\\( ( \\)", 
          round(qt(input$alpha / 2, df = test$parameter, lower.tail = FALSE), 6), 
          " * ", round(sqrt(s2), 6), " / ", 
          round(sqrt(n), 6), "\\( ) \\) ", "\\( = \\) ",
          "(", round(test$conf.int[1], 6), ",  ", 
          round(test$conf.int[2], 6), ")"
        )
    ),  
    br(),
    tags$b("Hypothesis test"),
    br(),
    paste0("1. \\(H_0 : \\mu = \\) ", test$null.value, " and \\(H_1 : \\mu \\) ", 
           ifelse(input$alternative_onemean == "two.sided", "\\( \\neq \\) ", 
                  ifelse(input$alternative_onemean == "greater", "\\( > \\) ", "\\( < \\) ")), 
           test$null.value),
    br(),
    paste0(
      "2. Test statistic : \\(t_{obs} = \\dfrac{\\bar{x} - \\mu_0}{s / \\sqrt{n}} = \\) ",
      "(", round(test$estimate, 6), ifelse(test$null.value >= 0, 
                                           paste0(" - ", test$null.value), paste0(" + ", abs(test$null.value))), ") / ", 
      round(sqrt(s2/n), 6), " \\( = \\) ",
      round(test$statistic, 6)
    ),
    br(),
    paste0(
      "3. p-value : ", 
      case_when( 
        input$alternative_onemean == "less" ~ 
          paste0("P\\( (T \\)  ",  "\\( \\leq  t_{obs} )  \\) = ", round(test$p.value,6)),
        input$alternative_onemean == "greater" ~ 
          paste0("P\\( (T \\)  ",  "\\( \\geq  t_{obs} )  \\) = ", round(test$p.value,6)),
        input$alternative_onemean == "two.sided"~ 
          paste0("2*min ", "(P\\( (T \\)  ",  "\\( \\leq  t_{obs} ),  \\) ",
                 "P\\( (T \\) ",  "\\( \\geq  t_{obs} )  \\) ) =", round(test$p.value,6))
      )
    ),
    br(),
    paste0("4. Conclusion : ", ifelse(test$p.value <= input$alpha, "Reject \\(H_0\\)", "Do not reject \\(H_0\\)")),
    br(),
    tags$b("Interpretation"),
    br(),
    paste0("At the ", input$alpha * 100, "% significance level, ", 
           ifelse(test$p.value <= input$alpha, 
                  "we reject the null hypothesis", "we do not reject the null hypothesis"), 
           " \\((p\\)-value ",  round(test$p.value, 6), ")", ".")
     )
  }
  
})   

output$plot_onemeant <- renderPlot({
  if ( any(is.na(input$n0), is.na(input$mean_onemean), is.na(input$sigma2_onemeanz), is.na(input$h0_mean) ) )
  {return(NULL)}else{
  C.level = 1 - (input$alpha) 
  n = input$n0 
  df=n-1 
  xbar= input$mean_onemean 
  s2=input$s2_onemeant 
  test<-tsum.test(mean.x=xbar, s.x = sqrt(s2), n.x=n,
                  alternative=input$alternative_onemean,
                  mu = input$h0_mean, conf.level=C.level) 
  
  tstat=test$statistic 
  abststat=abs(tstat) 
  range1=qt(0.9999, df = test$parameter, lower.tail = FALSE)  
  range2=qt(0.9999, df = test$parameter, lower.tail = TRUE) 
  p=ggplot(data.frame(x = c(range1, range2)), aes(x = x))
  if (input$alternative_onemean == "less") {
    p <- p +
      geom_area(stat = "function", fun = dt, args = list(df = df), 
                xlim = c(range1,tstat),fill="blue",alpha = 0.4) +
      geom_area(stat = "function", fun = dt,args = list(df = df), 
                xlim = c(tstat, range2),fill="grey",alpha = 0.2) 
  } else  if (input$alternative_onemean == "greater"){
    p <- p +
      geom_area(stat = "function", fun = dt, args = list(df = df), 
                xlim = c(range1,tstat),fill="grey",alpha = 0.2) +
      geom_area(stat = "function", fun = dt,args = list(df = df), 
                xlim = c(tstat, range2),fill="blue",alpha = 0.4) 
  } else {
    p <- p +
      geom_area(stat = "function", fun = dt, args = list(df = df), 
                xlim = c(range1,-abststat),fill="blue",alpha = 0.4) +
      geom_area(stat = "function", fun = dt,args = list(df = df), 
                xlim = c(-abststat, abststat),fill="grey",alpha = 0.2) +
      geom_area(stat = "function", fun = dt,args = list(df = df), 
                xlim = c(abststat, range2),fill="blue",alpha = 0.4)+
      geom_vline(xintercept = -tstat, color = "red")
  }
  p<-p+theme_minimal()+
    geom_vline(xintercept = tstat, color = "red") +
    geom_text(aes(x = tstat, 
                  label = paste0("Test statistic = ", round(test$statistic, 6)), y = 0.2), 
              colour = "red", angle = 90, vjust = 1.3, text = element_text(size = 11)) +
    ggtitle(paste0("Student distribution", " t(", round(test$parameter, 6), ")",
                   ", ",  "p-value is indicated by the shaded area")) +
    theme(plot.title = element_text(face = "bold", hjust = 0.5)) +
    ylab("Density") +
    xlab("x")
  p
  }
})



output$results_onemeant<- metaRender2( 
  renderPrint,
  {
if ( any(is.na(input$n0), is.na(input$mean_onemean), is.na(input$sigma2_onemeanz), is.na(input$h0_mean) ) )
    {
      metaExpr( paste("Please enter all required statistics and the parameter value under the null hypothesis."))
    }else{
      metaExpr({
    test<-tsum.test(mean.x=..(input$mean_onemean), s.x = sqrt(..(input$s2_onemeant)), 
                          n.x=..(input$n0), mu = ..(input$h0_mean),
                          alternative=..(input$alternative_onemean),
                           conf.level=1-..(input$alpha))
    CI= test$conf.int
    cat("Confidence interval", "of level", 1-..(input$alpha), "is :", CI)
       test    
      })
    } 
  }
) 

 
output$theory_oneprop <- renderUI({ 
  if ( any(is.na(input$n0), is.na(input$x_oneprop), is.na(input$h0_oneprop)  ) )
  {
paste("Please enter all required statistics and the parameter value under the null hypothesis.")
  }else{
  C.level = 1 - (input$alpha) 
  n = input$n0 
  x= input$x_oneprop 
  phat= x/n 
  p0= input$h0_oneprop  
  se1 = sqrt( phat*(1-phat)/n ) 
  se2=  sqrt( p0*(1-p0)/n ) 
  test=prop.test(x=input$x_oneprop, n=input$n0, p = p0,
                 alternative=input$alternative_oneprop,
                 conf.level = C.level, correct = FALSE) 
  
  withMathJax(
    tags$b("Confidence interval by Z-test"),
    br(),
    case_when(  
      input$alternative_oneprop=="less" ~ paste0(
        C.level * 100, "% CI for \\( p =   \\bigg(  0,  \\hat{p} + z_{\\alpha} 
          \\sqrt{\\dfrac{\\hat{p}(1-\\hat{p}}{n}}  \\bigg) = \\) ", 
        "(0,",round(test$estimate, 6), "  \\( + \\) ",  
        round(qnorm(input$alpha, lower.tail = FALSE), 6), " * ", 
        round(se1, 6), ")", "\\( = \\) ",
        "(0",   ",  ", round(phat+qnorm(input$alpha, lower.tail = FALSE)*se1, 6), ")"
      ),
      input$alternative_oneprop=="greater" ~ paste0(
        C.level * 100, "% CI for \\( p =   \\bigg(  \\hat{p} - z_{\\alpha} 
          \\sqrt{\\dfrac{\\hat{p}(1-\\hat{p})}{n}},   1 \\bigg) = \\) ", 
        "( ", round(test$estimate, 6), "  \\( - \\) ",  
        round(qnorm(input$alpha, lower.tail = FALSE), 6), " * ", 
        round(se1, 6), ", 1)", "\\( = \\) ",
        "(", round(phat-qnorm(input$alpha, lower.tail = FALSE)*se1, 6), ",  ", 1, ")"
      ),
      input$alternative_oneprop == "two.sided" ~ paste0(
        C.level * 100, "% CI for \\(p = \\hat{p} \\pm z_{\\alpha/2} 
          \\sqrt{\\dfrac{\\hat{p}(1-\\hat{p})}{n}} = \\) ",
        round(test$estimate, 6), "  \\( \\pm \\) ", "\\( ( \\)", 
        round(qnorm(input$alpha/2, lower.tail = FALSE), 6), " * ", 
        round(se1, 6), "\\( ) \\) ", "\\( = \\) ",
        "(", round(phat-qnorm(input$alpha/2, lower.tail = FALSE)*se1, 6), ",  ", 
        round(phat+qnorm(input$alpha/2, lower.tail = FALSE)*se1, 6), ")"
      )
    ),  
    br(),
    tags$b("Confidence interval using Wilson's score method (by the prop.test() function in R)"),
    br(),
    case_when(  
      input$alternative_oneprop=="less" ~ paste0(
        C.level * 100, "% CI for \\( p =   \\) ", 
      "(", round(test$conf.int[1], 6), ",  ", round(test$conf.int[2], 6), ")"
      ),
      input$alternative_oneprop=="greater" ~ paste0(
        C.level * 100, "% CI for \\( p =    \\) ", 
    "(", round(test$conf.int[1], 6), ",  ", round(test$conf.int[2], 6), ")"
      ),
      input$alternative_oneprop == "two.sided" ~ paste0(
        C.level * 100, "% CI for \\(p =  \\) ",
    "(", round(test$conf.int[1], 6), ",  ", round(test$conf.int[2], 6), ")"
              )
    ),  
    br(),
    tags$b("Hypothesis test"),
    br(),
    paste0("1. \\(H_0 : p = \\) ", test$null.value, " and \\(H_1 : p \\) ", 
           ifelse(input$alternative_oneprop == "two.sided", "\\( \\neq \\) ", 
                  ifelse(input$alternative_oneprop == "greater", "\\( > \\) ", 
                         "\\( < \\) ")), test$null.value),
    br(),
    paste0(
      "2. Test statistic : \\(z_{obs} = \\dfrac{\\hat{p} - p_0}{\\sqrt{\\dfrac{p_0(1-p_0)}{n}}} = \\) ",
      "(", round(test$estimate, 6), ifelse(test$null.value >= 0, paste0(" - ", test$null.value), 
                                           paste0(" + ", abs(test$null.value))), ") / ", 
      round(se2, 6), " \\( = \\) ",
      ifelse(test$null.value >= 0 & test$null.value <= 1, round( (phat-p0)/se2, 6), 
             "Error: \\( p_0 \\) must be \\( 0 \\leq p_0 \\leq 1\\)")
    ),
    br(),
    paste0(
      "3. p-value : ", 
      case_when( 
        input$alternative_oneprop == "less" ~ 
          paste0("P\\( (Z \\)  ",  "\\( \\leq  z_{obs} )  \\) = ", round(test$p.value,6)),
        input$alternative_oneprop == "greater" ~ 
          paste0("P\\( (Z \\)  ",  "\\( \\geq  z_{obs} )  \\) = ", round(test$p.value,6)),
        input$alternative_oneprop == "two.sided"~ 
          paste0("2*min ", "(P\\( (Z \\)  ",  "\\( \\leq  z_{obs} ),  \\) ",
                 "P\\( (Z \\) ",  "\\( \\geq  z_{obs} )  \\) ) =", round(test$p.value,6))
      )
    ),
    br(),
    paste0("4. Conclusion : ", ifelse(test$p.value <= input$alpha, "Reject \\(H_0\\)", "Do not reject \\(H_0\\)")),
    br(),
    tags$b("Interpretation"),
    br(),
    paste0("At the ", input$alpha * 100, "% significance level, ", 
           ifelse(test$p.value <= input$alpha, 
                  "we reject the null hypothesis", "we do not reject the null hypothesis"), 
           " \\((p\\)-value ",  round(test$p.value, 6), ")", ".")
  )
  }
})    


output$plot_oneprop <- renderPlot({
  if ( any(is.na(input$n0), is.na(input$x_oneprop), is.na(input$h0_oneprop)  ) )
  {return(NULL)}else{
  C.level = 1 - (input$alpha) 
  n = input$n0 
  x= input$x_oneprop 
  phat= x/n 
  p0=input$h0_oneprop 
  se = sqrt( p0*(1-p0)/n ) 
  stat = (phat-p0)/se 
  absstat=abs(stat) 
  test=prop.test(x=input$x_oneprop, n=input$n0, p = p0,
                 alternative=input$alternative_oneprop,
                 conf.level = C.level, correct = FALSE) 
  range1=qnorm(0.9999, lower.tail = FALSE)  
  range2=qnorm(0.9999, lower.tail = TRUE) 
  p= ggplot(data.frame(x = c(range1, range2)), aes(x = x)) 
  if (input$alternative_oneprop == "less") {
    p <- p +
      geom_area(stat = "function", fun = dnorm, args = list(mean = 0, sd = 1), 
                xlim = c(range1,stat),fill="blue",alpha = 0.4) +
      geom_area(stat = "function", fun = dnorm,args = list(mean = 0, sd = 1), 
                xlim = c(stat, range2),fill="grey",alpha = 0.2) 
  } else if (input$alternative_oneprop == "greater"){
    p <- p +
      geom_area(stat = "function", fun = dnorm, args = list(mean = 0, sd = 1), 
                xlim = c(range1,stat),fill="grey",alpha = 0.2) +
      geom_area(stat = "function", fun = dnorm,args = list(mean = 0, sd = 1), 
                xlim = c(stat, range2),fill="blue",alpha = 0.4) 
  } else  
  {
    p <- p +
      geom_area(stat = "function", fun = dnorm, args = list(mean = 0, sd = 1), 
                xlim = c(range1,-absstat),fill="blue",alpha = 0.4) +
      geom_area(stat = "function", fun = dnorm,args = list(mean = 0, sd = 1), 
                xlim = c(-absstat, absstat),fill="grey",alpha = 0.2) +
      geom_area(stat = "function", fun = dnorm,args = list(mean = 0, sd = 1), 
                xlim = c(absstat, range2),fill="blue",alpha = 0.4)+
      geom_vline(xintercept = -stat, color = "red")
  }
  p<-p+theme_minimal()+
    geom_vline(xintercept = stat, color = "red") +
    geom_text(aes(x = stat, 
                  label = paste0("Test statistic = ", round(stat, 6)), y = 0.2), 
              colour = "red", angle = 90, vjust = 1.3, text = element_text(size = 11)) +
    ggtitle(paste0("Standard Normal distribution", " N(0, 1)", 
                   ", ",  "p-value is indicated by the shaded area")) +
    theme(plot.title = element_text(face = "bold", hjust = 0.5)) +
    ylab("Density") +
    xlab("x")
  p
  }
})

 
output$results_oneprop<- metaRender2( 
  renderPrint,
  { 
    if ( any(is.na(input$n0), is.na(input$x_oneprop), is.na(input$h0_oneprop)  ) )
    {
      metaExpr( paste("Please enter all required statistics and the parameter value under the null hypothesis."))
    }else{
      metaExpr({
    test=prop.test(x=..(input$x_oneprop), n=..(input$n0), 
                   p = ..(input$h0_oneprop),
                   alternative=..(input$alternative_oneprop),
                   conf.level = 1-..(input$alpha), correct = FALSE)
    CI= test$conf.int
    cat("Confidence interval", "of level", 1-..(input$alpha), "is :", CI)
        test 
      })
    }  
  }
) 

 
output$theory_twomeanz <- renderUI({ 
  if ( any(is.na(input$n1_two), is.na(input$n2_two), is.na(input$mean1_twomean), is.na(input$mean2_twomean), is.na(input$sigma21_twomeanz), is.na(input$sigma22_twomeanz), is.na(input$h0_mean) ) )
  {
paste("Please enter all required statistics and the parameter value under the null hypothesis.")
  }else{  
  n1=input$n1_two   
  n2=input$n2_two 
  x1bar=input$mean1_twomean  
  x2bar=input$mean2_twomean 
  sigma1=sqrt(input$sigma21_twomeanz) 
  sigma2=sqrt(input$sigma22_twomeanz) 
  se= sqrt(input$sigma21_twomeanz/n1 + input$sigma22_twomeanz/n2) 
  C.level = 1 - (input$alpha) 
  
  test<-zsum.test(mean.x=x1bar, sigma.x=sigma1, n.x=n1,
                  mean.y=x2bar, sigma.y=sigma2, n.y=n2,
                  mu = input$h0_mean, 
                  alternative = input$alternative_twomean, conf.level = C.level) 
 
  withMathJax(
    
    tags$b("Confidence interval"),
    br(),
    case_when(  
      input$alternative_twomean=="less" ~ 
        paste0(
          C.level * 100, "% Confidence Interval for \\(\\mu_1 - \\mu_2 = \\bigg( -\\infty,
          \\bar{x}_1 - \\bar{x}_2 + z_{\\alpha} \\sqrt{\\dfrac{\\sigma^2_1}{n_1} + 
          \\dfrac{\\sigma^2_2}{n_2}} \\bigg)= \\) ", "\\( ( -\\infty,  \\) ",
          round(x1bar, 6), 
          ifelse(x2bar >= 0, paste0(" - ", round(x2bar, 6)), 
                 paste0(" + ", round(abs(x2bar), 6))), "  \\( + \\)",  
          round(qnorm(input$alpha, lower.tail = FALSE), 6), " * ", 
          round(se, 6), "\\( ) \\) ", "\\( = \\) ",
          "(", round(test$conf.int[1], 6), ", ", round(test$conf.int[2], 6), ")"
        ),
      input$alternative_twomean=="greater" ~ 
        paste0(
          C.level * 100, "% Confidence Interval for \\(\\mu_1 - \\mu_2 = \\bigg(
          \\bar{x}_1 - \\bar{x}_2 - z_{\\alpha} \\sqrt{\\dfrac{\\sigma^2_1}{n_1} + 
          \\dfrac{\\sigma^2_2}{n_2}}, \\infty \\bigg)= \\) ", "(",round(x1bar, 6), 
          ifelse(x2bar >= 0, paste0(" - ", round(x2bar, 6)), 
                 paste0(" + ", round(abs(x2bar), 6))), "  \\( - \\)",  
          round(qnorm(input$alpha, lower.tail = FALSE), 6), " * ", 
          round(se, 6), "\\(, \\infty) \\) ", "\\( = \\) ",
          "(", round(test$conf.int[1], 6), ",  ", 
          round(test$conf.int[2], 6), ")"
        ),
      input$alternative_twomean == "two.sided" ~
        paste0(
          C.level * 100, "% Confidence Interval for \\(\\mu_1 - \\mu_2 = 
          \\bar{x}_1 - \\bar{x}_2 \\pm z_{\\alpha/2} \\sqrt{\\dfrac{\\sigma^2_1}{n_1} + 
          \\dfrac{\\sigma^2_2}{n_2}} = \\) ",round(x1bar, 6), 
          ifelse(x2bar >= 0, paste0(" - ", round(x2bar, 6)), 
                 paste0(" + ", round(abs(x2bar), 6))), "  \\( \\pm \\)", " \\( ( \\)", 
          round(qnorm(input$alpha / 2, lower.tail = FALSE), 6), " * ", 
          round(se, 6), "\\( ) \\) ", "\\( = \\) ",
          "(", round(test$conf.int[1], 6), ", ", 
          round(test$conf.int[2], 6), ")"
        )
    ),  
    br(),
    tags$b("Hypothesis test"),
    br(),
    paste0("1. \\(H_0 : \\mu_1 - \\mu_2  = \\) ", test$null.value, " and \\(H_1 : \\mu_1 - \\mu_2 \\) ", 
           ifelse(input$alternative_twomean == "two.sided", "\\( \\neq \\) ", 
                  ifelse(input$alternative_twomean == "greater", "\\( > \\) ", "\\( < \\) ")), 
           test$null.value),
    br(),
    paste0(
      "2. Test statistic : \\(z_{obs} = \\dfrac{(\\bar{x}_1 - \\bar{x}_2) - (\\mu_1 - \\mu_2)}{\\sqrt{\\dfrac{\\sigma^2_1}{n_1} + \\dfrac{\\sigma^2_2}{n_2}}} = \\) ",
      "(", round(x1bar, 6), ifelse(x2bar >= 0, paste0(" - ", round(x2bar, 6)), paste0(" + ", round(abs(x2bar), 6))), ifelse(input$h0 >= 0, paste0(" - ", input$h0), paste0(" + ", abs(input$h0))), ") / ", 
      round(se, 6), " \\( = \\) ",
      round(test$statistic, 6)
    ),
    br(),
    paste0(
      "3. p-value : ", 
      case_when( 
        input$alternative_twomean == "less" ~ 
          paste0("P\\( (Z \\)  ",  "\\( \\leq  z_{obs} )  \\) = ", round(test$p.value,6)),
        input$alternative_twomean == "greater" ~ 
          paste0("P\\( (Z \\)  ",  "\\( \\geq  z_{obs} )  \\) = ", round(test$p.value,6)),
        input$alternative_twomean == "two.sided"~ 
          paste0("2*min ", "(P\\( (Z \\)  ",  "\\( \\leq  z_{obs} ),  \\) ",
                 "P\\( (Z \\) ",  "\\( \\geq  z_{obs} )  \\) ) =", round(test$p.value,6))
      )
    ),
    br(),
    paste0("4. Conclusion : ", ifelse(test$p.value <= input$alpha, "Reject \\(H_0\\)", "Do not reject \\(H_0\\)")),
    br(),
    tags$b("Interpretation"),
    br(),
    paste0("At the ", input$alpha * 100, "% significance level, ", 
           ifelse(test$p.value <= input$alpha, 
                  "we reject the null hypothesis", "we do not reject the null hypothesis"), 
           " \\((p\\)-value ",  round(test$p.value, 6), ")", ".")
   )
  }
})    


output$plot_twomeanz <- renderPlot({
  if ( any(is.na(input$n1_two), is.na(input$n2_two), is.na(input$mean1_twomean), is.na(input$mean2_twomean), is.na(input$sigma21_twomeanz), is.na(input$sigma22_twomeanz), is.na(input$h0_mean) ) )
  {return(NULL)}else{
  n1=input$n1_two   
  n2=input$n2_two 
  x1bar=input$mean1_twomean  
  x2bar=input$mean2_twomean 
  sigma1=sqrt(input$sigma21_twomeanz) 
  sigma2=sqrt(input$sigma22_twomeanz) 
  se= sqrt(input$sigma21_twomeanz/n1 + input$sigma22_twomeanz/n2)  
  C.level = 1 - (input$alpha) 
  test<-zsum.test(mean.x=x1bar, sigma.x=sigma1, n.x=n1,
                  mean.y=x2bar, sigma.y=sigma2, n.y=n2,
                  mu = input$h0_mean, 
                  alternative = input$alternative_twomean, conf.level = C.level) 
  
  zstat=test$statistic 
  abszstat=abs(zstat) 
  range1=qnorm(0.9999, lower.tail = FALSE)  
  range2=qnorm(0.9999, lower.tail = TRUE) 
  p = ggplot(data.frame(x = c(range1, range2)), aes(x = x))
  if (input$alternative_twomean == "less") {
    p <- p +
      geom_area(stat = "function", fun = dnorm, args = list(mean = 0, sd = 1), 
                xlim = c(range1,zstat),fill="blue",alpha = 0.4) +
      geom_area(stat = "function", fun = dnorm,args = list(mean = 0, sd = 1), 
                xlim = c(zstat, range2),fill="grey",alpha = 0.2) 
  } else if (input$alternative_twomean == "greater"){
    p <- p +
      geom_area(stat = "function", fun = dnorm, args = list(mean = 0, sd = 1), 
                xlim = c(range1,zstat),fill="grey",alpha = 0.2) +
      geom_area(stat = "function", fun = dnorm,args = list(mean = 0, sd = 1), 
                xlim = c(zstat, range2),fill="blue",alpha = 0.4) 
  } else {
    p <- p +
      geom_area(stat = "function", fun = dnorm, args = list(mean = 0, sd = 1), 
                xlim = c(range1,-abszstat),fill="blue",alpha = 0.4) +
      geom_area(stat = "function", fun = dnorm,args = list(mean = 0, sd = 1), 
                xlim = c(-abszstat, abszstat),fill="grey",alpha = 0.2) +
      geom_area(stat = "function", fun = dnorm,args = list(mean = 0, sd = 1), 
                xlim = c(abszstat, range2),fill="blue",alpha = 0.4)+
      geom_vline(xintercept = -zstat, color = "red")
  }
  p<-p+theme_minimal()+
    geom_vline(xintercept = zstat, color = "red") +
    geom_text(aes(x = zstat, 
                  label = paste0("Test statistic = ", round(test$statistic, 6)), y = 0.2), 
              colour = "red", angle = 90, vjust = 1.3, text = element_text(size = 11)) +
    ggtitle(paste0("Standard Normal distribution", " N(0, 1)", 
                   ", ",  "p-value is indicated by the shaded area")) +
    theme(plot.title = element_text(face = "bold", hjust = 0.5)) +
    ylab("Density") +
    xlab("x")
  p
  }
})

 
output$results_twomeanz<- metaRender2( 
  renderPrint,
  { 
    if ( any(is.na(input$n1_two), is.na(input$n2_two), is.na(input$mean1_twomean), is.na(input$mean2_twomean), is.na(input$sigma21_twomeanz), is.na(input$sigma22_twomeanz), is.na(input$h0_mean) ) )
    {
      metaExpr( paste("Please enter all required statistics and the parameter value under the null hypothesis."))
    }else{
      metaExpr({
    test<-zsum.test(mean.x=..(input$mean1_twomean), sigma.x=sqrt(..(input$sigma21_twomeanz)), n.x=..(input$n1_two),
                    mean.y=..(input$mean2_twomean), sigma.y=sqrt(..(input$sigma22_twomeanz)), n.y=..(input$n2_two),
                    mu = ..(input$h0_mean), 
                    alternative = ..(input$alternative_twomean), conf.level = 1-..(input$alpha)) 
    CI= test$conf.int 
    cat("Confidence interval", "of level", 1-..(input$alpha), "is :", CI) 
    
    test  
      })
    }
  }                                    
) 

 
output$theory_twomeant <- renderUI({ 
  if ( any(is.na(input$n1_two), is.na(input$n2_two), is.na(input$mean1_twomean), is.na(input$mean2_twomean), is.na(input$s21_twomeant), is.na(input$s22_twomeant), is.na(input$h0_mean) ) )
  {
 paste("Please enter all required statistics and the parameter value under the null hypothesis.")
  }else{  
  n1=input$n1_two   
  n2=input$n2_two 
  x1bar=input$mean1_twomean  
  x2bar=input$mean2_twomean 
  x1var=input$s21_twomeant   
  x2var=input$s22_twomeant   
  
  C.level = 1 - (input$alpha) 
  test<-tsum.test(mean.x = x1bar, s.x=sqrt(x1var), n.x=n1,
                  mean.y = x2bar, s.y=sqrt(x2var), n.y=n2,
                  mu = input$h0_mean, var.equal = input$var.equal,
                  alternative = input$alternative_twomean, 
                  conf.level = C.level) 
  df = test$parameter 
  
  se1= sqrt(x1var/n1+x2var/n2)     
  s_p <- sqrt(((n1 - 1)*x1var + (n2 - 1)*x2var) / (n1+n2-2)) 
  se2=s_p*sqrt(1/n1+1/n2)         
  
  if (input$inference == "TwoMeanT" & input$var.equal == FALSE)
  {
    
    withMathJax(
      
      tags$b("Confidence interval"),
      br(),
      case_when(     
        input$alternative_twomean=="less" ~ 
          paste0(
            C.level * 100, "% Confidence Interval for \\(\\mu_1 - \\mu_2 = \\bigg( -\\infty,
          \\bar{x}_1 - \\bar{x}_2 + t_{\\alpha, \\nu} \\sqrt{\\dfrac{s^2_1}{n_1} + 
          \\dfrac{s^2_2}{n_2}} \\bigg) \\) = \\(", "( -\\infty, ",
            round(x1bar, 6), 
            ifelse(x2bar >= 0, paste0(" - ", round(x2bar, 6)), 
                   paste0(" + ", round(abs(x2bar), 6)) ), "+",  
            round(qt(input$alpha, df=df, lower.tail = FALSE), 6), " * ", 
            round(se1, 6), ")", "\\ = \\) ",
            "(", round(test$conf.int[1], 6), ",  ", 
            round(test$conf.int[2], 6), ")"
          ),
        input$alternative_twomean=="greater" ~ 
          paste0(
            C.level * 100, "% Confidence Interval for \\(\\mu_1 - \\mu_2 = \\bigg(
          \\bar{x}_1 - \\bar{x}_2 - t_{\\alpha, \\nu} \\sqrt{\\dfrac{s^2_1}{n_1} + 
          \\dfrac{s^2_2}{n_2}}, \\infty \\bigg) \\)  = ", "(", 
            round(x1bar, 6), 
            ifelse(x2bar >= 0, paste0(" - ", round(x2bar, 6)), 
                   paste0(" + ", round(abs(x2bar), 6))), "  \\( - \\)",  
            round(qt(input$alpha, df=df, lower.tail = FALSE), 6), " * ", 
            round(se1, 6), "\\(, \\infty) \\) ", "\\( = \\) ",
            "(", round(test$conf.int[1], 6), ",  ", 
            round(test$conf.int[2], 6), ")"
          ),
        input$alternative_twomean == "two.sided" ~
          paste0(
            C.level * 100, "% Confidence Interval for \\(\\mu_1 - \\mu_2 = 
          \\bar{x}_1 - \\bar{x}_2 \\pm t_{\\alpha/2, \\nu} \\sqrt{\\dfrac{s^2_1}{n_1} + 
          \\dfrac{s^2_2}{n_2}} = \\) ",  "=", "(",
            round(x1bar, 6) , 
            paste0(" + ", round(abs(x2bar), 6)), "\\( \\pm \\)",  
            round(qt(input$alpha / 2, df=df, lower.tail = FALSE), 6), " * ", 
            round(se1, 6), "\\( ) \\) ", "\\( = \\) ",
            "(", round(test$conf.int[1], 6), ",  ", 
            round(test$conf.int[2], 6), ")"
          )
      ),  
      br(),
      paste0("where ", "\\( \\nu = \\dfrac{\\Bigg(\\dfrac{s^2_1}{n_1} + 
                 \\dfrac{s^2_2}{n_2}\\Bigg)^2}{\\dfrac{\\Bigg(\\dfrac{s^2_1}{n_1}\\Bigg)^2}{n_1-1} 
                 + \\dfrac{\\Bigg(\\dfrac{s^2_2}{n_2}\\Bigg)^2}{n_2-1}} = \\) ", 
             round(test$parameter, 2)),
      br(),
      tags$b("Hypothesis test"),
      br(),
      paste0("1. \\(H_0 : \\mu_1 - \\mu_2  = \\) ", test$null.value, " and \\(H_1 : \\mu_1 - \\mu_2 \\) ", 
             ifelse(input$alternative_twomean == "two.sided", "\\( \\neq \\) ", 
                    ifelse(input$alternative_twomean == "greater", "\\( > \\) ", "\\( < \\) ")), 
             test$null.value),
      br(),
      paste0(
        "2. Test statistic : \\(t_{obs} = \\dfrac{(\\bar{x}_1 - \\bar{x}_2) - 
        (\\mu_1 - \\mu_2)}{\\sqrt{\\dfrac{s^2_1}{n_1} + \\dfrac{s^2_2}{n_2}}} = \\) ",
        "(", round(x1bar, 6), ifelse(x2bar >= 0, paste0(" - ", round(x2bar, 6)), 
                                     paste0(" + ", round(abs(x2bar), 6))), 
        ifelse(input$h0_mean >= 0, paste0(" - ", input$h0_mean), 
               paste0(" + ", abs(input$h0_mean))), ") / ",  round(se1, 6), " \\( = \\) ",
        round(test$statistic, 6)
      ),
      br(),
      paste0(
        "3. p-value : ", 
        case_when( 
          input$alternative_twomean == "less" ~ 
            paste0("P\\( (T \\)  ",  "\\( \\leq  t_{obs} )  \\) = ", round(test$p.value,6)),
          input$alternative_twomean == "greater" ~ 
            paste0("P\\( (T \\)  ",  "\\( \\geq  t_{obs} )  \\) = ", round(test$p.value,6)),
          input$alternative_twomean == "two.sided"~ 
            paste0("2*min ", "(P\\( (T \\)  ",  "\\( \\leq  t_{obs} ),  \\) ",
                   "P\\( (T \\) ",  "\\( \\geq  t_{obs} )  \\) ) =", round(test$p.value,6))
        )
      ),
      br(),
      paste0("4. Conclusion : ", ifelse(test$p.value <= input$alpha, "Reject \\(H_0\\)", "Do not reject \\(H_0\\)")),
      br(),
      tags$b("Interpretation"),
      br(),
      paste0("At the ", input$alpha * 100, "% significance level, ", 
             ifelse(test$p.value <= input$alpha, 
                    "we reject the null hypothesis", "we do not reject the null hypothesis"), 
             " \\((p\\)-value ",  round(test$p.value, 6), ")", ".")
   
    )
  } else if (input$inference == "TwoMeanT" & input$var.equal == TRUE)
  {
    
    withMathJax(
      
      tags$b("Confidence interval"),
      br(),
      case_when(     
        input$alternative_twomean=="less" ~ 
          paste0(
            C.level * 100, "% Confidence Interval for \\(\\mu_1 - \\mu_2 = \\bigg( -\\infty,
          \\bar{x}_1 - \\bar{x}_2 + t_{\\alpha, n_1+n_2-2} \\cdot s_p \\sqrt{\\dfrac{1}{n_1} + 
          \\dfrac{1}{n_2}} \\bigg) \\) = \\(", "( -\\infty, ",
            round(x1bar, 6), 
            ifelse(x2bar >= 0, paste0(" - ", round(x2bar, 6)), 
                   paste0(" + ", round(abs(x2bar), 6)) ), "+",  
            round(qt(input$alpha, df=df, lower.tail = FALSE), 6), " * ", 
            round(se2, 6), ")", "\\ = \\) ",
            "(", round(test$conf.int[1], 6), ",  ", 
            round(test$conf.int[2], 6), ")"
          ),
        input$alternative_twomean=="greater" ~ 
          paste0(
            C.level * 100, "% Confidence Interval for \\(\\mu_1 - \\mu_2 = \\bigg(
          \\bar{x}_1 - \\bar{x}_2 - t_{\\alpha, n_1+n_2-2} \\cdot s_p \\sqrt{\\dfrac{1}{n_1} + 
          \\dfrac{1}{n_2}}, \\infty \\bigg) \\)  = ", "(", 
            round(x1bar, 6), 
            ifelse(x2bar >= 0, paste0(" - ", round(x2bar, 6)), 
                   paste0(" + ", round(abs(x2bar), 6))), "  \\( - \\)",  
            round(qt(input$alpha, df=df, lower.tail = FALSE), 6), " * ", 
            round(se2, 6), "\\(, \\infty) \\) ", "\\( = \\) ",
            "(", round(test$conf.int[1], 6), ",  ", 
            round(test$conf.int[2], 6), ")"
          ),
        input$alternative_twomean == "two.sided" ~
          paste0(
            C.level * 100, "% Confidence Interval for \\(\\mu_1 - \\mu_2 = 
          \\bar{x}_1 - \\bar{x}_2 \\pm t_{\\alpha/2, n_1+n_2-2} \\cdot s_p \\sqrt{\\dfrac{1}{n_1} + 
          \\dfrac{1}{n_2}} = \\) ",  "=", "(",
            round(x1bar, 6) , 
            paste0(" + ", round(abs(x2bar), 6)), "\\( \\pm \\)",  
            round(qt(input$alpha / 2, df=df, lower.tail = FALSE), 6), " * ", 
            round(se2, 6), "\\( ) \\) ", "\\( = \\) ",
            "(", round(test$conf.int[1], 6), ",  ", 
            round(test$conf.int[2], 6), ")"
          )
      ),  
      br(),
      paste0("where ", "\\( s_p = \\sqrt{\\dfrac{(n_1 - 1)s^2_1 + (n_2 - 1)s^2_2}{n_1 + n_2 - 2}} = \\) ", 
          round(s_p, 6)),
      br(),     
      tags$b("Hypothesis test"),
      br(),
      paste0("1. \\(H_0 : \\mu_1 - \\mu_2  = \\) ", test$null.value, " and \\(H_1 : \\mu_1 - \\mu_2 \\) ", 
             ifelse(input$alternative_twomean == "two.sided", "\\( \\neq \\) ", 
                    ifelse(input$alternative_twomean == "greater", "\\( > \\) ", "\\( < \\) ")), 
             test$null.value),
      br(),
      paste0(
        "2. Test statistic : \\(t_{obs} = \\dfrac{(\\bar{x}_1 - \\bar{x}_2) - (\\mu_1 - \\mu_2)}{\\sqrt{\\dfrac{s^2_1}{n_1} + \\dfrac{s^2_2}{n_2}}} = \\) ",
        "(", round(x1bar, 6), ifelse(x2bar >= 0, paste0(" - ", round(x2bar, 6)), paste0(" + ", round(abs(x2bar), 6))), ifelse(input$h0_mean >= 0, paste0(" - ", input$h0_mean), paste0(" + ", abs(input$h0_mean))), ") / ",  round(se1, 6), " \\( = \\) ",
        round(test$statistic, 6)
      ),
      br(),
      paste0(
        "3. p-value : ", 
        case_when( 
          input$alternative_twomean == "less" ~ 
            paste0("P\\( (T \\)  ",  "\\( \\leq  t_{obs} )  \\) = ", round(test$p.value,6)),
          input$alternative_twomean == "greater" ~ 
            paste0("P\\( (T \\)  ",  "\\( \\geq  t_{obs} )  \\) = ", round(test$p.value,6)),
          input$alternative_twomean == "two.sided"~ 
            paste0("2*min ", "(P\\( (T \\)  ",  "\\( \\leq  t_{obs} ),  \\) ",
                   "P\\( (T \\) ",  "\\( \\geq  t_{obs} )  \\) ) =", round(test$p.value,6))
        )
      ),
      br(),
      paste0("4. Conclusion : ", ifelse(test$p.value <= input$alpha, "Reject \\(H_0\\)", "Do not reject \\(H_0\\)")),
      br(),
      tags$b("Interpretation"),
      br(),
      paste0("At the ", input$alpha * 100, "% significance level, ", 
             ifelse(test$p.value <= input$alpha, 
                    "we reject the null hypothesis", "we do not reject the null hypothesis"), 
             " \\((p\\)-value ",  round(test$p.value, 6), ")", ".")
      )  
  }
  }
 
})   


output$plot_twomeant <- renderPlot({
  if ( any(is.na(input$n1_two), is.na(input$n2_two), is.na(input$mean1_twomean), is.na(input$mean2_twomean), is.na(input$s21_twomeant), is.na(input$s22_twomeant), is.na(input$h0_mean) ) )
  {return(NULL)}else{
  n1=input$n1_two   
  n2=input$n2_two 
  x1bar=input$mean1_twomean  
  x2bar=input$mean2_twomean 
  x1var=input$s21_twomeant   
  x2var=input$s22_twomeant   
  C.level = 1 - (input$alpha) 
  test<-tsum.test(mean.x = x1bar, s.x=sqrt(x1var), n.x=n1,
                  mean.y = x2bar, s.y=sqrt(x2var), n.y=n2,
                  mu = input$h0_mean, var.equal = input$var.equal,
                  alternative = input$alternative_twomean, 
                  conf.level = C.level) 
  
  df = round(test$parameter,2) 
  tstat=test$statistic 
  abststat=abs(tstat) 
  range1=qt(0.9999, df=df, lower.tail = FALSE)  
  range2=qt(0.9999, df=df, lower.tail = TRUE) 
  p= ggplot(data.frame(x = c(range1, range2)), aes(x = x))
  if (input$alternative_twomean == "less") {
    p <- p +
      geom_area(stat = "function", fun = dt, args = list(df=df), 
                xlim = c(range1,tstat),fill="blue",alpha = 0.4) +
      geom_area(stat = "function", fun = dt,args = list(df=df), 
                xlim = c(tstat, range2),fill="grey",alpha = 0.2) 
  } else if (input$alternative_twomean == "greater"){
    p <- p +
      geom_area(stat = "function", fun = dt, args = list(df=df), 
                xlim = c(range1,tstat),fill="grey",alpha = 0.2) +
      geom_area(stat = "function", fun = dt,args = list(df=df), 
                xlim = c(tstat, range2),fill="blue",alpha = 0.4) 
  } else  
  {
    p <- p +
      geom_area(stat = "function", fun = dt, args = list(df=df), 
                xlim = c(range1,-abststat),fill="blue",alpha = 0.4) +
      geom_area(stat = "function", fun = dt,args = list(df=df), 
                xlim = c(-abststat, abststat),fill="grey",alpha = 0.2) +
      geom_area(stat = "function", fun = dt,args = list(df=df), 
                xlim = c(abststat, range2),fill="blue",alpha = 0.4)+
      geom_vline(xintercept = -tstat, color = "red")
        }
  p<-p+theme_minimal()+
    geom_vline(xintercept = tstat, color = "red") +
    geom_text(aes(x = tstat, 
                  label = paste0("Test statistic = ", round(test$statistic, 6)), y = 0.2), 
              colour = "red", angle = 90, vjust = 1.3, text = element_text(size = 11)) +
    ggtitle(paste0("Student", " t(",  df, ")",
                   ", ",  "p-value is indicated by the shaded area")) +
    theme(plot.title = element_text(face = "bold", hjust = 0.5)) +
    ylab("Density") +
    xlab("x")
  p
  }
})


output$results_twomeant<- metaRender2( 
  renderPrint,
  { 
if ( any(is.na(input$n1_two), is.na(input$n2_two), is.na(input$mean1_twomean), is.na(input$mean2_twomean), is.na(input$s21_twomeant), is.na(input$s22_twomeant), is.na(input$h0_mean) ) )
    {
      metaExpr( paste("Please enter all required statistics and the parameter value under the null hypothesis."))
    }else{
      metaExpr({
test<-tsum.test(mean.x = ..(input$mean1_twomean), s.x=sqrt(..(input$s21_twomeant)), n.x=..(input$n1_two),
                    mean.y = ..(input$mean2_twomean), s.y=sqrt(..(input$s22_twomeant)), n.y=..(input$n2_two),
                    mu = ..(input$h0_mean), var.equal = ..(input$var.equal),
                    alternative = ..(input$alternative_twomean), 
                    conf.level = 1-..(input$alpha))
    CI= test$conf.int
    cat("Confidence interval", "of level", 1-..(input$alpha), "is :", CI)
    test  
      })
    }
  }                                    
) 



output$theory_twoprop <- renderUI({ 
  if ( any(is.na(input$n1_two), is.na(input$n2_two), is.na(input$x1_twoprop), is.na(input$x2_twoprop) ) )
  {
    paste("Please enter all required statistics.")
  }else{
  C.level = 1 - (input$alpha) 
  n1=input$n1_two   
  n2=input$n2_two 
  x1= input$x1_twoprop   
  x2= input$x2_twoprop 
  p1hat= x1/n1    
  p2hat= x2/n2 
  phat = (x1+x2)/(n1+n2)    
  se1 = sqrt( p1hat*(1-p1hat)/n1 + p2hat*(1-p2hat)/n2)  
  se2=  sqrt( phat*(1-phat)*(1/n1+1/n2) )    
  
  test=prop.test(x=c(x1, x2), n=c(n1, n2), 
                 alternative=input$alternative_twoprop,
                 conf.level = C.level, correct = FALSE)  

  zalpha= qnorm(input$alpha, lower.tail = FALSE) 
  zhalfalpha = qnorm(input$alpha/2, lower.tail = FALSE) 
  
  ## Remark: prop.test() seems estimate pooled pop proportion
  
withMathJax(       
      
      tags$b("Confidence interval without assuming \\(H_0  \\)"),
      br(),
      case_when(  
        input$alternative_twoprop=="less" ~ paste0(
          C.level * 100, "% CI for \\(p_1 - p_2 = \\bigg(-1, \\hat{p}_1 - \\hat{p}_2 + 
            z_{\\alpha} \\sqrt{\\dfrac{\\hat{p}_1(1-\\hat{p}_1)}{n_1} + 
            \\dfrac{\\hat{p}_2(1-\\hat{p}_2)}{n_2}} \\bigg) = \\) ", "(-1, ",
          round(p1hat, 6), 
          ifelse(p2hat >= 0, paste0(" - ", round(p2hat, 6)), 
                 paste0(" + ", round(abs(p2hat), 6))), "  \\( + \\) ", 
          round(zalpha, 6), " * ", 
          round(se1, 6), "\\( ) \\) ", "\\( = \\) ",
          "(", -1, ", ", round(p1hat-p2hat+zalpha*se1, 6), ")"
        ),
        input$alternative_twoprop=="greater" ~ paste0(
          C.level * 100, "% CI for \\(p_1 - p_2 = \\bigg( \\hat{p}_1 - \\hat{p}_2 - 
            z_{\\alpha} \\sqrt{\\dfrac{\\hat{p}_1(1-\\hat{p}_1)}{n_1} + 
            \\dfrac{\\hat{p}_2(1-\\hat{p}_2)}{n_2}}, 1 \\bigg) = \\) ", "(",
          round(p1hat, 6), 
          ifelse(p2hat >= 0, paste0(" - ", round(p2hat, 6)), 
                 paste0(" + ", round(abs(p2hat), 6))), "  \\( - \\) ", 
          round(zalpha, 6), " * ", 
          round(se1, 6), "\\( , 1) \\) ", "\\( = \\) ",
          "(", round(p1hat-p2hat-zalpha*se1, 6), ", ", 1, ")"
        ),
        input$alternative_twoprop == "two.sided" ~paste0(
          C.level * 100, "% CI for \\(p_1 - p_2 = \\hat{p}_1 - \\hat{p}_2 \\pm 
            z_{\\alpha/2} \\sqrt{\\dfrac{\\hat{p}_1(1-\\hat{p}_1)}{n_1} + 
            \\dfrac{\\hat{p}_2(1-\\hat{p}_2)}{n_2}} = \\) ",
          round(p1hat, 6), 
          ifelse(p2hat >= 0, paste0(" - ", round(p2hat, 6)), 
                 paste0(" + ", round(abs(p2hat), 6))), "  \\( \\pm \\) ", 
          "\\( ( \\)", round(zhalfalpha, 6), " * ", 
          round(se1, 6), "\\( ) \\) ", "\\( = \\) ",
          "(", 
          round(p1hat-p2hat-zhalfalpha*se1, 6),  #round(test$conf.int[1], 6), 
          ", ", 
          round(p1hat-p2hat+zhalfalpha*se1, 6),  #round(test$conf.int[2], 6), 
          ")"
        )
      ),  
      br(),
      tags$b("Confidence interval under \\(H_0: p_1=p_2  \\)"),
      br(),
      case_when(  
        input$alternative_twoprop=="less" ~ paste0(
          C.level * 100, "% CI for \\(p_1 - p_2 = \\bigg(-1, \\hat{p}_1 - \\hat{p}_2 + 
            z_{\\alpha} \\sqrt{ \\hat{p}(1-\\hat{p}) \\bigg(\\dfrac{1}{n_1} + 
            \\dfrac{1}{n_2} \\bigg) } \\bigg) = \\) ", "(-1, ",
          round(p1hat, 6), 
          ifelse(p2hat >= 0, paste0(" - ", round(p2hat, 6)), 
                 paste0(" + ", round(abs(p2hat), 6))), "  \\( + \\) ", 
          round(zalpha, 6), " * ", 
          round(se2, 6), "\\( ) \\) ", "\\( = \\) ",
          "(", -1, ", ", round(p1hat-p2hat+zalpha*se2, 6), "),"
        ),
        input$alternative_twoprop=="greater" ~ paste0(
          C.level * 100, "% CI for \\(p_1 - p_2 = \\bigg( \\hat{p}_1 - \\hat{p}_2 - 
            z_{\\alpha} \\sqrt{ \\hat{p}(1-\\hat{p}) \\bigg(\\dfrac{1}{n_1} + 
            \\dfrac{1}{n_2} \\bigg) }, 1 \\bigg) = \\) ", "(",
          round(p1hat, 6), 
          ifelse(p2hat >= 0, paste0(" - ", round(p2hat, 6)), 
                 paste0(" + ", round(abs(p2hat), 6))), "  \\( - \\) ", 
          round(zalpha, 6), " * ", 
          round(se2, 6), "\\( , 1) \\) ", "\\( = \\) ",
          "(", round(p1hat-p2hat-zalpha*se2, 6), ", ", 1, "),"
        ),
        input$alternative_twoprop == "two.sided" ~paste0(
          C.level * 100, "% CI for \\(p_1 - p_2 = \\hat{p}_1 - \\hat{p}_2 \\pm 
            z_{\\alpha/2} \\sqrt{ \\hat{p}(1-\\hat{p}) \\bigg(\\dfrac{1}{n_1} + 
            \\dfrac{1}{n_2} \\bigg) } = \\) ",
          round(p1hat, 6), 
          ifelse(p2hat >= 0, paste0(" - ", round(p2hat, 6)), 
                 paste0(" + ", round(abs(p2hat), 6))), "  \\( \\pm \\) ", 
          "\\( ( \\)", round(qnorm(input$alpha/2, lower.tail = FALSE), 6), " * ", 
          round(se2, 6), "\\( ) \\) ", "\\( = \\) ",
          "(", round(p1hat-p2hat-zhalfalpha*se2, 6), ", ", 
          round(p1hat-p2hat+zhalfalpha*se2, 6), "),"
        )
      ),  
      br(),
      paste0("where ", "\\( \\hat{p} = \\dfrac{x_1 + x_2 }{n_1 + n_2} = \\) ", 
             round((x1+x2)/(n1+n2), 6) ),
      br(),
      tags$b("Hypothesis test"),
      br(),
      paste0("1. \\(H_0 : p_1 - p_2 = \\) ", 0, " 
                 and \\(H_1 : p_1 - p_2 \\) ", 
             ifelse(input$alternative_twoprop == "two.sided", "\\( \\neq \\) ", 
                    ifelse(input$alternative_twoprop == "greater", "\\( > \\) ", 
                           "\\( < \\) ")), 0),
      br(),
      paste0(
        "2. Test statistic : \\(z_{obs} = \\dfrac{(\\hat{p}_1 - \\hat{p}_2) - 
        (p_1 - p_2)}{\\sqrt{ \\hat{p}(1-\\hat{p})\\bigg(\\dfrac{1}{n_1} + 
        \\dfrac{1}{n_2}} \\bigg)} = \\) ",
        "(", round(p1hat, 6), ifelse(p2hat >= 0, paste0(" - ", 
            round(p2hat, 6)), paste0(" + ", round(abs(p2hat), 6))), 
        ") / ", round(se2, 6), " \\( = \\) ", round((p1hat-p2hat)/se2, 6)
      ),
      br(),
      paste0(
        "3. p-value : ", 
        case_when( 
          input$alternative_twoprop == "less" ~ 
            paste0("P\\( (Z \\)  ",  "\\( \\leq  z_{obs} )  \\) = ", 
                   round(test$p.value,6)),
          input$alternative_twoprop == "greater" ~ 
            paste0("P\\( (Z \\)  ",  "\\( \\geq  z_{obs} )  \\) = ", 
                   round(test$p.value,6)),
          input$alternative_twoprop == "two.sided"~ 
            paste0("2*min ", "(P\\( (Z \\)  ",  "\\( \\leq  z_{obs} ),  \\) ",
                   "P\\( (Z \\) ",  "\\( \\geq  z_{obs} )  \\) ) =", 
                   round(test$p.value,6))
        )
      ),
      br(),
  paste0("4. Conclusion : ", ifelse(test$p.value <= input$alpha, 
            "Reject \\(H_0\\)", "Do not reject \\(H_0\\)")),
      br(),
  tags$b("Interpretation"),
  br(),
  paste0("At the ", input$alpha * 100, "% significance level, ", 
         ifelse(test$p.value <= input$alpha, 
                "we reject the null hypothesis", 
                "we do not reject the null hypothesis"), 
         " \\((p\\)-value ",  round(test$p.value, 6), ")." )
      
     
    )   
}
})   


 
output$plot_twoprop <- renderPlot({
  if ( any(is.na(input$n1_two), is.na(input$n2_two), is.na(input$x1_twoprop), is.na(input$x2_twoprop) ) )
  {return(NULL)}else{
  C.level = 1 - (input$alpha) 
  n1=input$n1_two   
  n2=input$n2_two 
  x1= input$x1_twoprop   
  x2= input$x2_twoprop 
  p1hat= x1/n1    
  p2hat= x2/n2 
  phat = (x1+x2)/(n1+n2)    
  se1 = sqrt( p1hat*(1-p1hat)/n1 + p2hat*(1-p2hat)/n2)  
  se2=  sqrt( phat*(1-phat)*(1/n1+1/n2) )    
  test=prop.test(x=c(x1, x2), n=c(n1, n2), 
                 alternative=input$alternative_twoprop,
                 conf.level = C.level, correct = FALSE) 
  stat1=ifelse(p1hat-p2hat>=0, sqrt(test$statistic), -sqrt(test$statistic)) 
  stat=stat1 
  range1=qnorm(0.9999, lower.tail = FALSE)  
  range2=qnorm(0.9999, lower.tail = TRUE) 
  p = ggplot(data.frame(x = c(range1, range2)), aes(x = x))
  if (input$alternative_twoprop == "less") {
      ##stat=ifelse(input$pooledse_twoprop==TRUE, stat1, stat2) 
    p <- p +
      geom_area(stat = "function", fun = dnorm, args = list(mean = 0, sd = 1), 
                xlim = c(range1,stat),fill="blue",alpha = 0.4) +
      geom_area(stat = "function", fun = dnorm,args = list(mean = 0, sd = 1), 
                xlim = c(stat, range2),fill="grey",alpha = 0.2) 
  } else if (input$alternative_twoprop == "greater"){
      p <- p +
      geom_area(stat = "function", fun = dnorm, args = list(mean = 0, sd = 1), 
                xlim = c(range1,stat),fill="grey",alpha = 0.2) +
      geom_area(stat = "function", fun = dnorm,args = list(mean = 0, sd = 1), 
                xlim = c(stat, range2),fill="blue",alpha = 0.4) 
  } else  
  {       
         absstat = abs(stat) 
  p <- p +
    geom_area(stat = "function", fun = dnorm, args = list(mean = 0, sd = 1), 
              xlim = c(range1,-absstat),fill="blue",alpha = 0.4) +
    geom_area(stat = "function", fun = dnorm,args = list(mean = 0, sd = 1), 
              xlim = c(-absstat, absstat),fill="grey",alpha = 0.2) +
    geom_area(stat = "function", fun = dnorm,args = list(mean = 0, sd = 1), 
              xlim = c(absstat, range2),fill="blue",alpha = 0.4)+
    geom_vline(xintercept = -stat, color = "red")
      }
  p<-p+theme_minimal()+
    geom_vline(xintercept = stat, color = "red") +
    geom_text(aes(x = stat, 
                  label = paste0("Test statistic = ", round(stat, 6)), y = 0.2), 
              colour = "red", angle = 90, vjust = 1.3, text = element_text(size = 11)) +
    ggtitle(paste0("Standard Normal distribution", " N(0, 1)", 
                   ", ",  "p-value is indicated by the shaded area")) +
    theme(plot.title = element_text(face = "bold", hjust = 0.5)) +
    ylab("Density") +
    xlab("x")
  p
  }
})


output$results_twoprop<- metaRender2( 
  renderPrint,
  { 
    if ( any(is.na(input$n1_two), is.na(input$n2_two), is.na(input$x1_twoprop), is.na(input$x2_twoprop) ) )
    {
      metaExpr( paste("Please enter all required statistics."))
    }else{
      metaExpr({
    test=prop.test(x=c(..(input$x1_twoprop), ..(input$x2_twoprop)), 
                   n=c(..(input$n1_two), ..(input$n2_two)), 
                   alternative=..(input$alternative_twoprop),
                   conf.level = 1-..(input$alpha), correct = FALSE)
    CI= test$conf.int
    cat("Confidence interval", "of level", 1-..(input$alpha), "is :", CI)
    test 
      })
    }
  }                                    
) 


var.sumTest<- function(s.x, n.x, sigma.squared, alternative = "two.sided", conf.level = 0.95)
{
  tstat <- NULL   # test statistic 
  p.val <- NULL   #p-value
  cint <- NULL    # confidence interval bounds
  df = n.x -1 
  tstat = df*s.x^2/sigma.squared 
  lowertail=pchisq(q=tstat, df=df, lower.tail = TRUE)   
  
  p.val <- if (alternative == "less") {
    lowertail
  } else if (alternative == "greater") {
    1-lowertail
  } else {
    2*ifelse(lowertail<=0.5, lowertail, 1-lowertail)
  }
  
  alpha= 1-conf.level  
  chi_alpha_lower= qchisq(alpha, df=df, lower.tail = TRUE) 
  chi_alpha_upper= qchisq(alpha, df=df, lower.tail = FALSE) 
  chi_halfalpha_lower= qchisq(alpha/2, df=df, lower.tail = TRUE) 
  chi_halfalpha_upper= qchisq(alpha/2, df=df, lower.tail = FALSE) 
  
  cint<-if (alternative == "less") {
    c(0, df*s.x^2/chi_alpha_lower) 
  } else if (alternative == "greater"){
    c(df*s.x^2/chi_alpha_upper,  Inf) 
  } else {
    c(df*s.x^2/chi_halfalpha_upper, df*s.x^2/chi_halfalpha_lower) 
  }
  
  return(list( statistic = tstat, p.value = p.val, conf.int = cint)) 
}

 
output$theory_onevariance <- renderUI({ 
  if ( any(is.na(input$n0), is.na(input$s2_onevariance), is.na(input$h0_onevariance) ) )
  {
     paste("Please enter all required statistics and the parameter value under the null hypothesis.")
  }else{  
  C.level = 1 - (input$alpha) 
  n=input$n0
  df = n-1 
  varx=input$s2_onevariance    
  test <- var.sumTest(s.x=sqrt(varx), n.x=input$n0, 
                      sigma.squared=input$h0_onevariance,
                      alternative = input$alternative_onevariance, conf.level = C.level) 
 
  withMathJax(
    
    tags$b("Confidence interval"),
    br(),
    case_when(  
      input$alternative_onevariance=="less" ~   paste0(
        C.level * 100, "% CI for \\(\\sigma^2 = \\Bigg( 0, \\dfrac{(n-1)s^2}{\\chi^2_{1-\\alpha, n-1}} \\Bigg) = \\) ",
        "( 0, ",   round((n - 1) * varx, 6), " / ", 
        round(qchisq(input$alpha, df = df, lower.tail = TRUE), 6), ") = ",
        "(", round(test$conf.int[1], 6), ",  ", 
        round(test$conf.int[2], 6), ")"
      ),
      input$alternative_onevariance=="greater" ~ paste0(
        C.level * 100, "% CI for \\( \\sigma^2 = \\Bigg( \\dfrac{(n-1)s^2}{\\chi^2_{\\alpha, n-1}}\\),   \\( \\infty  \\Bigg) \\)  = ",
        "(", round((n - 1) * varx, 6), " / ", round(qchisq(input$alpha, 
                                                           df = df, lower.tail = FALSE), 6), ",  ",  "\\( \\infty ) \\) = ",
        "(", round(test$conf.int[1], 6), ",  ", 
        round(test$conf.int[2], 6), ")"
      ),
      input$alternative_onevariance == "two.sided" ~ paste0(
        C.level * 100, "% CI for \\(\\sigma^2 = \\Bigg( \\dfrac{(n-1)s^2}{\\chi^2_{\\alpha/2, n-1}}, \\dfrac{(n-1)s^2}{\\chi^2_{1-\\alpha/2, n-1}} \\Bigg) = \\) ",
        "(", round((n - 1) * varx, 6), " / ", round(qchisq(input$alpha / 2, 
           df = df, lower.tail = FALSE), 6), ",  ", round((n - 1) * varx, 6), " / ", 
        round(qchisq(input$alpha / 2, df = df, lower.tail = TRUE), 6), ") = ",
        "(", round(test$conf.int[1], 6), ",  ", 
        round(test$conf.int[2], 6), ")"
      )
      
    ),  
    br(),
    tags$b("Hypothesis test"),
    br(),
    paste0("1. \\(H_0 : \\sigma^2 = \\) ", input$h0_onevariance, " and \\(H_1 : \\sigma^2 \\) ",
           ifelse(input$alternative_onevariance == "two.sided", "\\( \\neq \\) ", 
                  ifelse(input$alternative_onevariance == "greater", "\\( > \\) ", "\\( < \\) ")), input$h0_onevariance),
    br(),
    paste0(
      "2. Test statistic : \\(\\chi^2_{obs} = \\dfrac{(n-1)s^2}{\\sigma^2_0} = \\) ",
      "[(", n, " - 1) * ", 
      round(varx, 6), "] / ", input$h0_onevariance, " \\( = \\) ",
      round(test$statistic, 6)
    ),
    br(),
    paste0(
      "3. p-value : ", 
      case_when( 
        input$alternative_onevariance == "less" ~ 
          paste0("P\\( (\\chi^2 \\)  ",  "\\( \\leq  \\chi^2_{obs} )  \\) = ", round(test$p.value,6)),
        input$alternative_onevariance == "greater" ~ 
          paste0("P\\( (\\chi^2 \\)  ",  "\\( \\geq  \\chi^2_{obs} )  \\) = ", round(test$p.value,6)),
        input$alternative_onevariance == "two.sided"~ 
          paste0("2*min ", "(P\\( (\\chi^2 \\)  ",  "\\( \\leq  \\chi^2_{obs} ),  \\) ",
                 "P\\( (\\chi^2 \\) ",  "\\( \\geq  \\chi^2_{obs} )  \\) ) =", round(test$p.value,6))
      )
    ),
    br(),
    paste0("4. Conclusion : ", ifelse(test$p.value <= input$alpha, "Reject \\(H_0\\)", "Do not reject \\(H_0\\)")),
    br(),
    tags$b("Interpretation"),
    br(),
    paste0("At the ", input$alpha * 100, "% significance level, ", 
           ifelse(test$p.value <= input$alpha, 
                  "we reject the null hypothesis", "we do not reject the null hypothesis"), 
           " \\((p\\)-value ",  round(test$p.value, 6), ")", ".")
    ) 
  }
})   


output$plot_onevariance <- renderPlot({
  if ( any(is.na(input$n0), is.na(input$s2_onevariance), is.na(input$h0_onevariance) ) )
  {return(NULL)}else{
  C.level = 1 - (input$alpha) 
  n=input$n0
  df = n-1 
  varx=input$s2_onevariance    
  test <- var.sumTest(s.x=sqrt(varx), n.x=input$n0, 
                      sigma.squared=input$h0_onevariance,
                      alternative = input$alternative_onevariance, conf.level = C.level) 
  stat=test$statistic 
  range1=qchisq(0.9999, df = df, lower.tail = FALSE)  
  range2=qchisq(0.9999, df = df, lower.tail = TRUE) 
  p = ggplot(data.frame(x = c(range1, range2)), aes(x = x))
  if (input$alternative_onevariance == "less") {
    p <- p +
      geom_area(stat = "function", fun = dchisq, args = list(df = df), 
                xlim = c(range1,stat),fill="blue",alpha = 0.4) +
      geom_area(stat = "function", fun = dchisq,args = list(df = df), 
                xlim = c(stat, range2),fill="grey",alpha = 0.2) 
  } else if (input$alternative_onevariance == "greater"){
    p <- p +
      geom_area(stat = "function", fun = dchisq, args = list(df = df), 
                xlim = c(range1,stat),fill="grey",alpha = 0.2) +
      geom_area(stat = "function", fun = dchisq,args = list(df = df), 
                xlim = c(stat, range2),fill="blue",alpha = 0.4) 
  } else {   
    lowertailp =  pchisq(stat,df,lower.tail = TRUE) 
    if ( lowertailp<=0.5 )
    {
      upperstat = qchisq(lowertailp,df,lower.tail = FALSE) 
      p <- p +
        geom_area(stat = "function", fun = dchisq, args = list(df = df), 
                  xlim = c(range1,stat),fill="blue",alpha = 0.4) +
        geom_area(stat = "function", fun = dchisq,args = list(df = df), 
                  xlim = c(stat, upperstat),fill="grey",alpha = 0.2) +
        geom_area(stat = "function", fun = dchisq,args = list(df = df), 
                  xlim = c(upperstat, range2),fill="blue",alpha = 0.4)+
        geom_vline(xintercept = upperstat, color = "red")  
    }else if ( lowertailp>0.5 )
    {
      lowerstat = qchisq(lowertailp,df,lower.tail = FALSE)  
      p <- p +
        geom_area(stat = "function", fun = dchisq, args = list(df = df), 
                  xlim = c(range1,lowerstat),fill="blue",alpha = 0.4) +
        geom_area(stat = "function", fun = dchisq,args = list(df = df), 
                  xlim = c(lowerstat, stat),fill="grey",alpha = 0.2) +
        geom_area(stat = "function", fun = dchisq,args = list(df = df), 
                  xlim = c(stat, range2),fill="blue",alpha = 0.4)+
        geom_vline(xintercept = lowerstat, color = "red")
    }
  }
  p<-p+theme_minimal()+
    geom_vline(xintercept = stat, color = "red") +
    geom_text(aes(x = stat, 
                  label = paste0("Test statistic = ", round(test$statistic, 6)), y = 0.05), 
              colour = "red", angle = 90, vjust = 1.3, text = element_text(size = 11)) +
    ggtitle(paste0("Chi-square distribution", " Chi(", df, ")",
                   ", ",  "p-value is indicated by the shaded area")) +
    theme(plot.title = element_text(face = "bold", hjust = 0.5)) +
    ylab("Density") +
    xlab("x") 
  p
  }
}) 

 
output$results_onevariance<- metaRender2( 
  renderPrint,
  { 
    if ( any(is.na(input$n0), is.na(input$s2_onevariance), is.na(input$h0_onevariance) ) )
    {
      metaExpr( paste("Please enter all required statistics and the parameter value under the null hypothesis."))
    }else{
      metaExpr({
    cat("First, run the user-defined function for testing population variance when summary statistics are given:")
    cat("\n")
    var.sumTest<- function(s.x, n.x, sigma.squared, alternative = "two.sided", conf.level = 0.95)
    {
      tstat <- NULL  # test statistic 
      p.val <- NULL  #p-value
      cint <- NULL    # confidence interval bounds
      df = n.x -1
      tstat = df*s.x^2/sigma.squared
      lowertail=pchisq(q=tstat, df=df, lower.tail = TRUE)   
      
      p.val <- if (alternative == "less") {
        lowertail
      } else if (alternative == "greater") {
        1-lowertail
      } else {
        2*ifelse(lowertail<=0.5, lowertail, 1-lowertail)
      }
      
      alpha= 1-conf.level
      chi_alpha_lower= qchisq(alpha, df=df, lower.tail = TRUE)
      chi_alpha_upper= qchisq(alpha, df=df, lower.tail = FALSE)
      chi_halfalpha_lower= qchisq(alpha/2, df=df, lower.tail = TRUE)
      chi_halfalpha_upper= qchisq(alpha/2, df=df, lower.tail = FALSE)
      
      cint<-if (alternative == "less") {
        c(0, df*s.x^2/chi_alpha_lower)
      } else if (alternative == "greater"){
        c(df*s.x^2/chi_alpha_upper,  Inf)
      } else {
        c(df*s.x^2/chi_halfalpha_upper, df*s.x^2/chi_halfalpha_lower)
      }
      
      return(list( statistic = tstat, p.value = p.val, conf.int = cint))
    }
    
test <- var.sumTest(s.x=sqrt(..(input$s2_onevariance)), n.x=..(input$n0), 
                        sigma.squared=..(input$h0_onevariance),
                        alternative = ..(input$alternative_onevariance), 
                        conf.level = 1-..(input$alpha))
    test
      })
    }
  }                                    
) 

var.sum.test<- function(s.x, n.x, s.y, n.y, alternative = "two.sided", conf.level = 0.95)
{
  tstat <- NULL   # test statistic 
  p.val <- NULL   #p-value
  cint <- NULL    # confidence interval bounds
  df1 = n.x -1   
  df2=n.y-1 
  tstat = (s.x/s.y)^2 
  lowertail=pf(q=tstat, df1=df1, df2=df2, lower.tail = TRUE)  #lower tail probability
  
  p.val <- if (alternative == "less") {
    lowertail
  } else if (alternative == "greater") {
    1-lowertail
  } else {
    2*ifelse(lowertail<=0.5, lowertail, 1-lowertail)
  }
  
  alpha= 1-conf.level  
  F_alpha_lower= qf(alpha, df1=df1, df2=df2, lower.tail = TRUE) 
  F_alpha_upper= qf(alpha, df1=df1, df2=df2, lower.tail = FALSE) 
  F_halfalpha_lower= qf(alpha/2, df1=df1, df2=df2, lower.tail = TRUE) 
  F_halfalpha_upper= qf(alpha/2, df1=df1, df2=df2, lower.tail = FALSE) 
  
  cint<-if (alternative == "less") {
    c(0, tstat/F_alpha_lower) 
  } else if (alternative == "greater"){
    c( tstat/F_alpha_upper,  Inf) 
  } else {
    c(tstat/F_halfalpha_upper, tstat/F_halfalpha_lower) 
  }
  
  return(list( statistic = tstat, p.value = p.val, conf.int = cint)) 
}


 
output$theory_twovariance <- renderUI({
  if ( any(is.na(input$n1_two), is.na(input$n2_two), is.na(input$s21_twovariance), is.na(input$s22_twovariance)  ) )
  {
   paste("Please enter all required statistics.")
  }else{
  C.level = 1 - (input$alpha) 
  n1=input$n1_two  
  n2=input$n2_two 
  s21=input$s21_twovariance  
  s22=input$s22_twovariance 
  df1=n1-1           
  df2=n2-1 
  test <- var.sum.test(s.x=sqrt(s21), n.x=n1, s.y=sqrt(s22), n.y=n2, 
                       alternative = input$alternative_twovariance, 
                       conf.level = C.level) 
  ptest=s21/s22     
  varx1=s21  
  varx2=s22 
  
  
  withMathJax(
    
    tags$b("Confidence interval"),
    br(),
    case_when(  
      input$alternative_twovariance=="less" ~ paste0(
        C.level * 100, "% CI for \\( \\dfrac{\\sigma^2_1}{\\sigma^2_2} = 
    \\Bigg( 0,
    \\dfrac{s^2_1}{s^2_2}F_{\\alpha, n_2 - 1, n_1-1} \\Bigg) = \\) ",
        "\\( \\big( \\)", 0,  ", ", round(ptest, 6), " * ", 
        round(qf(input$alpha, df1 = df2, df2 = df1, lower.tail = FALSE), 6), 
        "\\( \\big) = \\) ",
        "(", round(test$conf.int[1], 6), ", ", round(test$conf.int[2], 6), ")"
      ),
      input$alternative_twovariance=="greater" ~  paste0(
        C.level * 100, "% CI for \\( \\dfrac{\\sigma^2_1}{\\sigma^2_2} = 
    \\Bigg( \\dfrac{s^2_1}{s^2_2}\\dfrac{1}{F_{\\alpha, n_1 - 1, n_2-1}},
    \\infty \\Bigg) = \\) ",
        "\\( \\big( \\)", round(ptest, 6), 
        " * (1 / ", round(qf(input$alpha, df1 = df1, df2 = df2, 
                             lower.tail = FALSE), 6), "), ",  
        "\\( \\infty \\big) = \\) ",
        "(", round(test$conf.int[1], 6), ", ", round(test$conf.int[2], 6), ")"
      ) ,
      input$alternative_twovariance=="two.sided" ~ paste0(
        C.level * 100, "% CI for \\( \\dfrac{\\sigma^2_1}{\\sigma^2_2} = 
    \\Bigg( \\dfrac{s^2_1}{s^2_2}\\dfrac{1}{F_{\\alpha/2, n_1 - 1, n_2-1}},
    \\dfrac{s^2_1}{s^2_2}F_{\\alpha/2, n_2 - 1, n_1-1} \\Bigg) = \\) ",
        "\\( \\big( \\)", round(ptest, 6), 
        " * (1 / ", round(qf(input$alpha/2, df1 = df1, df2 = df2, 
                             lower.tail = FALSE), 6), "), ", round(ptest, 6), " * ", 
        round(qf(input$alpha/2, df1 = df2, df2 = df1, lower.tail = FALSE), 6), 
        "\\( \\big) = \\) ",
        "(", round(test$conf.int[1], 6), ", ", round(test$conf.int[2], 6), ")"
      )   
      
    ),  
    br(),
    tags$b("Hypothesis test"),
    br(),
    if (input$alternative_twovariance == "two.sided") {
      withMathJax(
        paste0("1. \\(H_0 : \\sigma^2_1 = \\sigma^2_2 \\) and \\(H_1 : \\sigma^2_1 \\neq \\sigma^2_2 \\) ")
      )
    } else if (input$alternative_twovariance == "greater") {
      withMathJax(
        paste0("1. \\(H_0 : \\sigma^2_1 = \\sigma^2_2 \\) and \\(H_1 : \\sigma^2_1 > \\sigma^2_2 \\) ")
      )
    } else {
      withMathJax(
        paste0("1. \\(H_0 : \\sigma^2_1 = \\sigma^2_2 \\) and \\(H_1 : \\sigma^2_1 < \\sigma^2_2 \\) ")
      )
    },
    br(),
    paste0(
      "2. Test statistic : \\(F_{obs} = \\dfrac{s^2_1}{s^2_2} = \\) ",
      round(varx1, 6), " / ", round(varx2, 6),  " \\( = \\) ",
      round(test$statistic, 6)
    ),
    br(),
    paste0(
      "3. p-value : ", 
      case_when( 
        input$alternative_twovariance == "less" ~ 
          paste0("P\\( ( F \\)  ",  "\\( \\leq  F_{obs} )  \\) = ", round(test$p.value,6)),
        input$alternative_twovariance == "greater" ~ 
          paste0("P\\( (F \\)  ",  "\\( \\geq  F_{obs} )  \\) = ", round(test$p.value,6)),
        input$alternative_twovariance == "two.sided"~ 
          paste0("2*min ", "(P\\( (F \\)  ",  "\\( \\leq  F_{obs} ),  \\) ",
                 "P\\( (F \\) ",  "\\( \\geq  F_{obs} )  \\) ) =", round(test$p.value,6))
      )
    ),
    br(),
    paste0("4. Conclusion : ", ifelse(test$p.value <= input$alpha, "Reject \\(H_0\\)", "Do not reject \\(H_0\\)")),
    br(),
    tags$b("Interpretation"),
    br(),
    paste0("At the ", input$alpha * 100, "% significance level, ", 
           ifelse(test$p.value <= input$alpha, 
                  "we reject the null hypothesis", "we do not reject the null hypothesis"), 
           " \\((p\\)-value ",  round(test$p.value, 6), ")", ".")  
    
  )
  } 
})      

output$plot_twovariance <- renderPlot({
  if ( any(is.na(input$n1_two), is.na(input$n2_two), is.na(input$s21_twovariance), is.na(input$s22_twovariance)  ) )
  {return(NULL)}else{
  C.level = 1 - (input$alpha) 
  n1=input$n1_two 
  n2=input$n2_two 
  s21=input$s21_twovariance  
  s22=input$s22_twovariance 
  df1=n1-1           
  df2=n2-1 
  test <- var.sum.test(s.x=sqrt(s21), n.x=n1, s.y=sqrt(s22), n.y=n2, 
                       alternative = input$alternative_twovariance, 
                       conf.level = C.level) 
  stat=test$statistic 
  range1=qf(0.9999, df1=df1, df2=df2, lower.tail = FALSE)  
  range2=qf(0.9999, df1=df1, df2=df2, lower.tail = TRUE) 
  p = ggplot(data.frame(x = c(range1, range2)), aes(x = x))
  if (input$alternative_twovariance == "less") {
    p <- p +
      geom_area(stat = "function", fun = df, args = list(df1=df1, df2=df2), 
                xlim = c(range1,stat),fill="blue",alpha = 0.4) +
      geom_area(stat = "function", fun = df,args = list(df1=df1, df2=df2), 
                xlim = c(stat, range2),fill="grey",alpha = 0.2) 
  } else if (input$alternative_twovariance == "greater"){
    p <- p +
      geom_area(stat = "function", fun = df, args = list(df1=df1, df2=df2), 
                xlim = c(range1,stat),fill="grey",alpha = 0.2) +
      geom_area(stat = "function", fun = df,args = list(df1=df1, df2=df2), 
                xlim = c(stat, range2),fill="blue",alpha = 0.4) 
  } else {    
    lowertailp =  pf(stat,df1, df2, lower.tail = TRUE) 
    if ( lowertailp<=0.5 )
    {
      upperstat = qf(lowertailp,df1, df2, lower.tail = FALSE) 
      p <- p +
        geom_area(stat = "function", fun = df, args = list(df1=df1, df2=df2), 
                  xlim = c(range1,stat),fill="blue",alpha = 0.4) +
        geom_area(stat = "function", fun = df,args = list(df1=df1, df2=df2), 
                  xlim = c(stat, upperstat),fill="grey",alpha = 0.2) +
        geom_area(stat = "function", fun = df,args = list(df1=df1, df2=df2), 
                  xlim = c(upperstat, range2),fill="blue",alpha = 0.4)+
        geom_vline(xintercept = upperstat, color = "red")
      }else if ( lowertailp>0.5 )
    {
      lowerstat = qf(lowertailp,df1, df2, lower.tail = FALSE)  
      p <- p +
        geom_area(stat = "function", fun = df, args = list(df1=df1, df2=df2), 
                  xlim = c(range1,lowerstat),fill="blue",alpha = 0.4) +
        geom_area(stat = "function", fun = df,args = list(df1=df1, df2=df2), 
                  xlim = c(lowerstat, stat),fill="grey",alpha = 0.2) +
        geom_area(stat = "function", fun = df,args = list(df1=df1, df2=df2), 
                  xlim = c(stat, range2),fill="blue",alpha = 0.4)+
        geom_vline(xintercept = lowerstat, color = "red")
          }
  }
  p<-p+theme_minimal()+
    geom_vline(xintercept = stat, color = "red") +
    geom_text(aes(x = stat, 
                  label = paste0("Test statistic = ", round(test$statistic, 6)), y = 0.2), 
              colour = "red", angle = 90, vjust = 1.3, text = element_text(size = 11)) +
    ggtitle(paste0("F distribution", " F(", df1, ", ", df2, ")",
                   ", ",  "p-value is indicated by the shaded area")) +
    theme(plot.title = element_text(face = "bold", hjust = 0.5)) +
    ylab("Density") +
    xlab("x")
  p
  }
})   


 
output$results_twovariance<- metaRender2( 
  renderPrint,
  { 
    if ( any(is.na(input$n1_two), is.na(input$n2_two), is.na(input$s21_twovariance), is.na(input$s22_twovariance)  ) )
    {
      metaExpr( paste("Please enter all required statistics."))
    }else{
      metaExpr({
    cat("First, run the user-defined function for testing equality of two population variances when summary statistics are given:")
    cat("\n") 
    var.sum.test<- function(s.x, n.x, s.y, n.y, alternative = "two.sided", conf.level = 0.95)
    {
      tstat <- NULL  # test statistic 
      p.val <- NULL  #p-value
      cint <- NULL   # confidence interval bounds
      df1 = n.x -1 
      df2=n.y-1
      tstat = (s.x/s.y)^2
      lowertail=pf(q=tstat, df1=df1, df2=df2, lower.tail = TRUE)  
      
      p.val <- if (alternative == "less") {
        lowertail
      } else if (alternative == "greater") {
        1-lowertail
      } else {
        2*ifelse(lowertail<=0.5, lowertail, 1-lowertail)
      }
      
      alpha= 1-conf.level
      F_alpha_lower= qf(alpha, df1=df1, df2=df2, lower.tail = TRUE)
      F_alpha_upper= qf(alpha, df1=df1, df2=df2, lower.tail = FALSE)
      F_halfalpha_lower= qf(alpha/2, df1=df1, df2=df2, lower.tail = TRUE)
      F_halfalpha_upper= qf(alpha/2, df1=df1, df2=df2, lower.tail = FALSE)
      
      cint<-if (alternative == "less") {
        c(0, tstat/F_alpha_lower)
      } else if (alternative == "greater"){
        c( tstat/F_alpha_upper,  Inf)
      } else {
        c(tstat/F_halfalpha_upper, tstat/F_halfalpha_lower)
      }
      
      return(list( statistic = tstat, p.value = p.val, conf.int = cint)) 
    }
    

    test <- var.sum.test(s.x=sqrt(..(input$s21_twovariance)), n.x=..(input$n1_two), 
                         s.y=sqrt(..(input$s22_twovariance)), n.y=..(input$n2_two), 
                         alternative = ..(input$alternative_twovariance), 
                         conf.level = 1-..(input$alpha))
    test  
      })
    }
  }                                    
)   


output$theory_freqtable <- renderUI({ 
  freq <- extract(input$freq_freqtable)
  p_h0 <- extract(input$h0_freqtable)
  n1=length(freq)   
  n2=length(p_h0) 
  if ( any(is.na(freq)) | any(is.na(p_h0)) | is.null(freq) | is.null(p_h0) | n1 < 2 |  n2 < 2) {
   paste("Invalid input or not enough observations!")
  } else if (n1 != n2) {
   paste( "Number of values must be equal in the two rows!")
  } else if (sum(p_h0) !=1)
  {paste("Sum of probabilities must be equal to 1!")}
  else{
  C.level = 1 - (input$alpha) 
  test <- chisq.test(x=freq, p=p_h0) 
  df = n1-1 
  
  withMathJax(
    tags$b("Hypothesis test"),
    br(),
    paste("1. \\(H_0 : \\) The probabilities of the classification are: ", input$h0_freqtable), 
      br(),
      paste("versus"),
      br(),
      paste("\\(H_1 : \\) Not all probabilities are equal to the specified ones " ),
     br(),
    paste0(
      "2. Test statistic : \\( \\chi^2_{obs}  =\\sum \\limits_{i=1}^{n} \\frac{(O_i - E_i)^2}{E_i} \\) ",
      " \\( = \\) ",  round(test$statistic, 6), ","
    ),
    br(),
    paste0(
      "where  \\( n  = \\)",  n1, "  and thus  \\( df  = \\)", "\\( n-1  = \\)",
      n1-1, "."  ),
    br(),
    paste0(
      "3. p-value : ", 
      "P\\( (\\chi^2_{n-1}  \\geq  \\chi^2_{obs} ) =\\) ",  round(test$p.value,6) 
    ),
    br(),
    paste0("4. Conclusion : ", ifelse(test$p.value <= input$alpha, 
            "Reject \\(H_0\\)", "Do not reject \\(H_0\\)")),
    br(),
    tags$b("Interpretation"),
    br(),
    paste0("At the ", input$alpha * 100, "% significance level, ", 
           ifelse(test$p.value <= input$alpha, 
                  "we reject the null hypothesis", "we do not reject the null hypothesis"), 
           " \\((p\\)-value ",  round(test$p.value, 6), ")", ".")  
     )
  }
})  


output$plot_freqtable <- renderPlot({
    freq <- extract(input$freq_freqtable)
    p_h0 <- extract(input$h0_freqtable)
    n1=length(freq)  
    n2=length(p_h0)
    if ( any(is.na(freq)) | any(is.na(p_h0)) | is.null(freq) | is.null(p_h0) | n1 < 2 |  n2 < 2) {
      paste("Invalid input or not enough observations!")
    } else if (n1 != n2) {
      paste( "Number of values must be equal in the two rows!")
    } else if (sum(p_h0) !=1)
    {paste("Sum of probabilities must be equal to 1!")}
    else{ 
      df=n1-1
  C.level = 1 - (input$alpha) 
  test <- chisq.test(x=freq, p=p_h0) 
  stat=test$statistic 
  range1=qchisq(0.9999, df = df, lower.tail = FALSE)  
  range2=qchisq(0.9999, df = df, lower.tail = TRUE) 
  p <- ggplot(data.frame(x = c(range1, range2)), aes(x = x)) +
    geom_area(stat = "function", fun = dchisq, args = list(df = df), 
              xlim = c(range1,stat),fill="grey",alpha = 0.2) +
    geom_area(stat = "function", fun = dchisq,args = list(df = df), 
              xlim = c(stat, range2),fill="blue",alpha = 0.4) +
    theme_minimal()+
    geom_vline(xintercept = stat, color = "red") +
    geom_text(aes(x = stat, 
                  label = paste0("Test statistic = ", round(stat, 6)), y = 0.05), 
              colour = "red", angle = 90, vjust = 1.3, text = element_text(size = 11)) +
    ggtitle(paste0("Chi-square distribution", " Chi(", df, ")",
                   ", ", "p-value is indicated by the shaded area")) +
    theme(plot.title = element_text(face = "bold", hjust = 0.5)) +
    ylab("Density") +
    xlab("x") 
  p
  }
})

 
output$results_freqtable<- metaRender2( 
  renderPrint,
  { 
    interpolate3(~(freq=xa), xa=as.numeric( unlist(strsplit(input$freq_freqtable,",")) )    )
    interpolate3(~(p_h0=xb), xb=as.numeric(sapply( unlist(strsplit(input$h0_freqtable,",")), function(x) eval(parse(text = x))) )    )
    n1=length(freq)  
    n2=length(p_h0) 
    if ( any(is.na(freq)) | any(is.na(p_h0)) | is.null(freq) | is.null(p_h0) | n1 < 2 |  n2 < 2) {
      metaExpr( paste("Invalid input or not enough observations!") )
    } else if (n1 != n2) {
      metaExpr( paste( "Number of values must be equal in the two rows!"))
    } else if (sum(p_h0) !=1)
    { metaExpr( paste("Sum of probabilities must be equal to 1!") )}
    else{
      metaExpr({
    test <- chisq.test(x=freq, p=p_h0)
    test  
      })
    }
  }                                    
) 

 

output$theory_contable <- renderUI({ 
  if ( input$nrows == '2')
  {
  row1<- extract(input$row21) 
  row2 <- extract(input$row22) 
  dat = as.matrix(rbind(row1,row2)) 
  n1=length(row1)    
  n2=length(row2)  
  if (is.null(row1)| is.null(row2) | any(is.na(row1)) | n1 < 2 | any(is.na(row2)) | n2 < 2) {
    paste("Invalid input or not enough observations")
  } else if (n1 != n2) {
    paste("Number of values must be equal in the two rows!")
  } else{
  C.level = 1 - (input$alpha) 
  test <- chisq.test(dat, correct=FALSE) 
  df = (dim(dat)[1]-1)*(dim(dat)[2]-1) 
  
  withMathJax(
    tags$b("Hypothesis test"),
    br(),
    paste("1. \\(H_0 : \\) The two classifications are independent. "), 
    br(),
    paste("versus"),
    br(),
    paste("\\(H_1 : \\) The two classifications are not independent. " ),
    br(),
    paste0(
      "2. Test statistic : \\( \\chi^2_{obs}  =\\sum \\limits_{i=1}^{nrows} \\sum  \\limits_{j=1}^{ncols} 
      \\frac{(O_{ij} - E_{ij})^2}{E_{ij}} \\) ",
      " \\( = \\) ",  round(test$statistic, 6), ","
    ),
    br(),
    paste0(
      "where  \\( nrows  = \\)", dim(dat)[1],  " and  \\( ncols  = \\)", dim(dat)[2],
      "  and thus  \\( df  = \\)", "\\(  (nrows-1)*(ncols-1)  = \\)",
      df, "."  ),
    br(),
    paste0(
      "3. p-value : ", 
      "P\\( ( \\chi^2 \\)", "\\( ( \\)", df, "\\( ) \\)",  "\\( \\geq  \\chi^2_{obs} ) =\\) ",  round(test$p.value,6) 
    ),
    br(),
    paste0("4. Conclusion : ", ifelse(test$p.value <= input$alpha, 
                                      "Reject \\(H_0\\)", "Do not reject \\(H_0\\)")),
    br(),
    tags$b("Interpretation"),
    br(),
    paste0("At the ", input$alpha * 100, "% significance level, ", 
           ifelse(test$p.value <= input$alpha, 
                  "we reject the null hypothesis", "we do not reject the null hypothesis"), 
           " \\((p\\)-value ",  round(test$p.value, 6), ")", ".")  
    
  ) }} else  if ( input$nrows == '3')
  {
    row1<- extract(input$row31) 
    row2 <- extract(input$row32) 
    row3 <- extract(input$row33) 
    dat = as.matrix(rbind(row1,row2, row3)) 
    n1=length(row1)    
    n2=length(row2)   
    n3=length(row3)  
    if (is.null(row1)| is.null(row2) | is.null(row3)| any(is.na(row1)) | n1 < 2 | any(is.na(row2)) | n2 < 2 | any(is.na(row3)) | n3 < 2) {
     paste( "Invalid input or not enough observations")
    } else if (n1 != n2 | n1 != n3 | n2 != n3) {
      paste("Number of values must be equal in the three rows!")
    } else{
        C.level = 1 - (input$alpha) 
    test <- chisq.test(dat, correct=FALSE) 
    df = (dim(dat)[1]-1)*(dim(dat)[2]-1) 
    
    withMathJax(
      tags$b("Hypothesis test"),
      br(),
      paste("1. \\(H_0 : \\) The two classifications are independent. "), 
      br(),
      paste("versus"),
      br(),
      paste("\\(H_1 : \\) The two classifications are not independent. " ),
      br(),
      paste0(
        "2. Test statistic : \\( \\chi^2_{obs}  =\\sum \\limits_{i=1}^{nrows} \\sum  \\limits_{j=1}^{ncols} 
      \\frac{(O_{ij} - E_{ij})^2}{E_{ij}} \\) ",
        " \\( = \\) ",  round(test$statistic, 6), ","
      ),
      br(),
      paste0(
        "where  \\( nrows  = \\)", dim(dat)[1],  " and  \\( ncols  = \\)", dim(dat)[2],
        "  and thus  \\( df  = \\)", "\\(  (nrows-1)*(ncols-1)  = \\)",
        df, "."  ),
      br(),
      paste0(
        "3. p-value : ", 
        "P\\( ( \\chi^2 \\)", "\\( ( \\)", df, "\\( ) \\)",  "\\( \\geq  \\chi^2_{obs} ) =\\) ",  round(test$p.value,6) 
      ),
      br(),
      paste0("4. Conclusion : ", ifelse(test$p.value <= input$alpha, 
                                        "Reject \\(H_0\\)", "Do not reject \\(H_0\\)")),
      br(),
      tags$b("Interpretation"),
      br(),
      paste0("At the ", input$alpha * 100, "% significance level, ", 
             ifelse(test$p.value <= input$alpha, 
                    "we reject the null hypothesis", "we do not reject the null hypothesis"), 
             " \\((p\\)-value ",  round(test$p.value, 6), ")", ".")  
      
    ) 
    } 
  } else  if ( input$nrows == '4')
  {
    row1<- extract(input$row41) 
    row2 <- extract(input$row42) 
    row3 <- extract(input$row43) 
    row4 <- extract(input$row44) 
    dat = as.matrix(rbind(row1,row2, row3,row4)) 
    n1=length(row1)    
    n2=length(row2)   
    n3=length(row3)  
    n4=length(row4)  
    if (is.null(row1)| is.null(row2) | is.null(row3)| is.null(row4) | any(is.na(row1)) | n1 < 2 | any(is.na(row2)) | n2 < 2 | any(is.na(row3)) | n3 < 2 | any(is.na(row4)) | n4 < 2) {
      paste("Invalid input or not enough observations")
    } else if (n1 != n2 | n1 != n3 | n1 != n4 | n2 != n3 | n2 != n4 | n3 != n4) {
      paste("Number of values must be equal in the three rows!")
    } else{
    C.level = 1 - (input$alpha) 
    test <- chisq.test(dat, correct=FALSE) 
    df = (dim(dat)[1]-1)*(dim(dat)[2]-1) 
    
    withMathJax(
      tags$b("Hypothesis test"),
      br(),
      paste("1. \\(H_0 : \\) The two classifications are independent. "), 
      br(),
      paste("versus"),
      br(),
      paste("\\(H_1 : \\) The two classifications are not independent. " ),
      br(),
      paste0(
        "2. Test statistic : \\( \\chi^2_{obs}  =\\sum \\limits_{i=1}^{nrows} \\sum  \\limits_{j=1}^{ncols} 
      \\frac{(O_{ij} - E_{ij})^2}{E_{ij}} \\) ",
        " \\( = \\) ",  round(test$statistic, 6), ","
      ),
      br(),
      paste0(
        "where  \\( nrows  = \\)", dim(dat)[1],  " and  \\( ncols  = \\)", dim(dat)[2],
        "  and thus  \\( df  = \\)", "\\(  (nrows-1)*(ncols-1)  = \\)",
        df, "."  ),
      br(),
      paste0(
        "3. p-value : ", 
        "P\\( ( \\chi^2 \\)", "\\( ( \\)", df, "\\( ) \\)",  "\\( \\geq  \\chi^2_{obs} ) =\\) ",  round(test$p.value,6) 
      ),
      br(),
      paste0("4. Conclusion : ", ifelse(test$p.value <= input$alpha, 
                                        "Reject \\(H_0\\)", "Do not reject \\(H_0\\)")),
      br(),
      tags$b("Interpretation"),
      br(),
      paste0("At the ", input$alpha * 100, "% significance level, ", 
             ifelse(test$p.value <= input$alpha, 
                    "we reject the null hypothesis", "we do not reject the null hypothesis"), 
             " \\((p\\)-value ",  round(test$p.value, 6), ")", ".")  
      
    ) 
    }
  }
  
  
})  


output$plot_contable <- renderPlot({
  range1=0
  if (input$nrows == '2') 
  { 
  row1<- extract(input$row21) 
  row2 <- extract(input$row22) 
  dat = as.matrix(rbind(row1,row2)) 
  n1=length(row1)    
  n2=length(row2) 
  if (is.null(row1)| is.null(row2) | any(is.na(row1)) | n1 < 2 | any(is.na(row2)) | n2 < 2) {
    return(NULL)
  } else if (n1 != n2) {
    return(NULL)
  } else {
  C.level = 1 - (input$alpha) 
  test <- chisq.test(dat, correct=FALSE) 
  df = (dim(dat)[1]-1)*(dim(dat)[2]-1) 
  stat=test$statistic 
  range2=qchisq(0.9999, df = df, lower.tail = TRUE) 
  p <- ggplot(data.frame(x = c(range1, range2)), aes(x = x)) +
    geom_area(stat = "function", fun = dchisq, args = list(df = df), 
              xlim = c(range1,stat),fill="grey",alpha = 0.2) +
    geom_area(stat = "function", fun = dchisq,args = list(df = df), 
              xlim = c(stat, range2),fill="blue",alpha = 0.4) +
    theme_minimal()+
    geom_vline(xintercept = stat, color = "red") +
    geom_text(aes(x = stat, 
                  label = paste0("Test statistic = ", round(stat, 6)), y = 0.05), 
              colour = "red", angle = 90, vjust = 1.3, text = element_text(size = 11)) +
    ggtitle(paste0("Chi-square distribution", " Chi(", df, ")",
                   ", ", "p-value is indicated by the shaded area")) +
    theme(plot.title = element_text(face = "bold", hjust = 0.5)) +
    ylab("Density") +
    xlab("x") 
  p
    }
  }  else if ( input$nrows == '3') 
  {
    row1<- extract(input$row31) 
    row2 <- extract(input$row32) 
    row3 <- extract(input$row33) 
    dat = as.matrix(rbind(row1,row2,row3)) 
    n1=length(row1)    
    n2=length(row2)   
    n3=length(row3)  
    if (is.null(row1)| is.null(row2) | is.null(row3)| any(is.na(row1)) | n1 < 2 | any(is.na(row2)) | n2 < 2 | any(is.na(row3)) | n3 < 2) {
      return(NULL)
    } else if (n1 != n2 | n1 != n3 | n2 != n3) {
     return(NULL)
    }  else{
    C.level = 1 - (input$alpha) 
    test <- chisq.test(dat, correct=FALSE) 
    df = (dim(dat)[1]-1)*(dim(dat)[2]-1) 
    stat=test$statistic 

    range2=qchisq(0.9999, df = df, lower.tail = TRUE) 
    p <- ggplot(data.frame(x = c(range1, range2)), aes(x = x)) +
      geom_area(stat = "function", fun = dchisq, args = list(df = df), 
                xlim = c(range1,stat),fill="grey",alpha = 0.2) +
      geom_area(stat = "function", fun = dchisq,args = list(df = df), 
                xlim = c(stat, range2),fill="blue",alpha = 0.4) +
      theme_minimal()+
      geom_vline(xintercept = stat, color = "red") +
      geom_text(aes(x = stat, 
                    label = paste0("Test statistic = ", round(stat, 6)), y = 0.05), 
                colour = "red", angle = 90, vjust = 1.3, text = element_text(size = 11)) +
      ggtitle(paste0("Chi-square distribution", " Chi(", df, ")",
                     ", ", "p-value is indicated by the shaded area")) +
      theme(plot.title = element_text(face = "bold", hjust = 0.5)) +
      ylab("Density") +
      xlab("x") 
    p
    }
  } else if ( input$nrows == '4') 
  {
    row1<- extract(input$row41) 
    row2 <- extract(input$row42) 
    row3 <- extract(input$row43) 
    row4 <- extract(input$row44) 
    dat = as.matrix(rbind(row1,row2,row3, row4)) 
    n1=length(row1)    
    n2=length(row2)   
    n3=length(row3)  
    n4=length(row4)  
    if (is.null(row1)| is.null(row2) | is.null(row3)| is.null(row4) | any(is.na(row1)) | n1 < 2 | any(is.na(row2)) | n2 < 2 | any(is.na(row3)) | n3 < 2 | any(is.na(row4)) | n4 < 2) {
      return(NULL)
    } else if (n1 != n2 | n1 != n3 | n1 != n4 | n2 != n3 | n2 != n4 | n3 != n4) {
      return(NULL)
    } else{
    C.level = 1 - (input$alpha) 
    test <- chisq.test(dat, correct=FALSE) 
    df = (dim(dat)[1]-1)*(dim(dat)[2]-1) 
    stat=test$statistic 
  
    range2=qchisq(0.9999, df = df, lower.tail = TRUE) 
    p <- ggplot(data.frame(x = c(range1, range2)), aes(x = x)) +
      geom_area(stat = "function", fun = dchisq, args = list(df = df), 
                xlim = c(range1,stat),fill="grey",alpha = 0.2) +
      geom_area(stat = "function", fun = dchisq,args = list(df = df), 
                xlim = c(stat, range2),fill="blue",alpha = 0.4) +
      theme_minimal()+
      geom_vline(xintercept = stat, color = "red") +
      geom_text(aes(x = stat, 
                    label = paste0("Test statistic = ", round(stat, 6)), y = 0.05), 
                colour = "red", angle = 90, vjust = 1.3, text = element_text(size = 11)) +
      ggtitle(paste0("Chi-square distribution", " Chi(", df, ")",
                     ", ", "p-value is indicated by the shaded area")) +
      theme(plot.title = element_text(face = "bold", hjust = 0.5)) +
      ylab("Density") +
      xlab("x") 
    p 
    }
  }
  
})

 
output$results_contable2<- metaRender2( 
  renderPrint,
    { 
      interpolate3(~(row1=xa), xa=as.numeric( unlist(strsplit((input$row21),",")) )    )
      interpolate3(~(row2=xb), xb=as.numeric( unlist(strsplit((input$row22),",")) )    )
      n1=length(row1)  
      n2=length(row2)
      if (is.null(row1)| is.null(row2) | any(is.na(row1)) | n1 < 2 | any(is.na(row2)) | n2 < 2) {
        metaExpr( paste("Invalid input or not enough observations") )
      } else if (n1 != n2) {
        metaExpr( paste("Number of values must be equal in the two rows!"))
      } else {
        metaExpr({
    dat = as.matrix(rbind(row1,row2))
    test <- chisq.test(dat, correct=FALSE)
    test  
        })
      }
  }                                    
) 

output$results_contable3<- metaRender2( 
  renderPrint,
  { 
interpolate3(~(row1=xa), xa=as.numeric( unlist(strsplit((input$row31),",")) )    )
interpolate3(~(row2=xb), xb=as.numeric( unlist(strsplit((input$row32),",")) )    )   
interpolate3(~(row3=xc), xc=as.numeric( unlist(strsplit((input$row33),",")) )    )    
n1=length(row1)  
n2=length(row2)
n3=length(row3)
if (is.null(row1)| is.null(row2) | is.null(row3)| any(is.na(row1)) | n1 < 2 | any(is.na(row2)) | n2 < 2 | any(is.na(row3)) | n3 < 2) {
  metaExpr( paste("Invalid input or not enough observations") )
} else if (n1 != n2 | n1 != n3 | n2 != n3) {
  metaExpr( paste("Number of values must be equal in all three rows!"))
}  else{
  metaExpr({
    dat = as.matrix(rbind(row1,row2,row3))
    test <- chisq.test(dat, correct=FALSE)
    test  
  })
}
  }                                    
) 

output$results_contable4<- metaRender2( 
  renderPrint,
  { 
interpolate3(~(row1=xa), xa=as.numeric( unlist(strsplit((input$row41),",")) )    )
interpolate3(~(row2=xb), xb=as.numeric( unlist(strsplit((input$row42),",")) )    )   
interpolate3(~(row3=xc), xc=as.numeric( unlist(strsplit((input$row43),",")) )    )
interpolate3(~(row4=xd), xd=as.numeric( unlist(strsplit((input$row44),",")) )    )
    n1=length(row1)   
    n2=length(row2)  
    n3=length(row3) 
    n4=length(row4) 
    if (is.null(row1)| is.null(row2) | is.null(row3)| is.null(row4) | any(is.na(row1)) | n1 < 2 | any(is.na(row2)) | n2 < 2 | any(is.na(row3)) | n3 < 2 | any(is.na(row4)) | n4 < 2) {
      metaExpr( paste("Invalid input or not enough observations") )
    } else if (n1 != n2 | n1 != n3 | n1 != n4 | n2 != n3 | n2 != n4 | n3 != n4) {
      metaExpr( paste("Number of values must be equal in all four rows!"))
    } else{
      metaExpr({
    dat = as.matrix(rbind(row1,row2,row3, row4))
    test <- chisq.test(dat, correct=FALSE)
    test
      })
    }
  }                                    
) 


output$code_onemeanz <- renderPrint( expandChain(quote(library(BSDA)),output$results_onemeanz()) )   
output$code_onemeant <- renderPrint( expandChain(quote(library(BSDA)),output$results_onemeant()) ) 
output$code_oneprop <- renderPrint( expandChain(output$results_oneprop()) ) 
output$code_twomeanz <- renderPrint( expandChain(quote(library(BSDA)),output$results_twomeanz()) )   
output$code_twomeant <- renderPrint( expandChain(quote(library(BSDA)),output$results_twomeant()) )  
output$code_twoprop <- renderPrint( expandChain(output$results_twoprop()) )  
output$code_onevariance <- renderPrint( expandChain(output$results_onevariance()) )   
output$code_twovariance <- renderPrint( expandChain(output$results_twovariance()) )   
output$code_freqtable <- renderPrint( expandChain(output$results_freqtable()) )   
output$code_contable2 <- renderPrint( expandChain(output$results_contable2()) )   
output$code_contable3 <- renderPrint( expandChain(output$results_contable3()) )   
output$code_contable4 <- renderPrint( expandChain(output$results_contable4()) )   
 
  })   
}    
    