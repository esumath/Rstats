#' Data_Inferences UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_Data_Inferences_ui <- function(id){
  ns <- NS(id)  
  
 inferencetypes <- list(
   "One Population Mean - T Test/Interval"="OneMeanT",
   "Two Population Means - T Test/Interval"="TwoMeanT",
   "Two Population Means (paired samples)"="TwoMeans",
   "One Population Variance"="OneVariance", 
   "Two Population Variances"="TwoVariance",
   "One Population Mean - Z Test/Interval"="OneMeanZ",
   "Two Population Means - Z Test/Interval"="TwoMeanZ")
  
fluidPage(
    
    withMathJax(),
    sidebarLayout(
      
    sidebarPanel(          
h4("Inference about population parameters when small sample data is manually entered:"),
        br(),
selectInput(
  inputId = ns("inference"),
  label = "Inference about:",
  choices = inferencetypes,
  multiple = FALSE,
  selected = "one mean"
),
conditionalPanel(
  condition = "input.inference == 'OneMeanZ' | input.inference == 'OneMeanT' | input.inference == 'OneVariance' ",
  textInput(ns("onesample"), "Sample data", 
    value = "1.36, 1.42, 5.93, 5.36, 3.46, 2.87", 
    placeholder = "Enter values separated by a comma with decimals as points, e.g. 4.2, 4.4, 5, 5.03, etc."),
 ns=ns
),
conditionalPanel(
  condition = "input.inference != 'OneMeanZ' &  input.inference != 'OneMeanT' & input.inference != 'OneVariance' ",
  textInput(ns("sample1"), "Sample 1", 
     value = "3.2, 4.5, 3.8, 4.0, 3.7", 
     placeholder = "Enter values separated by a comma with decimals as points, e.g. 4.2, 4.4, 5, 5.03, etc."),
  textInput(ns("sample2"), "Sample 2", 
     value = "3.5, 3.2, 4.8, 3.0, 4.1", 
     placeholder = "Enter values separated by a comma with decimals as points, e.g. 4.2, 4.4, 5, 5.03, etc."),
  ns=ns
),
conditionalPanel(
  condition = "input.inference == 'OneMeanZ'",
  numericInput(ns("sigma2_onemeanz"), "Population variance \\(\\sigma^2 = \\)",
                 value = 15, min = 0, step = 1
    ), ns=ns
),
conditionalPanel(
  condition = "input.inference == 'TwoMeanZ'",
  numericInput(ns("sigma21_twomeanz"), "Population variance \\(\\sigma^2_1 = \\)",
               value = 0.20, min = 0, step = 1
  ), 
  numericInput(ns("sigma22_twomeanz"), "Population variance \\(\\sigma^2_2 = \\)",
               value = 2.0, min = 0, step = 1
  ),
  ns=ns
),
conditionalPanel(
  condition = "input.inference == 'TwoMeanT'",
  checkboxInput( ns("var.equal"), "Variances of the populations are equal", FALSE),
  ns=ns
),
  tags$b("Null hypothesis"),
conditionalPanel(
  condition = "input.inference == 'OneMeanZ' | input.inference == 'OneMeanT'",
  sprintf("\\( H_0 : \\mu = \\mu_0 =  \\)"), 
  ns=ns
),
conditionalPanel(
  condition = "input.inference == 'TwoMeanZ' | input.inference == 'TwoMeanT'",
  sprintf("\\( H_0 : \\mu_1 - \\mu_2 =\\mu_d= \\)"), ns=ns
),
conditionalPanel(
  condition = "input.inference == 'TwoMeans'",
  sprintf("\\( H_0 : \\mu_D =\\mu_0 = \\)"), ns=ns
),
conditionalPanel(
  condition = "input.inference == 'OneVariance'",
  sprintf("\\( H_0 : \\sigma^2 = \\sigma_0^2= \\)"), ns=ns
),
conditionalPanel(
  condition = "input.inference == 'TwoVariance'",
  sprintf("\\( H_0 : \\sigma^2_1 = \\sigma^2_2 \\)"), ns=ns
),  
conditionalPanel(
  condition = "input.inference != 'TwoVariance'",
  numericInput(ns("h0"),
               label = NULL,
               value = 5, step = 0.1
  ), ns=ns   
),   
conditionalPanel(
  condition = "input.inference == 'OneMeanZ' | input.inference == 'OneMeanT'",
  radioButtons(
    inputId = ns("alternative1"),
    label = "Alternative \\( H_1 :  \\)",
    choices = c(
      "\\( \\mu \\neq \\mu_0 \\)" = "two.sided",
      "\\( \\mu <  \\mu_0 \\)" = "less",
      "\\( \\mu >  \\mu_0  \\)" = "greater"
          )
  ),
  ns=ns 
),
conditionalPanel(
  condition = "input.inference == 'TwoMeanZ' | input.inference == 'TwoMeanT'",
  radioButtons(
    inputId = ns("alternative2"),
    label = "Alternative \\( H_1 :  \\)",
    choices = c(
      "\\(\\mu_1 - \\mu_2 \\neq \\mu_d  \\)" = "two.sided",
      "\\( \\mu_1 - \\mu_2 <  \\mu_d  \\)" = "less",
      "\\( \\mu_1 - \\mu_2 >  \\mu_d   \\)" = "greater"
          )
  ),  ns=ns
),
conditionalPanel(
  condition = "input.inference == 'TwoMeans'",
  radioButtons(
    inputId = ns("alternative3"),
    label = "Alternative \\( H_1 :  \\)",
    choices = c(
      "\\(\\mu_D \\neq  \\mu_0  \\)" = "two.sided",
      "\\( \\mu_D < \\mu_0   \\)" = "less",
      "\\( \\mu_D > \\mu_0    \\)" = "greater"
          )
  ),  
  ns=ns
),
conditionalPanel(
  condition = "input.inference == 'OneVariance'",
  radioButtons(
    inputId = ns("alternative4"),
    label = "Alternative \\( H_1 :  \\)",
    choices = c(
      "\\( \\sigma^2 \\neq \\sigma_0^2  \\)" = "two.sided",
      "\\( \\sigma^2 < \\sigma_0^2   \\)" = "less",
      "\\( \\sigma^2 >  \\sigma_0^2   \\)" = "greater"
          )
  ), 
  ns=ns
),
conditionalPanel(
  condition = "input.inference == 'TwoVariance'",
  radioButtons(
    inputId = ns("alternative5"),
    label = "Alternative \\( H_1 :  \\)",
    choices = c(
      "\\( \\sigma^2_1 \\neq \\sigma^2_2 \\)" = "two.sided",
      "\\( \\sigma^2_1 < \\sigma^2_2 \\)" = "less",
      "\\( \\sigma^2_1 > \\sigma^2_2 \\)" = "greater"
          )
  ), ns=ns
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
    condition = "input.inference == 'TwoMeanZ'",
    uiOutput(ns("theory_twomeanz")), ns = ns
  ),
  conditionalPanel(
    condition = "input.inference == 'TwoMeanT'",
    uiOutput(ns("theory_twomeant")), ns = ns
  ),
  conditionalPanel(
    condition = "input.inference == 'TwoMeans'",
    uiOutput(ns("theory_twomeans")), ns = ns
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
    condition = "input.inference == 'OneMeanZ'",
    plotOutput(ns("plot_onemeanz"), height="270px"), ns = ns
  ),
  conditionalPanel(
    condition = "input.inference == 'OneMeanT'",
    plotOutput(ns("plot_onemeant"), height="270px"), ns = ns
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
    condition = "input.inference == 'TwoMeans'",
    plotOutput(ns("plot_twomeans"), height="270px"), ns = ns
  ),
  conditionalPanel(
    condition = "input.inference == 'OneVariance'",
    plotOutput(ns("plot_onevariance"), height="270px"), ns = ns
  ),
  conditionalPanel(
    condition = "input.inference == 'TwoVariance'",
    plotOutput(ns("plot_twovariance"), height="270px"), ns = ns
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
    condition = "input.inference == 'TwoMeanZ'",
    verbatimTextOutput(ns("results_twomeanz")), ns = ns
  ),
  conditionalPanel(
    condition = "input.inference == 'TwoMeanT'",
    verbatimTextOutput(ns("results_twomeant")), ns = ns
  ),
  conditionalPanel(
    condition = "input.inference == 'TwoMeans'",
    verbatimTextOutput(ns("results_twomeans")), ns = ns
  ),
  conditionalPanel(
    condition = "input.inference == 'OneVariance'",
    verbatimTextOutput(ns("results_onevariance")), ns = ns
  ),
  conditionalPanel(
    condition = "input.inference == 'TwoVariance'",
    verbatimTextOutput(ns("results_twovariance")), ns = ns
  ),
     tags$b("R code for the test:"),   
  conditionalPanel(
    condition = "input.inference == 'OneMeanZ'",
    verbatimTextOutput(ns("code_onemeanz")), ns = ns
  ),
  conditionalPanel(
    condition = "input.inference == 'OneMeanT'",
    verbatimTextOutput(ns("code_onemeant")), ns = ns
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
  condition = "input.inference == 'TwoMeans'",
  verbatimTextOutput(ns("code_twomeans")), ns = ns
),
conditionalPanel(
  condition = "input.inference == 'OneVariance'",
  verbatimTextOutput(ns("code_onevariance")), ns = ns
),
conditionalPanel(
  condition = "input.inference == 'TwoVariance'",
  verbatimTextOutput(ns("code_twovariance")), ns = ns
)


      )       
    )
   )    
 
}


#' Data_Inferences Server Functions
#'
#' @noRd 
mod_Data_Inferences_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns  
    
dat<-reactive({
dat= as.numeric( unlist(strsplit((input$onesample),",")) )  
  return(dat)
})
dat1<-reactive({
  dat1 <- as.numeric( unlist(strsplit((input$sample1),",")) )
  return(dat1)
})
dat2<-reactive ({
  dat2 <- as.numeric( unlist(strsplit((input$sample2),",")) )
  return(dat2)
})
    

output$theory_onemeanz <- renderUI({ 
    dat <- dat()
    n=length(dat)
if (is.null(dat) | any(is.na(dat)) | n < 3) {
        "Invalid input or not enough observations."
      } else if (is.na(input$sigma2_onemeanz) | input$sigma2_onemeanz<0 |  is.na(input$h0)){
"The required parameter or the mean value under the null hypothesis is missing."       
      }  else{
C.level_onez = 1 - (input$alpha)  
  test_onez<-z.test(x = dat, mu = input$h0, sigma.x=sqrt(input$sigma2_onemeanz),
         alternative = input$alternative1, conf.level = C.level_onez)  
  
      withMathJax(
        
        tags$b("Confidence interval"),
        br(),
        case_when(  
          input$alternative1=="less" ~ 
            paste0(
              C.level_onez * 100, "% CI for \\(\\mu = \\bigg(-\\infty, \\bar{x} + \\)", 
              "\\( Z_{",  
              input$alpha,
              ", n-1}",
              " \\dfrac{\\sigma}{\\sqrt{n}} \\bigg)= \\) ",
              "\\( ( -\\infty,  \\) ", mean(dat),"\\( + \\)", 
              round(qnorm(input$alpha, lower.tail = FALSE), 6), 
              " * ", round(sqrt(input$sigma2_onemeanz), 6), " / ", 
              round(sqrt(n), 6), "\\( ) \\) ", "\\( = \\) ",
              "(", round(test_onez$conf.int[1], 6), ",  ", 
              round(test_onez$conf.int[2], 6), ")"
            ),
          input$alternative1=="greater" ~ 
            paste0(
              C.level_onez * 100, "% CI for \\(\\mu = \\bigg( \\bar{x} - \\)", 
              "\\( Z_{",  
              input$alpha,
              ", n-1}",
              " \\dfrac{\\sigma}{\\sqrt{n}} ,  \\infty \\bigg)  = \\) ",
              "\\( (   \\) ", mean(dat), "\\( - \\)",
              round(qnorm(input$alpha, lower.tail = FALSE), 6), 
              " * ", round(sqrt(input$sigma2_onemeanz), 6), " / ", 
              round(sqrt(n), 6), ",  ",
              "\\(  \\infty)  \\) ",  "\\( = \\) ",
              "(", round(test_onez$conf.int[1], 6), ",  ", 
              round(test_onez$conf.int[2], 6), ")"
            ),
          input$alternative1 == "two.sided" ~
            paste0(
              C.level_onez * 100, "% CI for \\(\\mu = \\bar{x} \\pm \\)", 
              "\\( Z_{",  
              input$alpha/2,
              ", n-1}",
              " \\dfrac{\\sigma}{\\sqrt{n}} = \\) ",
              round(test_onez$estimate, 6), "  \\( \\pm \\) ", "\\( ( \\)", 
              round(qnorm(input$alpha / 2, lower.tail = FALSE), 6), 
              " * ", round(sqrt(input$sigma2_onemeanz), 6), " / ", 
              round(sqrt(n), 6), "\\( ) \\) ", "\\( = \\) ",
              "(", round(test_onez$conf.int[1], 6), ",  ", 
              round(test_onez$conf.int[2], 6), ")"
            )
        ),  
        br(),
        tags$b("Hypothesis test"),
        br(),
        paste0("1. \\(H_0 : \\mu = \\) ", test_onez$null.value, " and \\(H_1 : \\mu \\) ", ifelse(input$alternative1 == "two.sided", "\\( \\neq \\) ", ifelse(input$alternative1 == "greater", "\\( > \\) ", "\\( < \\) ")), test_onez$null.value),
        br(),
        paste0(
          "2. Test statistic : \\(z_{obs} = \\dfrac{\\bar{x} - \\mu_0}{ \\sigma / \\sqrt{n}} = \\) ",
          "(", round(test_onez$estimate, 6), ifelse(test_onez$null.value >= 0, 
                                                    paste0(" - ", test_onez$null.value), paste0(" + ", abs(test_onez$null.value))), ") / ", round(sqrt(input$sigma2_onemeanz)/sqrt(n), 6), " \\( = \\) ",
          round(test_onez$statistic, 6)
        ),
        br(),
        paste0(
          "3. p-value : ", 
          case_when( 
            input$alternative1 == "less" ~ 
              paste0("P\\( (Z \\)  ",  "\\( \\leq  z_{obs} )  \\) = ", round(test_onez$p.value,6)),
            input$alternative1 == "greater" ~ 
              paste0("P\\( (Z \\)  ",  "\\( \\geq  z_{obs} )  \\) = ", round(test_onez$p.value,6)),
            input$alternative1 == "two.sided"~ 
              paste0("2*min ", "(P\\( (Z \\)  ",  "\\( \\leq  z_{obs} ),  \\) ",
                     "P\\( (Z \\) ",  "\\( \\geq  z_{obs} )  \\) ) =", round(test_onez$p.value,6))
          )
        ),
        br(),
        paste0("4. Conclusion : ", ifelse(test_onez$p.value <= input$alpha, "Reject \\(H_0\\)", "Do not reject \\(H_0\\)")),
        br(),
        tags$b("Interpretation"),
        br(),
        paste0("At the ", input$alpha * 100, "% significance level, ", 
               ifelse(test_onez$p.value <= input$alpha, 
                      "we reject the null hypothesis", "we do not reject the null hypothesis"), 
               " \\((p\\)-value ",  round(test_onez$p.value, 6), ")", ".")
        
        
      )
      }
    })    
    
 
 output$plot_onemeanz <- renderPlot({
   dat <- dat()
   n=length(dat)
if (is.null(dat) | any(is.na(dat)) | n < 3 | is.na(input$sigma2_onemeanz) | input$sigma2_onemeanz<0 | is.na(input$h0)) {return(NULL)}
else{
      test <- z.test(x = dat, mu = input$h0, sigma.x=sqrt(input$sigma2_onemeanz),
                     alternative = input$alternative1, conf.level = 1 - input$alpha)
      zstat1=test$statistic
      abszstat1=abs(zstat1)
      range1=qnorm(0.9999, lower.tail = FALSE) 
      range2=qnorm(0.9999, lower.tail = TRUE)
      p=ggplot(data.frame(x = c(range1, range2)), aes(x = x))
      if (input$alternative1 == "less") {
        p <- p +
          geom_area(stat = "function", fun = dnorm, args = list(mean = 0, sd = 1), 
                    xlim = c(range1,zstat1),fill="blue",alpha = 0.4) +
          geom_area(stat = "function", fun = dnorm,args = list(mean = 0, sd = 1), 
                    xlim = c(zstat1, range2),fill="grey",alpha = 0.2) 
      } else if (input$alternative1 == "greater"){
        p <- p +
          geom_area(stat = "function", fun = dnorm, args = list(mean = 0, sd = 1), 
                    xlim = c(range1,zstat1),fill="grey",alpha = 0.2) +
          geom_area(stat = "function", fun = dnorm,args = list(mean = 0, sd = 1), 
                    xlim = c(zstat1, range2),fill="blue",alpha = 0.4) 
      } else  
        {
        p <- p +
          geom_area(stat = "function", fun = dnorm, args = list(mean = 0, sd = 1), 
                    xlim = c(range1,-abszstat1),fill="blue",alpha = 0.4) +
          geom_area(stat = "function", fun = dnorm,args = list(mean = 0, sd = 1), 
                    xlim = c(-abszstat1, abszstat1),fill="grey",alpha = 0.2) +
          geom_area(stat = "function", fun = dnorm,args = list(mean = 0, sd = 1), 
                    xlim = c(abszstat1, range2),fill="blue",alpha = 0.4)+
          geom_vline(xintercept = -zstat1, color = "red")
        }
  p<-p+theme_minimal()+
        geom_vline(xintercept = zstat1, color = "red") +
        geom_text(aes(x = zstat1, 
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
        interpolate3(~(dat=x0), x0=as.numeric( unlist(strsplit((input$onesample),",")) )    )
        if (is.null(dat) | any(is.na(dat)) | length(dat) < 3 | is.na(input$sigma2_onemeanz) | input$sigma2_onemeanz<0 |  is.na(input$h0)) {
          return(NULL)
        } else{
        metaExpr({
          test=z.test(x =dat , mu = ..(input$h0), sigma.x=sqrt(..(input$sigma2_onemeanz)),
                      alternative = ..(input$alternative1), conf.level=1-..(input$alpha))
          CI= test$conf.int
          cat("Confidence interval", "of level", 1-..(input$alpha), "is :", CI)
          test    
        })    
        }
      }) 
    
    output$theory_onemeant <- renderUI({ 
      dat <- dat()
      n=length(dat)
      if (is.null(dat) | any(is.na(dat)) | n < 3) 
      {  "Invalid input or not enough observations"  } 
      else if (is.na(input$h0)){
        "Please enter the mean value under the null hypothesis." 
      }   else
      {
      C.level = 1 - (input$alpha)  
      test_onet<-t.test(x = dat, mu = input$h0, 
                        alternative = input$alternative1, conf.level = C.level)  
      
    withMathJax(
        
        tags$b("Confidence interval"),
        br(),
        case_when(  
          input$alternative1=="less" ~ 
            paste0(
              C.level * 100, "% CI for \\(\\mu = \\bigg(-\\infty, \\bar{x} + \\)", 
              "\\( t_{",  
              input$alpha,
              ", n-1}",
              " \\dfrac{s}{\\sqrt{n}} \\bigg)= \\) ",
              "\\( ( -\\infty,  \\) ", mean(dat),"\\( + \\)", 
              round(qt(input$alpha, df = test_onet$parameter, lower.tail = FALSE), 6), 
              " * ", round(test_onet$stderr * sqrt(n), 6), " / ", 
              round(sqrt(n), 6), "\\( ) \\) ", "\\( = \\) ",
              "(", round(test_onet$conf.int[1], 6), ",  ", 
              round(test_onet$conf.int[2], 6), ")"
            ),
          input$alternative1=="greater" ~ 
            paste0(
              C.level * 100, "% CI for \\(\\mu = \\bigg( \\bar{x} - \\)", 
              "\\( t_{",  
              input$alpha,
              ", n-1}",
              " \\dfrac{s}{\\sqrt{n}} ,  \\infty \\bigg)  = \\) ",
              "\\( (   \\) ", mean(dat), "\\( - \\)",
              round(qt(input$alpha, df = test_onet$parameter, lower.tail = FALSE), 6), 
              " * ", round(test_onet$stderr * sqrt(n), 6), " / ", 
              round(sqrt(n), 6), ",  ",
              "\\(  \\infty)  \\) ",  "\\( = \\) ",
              "(", round(test_onet$conf.int[1], 6), ",  ", 
              round(test_onet$conf.int[2], 6), ")"
            ),
          input$alternative1 == "two.sided" ~
            paste0(
              C.level * 100, "% CI for \\(\\mu = \\bar{x} \\pm \\)", 
              "\\( t_{",  
              input$alpha/2,
              ", n-1}",
              " \\dfrac{s}{\\sqrt{n}} = \\) ",
              round(test_onet$estimate, 6), "  \\( \\pm \\) ", "\\( ( \\)", 
              round(qt(input$alpha / 2, df = test_onet$parameter, lower.tail = FALSE), 6), 
              " * ", round(test_onet$stderr * sqrt(n), 6), " / ", 
              round(sqrt(n), 6), "\\( ) \\) ", "\\( = \\) ",
              "(", round(test_onet$conf.int[1], 6), ",  ", 
              round(test_onet$conf.int[2], 6), ")"
            )
        ),  
        br(),
        tags$b("Hypothesis test"),
        br(),
        paste0("1. \\(H_0 : \\mu = \\) ", test_onet$null.value, " and \\(H_1 : \\mu \\) ", 
               ifelse(input$alternative1 == "two.sided", "\\( \\neq \\) ", 
                      ifelse(input$alternative1 == "greater", "\\( > \\) ", "\\( < \\) ")), 
               test_onet$null.value),
        br(),
        paste0(
          "2. Test statistic : \\(t_{obs} = \\dfrac{\\bar{x} - \\mu_0}{s / \\sqrt{n}} = \\) ",
          "(", round(test_onet$estimate, 6), ifelse(test_onet$null.value >= 0, 
                  paste0(" - ", test_onet$null.value), 
                  paste0(" + ", abs(test_onet$null.value))), ") / ", round(test_onet$stderr, 6), " \\( = \\) ",
          round(test_onet$statistic, 6)
        ),
        br(),
        paste0(
          "3. p-value : ", 
          case_when( 
            input$alternative1 == "less" ~ 
              paste0("P\\( (T \\)  ",  "\\( \\leq  t_{obs} )  \\) = ", round(test_onet$p.value,6)),
            input$alternative1 == "greater" ~ 
              paste0("P\\( (T \\)  ",  "\\( \\geq  t_{obs} )  \\) = ", round(test_onet$p.value,6)),
            input$alternative1 == "two.sided"~ 
              paste0("2*min ", "(P\\( (T \\)  ",  "\\( \\leq  t_{obs} ),  \\) ",
                     "P\\( (T \\) ",  "\\( \\geq  t_{obs} )  \\) ) =", round(test_onet$p.value,6))
          )
        ),
        br(),
        paste0("4. Conclusion : ", ifelse(test_onet$p.value <= input$alpha, "Reject \\(H_0\\)", "Do not reject \\(H_0\\)")),
        br(),
        tags$b("Interpretation"),
        br(),
        paste0("At the ", input$alpha * 100, "% significance level, ", 
               ifelse(test_onet$p.value <= input$alpha, 
                      "we reject the null hypothesis", "we do not reject the null hypothesis"), 
               " \\((p\\)-value ",  round(test_onet$p.value, 6), ")", ".")
      )
      } 
    })    
    
    output$plot_onemeant <- renderPlot({
      dat <- dat()
  if (is.null(dat) | any(is.na(dat)) | length(dat) < 3 | is.na(input$h0)) {return(NULL)}
     else {
      test_onet <- t.test(x = dat, mu = input$h0, alternative = input$alternative1, 
                          conf.level = 1 - input$alpha)  
      df1=test_onet$parameter  
      tstat1=test_onet$statistic  
      abststat1=abs(tstat1)  
      range1=qt(0.9999, df = test_onet$parameter, lower.tail = FALSE)   
      range2=qt(0.9999, df = test_onet$parameter, lower.tail = TRUE)  
      p= ggplot(data.frame(x = c(range1, range2)), aes(x = x))
      if (input$alternative1 == "less") {
        p <- p +
          geom_area(stat = "function", fun = dt, args = list(df = df1), 
                    xlim = c(range1,tstat1),fill="blue",alpha = 0.4) +
          geom_area(stat = "function", fun = dt,args = list(df = df1), 
                    xlim = c(tstat1, range2),fill="grey",alpha = 0.2) 
      } else if (input$alternative1 == "greater"){
        p <- p +
          geom_area(stat = "function", fun = dt, args = list(df = df1), 
                    xlim = c(range1,tstat1),fill="grey",alpha = 0.2) +
          geom_area(stat = "function", fun = dt,args = list(df = df1), 
                    xlim = c(tstat1, range2),fill="blue",alpha = 0.4) 
      }else{
        p <- p +
          geom_area(stat = "function", fun = dt, args = list(df = df1), 
                    xlim = c(range1,-abststat1),fill="blue",alpha = 0.4) +
          geom_area(stat = "function", fun = dt,args = list(df = df1), 
                    xlim = c(-abststat1, abststat1),fill="grey",alpha = 0.2) +
          geom_area(stat = "function", fun = dt,args = list(df = df1), 
                    xlim = c(abststat1, range2),fill="blue",alpha = 0.4)+
          geom_vline(xintercept = -tstat1, color = "red")
      }
p<-p+theme_minimal()+
  geom_vline(xintercept = tstat1, color = "red") +
  geom_text(aes(x = tstat1, 
                label = paste0("Test statistic = ", round(test_onet$statistic, 6)), y = 0.2), 
            colour = "red", angle = 90, vjust = 1.3, text = element_text(size = 11)) +
  ggtitle(paste0("Student distribution", " t(", round(test_onet$parameter, 6), ")",
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
interpolate3(~(dat=x0),x0=as.numeric( unlist(strsplit((input$onesample),",")) ) )
        if (is.null(dat) | any(is.na(dat)) | length(dat) < 3 | is.na(input$h0)) {
          return(NULL)
        }else{
             metaExpr({
  test=t.test(x = dat, mu = ..(input$h0), alternative = ..(input$alternative1), 
              conf.level=1-..(input$alpha))
  CI= test$conf.int
        cat("Confidence interval", "of level", 1-..(input$alpha), "is :", CI)
    test  
        })
        } 
      }
    ) 

output$theory_twomeanz <- renderUI({ 
  dat1 <- dat1()
  dat2 <- dat2()
  n1=length(dat1) 
  n2=length(dat2)
if ( is.null(dat1) | is.null(dat2) | any(is.na(dat1)) | n1 < 3 | any(is.na(dat2)) | n2 < 3) 
  {    "Invalid input or not enough observations"   }
  else if (any(is.na(input$sigma21_twomeanz), is.na(input$sigma22_twomeanz), is.na(input$h0)) | input$sigma21_twomeanz<0 | input$sigma22_twomeanz<0 )
  {"The population variances are missing or negative, or the mean value under the null hypothesis is missing."} else{
  x1bar=mean(dat1)
  x2bar=mean(dat2) 
  se_z= sqrt(input$sigma21_twomeanz/n1 + input$sigma22_twomeanz/n2)   
  C.level = 1 - (input$alpha)  
  test_2meanz<-z.test(x = dat1, y=dat2, mu = input$h0, sigma.x=sqrt(input$sigma21_twomeanz),
                      sigma.y=sqrt(input$sigma22_twomeanz),
                      alternative = input$alternative2, conf.level = C.level)  
  
  withMathJax(
    
    tags$b("Confidence interval"),
    br(),
    case_when(  
      input$alternative2=="less" ~ 
        paste0(
          C.level * 100, "% Confidence Interval for \\(\\mu_1 - \\mu_2 = \\bigg( -\\infty,
          \\bar{x}_1 - \\bar{x}_2 + z_{\\alpha} \\sqrt{\\dfrac{\\sigma^2_1}{n_1} + 
          \\dfrac{\\sigma^2_2}{n_2}} \\bigg)= \\) ", "\\( ( -\\infty,  \\) ",
          round(x1bar, 6), 
          ifelse(x2bar >= 0, paste0(" - ", round(x2bar, 6)), 
                 paste0(" + ", round(abs(x2bar), 6))), "  \\( + \\)",  
          round(qnorm(input$alpha, lower.tail = FALSE), 6), " * ", 
          round(se_z, 6), "\\( ) \\) ", "\\( = \\) ",
          "(", round(test_2meanz$conf.int[1], 6), ", ", round(test_2meanz$conf.int[2], 6), ")"
        ),
      input$alternative2=="greater" ~ 
        paste0(
          C.level * 100, "% Confidence Interval for \\(\\mu_1 - \\mu_2 = \\bigg(
          \\bar{x}_1 - \\bar{x}_2 - z_{\\alpha} \\sqrt{\\dfrac{\\sigma^2_1}{n_1} + 
          \\dfrac{\\sigma^2_2}{n_2}}, \\infty \\bigg)= \\) ", "(",round(x1bar, 6), 
          ifelse(x2bar >= 0, paste0(" - ", round(x2bar, 6)), 
                 paste0(" + ", round(abs(x2bar), 6))), "  \\( - \\)",  
          round(qnorm(input$alpha, lower.tail = FALSE), 6), " * ", 
          round(se_z, 6), "\\(, \\infty) \\) ", "\\( = \\) ",
          "(", round(test_2meanz$conf.int[1], 6), ",  ", 
          round(test_2meanz$conf.int[2], 6), ")"
        ),
      input$alternative2 == "two.sided" ~
        paste0(
          C.level * 100, "% Confidence Interval for \\(\\mu_1 - \\mu_2 = 
          \\bar{x}_1 - \\bar{x}_2 \\pm z_{\\alpha/2} \\sqrt{\\dfrac{\\sigma^2_1}{n_1} + 
          \\dfrac{\\sigma^2_2}{n_2}} = \\) ",round(x1bar, 6), 
          ifelse(x2bar >= 0, paste0(" - ", round(x2bar, 6)), 
                 paste0(" + ", round(abs(x2bar), 6))), "  \\( \\pm \\)", " \\( ( \\)", 
          round(qnorm(input$alpha / 2, lower.tail = FALSE), 6), " * ", 
          round(se_z, 6), "\\( ) \\) ", "\\( = \\) ",
          "(", round(test_2meanz$conf.int[1], 6), ", ", 
          round(test_2meanz$conf.int[2], 6), ")"
        )
    ), 
    br(),
    tags$b("Hypothesis test"),
    br(),
    paste0("1. \\(H_0 : \\mu_1 - \\mu_2  = \\) ", test_2meanz$null.value, " and \\(H_1 : \\mu_1 - \\mu_2 \\) ", 
           ifelse(input$alternative2 == "two.sided", "\\( \\neq \\) ", 
                  ifelse(input$alternative2 == "greater", "\\( > \\) ", "\\( < \\) ")), 
           test_2meanz$null.value),
    br(),
    paste0(
      "2. Test statistic : \\(z_{obs} = \\dfrac{(\\bar{x}_1 - \\bar{x}_2) - (\\mu_1 - \\mu_2)}{\\sqrt{\\dfrac{\\sigma^2_1}{n_1} + \\dfrac{\\sigma^2_2}{n_2}}} = \\) ",
      "(", round(x1bar, 3), ifelse(x2bar >= 0, paste0(" - ", round(x2bar, 3)), paste0(" + ", round(abs(x2bar), 3))), ifelse(input$h0 >= 0, paste0(" - ", input$h0), paste0(" + ", abs(input$h0))), ") / ", 
      round(se_z, 3), " \\( = \\) ",
      round(test_2meanz$statistic, 3)
    ),
    br(),
    paste0(
      "3. p-value : ", 
      case_when( 
        input$alternative2 == "less" ~ 
          paste0("P\\( (Z \\)  ",  "\\( \\leq  z_{obs} )  \\) = ", round(test_2meanz$p.value,6)),
        input$alternative2 == "greater" ~ 
          paste0("P\\( (Z \\)  ",  "\\( \\geq  z_{obs} )  \\) = ", round(test_2meanz$p.value,6)),
        input$alternative2 == "two.sided"~ 
          paste0("2*min ", "(P\\( (Z \\)  ",  "\\( \\leq  z_{obs} ),  \\) ",
                 "P\\( (Z \\) ",  "\\( \\geq  z_{obs} )  \\) ) =", round(test_2meanz$p.value,6))
      )
    ),
    br(),
    paste0("4. Conclusion : ", ifelse(test_2meanz$p.value <= input$alpha, "Reject \\(H_0\\)", "Do not reject \\(H_0\\)")),
    br(),
    tags$b("Interpretation"),
    br(),
    paste0("At the ", input$alpha * 100, "% significance level, ", 
           ifelse(test_2meanz$p.value <= input$alpha, 
                  "we reject the null hypothesis", "we do not reject the null hypothesis"), 
           " \\((p\\)-value ",  round(test_2meanz$p.value, 6), ")", ".")
    
    
  )
  }
})    
output$plot_twomeanz <- renderPlot({
  dat1 <- dat1()
  dat2 <- dat2()
  if ( is.null(dat1) | is.null(dat2) | any(is.na(dat1)) | length(dat1) < 3 | any(is.na(dat2)) | length(dat2) < 3 | any(is.na(input$sigma21_twomeanz), is.na(input$sigma22_twomeanz), is.na(input$h0)) | input$sigma21_twomeanz<0 | input$sigma22_twomeanz<0 ) 
  {return(NULL)} else{
  test_2meanz <- z.test(x = dat1, y=dat2,  mu = input$h0, sigma.x=sqrt(input$sigma21_twomeanz),
                        sigma.y=sqrt(input$sigma22_twomeanz),
                        alternative = input$alternative2, conf.level = 1 - input$alpha)  
  
  zstat2=test_2meanz$statistic  
  abszstat2=abs(zstat2)  
  range1=qnorm(0.9999, lower.tail = FALSE)   
  range2=qnorm(0.9999, lower.tail = TRUE)  
  p= ggplot(data.frame(x = c(range1, range2)), aes(x = x))
    if (input$alternative2 == "less") {
    p <-  p +
      geom_area(stat = "function", fun = dnorm, args = list(mean = 0, sd = 1), 
                xlim = c(range1,zstat2),fill="blue",alpha = 0.4) +
      geom_area(stat = "function", fun = dnorm,args = list(mean = 0, sd = 1), 
                xlim = c(zstat2, range2),fill="grey",alpha = 0.2) 
  } else  if (input$alternative2 == "greater"){
    p <- p +
      geom_area(stat = "function", fun = dnorm, args = list(mean = 0, sd = 1), 
                xlim = c(range1,zstat2),fill="grey",alpha = 0.2) +
      geom_area(stat = "function", fun = dnorm,args = list(mean = 0, sd = 1), 
                xlim = c(zstat2, range2),fill="blue",alpha = 0.4) 
  } else {
    p <- p +
      geom_area(stat = "function", fun = dnorm, args = list(mean = 0, sd = 1), 
                xlim = c(range1,-abszstat2),fill="blue",alpha = 0.4) +
      geom_area(stat = "function", fun = dnorm,args = list(mean = 0, sd = 1), 
                xlim = c(-abszstat2, abszstat2),fill="grey",alpha = 0.2) +
      geom_area(stat = "function", fun = dnorm,args = list(mean = 0, sd = 1), 
                xlim = c(abszstat2, range2),fill="blue",alpha = 0.4)+
      geom_vline(xintercept = -zstat2, color = "red")
  }
p <- p + theme_minimal()+
  geom_vline(xintercept = zstat2, color = "red") +
  geom_text(aes(x = zstat2, 
                label = paste0("Test statistic = ", round(test_2meanz$statistic, 6)), y = 0.2), 
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
interpolate3(~(dat1=x1), x1=as.numeric( unlist(strsplit((input$sample1),",")) )    )
interpolate3(~(dat2=x2), x2=as.numeric( unlist(strsplit((input$sample2),",")) )    )
if ( is.null(dat1) | is.null(dat2) | any(is.na(dat1)) | length(dat1) < 3 | any(is.na(dat2)) | length(dat2) < 3 | any(is.na(input$sigma21_twomeanz), is.na(input$sigma22_twomeanz), is.na(input$h0)) | input$sigma21_twomeanz<0 | input$sigma22_twomeanz<0 ) 
{     return(NULL)
}else{ 
metaExpr({
  test=z.test(x = dat1, y=dat2,  mu = ..(input$h0), sigma.x=sqrt(..(input$sigma21_twomeanz)),
          sigma.y=sqrt(..(input$sigma22_twomeanz)),
          alternative = ..(input$alternative2), conf.level=1-..(input$alpha))  
      CI= test$conf.int
      cat("Confidence interval", "of level", 1-..(input$alpha), "is :", CI)
      test  
})
}
}
  ) 
  
 
  output$theory_twomeant <- renderUI({ 
    dat1 <- dat1()
    dat2 <- dat2()
    n1=length(dat1)    
    n2=length(dat2)
    if ( is.null(dat1) | is.null(dat2) | any(is.na(dat1)) | n1 < 3 | any(is.na(dat2)) | n2 < 3) 
    {   "Invalid input or not enough observations"    }
    else if (is.na(input$h0)){
      "The mean value under the null hypothesis is missing."
    } else{
    x1bar=mean(dat1)   
    x2bar=mean(dat2)  
    x1var=var(dat1)    
    x2var=var(dat2)    
    
    C.level = 1 - (input$alpha)  
    test_twot<-t.test(x = dat1, y=dat2, mu = input$h0, alternative = input$alternative2, 
                       paired = FALSE, var.equal = input$var.equal, conf.level = C.level)  
    df2 = test_twot$parameter  
    se_t2=test_twot$stderr      
    s_p <- sqrt(((n1 - 1)*x1var + (n2 - 1)*x2var) / test_twot$parameter)
   
    
    if (input$inference == "TwoMeanT" & input$var.equal == FALSE)
    {
      withMathJax(
        
        tags$b("Confidence interval"),
        br(),
        case_when(     
          input$alternative2=="less" ~ 
            paste0(
              C.level * 100, "% Confidence Interval for \\(\\mu_1 - \\mu_2 = \\bigg( -\\infty,
          \\bar{x}_1 - \\bar{x}_2 + t_{\\alpha, \\nu} \\sqrt{\\dfrac{s^2_1}{n_1} + 
          \\dfrac{s^2_2}{n_2}} \\bigg) \\) = \\(", "( -\\infty, ",
              round(x1bar, 6), 
              ifelse(x2bar >= 0, paste0(" - ", round(x2bar, 6)), 
                     paste0(" + ", round(abs(x2bar), 6)) ), "+",  
              round(qt(input$alpha, df=df2, lower.tail = FALSE), 6), " * ", 
              round(se_t2, 6), ")", "\\ = \\) ",
              "(", round(test_twot$conf.int[1], 6), ",  ", 
              round(test_twot$conf.int[2], 6), ")"
            ),
          input$alternative2=="greater" ~ 
            paste0(
              C.level * 100, "% Confidence Interval for \\(\\mu_1 - \\mu_2 = \\bigg(
          \\bar{x}_1 - \\bar{x}_2 - t_{\\alpha, \\nu} \\sqrt{\\dfrac{s^2_1}{n_1} + 
          \\dfrac{s^2_2}{n_2}}, \\infty \\bigg) \\)  = ", "(", 
              round(x1bar, 6), 
              ifelse(x2bar >= 0, paste0(" - ", round(x2bar, 6)), 
                     paste0(" + ", round(abs(x2bar), 6))), "  \\( - \\)",  
              round(qt(input$alpha, df=df2, lower.tail = FALSE), 6), " * ", 
              round(se_t2, 6), "\\(, \\infty) \\) ", "\\( = \\) ",
              "(", round(test_twot$conf.int[1], 6), ",  ", 
              round(test_twot$conf.int[2], 6), ")"
            ),
          input$alternative2 == "two.sided" ~
            paste0(
              C.level * 100, "% Confidence Interval for \\(\\mu_1 - \\mu_2 = 
          \\bar{x}_1 - \\bar{x}_2 \\pm t_{\\alpha/2, \\nu} \\sqrt{\\dfrac{s^2_1}{n_1} + 
          \\dfrac{s^2_2}{n_2}} = \\) ",  "=", "(",
              round(x1bar, 6) , 
              paste0(" + ", round(abs(x2bar), 6)), "\\( \\pm \\)",  
              round(qt(input$alpha / 2, df=df2, lower.tail = FALSE), 6), " * ", 
              round(se_t2, 6), "\\( ) \\) ", "\\( = \\) ",
              "(", round(test_twot$conf.int[1], 6), ",  ", 
              round(test_twot$conf.int[2], 6), ")"
            )
        ),  
        br(),
        paste0("where ", "\\( \\nu = \\dfrac{\\Bigg(\\dfrac{s^2_1}{n_1} + 
                 \\dfrac{s^2_2}{n_2}\\Bigg)^2}{\\dfrac{\\Bigg(\\dfrac{s^2_1}{n_1}\\Bigg)^2}{n_1-1} 
                 + \\dfrac{\\Bigg(\\dfrac{s^2_2}{n_2}\\Bigg)^2}{n_2-1}} = \\) ", 
               round(test_twot$parameter, 2)),
        br(),
        tags$b("Hypothesis test"),
        br(),
        paste0("1. \\(H_0 : \\mu_1 - \\mu_2  = \\) ", test_twot$null.value, " and \\(H_1 : \\mu_1 - \\mu_2 \\) ", 
               ifelse(input$alternative2 == "two.sided", "\\( \\neq \\) ", 
                      ifelse(input$alternative2 == "greater", "\\( > \\) ", "\\( < \\) ")), 
               test_twot$null.value),
        br(),
        paste0(
          "2. Test statistic : \\(t_{obs} = \\dfrac{(\\bar{x}_1 - \\bar{x}_2) - 
        (\\mu_1 - \\mu_2)}{\\sqrt{\\dfrac{s^2_1}{n_1} + \\dfrac{s^2_2}{n_2}}} = \\) ",
          "(", round(x1bar, 6), ifelse(x2bar >= 0, paste0(" - ", round(x2bar, 6)), 
                                       paste0(" + ", round(abs(x2bar), 6))), 
          ifelse(input$h0 >= 0, paste0(" - ", input$h0), 
                 paste0(" + ", abs(input$h0))), ") / ",  round(se_t2, 6), " \\( = \\) ",
          round(test_twot$statistic, 6)
        ),
        br(),
        paste0(
          "3. p-value : ", 
          case_when( 
            input$alternative2 == "less" ~ 
              paste0("P\\( (T \\)  ",  "\\( \\leq  t_{obs} )  \\) = ", round(test_twot$p.value,6)),
            input$alternative2 == "greater" ~ 
              paste0("P\\( (T \\)  ",  "\\( \\geq  t_{obs} )  \\) = ", round(test_twot$p.value,6)),
            input$alternative2 == "two.sided"~ 
              paste0("2*min ", "(P\\( (T \\)  ",  "\\( \\leq  t_{obs} ),  \\) ",
                     "P\\( (T \\) ",  "\\( \\geq  t_{obs} )  \\) ) =", round(test_twot$p.value,6))
          )
        ),
        br(),
        paste0("4. Conclusion : ", ifelse(test_twot$p.value <= input$alpha, "Reject \\(H_0\\)", "Do not reject \\(H_0\\)")),
        br(),
        tags$b("Interpretation"),
        br(),
        paste0("At the ", input$alpha * 100, "% significance level, ", 
               ifelse(test_twot$p.value <= input$alpha, 
                      "we reject the null hypothesis", "we do not reject the null hypothesis"), 
               " \\((p\\)-value ",  round(test_twot$p.value, 6), ")", ".")
        
        
      ) 
      
    } else if (input$inference == "TwoMeanT" & input$var.equal == TRUE)
    {
      withMathJax(
        
        tags$b("Confidence interval"),
        br(),
        case_when(     
          input$alternative2=="less" ~ 
            paste0(
              C.level * 100, "% Confidence Interval for \\(\\mu_1 - \\mu_2 = \\bigg( -\\infty,
          \\bar{x}_1 - \\bar{x}_2 + t_{\\alpha, n_1+n_2-2} \\cdot s_p \\sqrt{\\dfrac{1}{n_1} + 
          \\dfrac{1}{n_2}} \\bigg) \\) = \\(", "( -\\infty, ",
              round(x1bar, 6), 
              ifelse(x2bar >= 0, paste0(" - ", round(x2bar, 6)), 
                     paste0(" + ", round(abs(x2bar), 6)) ), "+",  
              round(qt(input$alpha, df=df2, lower.tail = FALSE), 6), " * ", 
              round(se_t2, 6), ")", "\\ = \\) ",
              "(", round(test_twot$conf.int[1], 6), ",  ", 
              round(test_twot$conf.int[2], 6), ")"
            ),
          input$alternative2=="greater" ~ 
            paste0(
              C.level * 100, "% Confidence Interval for \\(\\mu_1 - \\mu_2 = \\bigg(
          \\bar{x}_1 - \\bar{x}_2 - t_{\\alpha, n_1+n_2-2} \\cdot s_p \\sqrt{\\dfrac{1}{n_1} + 
          \\dfrac{1}{n_2}}, \\infty \\bigg) \\)  = ", "(", 
              round(x1bar, 6), 
              ifelse(x2bar >= 0, paste0(" - ", round(x2bar, 6)), 
                     paste0(" + ", round(abs(x2bar), 6))), "  \\( - \\)",  
              round(qt(input$alpha, df=df2, lower.tail = FALSE), 6), " * ", 
              round(se_t2, 6), "\\(, \\infty) \\) ", "\\( = \\) ",
              "(", round(test_twot$conf.int[1], 6), ",  ", 
              round(test_twot$conf.int[2], 6), ")"
            ),
          input$alternative2 == "two.sided" ~
            paste0(
              C.level * 100, "% Confidence Interval for \\(\\mu_1 - \\mu_2 = 
          \\bar{x}_1 - \\bar{x}_2 \\pm t_{\\alpha/2, n_1+n_2-2} \\cdot s_p \\sqrt{\\dfrac{1}{n_1} + 
          \\dfrac{1}{n_2}} = \\) ",  "=", "(",
              round(x1bar, 6) , 
              paste0(" + ", round(abs(x2bar), 6)), "\\( \\pm \\)",  
              round(qt(input$alpha / 2, df=df2, lower.tail = FALSE), 6), " * ", 
              round(se_t2, 6), "\\( ) \\) ", "\\( = \\) ",
              "(", round(test_twot$conf.int[1], 6), ",  ", 
              round(test_twot$conf.int[2], 6), ")"
            )
        ),  
        br(),
        paste0("where ", "\\( s_p = \\sqrt{\\dfrac{(n_1 - 1)s^2_1 + (n_2 - 1)s^2_2}{n_1 + n_2 - 2}} = \\) ", 
               round(s_p, 6)),
        br(),     
        tags$b("Hypothesis test"),
        br(),
        paste0("1. \\(H_0 : \\mu_1 - \\mu_2  = \\) ", test_twot$null.value, " and \\(H_1 : \\mu_1 - \\mu_2 \\) ", 
               ifelse(input$alternative2 == "two.sided", "\\( \\neq \\) ", 
                      ifelse(input$alternative2 == "greater", "\\( > \\) ", "\\( < \\) ")), 
               test_twot$null.value),
        br(),
        paste0(
          "2. Test statistic : \\(t_{obs} = \\dfrac{(\\bar{x}_1 - \\bar{x}_2) - (\\mu_1 - \\mu_2)}{\\sqrt{\\dfrac{s^2_1}{n_1} + \\dfrac{s^2_2}{n_2}}} = \\) ",
          "(", round(x1bar, 6), ifelse(x2bar >= 0, paste0(" - ", round(x2bar, 6)), paste0(" + ", round(abs(x2bar), 6))), ifelse(input$h0 >= 0, paste0(" - ", input$h0), paste0(" + ", abs(input$h0))), ") / ",  round(se_t2, 6), " \\( = \\) ",
          round(test_twot$statistic, 6)
        ),
        br(),
        paste0(
          "3. p-value : ", 
          case_when( 
            input$alternative2 == "less" ~ 
              paste0("P\\( (T \\)  ",  "\\( \\leq  t_{obs} )  \\) = ", round(test_twot$p.value,6)),
            input$alternative2 == "greater" ~ 
              paste0("P\\( (T \\)  ",  "\\( \\geq  t_{obs} )  \\) = ", round(test_twot$p.value,6)),
            input$alternative2 == "two.sided"~ 
              paste0("2*min ", "(P\\( (T \\)  ",  "\\( \\leq  t_{obs} ),  \\) ",
                     "P\\( (T \\) ",  "\\( \\geq  t_{obs} )  \\) ) =", round(test_twot$p.value,6))
          )
        ),
        br(),
        paste0("4. Conclusion : ", ifelse(test_twot$p.value <= input$alpha, "Reject \\(H_0\\)", "Do not reject \\(H_0\\)")),
        br(),
        tags$b("Interpretation"),
        br(),
        paste0("At the ", input$alpha * 100, "% significance level, ", 
               ifelse(test_twot$p.value <= input$alpha, 
                      "we reject the null hypothesis", "we do not reject the null hypothesis"), 
               " \\((p\\)-value ",  round(test_twot$p.value, 6), ")", ".")
      
      )  
    }
    }
  })   
  
output$plot_twomeant <- renderPlot({
    dat1 <- dat1()
    dat2 <- dat2()
    if ( is.null(dat1) | is.null(dat2) | any(is.na(dat1)) | length(dat1) < 3 | any(is.na(dat2)) | length(dat2) < 3 | is.na(input$h0) ) 
    {return(NULL)} else{
    test_twot <- t.test(x = dat1, y=dat2,  mu = input$h0, alternative = input$alternative2, 
                         paired = FALSE, var.equal = input$var.equal, conf.level = 1 - input$alpha)  
    df2 = round(test_twot$parameter,2)
    t1stat=test_twot$statistic  
    abst1stat=abs(t1stat)  
    range1=qt(0.9999, df=df2, lower.tail = FALSE)   
    range2=qt(0.9999, df=df2, lower.tail = TRUE)  
    p= ggplot(data.frame(x = c(range1, range2)), aes(x = x))
    if (input$alternative2 == "less") {
      p <- p +
        geom_area(stat = "function", fun = dt, args = list(df=df2), 
                  xlim = c(range1,t1stat),fill="blue",alpha = 0.4) +
        geom_area(stat = "function", fun = dt,args = list(df=df2), 
                  xlim = c(t1stat, range2),fill="grey",alpha = 0.2) 
    } else if (input$alternative2 == "greater"){
      p <- p +
        geom_area(stat = "function", fun = dt, args = list(df=df2), 
                  xlim = c(range1,t1stat),fill="grey",alpha = 0.2) +
        geom_area(stat = "function", fun = dt,args = list(df=df2), 
                  xlim = c(t1stat, range2),fill="blue",alpha = 0.4) 
    } else {
      p <- p +
        geom_area(stat = "function", fun = dt, args = list(df=df2), 
                  xlim = c(range1,-abst1stat),fill="blue",alpha = 0.4) +
        geom_area(stat = "function", fun = dt,args = list(df=df2), 
                  xlim = c(-abst1stat, abst1stat),fill="grey",alpha = 0.2) +
        geom_area(stat = "function", fun = dt,args = list(df=df2), 
                  xlim = c(abst1stat, range2),fill="blue",alpha = 0.4)+
        geom_vline(xintercept = -t1stat, color = "red")
    }
p<-p+theme_minimal()+
  geom_vline(xintercept = t1stat, color = "red") +
  geom_text(aes(x = t1stat, 
                label = paste0("Test statistic = ", round(test_twot$statistic, 6)), y = 0.2), 
            colour = "red", angle = 90, vjust = 1.3, text = element_text(size = 11)) +
  ggtitle(paste0("Student", " t(",  df2, ")",
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
interpolate3(~(dat1=x1), x1=as.numeric( unlist(strsplit((input$sample1),",")) )    )
interpolate3(~(dat2=x2), x2=as.numeric( unlist(strsplit((input$sample2),",")) )    )
if ( is.null(dat1) | is.null(dat2) | any(is.na(dat1)) | length(dat1) < 3 | any(is.na(dat2)) | length(dat2) < 3 | is.na(input$h0) ) 
{    return(NULL)
}else{
metaExpr({
      test=t.test(x = dat1, y=dat2,  mu = ..(input$h0), 
                  alternative = ..(input$alternative2), 
                  paired = FALSE, var.equal = ..(input$var.equal),
                  conf.level=1-..(input$alpha))  
      CI= test$conf.int
      cat("Confidence interval", "of level", 1-..(input$alpha), "is :", CI)
      test   
})
} 
}
  ) 
  
 
  output$theory_twomeans <- renderUI({ 
    dat1 <- dat1()
    dat2 <- dat2() 
    n1=length(dat1)  
    n2=length(dat2)
    if ( is.null(dat1) | is.null(dat2) | any(is.na(dat1)) | length(dat1) < 3 | any(is.na(dat2)) | length(dat2) < 3) 
      {
      "Invalid input or not enough observations."
    } else if (is.na(input$h0))
    { "Please enter the mean value under the null hypothesis." } else if ( n1 != n2) {
      "Number of observations must be equal in the two samples."
    }else{
    dat=dat1-dat2  
    dbar=mean(dat)   
    dvar=var(dat1-dat2)    
    C.level = 1 - (input$alpha)     
    test_p<-t.test(x = dat1, y=dat2, mu = input$h0, alternative = input$alternative3, 
                   paired = TRUE,  conf.level = C.level)  
    df3 = test_p$parameter  
    se_p=test_p$stderr      
    
    withMathJax(
      
      tags$b("Confidence interval"),
      br(),
      case_when(  
        input$alternative3=="less" ~ 
          paste0(
            C.level * 100, "% CI for \\(\\mu = \\bigg( -\\infty, \\bar{d} + \\)", 
            "\\( t_{",  
            input$alpha,
            ", n-1}",
            " \\dfrac{s_d}{\\sqrt{n}} \\bigg)= \\) ",
            "\\( ( -\\infty,  \\) ", mean(dat),"\\( + \\)", 
            round(qt(input$alpha, df = test_p$parameter, lower.tail = FALSE), 6), 
            " * ", round(test_p$stderr * sqrt(length(dat)), 6), " / ", 
            round(sqrt(length(dat)), 6), "\\( ) \\) ", "\\( = \\) ",
            "(", round(test_p$conf.int[1], 6), ",  ", 
            round(test_p$conf.int[2], 6), ")"
          ),
        input$alternative3=="greater" ~ 
          paste0(
            C.level * 100, "% CI for \\(\\mu = \\bigg( \\bar{d} - \\)", 
            "\\( t_{",  
            input$alpha,
            ", n-1}",
            " \\dfrac{s_d}{\\sqrt{n}} ,  \\infty \\bigg)  = \\) ",
            "\\( (   \\) ", mean(dat), "\\( - \\)",
            round(qt(input$alpha, df = test_p$parameter, lower.tail = FALSE), 6), 
            " * ", round(test_p$stderr * sqrt(length(dat)), 6), " / ", 
            round(sqrt(length(dat)), 6), ",  ",
            "\\(  \\infty)  \\) ",  "\\( = \\) ",
            "(", round(test_p$conf.int[1], 6), ",  ", 
            round(test_p$conf.int[2], 6), ")"
          ),
        input$alternative3 == "two.sided" ~
          paste0(
            C.level * 100, "% CI for \\(\\mu = \\bar{d} \\pm \\)", 
            "\\( t_{",  
            input$alpha/2,
            ", n-1}",
            " \\dfrac{s_d}{\\sqrt{n}} = \\) ",
            round(test_p$estimate, 6), "  \\( \\pm \\) ", "\\( ( \\)", 
            round(qt(input$alpha / 2, df = test_p$parameter, lower.tail = FALSE), 6), 
            " * ", round(test_p$stderr * sqrt(length(dat)), 6), " / ", 
            round(sqrt(length(dat)), 6), "\\( ) \\) ", "\\( = \\) ",
            "(", round(test_p$conf.int[1], 6), ",  ", 
            round(test_p$conf.int[2], 6), ")"
          )
      ),  
      br(),
      tags$b("Hypothesis test"),
      br(),
      paste0("1. \\(H_0 : \\mu_d = \\) ", test_p$null.value, " and \\(H_1 : \\mu_d \\) ", 
             ifelse(input$alternative3 == "two.sided", "\\( \\neq \\) ", 
                    ifelse(input$alternative3 == "greater", "\\( > \\) ", "\\( < \\) ")), 
             test_p$null.value),
      br(),
      paste0(
        "2. Test statistic : \\(t_{obs} = \\dfrac{\\bar{d} - \\mu_0}{s_d / \\sqrt{n}} = \\) ",
        "(", round(test_p$estimate, 6), ifelse(test_p$null.value >= 0, 
                                               paste0(" - ", test_p$null.value), paste0(" + ", abs(test_p$null.value))), ") / ", round(test_p$stderr, 6), " \\( = \\) ",
        round(test_p$statistic, 6)
      ),
      br(),
      paste0(
        "3. p-value : ", 
        case_when( 
          input$alternative3 == "less" ~ 
            paste0("P\\( (T \\)  ",  "\\( \\leq  t_{obs} )  \\) = ", round(test_p$p.value,6)),
          input$alternative3 == "greater" ~ 
            paste0("P\\( (T \\)  ",  "\\( \\geq  t_{obs} )  \\) = ", round(test_p$p.value,6)),
          input$alternative3 == "two.sided"~ 
            paste0("2*min ", "(P\\( (T \\)  ",  "\\( \\leq  t_{obs} ),  \\) ",
                   "P\\( (T \\) ",  "\\( \\geq  t_{obs} )  \\) ) =", round(test_p$p.value,6))
        )
      ),
      br(),
      paste0("4. Conclusion : ", ifelse(test_p$p.value <= input$alpha, "Reject \\(H_0\\)", "Do not reject \\(H_0\\)")),
      br(),
      tags$b("Interpretation"),
      br(),
      paste0("At the ", input$alpha * 100, "% significance level, ", 
             ifelse(test_p$p.value <= input$alpha, 
                    "we reject the null hypothesis", "we do not reject the null hypothesis"), 
             " \\((p\\)-value ",  round(test_p$p.value, 6), ")", ".")
      
    ) 
    }
  })   
  
  output$plot_twomeans <- renderPlot({
    dat1 <- dat1()
    dat2 <- dat2() 
    if ( is.null(dat1) | is.null(dat2) | any(is.na(dat1)) | length(dat1) < 3 | any(is.na(dat2)) | length(dat2) < 3 | is.na(input$h0)) 
    {return(NULL)} else{
    test_p<-t.test(x = dat1, y=dat2, mu = input$h0, alternative = input$alternative3, 
                   paired = TRUE,  1 - input$alpha)  
    df3=test_p$parameter  
    tstat_p=test_p$statistic  
    abststat_p=abs(tstat_p)  
    range1=qt(0.9999, df = test_p$parameter, lower.tail = FALSE)   
    range2=qt(0.9999, df = test_p$parameter, lower.tail = TRUE)  
    p = ggplot(data.frame(x = c(range1, range2)), aes(x = x))
    if (input$alternative3 == "less") {
      p <-  p +
        geom_area(stat = "function", fun = dt, args = list(df = df3), 
                  xlim = c(range1,tstat_p),fill="blue",alpha = 0.4) +
        geom_area(stat = "function", fun = dt,args = list(df = df3), 
                  xlim = c(tstat_p, range2),fill="grey",alpha = 0.2) 
    } else if (input$alternative3 == "greater"){
      p <- p +
        geom_area(stat = "function", fun = dt, args = list(df = df3), 
                  xlim = c(range1,tstat_p),fill="grey",alpha = 0.2) +
        geom_area(stat = "function", fun = dt,args = list(df = df3), 
                  xlim = c(tstat_p, range2),fill="blue",alpha = 0.4) 
    }else{
      p <- p +
        geom_area(stat = "function", fun = dt, args = list(df = df3), 
                  xlim = c(range1,-abststat_p),fill="blue",alpha = 0.4) +
        geom_area(stat = "function", fun = dt,args = list(df = df3), 
                  xlim = c(-abststat_p, abststat_p),fill="grey",alpha = 0.2) +
        geom_area(stat = "function", fun = dt,args = list(df = df3), 
                  xlim = c(abststat_p, range2),fill="blue",alpha = 0.4)+
        geom_vline(xintercept = -tstat_p, color = "red")
      }
p<-p+theme_minimal()+
  geom_vline(xintercept = tstat_p, color = "red") +
  geom_text(aes(x = tstat_p, 
                label = paste0("Test statistic = ", round(test_p$statistic, 6)), y = 0.2), 
            colour = "red", angle = 90, vjust = 1.3, text = element_text(size = 11)) +
  ggtitle(paste0("Student distribution", " t(", round(test_p$parameter, 6), ")",
                 ", ",  "p-value is indicated by the shaded area")) +
  theme(plot.title = element_text(face = "bold", hjust = 0.5)) +
  ylab("Density") +
  xlab("x")
    p
    }
  })
  

 
  output$results_twomeans<- metaRender2( 
    renderPrint,
    { 
interpolate3(~(dat1=x1), x1=as.numeric( unlist(strsplit((input$sample1),",")) )    )
interpolate3(~(dat2=x2), x2=as.numeric( unlist(strsplit((input$sample2),",")) )    )
if ( is.null(dat1) | is.null(dat2) | any(is.na(dat1)) | length(dat1) < 3 | any(is.na(dat2)) | length(dat2) < 3 | is.na(input$h0)) 
{     return(NULL)
}else{
metaExpr({
      dif = dat1 - dat2
      cat("Difference data:", dif)
      cat("\n")
      test= t.test(x = dat1, y=dat2, mu = ..(input$h0), alternative = ..(input$alternative3), 
                   paired = TRUE,  1 - input$alpha)
      CI= test$conf.int
      cat("Confidence interval", "of level", 1-..(input$alpha), "is :", CI)
      test    
      })
} 
}
  ) 
  

  
  output$theory_onevariance <- renderUI({
    dat <- dat() 
    n = length(dat)
    if (is.null(dat) | any(is.na(dat)) | n < 3) {
      paste("Invalid input or not enough observations")
    } else if (is.na(input$h0) | input$h0<0)
    { "The population variance under the null hypothesis is missing or negative!" }
    else{
    C.level = 1 - (input$alpha)  
    df = n-1  
    test <- varTest(x = dat, alternative = input$alternative4, 
                    sigma.squared = input$h0,  conf.level = C.level)  
    varx=test$estimate    
    
withMathJax(
    
    tags$b("Confidence interval"),
    br(),
    case_when(  
      input$alternative4=="less" ~   paste0(
        C.level * 100, "% CI for \\(\\sigma^2 = \\Bigg( 0, \\dfrac{(n-1)s^2}{\\chi^2_{1-\\alpha, n-1}} \\Bigg) = \\) ",
        "( 0, ",   round((n - 1) * varx, 6), " / ", 
        round(qchisq(input$alpha, df = df, lower.tail = TRUE), 6), ") = ",
        "(", round(test$conf.int[1], 6), ",  ", 
        round(test$conf.int[2], 6), ")"
      ),
      input$alternative4=="greater" ~ paste0(
        C.level * 100, "% CI for \\( \\sigma^2 = \\Bigg( \\dfrac{(n-1)s^2}{\\chi^2_{\\alpha, n-1}}\\),   \\( \\infty  \\Bigg) \\)  = ",
        "(", round((n - 1) * varx, 6), " / ", round(qchisq(input$alpha, 
                      df = df, lower.tail = FALSE), 6), ",  ",  "\\( \\infty ) \\) = ",
        "(", round(test$conf.int[1], 6), ",  ", 
        round(test$conf.int[2], 6), ")"
      ),
      input$alternative4 == "two.sided" ~ paste0(
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
    paste0("1. \\(H_0 : \\sigma^2 = \\) ", test$null.value, " and \\(H_1 : \\sigma^2 \\) ", ifelse(input$alternative4 == "two.sided", "\\( \\neq \\) ", ifelse(input$alternative4 == "greater", "\\( > \\) ", "\\( < \\) ")), test$null.value),
    br(),
    paste0(
      "2. Test statistic : \\(\\chi^2_{obs} = \\dfrac{(n-1)s^2}{\\sigma^2_0} = \\) ",
      "[(", n, " - 1) * ", round(test$estimate, 6), "] / ", test$null.value, " \\( = \\) ",
      round(test$statistic, 6)
    ),
    br(),
    paste0(
      "3. p-value : ", 
      case_when( 
        input$alternative4 == "less" ~ 
          paste0("P\\( (\\chi^2 \\)  ",  "\\( \\leq  \\chi^2_{obs} )  \\) = ", round(test$p.value,6)),
        input$alternative4 == "greater" ~ 
          paste0("P\\( (\\chi^2 \\)  ",  "\\( \\geq  \\chi^2_{obs} )  \\) = ", round(test$p.value,6)),
        input$alternative4 == "two.sided"~ 
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
    dat <- dat()   
    if (is.null(dat) | any(is.na(dat)) | length(dat) < 3 | is.na(input$h0) | input$h0<0) {
      return(NULL)
    }else{
    C.level = 1 - (input$alpha)  
    test <- varTest(x = dat, alternative = input$alternative4, 
                    sigma.squared = input$h0,  conf.level = C.level)  
    df=test$parameter  
    stat=test$statistic  
    range1=qchisq(0.9999, df = df, lower.tail = FALSE)   
    range2=qchisq(0.9999, df = df, lower.tail = TRUE)  
    p = ggplot(data.frame(x = c(range1, range2)), aes(x = x))
    if (input$alternative4 == "less") {
      p <- p +
        geom_area(stat = "function", fun = dchisq, args = list(df = df), 
                  xlim = c(range1,stat),fill="blue",alpha = 0.4) +
        geom_area(stat = "function", fun = dchisq,args = list(df = df), 
                  xlim = c(stat, range2),fill="grey",alpha = 0.2) 
    } else if (input$alternative4 == "greater"){
      p <- p +
        geom_area(stat = "function", fun = dchisq, args = list(df = df), 
                  xlim = c(range1,stat),fill="grey",alpha = 0.2) +
        geom_area(stat = "function", fun = dchisq,args = list(df = df), 
                  xlim = c(stat, range2),fill="blue",alpha = 0.4) 
    }else {
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
interpolate3(~(dat=x0),x0=as.numeric( unlist(strsplit((input$onesample),",")) ) ) 
      if (is.null(dat) | any(is.na(dat)) | length(dat) < 3 | is.na(input$h0) | input$h0<0) {
        return(NULL)
      }else{
      metaExpr({                   
      test <- varTest(x = dat, alternative = ..(input$alternative4), 
                      sigma.squared = ..(input$h0),  conf.level = 1-..(input$alpha))
      CI= test$conf.int
      cat("Confidence interval", "of level", 1-..(input$alpha), "is :", CI)
      test  
             })
      }   
    }
  ) 
 

  output$theory_twovariance <- renderUI({ 
    dat1 <- dat1()
    dat2 <- dat2()
    n1=length(dat1)     
    n2=length(dat2)
    if ( is.null(dat1) | is.null(dat2) | any(is.na(dat1)) | n1 < 3 | any(is.na(dat2)) | n2 < 3) 
    { "Invalid input or not enough observations." } 
    else{
    C.level = 1 - (input$alpha)  
    df1=n1-1            
    df2=n2-1  
    varx1=var(dat1)     
    varx2=var(dat2)  
    test <- var.test(x = dat1, y = dat2, ratio = 1, 
              alternative = input$alternative5, conf.level = C.level)  
    ptest=test$estimate    
  
withMathJax(
    tags$b("Confidence interval"),
    br(),
    case_when(  
  input$alternative5=="less" ~ paste0(
    C.level * 100, "% CI for \\( \\dfrac{\\sigma^2_1}{\\sigma^2_2} = 
    \\Bigg( 0,
    \\dfrac{s^2_1}{s^2_2}F_{\\alpha, n_2 - 1, n_1-1} \\Bigg) = \\) ",
    "\\( \\big( \\)", 0,  ", ", round(ptest, 6), " * ", 
    round(qf(input$alpha, df1 = df2, df2 = df1, lower.tail = FALSE), 6), 
    "\\( \\big) = \\) ",
    "(", round(test$conf.int[1], 6), ", ", round(test$conf.int[2], 6), ")"
  ),
  input$alternative5=="greater" ~  paste0(
    C.level * 100, "% CI for \\( \\dfrac{\\sigma^2_1}{\\sigma^2_2} = 
    \\Bigg( \\dfrac{s^2_1}{s^2_2}\\dfrac{1}{F_{\\alpha, n_1 - 1, n_2-1}},
    \\infty \\Bigg) = \\) ",
    "\\( \\big( \\)", round(ptest, 6), 
    " * (1 / ", round(qf(input$alpha, df1 = df1, df2 = df2, 
                         lower.tail = FALSE), 6), "), ",  
    "\\( \\infty \\big) = \\) ",
    "(", round(test$conf.int[1], 6), ", ", round(test$conf.int[2], 6), ")"
  ) ,
  input$alternative5=="two.sided" ~ paste0(
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
  if (test$alternative == "two.sided") {
    withMathJax(
      paste0("1. \\(H_0 : \\sigma^2_1 = \\sigma^2_2 \\) and \\(H_1 : \\sigma^2_1 \\neq \\sigma^2_2 \\) ")
    )
  } else if (test$alternative == "greater") {
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
      input$alternative5 == "less" ~ 
        paste0("P\\( ( F \\)  ",  "\\( \\leq  F_{obs} )  \\) = ", round(test$p.value,6)),
      input$alternative5 == "greater" ~ 
        paste0("P\\( (F \\)  ",  "\\( \\geq  F_{obs} )  \\) = ", round(test$p.value,6)),
      input$alternative5 == "two.sided"~ 
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
    dat1 <- dat1()
    dat2 <- dat2() 
    n1=length(dat1)     
    n2=length(dat2)
    if ( is.null(dat1) | is.null(dat2) | any(is.na(dat1)) | n1 < 3 | any(is.na(dat2)) | n2 < 3) 
    {return(NULL)} else{  
    C.level = 1 - (input$alpha)  
    df1=n1-1            
    df2=n2-1  
test <-var.test(x = dat1, y = dat2, ratio = 1, 
                    alternative = input$alternative5, conf.level = C.level)  
stat=test$statistic  
    range1=qf(0.99, df1=df1, df2=df2, lower.tail = FALSE)   
    range2=qf(0.99, df1=df1, df2=df2, lower.tail = TRUE)  
    p = ggplot(data.frame(x = c(range1, range2)), aes(x = x))
    if (input$alternative5 == "less") {
      p <- p +
        geom_area(stat = "function", fun = df, args = list(df1=df1, df2=df2), 
                  xlim = c(range1,stat),fill="blue",alpha = 0.4) +
        geom_area(stat = "function", fun = df,args = list(df1=df1, df2=df2), 
                  xlim = c(stat, range2),fill="grey",alpha = 0.2) 
    } else if (input$alternative5 == "greater"){
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
interpolate3(~(dat1=x1), x1=as.numeric( unlist(strsplit((input$sample1),",")) )    )
interpolate3(~(dat2=x2), x2=as.numeric( unlist(strsplit((input$sample2),",")) )    )
if ( is.null(dat1) | is.null(dat2) | any(is.na(dat1)) | length(dat1) < 3 | any(is.na(dat2)) | length(dat2) < 3) 
{  return(NULL) }else{
metaExpr({
      test <- var.test(x = dat1, y = dat2, ratio = 1,
                        alternative = ..(input$alternative5), 
                        conf.level = 1-..(input$alpha))
      CI= test$conf.int
      cat("Confidence interval", "of level", 1-..(input$alpha), "is :", CI)
      test  
      })
}  
}
  )   
    

output$code_onemeanz <- renderPrint( expandChain(quote(library(BSDA)),output$results_onemeanz()) )    
output$code_onemeant <- renderPrint( expandChain(output$results_onemeant()) )    
output$code_twomeanz <- renderPrint( expandChain(quote(library(BSDA)),output$results_twomeanz()) )    
output$code_twomeant <- renderPrint( expandChain(output$results_twomeant()) )    
output$code_twomeans <- renderPrint( expandChain(output$results_twomeans()) )    
output$code_onevariance <- renderPrint( expandChain(quote(library(EnvStats)),output$results_onevariance()) )    
output$code_twovariance <- renderPrint( expandChain(output$results_twovariance()) )    

  }
)
}
  
