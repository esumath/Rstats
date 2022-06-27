# This file defines the module UI and Server functions
#' Probability_Calculation UI Function
#' Probability calculations for Finite, Binomial, Poisson  
#' Normal, t, Chi-squared, and F distributions
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 

mod_Probability_Calculation_ui <- function(id){
  ns <- NS(id)   
  
  fluidPage( 
    
    waiter::use_waiter(),
    waiter::waiter_hide_on_render(ns("normalPlot")),
    
    
    withMathJax(),
    sidebarLayout(
      
      sidebarPanel(          
        
        titlePanel("Probability Calculation:"),
        
        selectInput(
          inputId = ns("distribution"),
          label = "Choose the distribution of \\(X\\):",
          choices = c( "Finite",
                       "Binomial", "Poisson",
                       "Normal","t", "Chi-square", "F"),
          multiple = FALSE,
          selected = "Normal"
        ),
    conditionalPanel(
          condition = "input.distribution == 'Finite'",  
          textInput(ns("values"), "Distinct values of \\( X \\):", value = "3, 5, 4, 1, 2", placeholder = "Enter values separated by a comma, e.g. 4.2, 4.4, 5, 5.03, etc."),
          textInput(ns("weights"), "Weights (probabilities or frequencies) \\( w \\):", value = "0.1, 0.2, 0.4, 0.2, 0.1", placeholder = "Enter values separated by a comma, e.g. 4, 6, 7, 9, etc."),
          ns=ns
        ),
        conditionalPanel(
          condition = "input.distribution == 'Binomial'",
          numericInput(ns("n_binomial"), "Number of trials \\(n\\):",
                       value = 12, min = 0, step = 1
          ),
          numericInput(ns("p_binomial"), "Probability of success \\(p\\):",
                       value = 0.5, min = 0, max = 1, step = 0.1
          ),ns=ns
        ),   
        conditionalPanel(
          condition = "input.distribution == 'Poisson'",
          numericInput(ns("lambda_poisson"), "Mean or rate \\(\\lambda\\):",
                       value = 5, min = 0, step = 0.1
          ),ns=ns
        ),    
        conditionalPanel(
          condition = "input.distribution == 'Normal'",
          numericInput(ns("mean_normal"), "Mean \\(\\mu\\):",
                       value = 0,  step = 1
          ),
          numericInput(ns("sd_normal"), "Standard deviation \\(\\sigma\\):",
                       value = 1, min = 0, step = 1
          ),ns=ns
        ),    
        conditionalPanel(
          condition = "input.distribution == 't'",
          numericInput(ns("df_t"), "Degrees of freedom \\(df\\):",
                       value = 11, min = 1, step = 1
          ),ns=ns
        ),   
        conditionalPanel(
          condition = "input.distribution == 'Chi-square'",
          numericInput(ns("df_chisquare"), "Degrees of freedom \\(df\\):",
                       value = 11, min = 1, step = 1
          ),ns=ns
        ),    
        conditionalPanel(
          condition = "input.distribution == 'F'",
          numericInput(ns("df1_F"), "Numerator Degrees of freedom \\(df1\\):",
                       value = 11, min = 1, step = 1
          ),
          numericInput(ns("df2_F"), "Denominator Degrees of freedom \\(df2\\):",
                       value = 20, min = 1, step = 1
          ),ns=ns
        ),   
    conditionalPanel(
          condition = "input.distribution == 'Binomial' | input.distribution == 'Poisson'",     
          radioButtons(
            inputId = ns("lower_tail0"),
            label = "Tail:",
            choices = c(
              "Point : \\(P(X = x)\\)" = "point0",
              "Lower tail : \\(P(X \\leq x)\\)" = "lower.tail0",
              "Upper tail : \\(P(X > x)\\)" = "upper.tail0",
              "Interval : \\(P(a \\leq X \\leq b)\\)" = "interval0"
            )
          ),ns=ns
        ),  
    conditionalPanel(
          condition = "input.distribution != 'Binomial' & input.distribution != 'Poisson'",
          radioButtons(
            inputId = ns("lower_tail"),
            label = "Tail:",
            choices = c(
              "Lower tail : \\(P(X \\leq x)\\)" = "lower.tail",
              "Upper tail : \\(P(X > x)\\)" = "upper.tail",
              "Interval : \\(P(a \\leq X \\leq b)\\)" = "interval"
            )
          ), ns=ns
        ), 
        conditionalPanel(
          condition = "(input.distribution == 'Finite' | input.distribution == 't' | input.distribution == 'Normal') & input.lower_tail == 'lower.tail'",
          numericInput(ns("x1_dist1"), "\\( x: \\)",
                       value = 2,  step = 1
          ),ns=ns
        ),
        conditionalPanel(
          condition = "(input.distribution == 'Finite' | input.distribution == 't' | input.distribution == 'Normal') & input.lower_tail == 'upper.tail'",
          numericInput(ns("x2_dist1"), "\\( x: \\)",
                       value = 2,  step = 1
          ),ns=ns
        ),
        conditionalPanel(
          condition = "(input.distribution == 'Finite' | input.distribution == 't' | input.distribution == 'Normal') & input.lower_tail == 'interval'",
          numericInput(ns("a_dist1"), "\\( a: \\)",
                       value = 2,  step = 1
          ),
          numericInput(ns("b_dist1"), "\\(b:  (a \\leq b) \\)",
                       value = 3,  step = 1
          ), ns=ns
        ),   
        conditionalPanel(
          condition = "(input.distribution == 'Binomial' | input.distribution == 'Poisson') & input.lower_tail0 == 'point0'",
          numericInput(ns("x0_dist2"), "\\( x: \\)",
                       value = 5, min = 0, step = 1
          ),ns=ns
        ),
        conditionalPanel(
          condition = "(input.distribution == 'Binomial' | input.distribution == 'Poisson') & input.lower_tail0 == 'lower.tail0'",
          numericInput(ns("x1_dist2"), "\\( x: \\)",
                       value = 5, min = 0, step = 1
          ),ns=ns
        ),
        conditionalPanel(
          condition = "(input.distribution == 'Binomial' | input.distribution == 'Poisson') & input.lower_tail0 == 'upper.tail0'",
          numericInput(ns("x2_dist2"), "\\( x: \\)",
                       value = 5, min = 0, step = 1
          ),ns=ns
        ),
        conditionalPanel(
          condition = "(input.distribution == 'Binomial' | input.distribution == 'Poisson') & input.lower_tail0 == 'interval0'",
          numericInput(ns("a_dist2"), "\\( a: \\)",
                       value = 5, min = 0, step = 1
          ),
          numericInput(ns("b_dist2"), "\\(b:  (a \\leq b) \\)",
                       value = 8, min = 0, step = 1
          ), ns=ns
        ),
        conditionalPanel(
          condition = "(input.distribution == 'Chi-square' | input.distribution == 'F') & input.lower_tail == 'lower.tail'",
          numericInput(ns("x1_dist3"), "\\( x: \\)",
                       value = 5, min = 0, step = 1
          ),ns=ns
        ),
        conditionalPanel(
          condition = "(input.distribution == 'Chi-square' | input.distribution == 'F') & input.lower_tail == 'upper.tail'",
          numericInput(ns("x2_dist3"), "\\( x: \\)",
                       value = 5, min = 0, step = 1
          ),ns=ns
        ),
        conditionalPanel(
          condition = "(input.distribution == 'Chi-square' | input.distribution == 'F') & input.lower_tail == 'interval'",
          numericInput(ns("a_dist3"), "\\( a: \\)",
                       value = 4, min = 0, step = 1
          ),
          numericInput(ns("b_dist3"), "\\(b:  (a \\leq b) \\)",
                       value = 6, min = 0, step = 1
          ), ns=ns
        ),
        br()
      ),  
      
      
      mainPanel(       
        
        tags$b("Parameters:"),
        br(),
        htmlOutput(ns("parameters_distribution")),
        tags$b("Formula:"),
        br(),
        htmlOutput(ns("Formula")),   
        tags$b("Answer:"),
        conditionalPanel(
          condition = "input.distribution == 'Finite' & input.lower_tail == 'lower.tail'",
          htmlOutput(ns("finite_lower")), ns = ns
        ),
        conditionalPanel(
          condition = "input.distribution == 'Finite' & input.lower_tail == 'upper.tail'",
          htmlOutput(ns("finite_upper")), ns = ns
        ),
        conditionalPanel(
          condition = "input.distribution == 'Finite' & input.lower_tail == 'interval'",
          htmlOutput(ns("finite_interval")), ns = ns
        ),    
        conditionalPanel(
          condition = "input.distribution == 'Binomial'",
          htmlOutput(ns("binomial_prob")), ns = ns
        ),    
        conditionalPanel(
          condition = "input.distribution == 'Poisson'",
          htmlOutput(ns("poisson_prob")), ns = ns
        ),    
        conditionalPanel(
          condition = "input.distribution == 'Normal'",
          htmlOutput(ns("normal_prob")), ns = ns
        ),   
        conditionalPanel(
          condition = "input.distribution == 't'",
          htmlOutput(ns("t_prob")), ns = ns
        ),    
        conditionalPanel(
          condition = "input.distribution == 'Chi-square'",
          htmlOutput(ns("chisquare_prob")), ns = ns
        ),   
        conditionalPanel(
          condition = "input.distribution == 'F'",
          htmlOutput(ns("F_prob")), ns = ns
        ),     
        tags$b("R code:"),
        conditionalPanel(
          condition = "input.distribution == 'Finite'",
          verbatimTextOutput(ns("finitecode")), ns = ns
        ),
        conditionalPanel(
          condition = "input.distribution == 'Binomial'",
          verbatimTextOutput(ns("binomialcode")), ns = ns
        ),   
        conditionalPanel(
          condition = "input.distribution == 'Poisson'",
          verbatimTextOutput(ns("poissoncode")), ns = ns
        ),  
        conditionalPanel(
          condition = "input.distribution == 'Normal'",
          verbatimTextOutput(ns("normalcode")), ns = ns
        ),  
        
        conditionalPanel(
          condition = "input.distribution == 't'",
          verbatimTextOutput(ns("tcode")), ns = ns
        ),  
        conditionalPanel(
          condition = "input.distribution == 'Chi-square'",
          verbatimTextOutput(ns("chisquarecode")), ns = ns
        ), 
        conditionalPanel(
          condition = "input.distribution == 'F'",
          verbatimTextOutput(ns("Fcode")), ns = ns
        ),  
    tags$b("Graph illustration:"),
        conditionalPanel(
          condition = "input.distribution == 'Finite' & input.lower_tail == 'lower.tail'",
          plotOutput(ns("finitePlot_lower"), height="320px"), ns = ns
        ),
        conditionalPanel(
          condition = "input.distribution == 'Finite' & input.lower_tail == 'upper.tail'",
          plotOutput(ns("finitePlot_upper"), height="320px"), ns = ns
        ),
        conditionalPanel(
          condition = "input.distribution == 'Finite' & input.lower_tail == 'interval'",
          plotOutput(ns("finitePlot_interval"), height="320px"), ns = ns
        ),   
        conditionalPanel(
          condition = "input.distribution == 'Binomial' ",
          plotOutput(ns("binomialPlot"), height="320px"), ns = ns
        ),   
        conditionalPanel(
          condition = "input.distribution == 'Poisson'",
          plotOutput(ns("poissonPlot"), height="320px"), ns = ns
        ),   
        conditionalPanel(
          condition = "input.distribution == 'Normal' ",
          plotOutput(ns("normalPlot"), height="320px"), ns = ns
        ),    
        conditionalPanel(
          condition = "input.distribution == 't'",
          plotOutput(ns("tPlot"), height="320px"), ns = ns
        ),  
        conditionalPanel(
          condition = "input.distribution == 'Chi-square'",
          plotOutput(ns("chisquarePlot"), height="320px"), ns = ns
        ),     
        conditionalPanel(
          condition = "input.distribution == 'F'",
          plotOutput(ns("FPlot"), height="320px"), ns = ns
        )    
        
      )       
    )
  )
  
}

#' Probability_Calculation Server Functions
#'
#' @noRd 
mod_Probability_Calculation_server <- function(id){
  
  moduleServer(id, function(input, output, session){
    ns <- session$ns  
    
    
waiter2 <- waiter::Waiter$new(html = "Loading ... <br />The app may take about 10 seconds to appear.<br />Thanks for your patience!", color = "#11A7D6")
waiter2$show()
    
    output$parameters_distribution <- renderUI({  
      if (input$distribution == "Finite") {
        xvalues <- extract(input$values)  
        xweights <- extract(input$weights) 
        if (any(is.na(xvalues)) | length(xvalues) < 2 | any(is.na(xweights)) | length(xweights) < 2) {
          "Invalid input or not enough observations!"
        } else if (any(xweights<0) |  sum(xweights)<1  | (sum(xweights)>1 & sum(xweights)<2) )
        {
          "Some probabilities are less than 0 or the probabilities do not sum to 1"
        } else if ( sum(xweights)>=2 & any(xweights %% 1 != 0) )
        {
          "The frequencies must be integers!"
        } else if (length(xweights)!=length(xvalues)){
          "The values and the probabilities must have the same length!"
        } else{
          mu= sum(xvalues*xweights)/sum(xweights)  
          var = sum((xvalues-mu)^2*xweights)/sum(xweights)
          s2=ifelse(sum(xweights)>=2, sum((xvalues-mu)^2*xweights)/(sum(xweights)-1), NA)
    withMathJax(
        helpText("\\(\\mu = \\bar{x} = E(X)=\\sum xw(x)/ \\sum w(x) = \\)", 
                 round(mu, 6)),
        helpText("\\(\\sigma^2 = Var(X) = \\sum (x-\\mu)^2w(x)/ \\sum w(x) = \\)", 
                 round(var, 6)),
        helpText("\\(\\sigma = SD(X) = \\sqrt{\\sum (x-\\mu)^2w(x)/ \\sum w(x)} = \\)", 
                 round(sqrt( var ), 6)),
        helpText("\\( \\bf{Sample \\; statistics:} \\)" ),
        helpText("\\(S^2 = Var(X) = \\sum (x-\\bar{x})^2w(x)/ (\\sum w(x)-1) = \\)", 
                 round(s2, 6)),
        helpText("\\( S = \\sqrt{\\sum (x-\\bar{x})^2w(x)/ (\\sum w(x)-1)} = \\)", 
                 round(sqrt( s2 ), 6))
          )
          
          
        }
      } else if (input$distribution == "Binomial") {
        if (any(is.na(input$n_binomial), is.na(input$p_binomial))) {"n or p is missing."}
        else if (input$n_binomial %% 1 != 0  | input$n_binomial<0 ) {"n must be a positive integer!"}
        else if (input$p_binomial<0 | input$p_binomial >1) {"p must be between 0 and 1."}
        else{
          withMathJax(
            helpText("\\(\\mu = E(X) = np = \\)", round(input$n_binomial * input$p_binomial, 6)),
            helpText("\\(\\sigma = SD(X) = \\sqrt{np(1-p)} = \\)", round(sqrt(input$n_binomial * input$p_binomial * (1 - input$p_binomial)), 6)),
            helpText("\\(\\sigma^2 = Var(X) = np(1-p) = \\)", round(input$n_binomial * input$p_binomial * (1 - input$p_binomial), 6))
          )
        }
      } else if (input$distribution == "Poisson") {
        if (is.na(input$lambda_poisson) ) {"The mean parameter is missing."} else if (input$lambda_poisson<0)
        {"The mean parameter must be greater than 0."} else{
          withMathJax(
            helpText("\\(\\mu = E(X) = \\lambda = \\)", round(input$lambda_poisson, 6)),
            helpText("\\(\\sigma = SD(X) = \\sqrt{\\lambda} = \\)", round(sqrt(input$lambda_poisson), 6)),
            helpText("\\(\\sigma^2 = Var(X) = \\lambda = \\)", round(input$lambda_poisson, 6))
          )
        }
      }   else if (input$distribution == "Normal") {
        if(any(is.na(input$mean_normal), is.na(input$sd_normal))) {"One of the parameters is missing."}
        else if (input$sd_normal<0) {"The standard deviation must be positive."}
        else{
          withMathJax(
            helpText("\\(\\mu = E(X) = \\)", round(input$mean_normal, 6)),
            helpText("\\(\\sigma = SD(X) = \\)",  round(input$sd_normal, 6)),
            helpText("\\(\\sigma^2 = Var(X) = \\)", round(input$sd_normal^2, 6))
          )
        }
      } else if (input$distribution == "t") {
        if(is.na(input$df_t) | input$df_t<0) {"The degrees of freedom is missing or negative."}
        else if (input$df_t %% 1 != 0) {"The degrees of freedom must be an integer!"}
        else{
          withMathJax(
            helpText("\\(\\mu = E(X) = \\)", ifelse(input$df_t > 1, 0, "Undefined")),
            helpText("\\(\\sigma = SD(X) = \\sqrt{\\dfrac{df}{df - 2}} = \\)", ifelse(input$df_t > 2, round(sqrt(input$df_t / (input$df_t - 2)), 6), "Undefined")),
            helpText("\\(\\sigma^2 = Var(X) = \\dfrac{df}{df-2} = \\)", ifelse(input$df_t > 2, round(input$df_t / (input$df_t - 2), 6), "Undefined"))
          )
        }
      } else if (input$distribution == "Chi-square") {
        if(is.na(input$df_chisquare) | input$df_chisquare<0 ) {"The degrees of freedom is missing or negative."}
        else if (input$df_chisquare %% 1 != 0 ) {"The degrees of freedom must be an integer!"}
        else{
          withMathJax(
            helpText("\\(\\mu = E(X) = df = \\)", round(input$df_chisquare, 6)),
            helpText("\\(\\sigma = SD(X) = \\sqrt{2df} = \\)", round(sqrt(2 * input$df_chisquare), 6)),
            helpText("\\(\\sigma^2 = Var(X) = 2df = \\)", round(2 * input$df_chisquare, 6))
          )
        }
      } else if (input$distribution == "F") {
        if(any(is.na(input$df1_F), is.na(input$df2_F)) | input$df1_F<0 | input$df2_F<0) {"One of the degrees of freedom is missing or negative."}
        else if (input$df1_F %% 1 != 0 | input$df2_F %% 1 != 0 ) {"The degrees of freedoms must be integers!"}
        else{
          withMathJax(
            helpText("\\(\\mu = E(X) = \\dfrac{df2}{df2-2} = \\)", ifelse(input$df2_F > 2, round(input$df2_F / (input$df2_F - 2), 6), "Undefined")),
            helpText("\\(\\sigma = SD(X) = \\sqrt{\\dfrac{2df2^2(df1+df2-2)}{df1(df2-2)^2(df2-4)}} = \\)", ifelse(input$df2_F > 4, round(sqrt(2*input$df2_F**2*(input$df1_F+input$df2_F-2)/( input$df1_F*(input$df2_F-2)**2*(input$df2_F-4)  )), 6), "Undefined")),
            helpText("\\(\\sigma^2 = Var(X) = \\dfrac{2df2^2(df1+df2-2)}{df1(df2-2)^2(df2-4)} = \\)", ifelse(input$df2_F > 4, round(2*input$df2_F**2*(input$df1_F+input$df2_F-2)/( input$df1_F*(input$df2_F-2)**2*(input$df2_F-4)  ), 6), "Undefined"))
          )
        }
      }
    })      
    
    
    output$Formula <- renderUI({
      if ( input$distribution == "Finite" | input$distribution == "Normal" | input$distribution == "t") {
        withMathJax(
          case_when(
            input$lower_tail == "lower.tail" ~ paste0("\\(P(X \\leq \\)", " ", 
                                                      input$x1_dist1, "\\()\\)", " ", "\\( = \\)", " ? " ),
            input$lower_tail == "upper.tail" ~ paste0("\\(P(X > \\)", " ", 
                                                      input$x2_dist1, "\\()\\)", " ", "\\( = \\)", " ? " ),
            input$lower_tail == "interval" ~ paste0("\\(P(\\)", input$a_dist1, " ", "\\(\\leq X\\leq \\)", " ", 
                                                    input$b_dist1, "\\()\\)", " ", "\\( = \\)", " ", 
                                                    ifelse(input$a_dist1 > input$b_dist1, "Error: a must be less than or equal to b ! ! !", 
                                                           "?"))
          )
        )
      } else if (input$distribution == "Binomial" | input$distribution == "Poisson") {
        withMathJax(
          case_when(
            input$lower_tail0 == "point0" ~ paste0("\\(P(X = \\)", " ", input$x0_dist2, "\\()\\)", " ", "\\( = \\)", " ? " ),
            input$lower_tail0 == "lower.tail0" ~ paste0("\\(P(X \\leq \\)", " ", input$x1_dist2, "\\()\\)", " ", "\\( = \\)", " ? " ),
            input$lower_tail0 == "upper.tail0" ~ paste0("\\(P(X > \\)", " ", input$x2_dist2, "\\()\\)", " ", "\\( = \\)", " ? ", 
                                                        "\\( = \\)",   "\\(P(X \\geq \\)",  input$x2_dist2+1, "\\()\\)", "\\( = \\)" ),
            input$lower_tail0 == "interval0" ~ paste0("\\(P(\\)", input$a_dist2, " ", "\\(\\leq X\\leq \\)", " ", input$b_dist2, "\\()\\)", " ", "\\( = \\)", " ", 
                                                      ifelse(input$a_dist2 > input$b_dist2, "Error: a must be less than or equal to b ! ! !", 
                                                             paste0("\\(P(X \\leq \\)", input$b_dist2, "\\()\\)", "\\(-\\)",
                                                                    "\\(P(X \\leq \\)", input$a_dist2-1, "\\()\\)", "\\( = \\)")
                                                      ))
          )
        )
      } else if (input$distribution == "Chi-square" | input$distribution == "F") {
        withMathJax(
          case_when(
            input$lower_tail == "lower.tail" ~ paste0("\\(P(X \\leq \\)", " ", input$x1_dist3, "\\()\\)", " ", "\\( = \\)", " ", 
                                                      " ? "),
            input$lower_tail == "upper.tail" ~ paste0("\\(P(X > \\)", " ", input$x2_dist3, "\\()\\)", " ", "\\( = \\)", " ", 
                                                      " ? "),
            input$lower_tail == "interval" ~ paste0("\\(P(\\)", input$a_dist3, " ", "\\(\\leq X\\leq \\)", " ", input$b_dist3, "\\()\\)", " ", "\\( = \\)", " ", 
                                                    ifelse(input$a_dist3 > input$b_dist3, "Error: a must be less than or equal to b ! ! !", 
                                                           " ? "))
          )
        )
      }  
      
    }) 
    
    
    position<-function(y, dat) 
    {
      n=length(dat)  
      if(y > dat[n]) {return(n)}
      for (i in 1:n)
      {
        if (y ==dat[i]) {
          return(i)} else if (y <dat[i]) 
          {return(i-1+0.5)}
      }
    }
    
    output$finite_lower<- renderPrint({ 
      xi= extract(input$values)     
      wi= extract(input$weights) 
      if (any(is.na(xi)) | length(xi) < 2 | any(is.na(wi)) | length(wi) < 2) {
        return(NULL)
      } else if (any(wi<0) |  sum(wi)<1  | (sum(wi)>1 & sum(wi)<2) )
      {
        return(NULL)
      } else if ( sum(wi)>=2 & any(wi%% 1 != 0)  )
      {
        return(NULL)
      } else if (length(xi)!=length(wi)){
        return(NULL)
      } else{
        pi= wi/sum(wi)    
        data=as.data.frame(cbind(xi, pi))  
        data=dplyr::arrange(data, xi)    
        k= floor( position((input$x1_dist1), data$xi) )  
        return( ifelse(k<1, 0,  sum(data$pi[1:k])) ) 
      }
    }) 
    
    output$finite_upper<- renderPrint({ 
      xi=extract(input$values)    
      wi=extract(input$weights)   
      if (any(is.na(xi)) | length(xi) < 2 | any(is.na(wi)) | length(wi) < 2) {
        return(NULL)
      } else if (any(wi<0) |  sum(wi)<1  | (sum(wi)>1 & sum(wi)<2) )
      {
        return(NULL)
      } else if ( sum(wi)>=2 & any(wi %% 1 != 0)  )
      {
        return(NULL)
      } else if (length(xi)!=length(wi)){
        return(NULL)
      }
      else{
        pi= wi/sum(wi)    
        data=as.data.frame(cbind(xi, pi))  
        data=dplyr::arrange(data, xi)    
        n=dim(data)[1]  
        k= floor( position((input$x2_dist1), data$xi) )  
        dplyr::case_when(
          k<1 ~1,
          k>=n ~ 0,
          k >=1 & k<n ~ sum(data$pi[(k+1):n])
        )
      }
    }) 
    
    output$finite_interval<- renderPrint({ 
      xi=extract(input$values)    
      wi=extract(input$weights)
      if (any(is.na(xi)) | length(xi) < 2 | any(is.na(wi)) | length(wi) < 2) {
        return(NULL)
      } else if (any(wi<0) |  sum(wi)<1  | (sum(wi)>1 & sum(wi)<2) )
      {
        return(NULL)
      } else if ( sum(wi)>=2 & any(wi%%1!=0)  )
      {
        return(NULL)
      } else if (length(xi)!=length(wi) ){
        return(NULL)
      }
      else{ 
        pi= wi/sum(wi)    
        data=as.data.frame(cbind(xi, pi))  
        data=dplyr::arrange(data, xi)   
        
        kb= floor( position((input$b_dist1), data$xi) )  
        pb= ifelse(kb<1, 0, sum(data$pi[1:kb]))   
        
        k1= position((input$a_dist1), data$xi)   
        ka= ifelse((input$a_dist1) %in% data$xi, k1-1, floor(k1))  
        pa = ifelse(ka<1, 0, sum(data$pi[1:ka]))   
        
        pb-pa  
      }
    }) 
    
    output$binomial_prob<- metaRender2( 
      renderPrint,
      { 
        if (any(is.na(input$n_binomial), is.na(input$p_binomial))) {return(NULL)}
        else if (input$n_binomial%% 1 != 0  | input$n_binomial<0) {return(NULL)}
        else if (input$p_binomial<0 | input$p_binomial >1 | input$a_dist2>input$b_dist2) {return(NULL)}
        else{
          if (input$lower_tail0 == 'point0'){
            metaExpr(   dbinom(..(input$x0_dist2), size = ..(input$n_binomial), 
                               prob = ..(input$p_binomial))   ) 
          } else if (input$lower_tail0 == 'lower.tail0'){
            metaExpr(   pbinom(..(input$x1_dist2), size = ..(input$n_binomial), 
                               prob = ..(input$p_binomial), lower.tail = TRUE)   ) 
          } else if (input$lower_tail0 == 'upper.tail0')
          {
            metaExpr( pbinom(..(input$x2_dist2), size = ..(input$n_binomial), 
                             prob = ..(input$p_binomial), lower.tail = FALSE)  )         
          } else {
            metaExpr( pbinom(..(input$b_dist2), size = ..(input$n_binomial), 
                             prob = ..(input$p_binomial), lower.tail = TRUE) -
                        pbinom(..(input$a_dist2-1), size = ..(input$n_binomial), 
                               prob = ..(input$p_binomial), lower.tail = TRUE)  )
          }
        }
      })    
    
    output$poisson_prob<- metaRender2( 
      renderPrint,
      { 
        if (is.na(input$lambda_poisson) | input$lambda_poisson<0  | input$a_dist2>input$b_dist2) {return(NULL)} 
        else{
          if (input$lower_tail0 == 'point0'){
            metaExpr( dpois(..(input$x0_dist2), lambda = ..(input$lambda_poisson) )  )         
          } else if (input$lower_tail0 == 'lower.tail0'){
            metaExpr( ppois(..(input$x1_dist2), lambda = ..(input$lambda_poisson), lower.tail = TRUE)  )         
          } else if (input$lower_tail0 == 'upper.tail0'){
            metaExpr( ppois(..(input$x2_dist2), lambda = ..(input$lambda_poisson), lower.tail = FALSE)  )          
          } else{
            metaExpr( ppois(..(input$b_dist2), lambda = ..(input$lambda_poisson), lower.tail = TRUE) - ppois(..(input$a_dist2) - 1, lambda = ..(input$lambda_poisson), lower.tail = TRUE) ) 
          }       
        } 
      })   
    
    output$normal_prob<- metaRender2( 
      renderPrint,
      { 
        if(any(is.na(input$mean_normal), is.na(input$sd_normal)) | input$sd_normal<0 | input$a_dist1 >input$b_dist1) {return(NULL)}
        else{     
          if (input$lower_tail == 'lower.tail'){
            metaExpr( pnorm(..(input$x1_dist1), mean=..(input$mean_normal), sd=..(input$sd_normal), lower.tail = TRUE)  )
          } else if (input$lower_tail == 'upper.tail'){
            metaExpr( pnorm(..(input$x2_dist1), mean=..(input$mean_normal), sd=..(input$sd_normal), lower.tail = FALSE) ) 
          } else{
            metaExpr(  pnorm(..(input$b_dist1), mean=..(input$mean_normal), sd=..(input$sd_normal), lower.tail = TRUE) - 
                         pnorm(..(input$a_dist1), mean=..(input$mean_normal), sd=..(input$sd_normal), lower.tail = TRUE)  )
          } 
        }
      })     
    
    
    output$t_prob<- metaRender2( 
      renderPrint,
      { 
        if(is.na(input$df_t) | input$df_t<0  | input$df_t%% 1 != 0  | input$a_dist1>input$b_dist1) {return(NULL)}
        else{ 
          if (input$lower_tail == 'lower.tail'){
            metaExpr(  pt(..(input$x1_dist1), df = ..(input$df_t), lower.tail = TRUE)  )         
          } else if (input$lower_tail == 'upper.tail'){
            metaExpr(  pt(..(input$x2_dist1), df = ..(input$df_t), lower.tail = FALSE)  )          
          } else{
            metaExpr( pt(..(input$b_dist1), df = ..(input$df_t), lower.tail = TRUE) - pt(..(input$a_dist1), df = ..(input$df_t), lower.tail = TRUE) ) 
          }     
        }
      })
    
    output$chisquare_prob<- metaRender2( 
      renderPrint,
      { 
        if(is.na(input$df_chisquare) | input$df_chisquare<0 | input$x1_dist3<0 | input$x2_dist3<0 |input$a_dist3<0 | input$b_dist3<0 | input$a_dist3 > input$b_dist3 | input$df_chisquare%% 1 != 0  ) {return(NULL)}
        else{
          if (input$lower_tail == 'lower.tail'){
            metaExpr(   pchisq(..(input$x1_dist3), df = ..(input$df_chisquare), lower.tail = TRUE)  )    
          } else if (input$lower_tail == 'upper.tail'){
            metaExpr( pchisq(..(input$x2_dist3), df = ..(input$df_chisquare), lower.tail = FALSE)  )     
          } else{
            metaExpr(  pchisq(..(input$b_dist3), df = ..(input$df_chisquare), lower.tail = TRUE) -
                         pchisq(..(input$a_dist3), df = ..(input$df_chisquare), lower.tail = TRUE) )     
          }  
        }
      })
    
    output$F_prob<- metaRender2( 
      renderPrint,
      { 
        if(any(is.na(input$df1_F), is.na(input$df2_F), is.na(input$x1_dist3), is.na(input$x2_dist3), is.na(input$a_dist3), is.na(input$b_dist3)) | input$df1_F<0 | input$df2_F<0 | input$df1_F%% 1 != 0 | input$df2_F%% 1 != 0  | input$x1_dist3<0 | input$x2_dist3<0 | input$a_dist3<0 |  input$b_dist3<0 | input$a_dist3 > input$b_dist3) {return(NULL)}       
        else{
          if (input$lower_tail == 'lower.tail'){
            metaExpr(  pf(..(input$x1_dist3), df1 = ..(input$df1_F), df2 = ..(input$df2_F), lower.tail = TRUE)  )
          } else if (input$lower_tail == 'upper.tail'){
            metaExpr(  pf(..(input$x2_dist3), df1 = ..(input$df1_F), df2 = ..(input$df2_F), lower.tail = FALSE)  ) 
          } else{
            metaExpr(  pf(..(input$b_dist3), df1 = ..(input$df1_F), df2 = ..(input$df2_F), lower.tail = TRUE) -
                         pf(..(input$a_dist3), df1 = ..(input$df1_F), df2 = ..(input$df2_F), lower.tail = TRUE) )
          }     
        }
      }) 
    
    
#######code for calculating paramters in finite distributions
  finitecode<- metaRender2( 
      renderPrint,
      {
interpolate3(~(x=x1), x1=as.numeric(sapply( unlist(strsplit(input$values,",")), function(x) eval(parse(text = x))) )    )
interpolate3(~(w=x2), x2=as.numeric(sapply( unlist(strsplit(input$weights,",")), function(x) eval(parse(text = x))) )   )
  if (any(is.na(x)) | length(x) < 2 | any(is.na(w)) | length(w) < 2)
  {return(NULL) } else if (any(w<0) |  sum(w)<1  | (sum(w)>1 & sum(w)<2) )
  {return(NULL) } else if ( sum(w)>=2 & any(w %% 1 != 0) )
  {return(NULL) } else if (length(w)!=length(x))
  {return(NULL) } else if (sum(w)>=2){
  metaExpr({
    mu= sum(x*w)/sum(w)
    cat("Population/Sample mean:", mu)
    sigma2 = sum((x-mu)^2*w)/sum(w)
    cat("Population variance:", sigma2)
    sigma=sqrt(sigma2)
    cat("Population standard deviation:", sigma)
    s2=sum((x-mu)^2*w)/(sum(w)-1)
    cat("Sample variance:", s2)
    s=sqrt(s2)
    cat("Sample standard deviation:", s)
  })
} else
  {
    metaExpr({
    mu= sum(x*w)/sum(w)
    cat("Population/Sample mean:", mu)
    sigma2 = sum((x-mu)^2*w)/sum(w)
    cat("Population variance:", sigma2)
    sigma=sqrt(sigma2)
     cat("Population standard deviation:", sigma)
      })
  
  }
      }
   )
########
    
    
output$finitecode<- renderPrint(expandChain(finitecode())) 
    #output$finitecode1 <-renderPrint(print("No R code is available."))  
    #output$finitecode2 <-renderPrint(print("No R code is available."))  
    #output$finitecode3 <-renderPrint(print("No R code is available."))  
    output$binomialcode <- renderPrint(expandChain(output$binomial_prob()))  
    output$poissoncode <- renderPrint(expandChain(output$poisson_prob()))  
    output$normalcode <- renderPrint(expandChain(output$normal_prob()))  
    output$tcode <- renderPrint(expandChain(output$t_prob()))  
    output$chisquarecode <- renderPrint(expandChain(output$chisquare_prob()))  
    output$Fcode <- renderPrint(expandChain(output$F_prob()))  
    

    output$finitePlot_lower <- renderPlot({
      xi <- extract(input$values)  
      wi <- extract(input$weights)  
      if (any(is.na(xi)) | length(xi) < 2 | any(is.na(wi)) | length(wi) < 2 ) {
        return(NULL)
      } else if (any(wi<0) |  sum(wi)<1  | (sum(wi)>1 & sum(wi)<2) | length(xi)!=length(wi) )
      {
        return(NULL)
      } else if ( sum(wi)>=2 & any(wi%% 1 != 0) | is.na(input$x1_dist1) )
      {
        return(NULL)   
      }
      else{ 
        pi= wi/sum(wi)    
        data=as.data.frame(cbind(xi, pi))  
        data=dplyr::arrange(data, xi)    
        k= floor( position(input$x1_dist1, data$xi) )  
        if (k>=1) {
          p<- data %>% mutate(success = ifelse(xi <= xi[k], "2", "Other")) %>%
            ggplot(aes(x = xi, y = pi, fill = success))+
            geom_col() +
            geom_text(
              aes(label = round(pi, 3), y = pi + 0.005),
              position = position_dodge(0.9),
              size = 3,
              vjust = 0
            ) 
        } else if (k<1)
        {
          p= ggplot(data, aes(x = xi, y = pi))+
            geom_bar(stat="identity", color='grey')
        }
        p<-p+theme_minimal()+
          theme(legend.position = "none") +
          ggtitle(paste0(input$distribution, " distribution:")) +
          theme(plot.title = element_text(face = "bold", hjust = 0.5)) +
          ylab("Probabilities or Relative Frequencies") +
          xlab("x")
        p 
      }
    })
    
    output$finitePlot_upper <- renderPlot({
      xi <- extract(input$values)  
      wi <- extract(input$weights) 
      if (any(is.na(xi)) | length(xi) < 2 | any(is.na(wi)) | length(wi) < 2) {
        return(NULL)
      } else if (any(wi<0) |  sum(wi)<1  | (sum(wi)>1 & sum(wi)<2) | length(xi)!=length(wi) )
      {
        return(NULL)
      } else if ( sum(wi)>=2 & any(wi%%1!=0) | is.na(input$x2_dist1))
      {
        return(NULL)
      }
      else{ 
        pi= wi/sum(wi)    
        data=as.data.frame(cbind(xi, pi))  
        data=dplyr::arrange(data, xi)    
        n=dim(data)[1]  
        k= floor( position( (input$x2_dist1), data$xi) )  
        if (k>=n){
          p= ggplot(data, aes(x = xi, y = pi))+
            geom_bar(stat="identity", color='grey')
        } else if (k>=1 & k<n){
          p <- data %>% mutate(success = ifelse(xi >= xi[k+1], "2", "Other"))%>%
            ggplot(aes(x = xi, y = pi, fill = success))+
            geom_col() +
            geom_text(
              aes(label = round(pi, 3), y = pi + 0.005),
              position = position_dodge(0.9),
              size = 3,
              vjust = 0 ) 
        } else  {
          p<- data  %>%
            ggplot(aes(x = xi, y = pi, fill = "#F8766D"))+
            geom_col() +
            geom_text(
              aes(label = round(pi, 3), y = pi + 0.005),
              position = position_dodge(0.9),
              size = 3,
              vjust = 0 ) 
        }
        p<-p+theme_minimal()+
          theme(legend.position = "none") +
          ggtitle(paste0(input$distribution, " distribution:")) +
          theme(plot.title = element_text(face = "bold", hjust = 0.5)) +
          ylab("Probabilities or Relative Frequencies") +
          xlab("x")
        p  
      }  
    })
    
    
    output$finitePlot_interval <- renderPlot({
      xi <- extract(input$values)  
      wi <- extract(input$weights) 
      if (any(is.na(xi)) | length(xi) < 2 | any(is.na(wi)) | length(wi) < 2) {
        return(NULL)
      } else if (any(wi<0) |  sum(wi)<1  | (sum(wi)>1 & sum(wi)<2) | length(xi)!=length(wi))
      {
        return(NULL)
      } else if ( sum(wi)>=2 & any(wi%%1!=0) | is.na(input$a_dist1) | is.na(input$b_dist1) |input$a_dist1>input$b_dist1)
      {
        return(NULL)
      }
      else{ 
        pi= wi/sum(wi)    
        data=as.data.frame(cbind(xi, pi))  
        data=dplyr::arrange(data, xi)    
        n=dim(data)[1]  
        kb= floor( position(input$b_dist1, data$xi) )  
        k1= position(input$a_dist1, data$xi)   
        ka= ifelse(input$a_dist1 %in% data$xi, k1-1, floor(k1))
        if (kb<1 | ka>n){
          p= ggplot(data, aes(x = xi, y = pi))+
            geom_bar(stat="identity", color='grey')
        } else {
          p <- data %>% mutate(success = ifelse( (xi >= xi[ka+1] & xi<= xi[kb]), "2", "Other"))%>%
            ggplot(aes(x = xi, y = pi, fill = success))+
            geom_col() +  geom_text(   aes(label = round(pi, 3), y = pi + 0.005),
                                       position = position_dodge(0.9),
                                       size = 3,
                                       vjust = 0 ) 
        }
        p<-p+theme_minimal()+
          theme(legend.position = "none") +
          ggtitle(paste0(input$distribution, " distribution:")) +
          theme(plot.title = element_text(face = "bold", hjust = 0.5)) +
          ylab("Probabilities or Relative Frequencies") +
          xlab("x") 
        p  
      }
    })  
    
    output$binomialPlot <- renderPlot({
      if (any(is.na(input$n_binomial), is.na(input$p_binomial), is.na(input$x1_dist2), is.na(input$x2_dist2),is.na(input$a_dist2), is.na(input$b_dist2) ) | input$n_binomial%%1!=0 | input$n_binomial<0 | input$p_binomial<0 | input$p_binomial >1 | input$a_dist2>input$b_dist2) {return(NULL)}
      else{
        heads = qbinom(0.9999, size = input$n_binomial, prob = input$p_binomial, lower.tail = FALSE):qbinom(0.9999, size = input$n_binomial, prob = input$p_binomial, lower.tail = TRUE)  
        prob = dbinom(x = heads, size = input$n_binomial, prob = input$p_binomial)  
        dataset<-data.frame(heads,prob)
        if (input$lower_tail0 == 'point0')
        {
          p <- dataset %>%  
            mutate(Heads = ifelse(heads == input$x0_dist2, "2", "Other")) %>%
            ggplot(aes(x = factor(heads), y = prob, fill = Heads)) 
        } else if (input$lower_tail0 == 'lower.tail0')
        {
          p <- dataset %>%  
            mutate(Heads = ifelse(heads <= input$x1_dist2, "2", "Other")) %>%
            ggplot(aes(x = factor(heads), y = prob, fill = Heads)) 
        } else if (input$lower_tail0 == 'upper.tail0')
        {
          p <- dataset %>%
            mutate(Heads = ifelse(heads > input$x2_dist2, "2", "other")) %>%
            ggplot(aes(x = factor(heads), y = prob, fill = Heads))  
        } else{
          p <- dataset %>%
            mutate(Heads = ifelse(heads >= input$a_dist2 & heads <= input$b_dist2, "2", "other")) %>%
            ggplot(aes(x = factor(heads), y = prob, fill = Heads)) 
        }
        p<-p+geom_col() + geom_text(
          aes(label = round(prob, 3), y = prob + 0.005),
          position = position_dodge(0.9),
          size = 3,
          vjust = 0) +
          theme_minimal() +
          theme(legend.position = "none") +
          ggtitle(paste0(input$distribution, " distribution: Bin(", input$n_binomial, ", ", input$p_binomial, ")")) +
          theme(plot.title = element_text(face = "bold", hjust = 0.5)) +
          ylab("Probability mass function") +
          xlab("x")
        p
      } 
    })
    
    output$poissonPlot <- renderPlot({
      if (any(is.na(input$lambda_poisson), is.na(input$x1_dist2), is.na(input$x2_dist2),is.na(input$a_dist2), is.na(input$b_dist2)) | input$lambda_poisson<0  | input$a_dist2>input$b_dist2) {return(NULL)} 
      else{
        range1= qpois(0.9999, lambda = input$lambda_poisson, lower.tail = FALSE)  
        range2= qpois(0.9999, lambda = input$lambda_poisson, lower.tail = TRUE)  
        heads=range1:range2  
        prob=dpois(x = heads, lambda = input$lambda_poisson)
        dataset<-data.frame(heads,  prob)
        if (input$lower_tail0 == 'point0'){
          p <- dataset %>%
            mutate(Heads = ifelse(heads == input$x0_dist2, "2", "Other")) %>%
            ggplot(aes(x = factor(heads), y = prob, fill = Heads))
        } else if (input$lower_tail0 == 'lower.tail0'){
          p <- dataset %>%
            mutate(Heads = ifelse(heads <= input$x1_dist2, "2", "Other")) %>%
            ggplot(aes(x = factor(heads), y = prob, fill = Heads))
        } else if (input$lower_tail0 == 'upper.tail0'){
          p <- dataset %>%
            mutate(Heads = ifelse(heads > input$x2_dist2, "2", "other")) %>%
            ggplot(aes(x = factor(heads), y = prob, fill = Heads))
        }else{
          p <- dataset %>%
            mutate(Heads = ifelse(heads >= input$a_dist2 & heads <= input$b_dist2, "2", "other")) %>%
            ggplot(aes(x = factor(heads), y = prob, fill = Heads))  
        }
        p<-p+ geom_col() +
          geom_text(
            aes(label = round(prob, 3), y = prob + 0.005),
            position = position_dodge(0.9),
            size = 3,
            vjust = 0  ) +
          theme_minimal() +
          theme(legend.position = "none") +
          ggtitle(paste0(input$distribution, " distribution: Pois(", input$lambda_poisson, ")")) +
          theme(plot.title = element_text(face = "bold", hjust = 0.5)) +
          ylab("Probability mass function") +
          xlab("x")
        p 
      }
    })  
    
    output$normalPlot <- renderPlot({
      if(any(is.na(input$mean_normal), is.na(input$sd_normal),is.na(input$x1_dist1), is.na(input$x2_dist1), is.na(input$a_dist1),is.na(input$b_dist1) ) | input$sd_normal<0 | input$a_dist1 >input$b_dist1) {return(NULL)}
      else{
        range1=qnorm(0.9999, mean = input$mean_normal, sd = input$sd_normal, lower.tail = FALSE)  
        range2=qnorm(0.9999, mean = input$mean_normal, sd = input$sd_normal, lower.tail = TRUE)
        p<-ggplot(data.frame(x = c(range1, range2)), aes(x = x))
        if (input$lower_tail == 'lower.tail'){
          p<-p+geom_area(stat = "function", fun = dnorm, args = list(
            mean = input$mean_normal, sd = input$sd_normal),
            xlim = c(range1, input$x1_dist1),fill="#F8766D",alpha = 0.8) +
            geom_area(stat = "function", fun = dnorm, args = list(
              mean = input$mean_normal, sd = input$sd_normal),
              xlim = c(input$x1_dist1, range2),fill="#00BFC4",alpha = 0.4)+
            scale_x_continuous(breaks = input$x1_dist1)
        } else if (input$lower_tail == 'upper.tail'){
          p<-p+ geom_area(stat = "function", fun = dnorm, args = list(
            mean = input$mean_normal, sd = input$sd_normal),
            xlim = c(range1, input$x2_dist1),fill="#00BFC4",alpha = 0.4) +
            geom_area(stat = "function", fun = dnorm, args = list(
              mean = input$mean_normal, sd = input$sd_normal),
              xlim = c(input$x2_dist1, range2),fill="#F8766D",alpha = 0.8)+
            scale_x_continuous(breaks = input$x2_dist1)
        } else {
          p<-p+geom_area(stat = "function", fun = dnorm, args = list(
            mean = input$mean_normal, sd = input$sd_normal),
            xlim = c(range1, input$a_dist1),fill="#00BFC4",alpha = 0.4) +
            geom_area(stat = "function", fun = dnorm, args = list(
              mean = input$mean_normal, sd = input$sd_normal),
              xlim = c(input$a_dist1, input$b_dist1),fill="#F8766D",alpha = 0.8) +
            geom_area(stat = "function", fun = dnorm, args = list(
              mean = input$mean_normal, sd = input$sd_normal),
              xlim = c(input$b_dist1, range2),fill="#00BFC4",alpha = 0.4)+
            scale_x_continuous(breaks = c(input$a_dist1,input$b_dist1))
        } 
        p<-p+theme_minimal() +
          ggtitle(paste0(input$distribution, " distribution: N(", input$mean_normal, ", ", input$sd_normal, ")")) +
          theme(plot.title = element_text(face = "bold", hjust = 0.5)) +
          ylab("Density") +
          xlab("x") 
        p
      }
    })
    
    output$tPlot <- renderPlot({
      if( any(is.na(input$df_t), is.na(input$x1_dist1), is.na(input$x2_dist1),is.na(input$a_dist1), is.na(input$b_dist1)) |input$df_t<0  | input$df_t%%1!=0 | input$a_dist1>input$b_dist1) {return(NULL)}
      else{
        range1=qt(0.9999, df = input$df_t, lower.tail = FALSE)  
        range2=qt(0.9999, df = input$df_t, lower.tail = TRUE)
        p<-ggplot(data.frame(x = c(range1, range2)), aes(x = x))
        if (input$lower_tail == 'lower.tail'){
          p<-p+geom_area(stat = "function", fun = dt, args = list(df = input$df_t), 
                         xlim = c(range1, input$x1_dist1),fill="#F8766D",alpha = 0.8) +
            geom_area(stat = "function", fun = dt,args = list(df = input$df_t), 
                      xlim = c(input$x1_dist1, range2),fill="#00BFC4",alpha = 0.4)+
            scale_x_continuous(breaks = input$x1_dist1)
        } else if (input$lower_tail == 'upper.tail'){
          p<-p+ geom_area(stat = "function", fun = dt, args = list(df = input$df_t), 
                          xlim = c(range1, input$x2_dist1),fill="#00BFC4",alpha = 0.4) +
            geom_area(stat = "function", fun = dt,args = list(df = input$df_t), 
                      xlim = c(input$x2_dist1, range2),fill="#F8766D",alpha = 0.8)+
            xlab("x")+ scale_x_continuous(breaks = input$x2_dist1)
        } else{
          p<-p+ geom_area(stat = "function", fun = dt, args = list(df = input$df_t), 
                          xlim = c(range1, input$a_dist1),fill="#00BFC4",alpha = 0.4) +
            geom_area(stat = "function", fun = dt,args = list(df = input$df_t), 
                      xlim = c(input$a_dist1, input$b_dist1),fill="#F8766D",alpha = 0.8) +
            geom_area(stat = "function", fun = dt,args = list(df = input$df_t), 
                      xlim = c(input$b_dist1, range2),fill="#00BFC4",alpha = 0.4)+
            scale_x_continuous(breaks = c(input$a_dist1,input$b_dist1))
        }
        p<-p+ theme_minimal() +
          ggtitle(paste0(input$distribution, " distribution: t(", input$df_t, ")")) +
          theme(plot.title = element_text(face = "bold", hjust = 0.5)) +
          ylab("Density") +
          xlab("x")
        p
      }
    })
    
    output$chisquarePlot <- renderPlot({
      if( any(is.na(input$df_chisquare), is.na(input$x1_dist3), is.na(input$x2_dist3),is.na(input$a_dist3), is.na(input$a_dist3) ) | input$df_chisquare<0  | input$df_chisquare%%1!=0 | input$a_dist3<0 |  input$b_dist3<0 | input$a_dist3 > input$b_dist3) {return(NULL)}
      else { 
        range1=qchisq(0.9999, df = input$df_chisquare, lower.tail = FALSE)  
        range2=qchisq(0.9999, df = input$df_chisquare, lower.tail = TRUE)  
        p <- ggplot(data.frame(x = c(range1, range2)), aes(x = x))   
        if (input$lower_tail == 'lower.tail') {
          p<-p+geom_area(stat = "function", fun = dchisq, args = list(
            df = input$df_chisquare),
            xlim = c(range1, input$x1_dist3),fill="#F8766D",alpha = 0.8) +
            geom_area(stat = "function", fun = dchisq, args = list(
              df = input$df_chisquare),
              xlim = c(input$x1_dist3, range2),fill="#00BFC4",alpha = 0.4) +
            scale_x_continuous(breaks = input$x1_dist3)
        } else if (input$lower_tail == 'upper.tail'){
          p<-p+geom_area(stat = "function", fun = dchisq, args = list(
            df = input$df_chisquare),
            xlim = c(range1, input$x2_dist3),fill="#00BFC4",alpha = 0.4) +
            geom_area(stat = "function", fun = dchisq, args = list(
              df = input$df_chisquare),
              xlim = c(input$x2_dist3, range2),fill="#F8766D",alpha = 0.8)+ 
            scale_x_continuous(breaks = input$x2_dist3)
        } else{
          p<-p+geom_area(stat = "function", fun = dchisq, args = list(
            df = input$df_chisquare),
            xlim = c(range1, input$a_dist3),fill="#00BFC4",alpha = 0.4) +
            geom_area(stat = "function", fun = dchisq, args = list(
              df = input$df_chisquare),
              xlim = c(input$a_dist3, input$b_dist3),fill="#F8766D",alpha = 0.8)+
            geom_area(stat = "function", fun = dchisq, args = list(
              df = input$df_chisquare),
              xlim = c(input$b_dist3, range2),fill="#00BFC4",alpha = 0.4)+
            scale_x_continuous(breaks = c(input$a_dist3,input$b_dist3))
        }  
        p<-p+theme_minimal() +
          ggtitle(paste0(input$distribution, " distribution: Chi(", input$df_chisquare, ")")) +
          theme(plot.title = element_text(face = "bold", hjust = 0.5)) +
          ylab("Density") +
          xlab("x")    
        p
      }
    })
    
    output$FPlot <- renderPlot({
      if(any(is.na(input$df1_F), is.na(input$df2_F), is.na(input$x1_dist3), is.na(input$x2_dist3), is.na(input$a_dist3), is.na(input$b_dist3)) | input$df1_F<0 | input$df2_F<0 | input$df1_F%%1!=0 | input$df2_F%%1!=0 | input$a_dist3<0 |  input$b_dist3<0 | input$a_dist3 > input$b_dist3) {return(NULL)}   
      else{
        range1=qf(0.9999, df1 = input$df1_F, df2 = input$df2_F, lower.tail = FALSE)  
        range2=qf(0.9999, df1 = input$df1_F, df2 = input$df2_F, lower.tail = TRUE)
        p <- ggplot(data.frame(x = c(range1, range2)), aes(x = x))
        if (input$lower_tail == 'lower.tail') {
          p<-p+ geom_area(stat = "function", fun = df, args = list(
            df1 = input$df1_F, df2 = input$df2_F),
            xlim = c(range1, input$x1_dist3),fill="#F8766D",alpha = 0.8) +
            geom_area(stat = "function", fun = df, args = list(
              df1 = input$df1_F, df2 = input$df2_F),
              xlim = c(input$x1_dist3, range2),fill="#00BFC4",alpha = 0.4) + 
            scale_x_continuous(breaks = input$x1_dist3)
        } else if (input$lower_tail == 'upper.tail'){
          p<-p+ geom_area(stat = "function", fun = df, args = list(
            df1 = input$df1_F, df2 = input$df2_F),
            xlim = c(range1, input$x2_dist3),fill="#00BFC4",alpha = 0.4) +
            geom_area(stat = "function", fun = df, args = list(
              df1 = input$df1_F, df2 = input$df2_F),
              xlim = c(input$x2_dist3, range2),fill="#F8766D",alpha = 0.8)+
            scale_x_continuous(breaks = input$x2_dist3)
        }  else{
          p<-p+ geom_area(stat = "function", fun = df, args = list(
            df1 = input$df1_F, df2 = input$df2_F),
            xlim = c(range1, input$a_dist3),fill="#00BFC4",alpha = 0.4) +
            geom_area(stat = "function", fun = df, args = list(
              df1 = input$df1_F, df2 = input$df2_F),
              xlim = c(input$a_dist3, input$b_dist3),fill="#F8766D",alpha = 0.8)+
            geom_area(stat = "function", fun = df, args = list(
              df1 = input$df1_F, df2 = input$df2_F),
              xlim = c(input$b_dist3, range2),fill="#00BFC4",alpha = 0.4) +
            scale_x_continuous(breaks = c(input$a_dist3,input$b_dist3))
        }
        p<-p+theme_minimal() +
          ggtitle(paste0(input$distribution, " distribution: F(", input$df1_F, ", ", input$df2_F,")")) +
          theme(plot.title = element_text(face = "bold", hjust = 0.5)) +
          ylab("Density") +
          xlab("x")  
        p
      } 
    })
    
  })   
}
