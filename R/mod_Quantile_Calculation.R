#' Quantile_Calculation UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_Quantile_Calculation_ui <- function(id){
  ns <- NS(id) 
  
  distchoices <- list( "Normal", "t", "Chi-square", "F"  )

  fluidPage(
    
    withMathJax(),
    sidebarLayout(
      
  sidebarPanel(          
    
  titlePanel("Quantitle Calculation:"),
        
    selectInput(
          inputId = ns("distribution"),
          label = "Choose the distribution of \\(X\\):",
          choices = distchoices,
          multiple = FALSE,
          selected = "Normal"
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
                       value = 8, min = 1, step = 1
          ),
          numericInput(ns("df2_F"), "Denominator Degrees of freedom \\(df2\\):",
                       value = 11, min = 1, step = 1
          ),ns=ns
        ),   
      conditionalPanel(
          condition = "input.lower_tail == 'lower.tail'",
          numericInput(ns("p1"), "Tail probability \\(p\\):",
                       value = 0.5, min=0, max=1, step = 0.1
          ), ns=ns
        ),
        conditionalPanel(
          condition = "input.lower_tail == 'upper.tail'",
          numericInput(ns("p2"), "Tail probability \\(p\\):",
                       value = 0.5, min=0, max=1, step = 0.1
          ), ns=ns
        ),      
    radioButtons(
          inputId = ns("lower_tail"),
          label = NULL,
          choices = c(
            "Lower tail : \\(P(X \\leq ? )\\) \\(= p\\)" = "lower.tail",
            "Upper tail : \\(P(X > ? )\\) \\(= p\\)  " = "upper.tail"
          )
        )

      ),  
      
 
      mainPanel(       
        
        tags$b("Parameters:"),
        br(),
        htmlOutput(ns("parameters_distribution")),
        tags$b("Problem:"),
        br(),
        htmlOutput(ns("Problem")),   
        tags$b("Answer:"),
         conditionalPanel(
          condition = "input.distribution == 'Normal'",
          htmlOutput(ns("normal_Q")), ns = ns
        ),   
        conditionalPanel(
          condition = "input.distribution == 't'",
          htmlOutput(ns("t_Q")), ns = ns
        ),
        conditionalPanel(
          condition = "input.distribution == 'Chi-square'",
          htmlOutput(ns("chisquare_Q")), ns = ns
        ),
        conditionalPanel(
          condition = "input.distribution == 'F'",
          htmlOutput(ns("F_Q")), ns = ns
        ),    
        tags$b("R code:"),
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
          condition = "input.distribution == 'Normal'",
          plotOutput(ns("normalPlot"), height="300px"), ns = ns
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

#' Quantile_Calculation Server Functions
#'
#' @noRd 
mod_Quantile_Calculation_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns 

    
    output$parameters_distribution <- renderUI({  
       if (input$distribution == "Normal") {
  if(any(is.na(input$mean_normal),is.na(input$sd_normal))){"Please enter all distribution parameters."}
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
        else if (input$df_t%%1!=0) {"The degrees of freedom must be an integer!"}
        else {
        withMathJax(
          helpText("\\(\\mu = E(X) = \\)", ifelse(input$df_t > 1, 0, "Undefined")),
          helpText("\\(\\sigma = SD(X) = \\sqrt{\\dfrac{df}{df - 2}} = \\)", ifelse(input$df_t > 2, round(sqrt(input$df_t / (input$df_t - 2)), 6), "Undefined")),
          helpText("\\(\\sigma^2 = Var(X) = \\dfrac{df}{df-2} = \\)", ifelse(input$df_t > 2, round(input$df_t / (input$df_t - 2), 6), "Undefined"))
        )
        }
      } else if (input$distribution == "Chi-square") {
        if(is.na(input$df_chisquare) | input$df_chisquare<0 ) {"The degrees of freedom is missing or negative."}
        else if (input$df_chisquare%%1!=0) {"The degrees of freedom must be an integer!"}
      else{
        withMathJax(
          helpText("\\(\\mu = E(X) = df = \\)", round(input$df_chisquare, 6)),
          helpText("\\(\\sigma = SD(X) = \\sqrt{2df} = \\)", round(sqrt(2 * input$df_chisquare), 6)),
          helpText("\\(\\sigma^2 = Var(X) = 2df = \\)", round(2 * input$df_chisquare, 6))
        )
        }
      } else if (input$distribution == "F") {
        if(any(is.na(input$df1_F), is.na(input$df2_F)) | input$df1_F<0 | input$df2_F<0) {"One of the degrees of freedom is missing or negative."}
        else if (input$df1_F%%1!=0 | input$df2_F%%1!=0) {"The degrees of freedoms must be integers!"}
    else {
        withMathJax(
          helpText("\\(\\mu = E(X) = \\dfrac{df2}{df2-2} = \\)", ifelse(input$df2_F > 2, round(input$df2_F / (input$df2_F - 2), 6), "Undefined")),
          helpText("\\(\\sigma = SD(X) = \\sqrt{\\dfrac{2df2^2(df1+df2-2)}{df1(df2-2)^2(df2-4)}} = \\)", ifelse(input$df2_F > 4, round(sqrt(2*input$df2_F**2*(input$df1_F+input$df2_F-2)/( input$df1_F*(input$df2_F-2)**2*(input$df2_F-4)  )), 6), "Undefined")),
          helpText("\\(\\sigma^2 = Var(X) = \\dfrac{2df2^2(df1+df2-2)}{df1(df2-2)^2(df2-4)} = \\)", ifelse(input$df2_F > 4, round(2*input$df2_F**2*(input$df1_F+input$df2_F-2)/( input$df1_F*(input$df2_F-2)**2*(input$df2_F-4)  ), 6), "Undefined"))
        )
        }
      }
      
    })       
    
 
    output$Problem <- renderUI({
        withMathJax(
    case_when(
      input$lower_tail == "lower.tail" ~ paste0("\\(P(X \\leq  \\)", " ? ",  "\\()\\)", 
                   " ", "\\( = \\)", " ", input$p1),
     input$lower_tail == "upper.tail" ~ paste0("\\(P(X > \\)", " ? ",  "\\()\\)", 
                   " ", "\\( = \\)", " ",input$p2) 
          )
        )
    }) 
    
 
    output$normal_Q<- metaRender2( 
      renderPrint,
      { 
    if(any(is.na(input$mean_normal),is.na(input$sd_normal)) | input$sd_normal<0 )
     {
metaExpr( paste("One of the parameter values is missing or standard deviation is negative !"))
      }else{
      if (input$lower_tail=='lower.tail')
       {
      if(is.na(input$p1) | input$p1<0){metaExpr(paste("The tail probability is missing or negative."))}
       else{
metaExpr(qnorm(..(input$p1), mean=..(input$mean_normal), sd=..(input$sd_normal), lower.tail = TRUE)  ) 
        }
          } else{
   if(is.na(input$p2) | input$p2<0){metaExpr(paste("The tail probability is missing or negative."))}
      else{
metaExpr(  qnorm(..(input$p2), mean=..(input$mean_normal), sd=..(input$sd_normal), lower.tail = FALSE)  ) 
            }           
          }
        } 
      }) 
    
    
 
    output$t_Q<- metaRender2( 
      renderPrint,
      { 
        if(is.na(input$df_t) | input$df_t<0)
        {
          metaExpr( paste("The degrees of freedom is missing or negative!"))
        }else{ 
if (input$lower_tail=='lower.tail')
{
  if(is.na(input$p1) | input$p1<0){metaExpr(paste("The tail probability is missing or negative."))}
  else{
    metaExpr( qt(..(input$p1), df = ..(input$df_t), lower.tail = TRUE) )    
  }
} else{
  if(is.na(input$p2) | input$p2<0){metaExpr(paste("The tail probability is missing or negative."))}
  else{
    metaExpr(  qt(..(input$p2), df = ..(input$df_t), lower.tail = FALSE) )  
  }           
}
          }
      }) 
    

output$chisquare_Q<- metaRender2( 
      renderPrint,
      { 
        if(is.na(input$df_chisquare) | input$df_chisquare<0 )
        { metaExpr( paste("The degrees of freedom is missing or negative!")) }
  else{
    if (input$lower_tail=='lower.tail')
    {
      if(is.na(input$p1) | input$p1<0){metaExpr(paste("The tail probability is missing or negative."))}
      else{
metaExpr(  qchisq(..(input$p1), df = ..(input$df_chisquare), lower.tail = TRUE)  )   
      }      
    } else{
      if(is.na(input$p2) | input$p2<0){metaExpr(paste("The tail probability is missing or negative."))}
      else{
metaExpr( qchisq(..(input$p2), df = ..(input$df_chisquare), lower.tail = FALSE)  )  
      }
    } 
  }      
      }) 
    

    output$F_Q<- metaRender2( 
      renderPrint,
      { 
  if(any(is.na(input$df1_F), is.na(input$df2_F)) | input$df1_F<0 | input$df2_F<0 )
        {
          metaExpr( paste("One of the degrees of freedoms is missing or negative!"))
        }else{
  if (input$lower_tail=='lower.tail')
  {
    if(is.na(input$p1) | input$p1<0){metaExpr(paste("The tail probability is missing or negative."))}
    else{
metaExpr(   qf(..(input$p1), df1 = ..(input$df1_F), df2 = ..(input$df2_F), lower.tail = TRUE)  )      
    }
  } else{
    if(is.na(input$p2) | input$p2<0){metaExpr(paste("The tail probability is missing or negative."))}
    else{
metaExpr( qf(..(input$p2), df1 = ..(input$df1_F), df2 = ..(input$df2_F), lower.tail = FALSE)  )      
    }
  } 
} 
      }) 
    

    

    output$normalcode <- renderPrint(expandChain(output$normal_Q())) 
    output$tcode <- renderPrint(expandChain(output$t_Q())) 
    output$chisquarecode <- renderPrint(expandChain(output$chisquare_Q())) 
    output$Fcode <- renderPrint(expandChain(output$F_Q())) 

output$normalPlot <- renderPlot({
  if(any(is.na(input$mean_normal),is.na(input$sd_normal), is.na(input$p1), is.na(input$p2)) | input$sd_normal<0 )
      {return(NULL)} else{
      range1=qnorm(0.9999, mean = input$mean_normal, sd = input$sd_normal, lower.tail = FALSE) 
      range2=qnorm(0.9999, mean = input$mean_normal, sd = input$sd_normal, lower.tail = TRUE)
      p <- ggplot(data.frame(x = c(range1, range2)), aes(x = x))
    if (input$lower_tail=='lower.tail'){
      if (input$p1<0) {return(NULL)} else{
      Q1=qnorm(input$p1, mean = input$mean_normal, sd = input$sd_normal, lower.tail = TRUE) 
      p<-p+geom_area(stat = "function", fun = dnorm, args = list(
        mean = input$mean_normal, sd = input$sd_normal),
        xlim = c(range1, Q1),fill="blue",alpha = 0.5) +
        geom_area(stat = "function", fun = dnorm, args = list(
          mean = input$mean_normal, sd = input$sd_normal),
          xlim = c(Q1, range2),fill="grey",alpha = 0.5)+
        scale_x_continuous(breaks = Q1)
      }
    } else{
      if (input$p2<0) {return(NULL)} else{
      Q2=qnorm(input$p2, mean = input$mean_normal, sd = input$sd_normal, lower.tail = FALSE)
      p<-p+geom_area(stat = "function", fun = dnorm, args = list(
        mean = input$mean_normal, sd = input$sd_normal),
        xlim = c(range1, Q2),fill="grey",alpha = 0.5) +
        geom_area(stat = "function", fun = dnorm, args = list(
          mean = input$mean_normal, sd = input$sd_normal),
          xlim = c(Q2, range2),fill="blue",alpha = 0.7)+
        scale_x_continuous(breaks = Q2)
      }
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
      if(any(is.na(input$df_t), is.na(input$p1), is.na(input$p2))  ) {return(NULL)} else{
   range1=qt(0.9999, df = input$df_t, lower.tail = FALSE) 
   range2=qt(0.9999, df = input$df_t, lower.tail = TRUE) 
   p <- ggplot(data.frame(x = c(range1, range2)), aes(x = x))
   if (input$lower_tail=='lower.tail'){
     if (input$p1<0) {return(NULL)} else{
     Q1=qt(input$p1, df = input$df_t, lower.tail = TRUE)
     p<-p+geom_area(stat = "function", fun = dt, args = list(df = input$df_t), 
                    xlim = c(range1, Q1),fill="blue",alpha = 0.7) +
       geom_area(stat = "function", fun = dt,args = list(df = input$df_t), 
                 xlim = c(Q1, range2),fill="grey",alpha = 0.5) +
       scale_x_continuous(breaks = Q1)
     }
   } else {
     if (input$p2<0) {return(NULL)} else{
     Q2=qt(input$p2, df = input$df_t, lower.tail = FALSE)
     p<-p+geom_area(stat = "function", fun = dt, args = list(df = input$df_t), 
                    xlim = c(range1, Q2),fill="grey",alpha = 0.5) +
       geom_area(stat = "function", fun = dt,args = list(df = input$df_t), 
                 xlim = c(Q2, range2),fill="blue",alpha = 0.7) +    
       scale_x_continuous(breaks = Q2)
     }
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
      if(any(is.na(input$df_chisquare), is.na(input$p1), is.na(input$p2))) {return(NULL)} else{
   range1=qchisq(0.9999, df = input$df_chisquare, lower.tail = FALSE) 
   range2=qchisq(0.9999, df = input$df_chisquare, lower.tail = TRUE) 
   p <- ggplot(data.frame(x = c(range1, range2)), aes(x = x))
   if (input$lower_tail=='lower.tail'){
     if (input$p1<0) {return(NULL)} else{
     Q1=qchisq(input$p1, df = input$df_chisquare, lower.tail = TRUE)
     p<-p+geom_area(stat = "function", fun = dchisq, args = list(
       df = input$df_chisquare),
       xlim = c(range1, Q1),fill="blue",alpha = 0.7) +
       geom_area(stat = "function", fun = dchisq, args = list(
         df = input$df_chisquare),
         xlim = c(Q1, range2),fill="grey",alpha = 0.5) +
       scale_x_continuous(breaks = Q1)
     }
   } else{
     if (input$p2<0) {return(NULL)} else{
    Q2=qchisq(input$p2, df = input$df_chisquare, lower.tail = FALSE)  
    p<-p+geom_area(stat = "function", fun = dchisq, args = list(
      df = input$df_chisquare),
      xlim = c(range1, Q2),fill="grey",alpha = 0.5) +
      geom_area(stat = "function", fun = dchisq, args = list(
        df = input$df_chisquare),
        xlim = c(Q2, range2),fill="blue",alpha = 0.7) +
      scale_x_continuous(breaks = Q2)
     }
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
      if(any(is.na(input$df1_F), is.na(input$df2_F),is.na(input$p1),is.na(input$p2))) {return(NULL)} 
  else{
      range1=qf(0.9999, df1 = input$df1_F, df2 = input$df2_F, lower.tail = FALSE) 
      range2=qf(0.9999, df1 = input$df1_F, df2 = input$df2_F, lower.tail = TRUE) 
      p <- ggplot(data.frame(x = c(range1, range2)), aes(x = x))
if (input$lower_tail=='lower.tail'){ 
  if (input$p2<0) {return(NULL)} else{
  Q1=qf(input$p1, df1 = input$df1_F, df2 = input$df2_F, lower.tail = TRUE)
  p<-p+geom_area(stat = "function", fun = df, args = list(
    df1 = input$df1_F, df2 = input$df2_F),
    xlim = c(range1, Q1),fill="blue",alpha = 0.7) +
    geom_area(stat = "function", fun = df, args = list(
      df1 = input$df1_F, df2 = input$df2_F),
      xlim = c(Q1, range2),fill="grey",alpha = 0.5) +
    scale_x_continuous(breaks = Q1)
  }
} else {
  if (input$p2<0) {return(NULL)} else{
  Q2=qf(input$p2, df1 = input$df1_F, df2 = input$df2_F, lower.tail = FALSE)
  p<-p+geom_area(stat = "function", fun = df, args = list(
    df1 = input$df1_F, df2 = input$df2_F),
    xlim = c(range1, Q2),fill="grey",alpha = 0.5) +
    geom_area(stat = "function", fun = df, args = list(
      df1 = input$df1_F, df2 = input$df2_F),
      xlim = c(Q2, range2),fill="blue",alpha = 0.7) +
    scale_x_continuous(breaks = Q2)
  }
}
 p<-p+ theme_minimal() +
        ggtitle(paste0(input$distribution, " distribution: F(", input$df1_F, ", ", input$df2_F, ")")) +
        theme(plot.title = element_text(face = "bold", hjust = 0.5)) +
        ylab("Density") +
        xlab("x")
      p
      }
    })
    
  })   
}
