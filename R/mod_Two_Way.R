#' Two_Way UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_Two_Way_ui <- function(id){
  ns <- NS(id)

  fluidPage(
    sidebarLayout(
      sidebarPanel(         
        
h3("Two-way ANOVA"),
br(), 
selectizeInput(ns("numvar"), label = "Select a response variable (y)", 
               choices = names(mtcars), multiple = FALSE), 
selectizeInput(ns("catvar1"), label = "Select the first factor", 
               choices = names(mtcars), multiple = FALSE), 
selectizeInput(ns("catvar2"), label = "Select the second factor", 
               choices = names(mtcars), multiple = FALSE), 
radioButtons(
  inputId = ns("twoway"),
  label = "Types of analysis",
  choices = c(
    "Detailed data" = "data",
    "Descriptive statistics and plots" = "descriptive",
    "Model fit" = "modelfit",
    "Analysis of residuals" = "residuals",
    "Post hoc multiple comparisons" = "posthoc"
  )
),
conditionalPanel(
  condition = "input.twoway == 'descriptive'",
  selectizeInput(ns("descriptive"), label = "Data summary", 
                 choices = c("Group means and medians" = "summary",
                             "Interaction plot" = "interplot",
                             "Boxplot with jitter" = "boxplot",
                             "Histogram" = "histplot1"
                             )
  ), 
conditionalPanel(
    condition = "input.descriptive != 'interplot'", 
    radioButtons(
      inputId = ns("bygroup"),
      label = "by a factor",
      choices = c(
        "None" = "bygroup0",
        "by the first factor" = "bygroup1",
        "by the second factor" = "bygroup2",
        "by all treatments" = "bygroup3"
      )
    ), ns=ns), 
conditionalPanel(
  condition = "input.descriptive == 'histplot1'", 
  sliderInput(ns("bins"), "Number of bins:",
              min = 1,   max = 50,   value = 5),
  ns=ns),
  ns=ns
),  
conditionalPanel(
  condition = "input.twoway == 'modelfit'",
  selectizeInput(ns("modelfit"), label = "Two-way ANOVA results", 
                 choices = c("Model fit summary" = "fitsummary",
                             "ANOVA table" = "anova")
  ),
  ns=ns
), 
conditionalPanel(
  condition = "input.twoway == 'residuals'",
  selectizeInput(ns("residuals"), label = "Check model assumptions", 
                 choices = c("Histogram of residuals" = "histplot2",
                             "QQ plot of residuals" = "normalityplot",
                             "Test normality of residuals" = "normalitytest",
                             "Residual plot (check independence and constant variance)" = "residualplot",
                             "Test of constant variance" = "variancetest"
                 )),
  conditionalPanel(
    condition = "input.residuals == 'variancetest'",
    radioButtons(
      inputId = ns("variancetest"),
      label = "Choose a test",
      choices = c(
        "Bartlett's test" = "variancetest1",
        "Levene's test" = "variancetest2",
        "Fligner-Killeen test " = "variancetest3"
      )
    ), ns=ns),
  ns=ns
),
conditionalPanel(
  condition = "input.twoway == 'posthoc'", 
  radioButtons(
    inputId = ns("posthoc"),
    label = "Choose a test",
    choices = c(
      "Bonferroni test" = "mtest1",
      "Holm procedure" = "mtest2",
      "Tukey's test " = "mtest3"
    )
  ),
  conditionalPanel(
    condition = "input.posthoc == 'mtest3'",
    sliderInput(ns("alpha"), "Significance level \\(\\alpha = \\)", 
                min=0.001, max=0.2, step=0.01, value=0.05),
    ns=ns),
  ns=ns
)

      ),   


mainPanel(  
  htmlOutput(ns("selected_variable")),
  br(),
  conditionalPanel(
    condition = "input.twoway=='data'",
    verbatimTextOutput(ns("data")), ns=ns
  ),
  conditionalPanel(
    condition = "input.twoway=='descriptive' & input.descriptive=='summary'",
    verbatimTextOutput(ns("summary")), ns=ns
  ),
  conditionalPanel(
    condition = "input.twoway=='descriptive' & input.descriptive=='interplot'",
    plotOutput(ns("interplot"), height="320px"), ns=ns
  ),
  conditionalPanel(
    condition = "input.twoway=='descriptive' & input.descriptive=='boxplot'",
    plotOutput(ns("boxplot"), height="320px"), ns=ns
  ),
  conditionalPanel(
    condition = "input.twoway=='descriptive' & input.descriptive=='histplot1'",
    plotOutput(ns("histplot1"), height="320px"), ns=ns
  ), 
  conditionalPanel(
    condition = "input.twoway=='modelfit' & input.modelfit=='fitsummary'",
    verbatimTextOutput(ns("fitsummary")), ns=ns
  ),
  conditionalPanel(
    condition = "input.twoway=='modelfit' & input.modelfit=='anova'",
    verbatimTextOutput(ns("anova")), ns=ns
  ),
  conditionalPanel(
    condition = "input.twoway=='residuals' & input.residuals=='histplot2'",
    plotOutput(ns("histplot2"), height="320px"), ns=ns
  ), 
  conditionalPanel(
    condition = "input.twoway=='residuals' & input.residuals=='normalityplot'",
    plotOutput(ns("normalityplot"), height="320px"), ns=ns
  ),  
  conditionalPanel(
    condition = "input.twoway=='residuals' & input.residuals=='normalitytest'",
    verbatimTextOutput(ns("normalitytest")), ns=ns
  ), 
  conditionalPanel(
    condition = "input.twoway=='residuals' & input.residuals=='residualplot'",
    plotOutput(ns("residualplot"), height="320px"), ns=ns
  ),  
  conditionalPanel(
    condition = "input.twoway=='residuals' & input.residuals=='variancetest' & input.variancetest=='variancetest1'",
    verbatimTextOutput(ns("variancetest1")), ns=ns
  ),
  conditionalPanel(
    condition = "input.twoway=='residuals' & input.residuals=='variancetest' & input.variancetest=='variancetest2'",
    verbatimTextOutput(ns("variancetest2")), ns=ns
  ),
  conditionalPanel(
    condition = "input.twoway=='residuals' & input.residuals=='variancetest' & input.variancetest=='variancetest3'",
    verbatimTextOutput(ns("variancetest3")), ns=ns
  ),
  conditionalPanel(
    condition = "input.twoway=='posthoc' & input.posthoc=='mtest1' ",
    verbatimTextOutput(ns("mtest1")), ns=ns
  ),
  conditionalPanel(
    condition = "input.twoway=='posthoc' & input.posthoc=='mtest2' ",
    verbatimTextOutput(ns("mtest2")), ns=ns
  ),
  conditionalPanel(
    condition = "input.twoway=='posthoc' & input.posthoc=='mtest3' ",
    verbatimTextOutput(ns("mtest3")), ns=ns
  ),
  rclipboard::rclipboardSetup(),
  uiOutput(ns("clip")),  
shinyAce::aceEditor(ns("code_twoway"), "", mode = "r", readOnly = TRUE, 
            height = "600px", fontSize = 16, wordWrap = TRUE,
            theme = "chrome")
  
)    

    )
  )
}
    
#' Two_Way Server Functions
#'
#' @noRd 
mod_Two_Way_server <- function(id, Data, userdir){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
R.numericnames <- reactive({
  if (is.null(Data())) return(NULL)
  return(numericNames(Data()))
})  
R.categoricnames <- reactive({
  if (is.null(Data())) return(NULL)
  return(categoricNames(Data()))
  })     
    
 observe({
   updateSelectizeInput(session, "numvar", 
                 choices = c("None" = "none", R.numericnames())  )
   updateSelectizeInput(session, "catvar1", 
                 choices = c("None" = "none", R.categoricnames()) )
   updateSelectizeInput(session, "catvar2", 
                        choices = c("None" = "none", R.categoricnames()) )
  })    

 output$selected_variable <- renderText({
   if (is.null(Data())) return(NULL)
   return(paste0("<B>Selected Response Variable: </B>", input$numvar, "; ",
                 " <B>The first selected factor: </B>",  input$catvar1, "; ",
                 " <B>The second selected factor: </B>",  input$catvar2, "."))
 }) 
 
 output$data <- renderPrint({
   if (input$numvar=="none" | input$catvar1=="none" | input$catvar2=="none" ) {return(NULL)}
   else if (input$catvar1==input$catvar2) {paste0("Please choose two different factors!")}
   else{
     Data=Data()
   interpolate(~( aggregate(var1 ~ var2+var3, data=df,FUN=identity) ), 
            df = quote(Data), var1=as.name(input$numvar), var2=as.name(input$catvar1), 
            var3=as.name(input$catvar2),  file = "code_twoway.R", 
           mydir = userdir, append = FALSE, save_result = TRUE, eval = TRUE)
   }
 })
    
 output$summary <- renderPrint({
   Data=Data()
   if (input$numvar=="none" | input$catvar1=="none" | input$catvar2=="none" ) {return(NULL)}
   else if (input$catvar1==input$catvar2) {paste0("Please choose two different factors!")}
     else{
     if (input$bygroup =='bygroup0'){
       if( anyNA(Data[,input$numvar])  ){
interpolate(~(  df%>%select(var1)%>%na.omit()%>%summarize(median=median(var1), 
          mean=mean(var1), variance=var(var1), count=n())%>%as.data.frame() ), 
          df = quote(Data), var1=as.name(input$numvar),  
          file = "code_twoway.R", 
          mydir = userdir, append = FALSE, save_result = TRUE, eval = TRUE)       
        }else{
interpolate(~(  df%>%summarize(median=median(var1), 
          mean=mean(var1), variance=var(var1), count=n())%>%as.data.frame() ), 
          df = quote(Data), var1=as.name(input$numvar),  
          file = "code_twoway.R", 
          mydir = userdir, append = FALSE, save_result = TRUE, eval = TRUE)       
           }
       }
     else if (input$bygroup =='bygroup1'){
   if( anyNA(Data[,input$numvar]) | anyNA(Data[,input$catvar1]) ){
interpolate(~(  df%>%select(var1,var2)%>%na.omit()%>%group_by(var2)%>%
       summarize(median=median(var1), mean=mean(var1), variance=var(var1), count=n())%>%as.data.frame() ), 
       df = quote(Data), var1=as.name(input$numvar),  
       var2=as.name(input$catvar1), file = "code_twoway.R", 
       mydir = userdir, append = FALSE, save_result = TRUE, eval = TRUE)
   }else{
interpolate(~(  df%>%group_by(var2)%>%
       summarize(median=median(var1), mean=mean(var1), variance=var(var1), count=n())%>%as.data.frame() ), 
       df = quote(Data), var1=as.name(input$numvar),  
       var2=as.name(input$catvar1), file = "code_twoway.R", 
       mydir = userdir, append = FALSE, save_result = TRUE, eval = TRUE)
   }
      } else if (input$bygroup=='bygroup2')
     {
if( anyNA(Data[,input$numvar]) | anyNA(Data[,input$catvar2]) ){
interpolate(~(  df%>%select(var1,var2)%>%na.omit()%>%group_by(var2)%>%
        summarize(median=median(var1), mean=mean(var1), variance=var(var1), count=n())%>%as.data.frame() ), 
        df = quote(Data), var1=as.name(input$numvar),  
        var2=as.name(input$catvar2),  file = "code_twoway.R", 
        mydir = userdir, append = FALSE, save_result = TRUE, eval = TRUE)       
}else{
interpolate(~(  df%>%group_by(var2)%>%
        summarize(median=median(var1), mean=mean(var1), variance=var(var1), count=n())%>%as.data.frame() ), 
        df = quote(Data), var1=as.name(input$numvar),  
        var2=as.name(input$catvar2),  file = "code_twoway.R", 
        mydir = userdir, append = FALSE, save_result = TRUE, eval = TRUE)       
 }   
     } else{
if( anyNA(Data[,input$numvar]) | anyNA(Data[,input$catvar1]) | anyNA(Data[,input$catvar2])){
interpolate(~(  df%>%select(var1,var2,var3)%>%na.omit()%>%group_by(var2, var3)%>%
        summarize(median=median(var1), mean=mean(var1), variance=var(var1), count=n())%>%as.data.frame() ), 
        df = quote(Data), var1=as.name(input$numvar),  
        var2=as.name(input$catvar1),  var3=as.name(input$catvar2),
        file = "code_twoway.R", 
        mydir = userdir, append = FALSE, save_result = TRUE, eval = TRUE)      
 }else{
interpolate(~(  df%>%group_by(var2, var3)%>%
       summarize(median=median(var1), mean=mean(var1), variance=var(var1), count=n())%>%as.data.frame() ), 
       df = quote(Data), var1=as.name(input$numvar),  
       var2=as.name(input$catvar1),  var3=as.name(input$catvar2),
       file = "code_twoway.R", 
       mydir = userdir, append = FALSE, save_result = TRUE, eval = TRUE)      
 }       
      }
       }
})

output$interplot<- renderPlot({
   if (input$numvar=="none" | input$catvar1=="none" | input$catvar2=="none" ) {return(NULL)}
   else if (input$catvar1==input$catvar2) {return(NULL)}
   else{
     Data=Data()
     interpolate2(~(  cellcount=df%>%group_by(var2, var3)%>%
                  summarize(n=n())%>%as.data.frame() ), 
                 df = quote(Data), var2=as.name(input$catvar1),  var3=as.name(input$catvar2) )
  if ( all(cellcount$n<=1) )  return(NULL)
  else{
  interpolate(~(  df%>%ggplot(aes(y=var1,x=var2,group=var3, col=var3))+ 
         geom_point() + stat_summary(fun= mean, geom = "line") ), 
        df = quote(Data), var1=as.name(input$numvar),  
        var2=as.name(input$catvar1), var3=as.name(input$catvar2), 
        file = "code_twoway.R", 
        mydir = userdir, append = FALSE, save_result = TRUE, eval = TRUE) 
  }
   }
 })

output$boxplot<- renderPlot({
   if (input$numvar=="none" | input$catvar1=="none" | input$catvar2=="none" ) {return(NULL)}
   else if (input$catvar1==input$catvar2) {return(NULL)}
   else{
     Data=Data()
     if (input$bygroup =='bygroup0'){
interpolate(~(  df%>%ggplot(aes(x="",y=var1))+ 
            geom_boxplot()+geom_jitter(color="blue") ), 
            df = quote(Data), var1=as.name(input$numvar),  
            file = "code_twoway.R", 
            mydir = userdir, append = FALSE, save_result = TRUE, eval = TRUE)       
     } else if (input$bygroup=='bygroup1')
     {
interpolate(~(  df%>%ggplot(aes(y=var1,x=var2))+ 
        geom_boxplot()+geom_jitter(aes(color=var2)) ), 
          df = quote(Data), var1=as.name(input$numvar),  
          var2=as.name(input$catvar1), file = "code_twoway.R", 
          mydir = userdir, append = FALSE, save_result = TRUE, eval = TRUE)
     }
     else if (input$bygroup=='bygroup2')
     {
interpolate(~(  df%>%ggplot(aes(y=var1,x=var2))+ 
        geom_boxplot()+geom_jitter(aes(color=var2)) ), 
           df = quote(Data), var1=as.name(input$numvar),  
           var2=as.name(input$catvar2), file = "code_twoway.R", 
           mydir = userdir, append = FALSE, save_result = TRUE, eval = TRUE) 
     } else{
interpolate(~(  df%>%ggplot(aes(y=var1,x=interaction(var2,var3)))+ 
             geom_boxplot()+
             geom_jitter(aes(color=interaction(var2,var3))) ), 
             df = quote(Data), var1=as.name(input$numvar),var2=as.name(input$catvar1), 
             var3=as.name(input$catvar2), file = "code_twoway.R", 
             mydir = userdir, append = FALSE, save_result = TRUE, eval = TRUE) 
     }
   }
 })
 
output$histplot1<- renderPlot({
   if (input$numvar=="none" | input$catvar1=="none" | input$catvar2=="none" ) {return(NULL)}
   else if (input$catvar1==input$catvar2) {return(NULL)}
   else{
     Data=Data()
     if (input$bygroup =='bygroup0'){
interpolate(~( df%>%ggplot(aes(x=var1))+ 
        geom_histogram(fill="blue",bins=bins)  ), 
       df = quote(Data), var1=as.name(input$numvar), 
       bins=input$bins, file = "code_twoway.R", 
       mydir = userdir, append = FALSE, save_result = TRUE, eval = TRUE)        
     } else if (input$bygroup=='bygroup1')
     {
interpolate(~( df%>%ggplot(aes(x=var1))+ 
      geom_histogram(aes(fill=var2),bins=bins)+facet_wrap(~var2) ), 
           df = quote(Data), var1=as.name(input$numvar),var2=as.name(input$catvar1), 
           bins=input$bins, file = "code_twoway.R", 
           mydir = userdir, append = FALSE, save_result = TRUE, eval = TRUE) 
       } else if (input$bygroup=='bygroup2')
     {
interpolate(~( df%>%ggplot(aes(x=var1))+ 
              geom_histogram(aes(fill=var2),bins=bins)+facet_wrap(~var2) ), 
           df = quote(Data), var1=as.name(input$numvar),var2=as.name(input$catvar2), 
           bins=input$bins, file = "code_twoway.R", 
           mydir = userdir, append = FALSE, save_result = TRUE, eval = TRUE)
       }
     else{
interpolate(~( df%>%ggplot(aes(x=var1))+ 
        geom_histogram(aes(fill=interaction(var2,var3)),bins=bins)+facet_wrap(~interaction(var2,var3)) ), 
        df = quote(Data), var1=as.name(input$numvar),var2=as.name(input$catvar1), 
        var3=as.name(input$catvar2), bins=input$bins, file = "code_twoway.R", 
        mydir = userdir, append = FALSE, save_result = TRUE, eval = TRUE)
       
     }
   }
 })
 
output$fitsummary <- renderPrint({
   if (input$numvar=="none" | input$catvar1=="none" | input$catvar2=="none" ) {return(NULL)}
   else if (input$catvar1==input$catvar2) {return(NULL)}
   else{
     Data=Data()
     interpolate( ~( fit=lm(var1 ~ var2*var3, data= df) ), 
                  df = quote(Data), var1=as.name(input$numvar), var2=as.name(input$catvar1),
                  var3=as.name(input$catvar2),  file = "code_twoway.R", 
                  mydir = userdir, append = FALSE, save_result = TRUE, eval = TRUE) 
     interpolate( ~( summary(fit) ), 
                  file = "code_twoway.R", 
                  mydir = userdir, append = FALSE, save_result = TRUE, eval = TRUE) 
   }
 })
 
 
fit <- reactive ({ 
if (input$numvar=="none" | input$catvar1=="none" | input$catvar2=="none" ) {return(NULL)}
   else if (input$catvar1==input$catvar2) {return(NULL)}
   else{
     Data=Data()
     interpolate2(~( fit=lm(var1~var2*var3, data=df) ), 
                  df = quote(Data), var1=as.name(input$numvar), 
                  var2= as.name(input$catvar1), var3= as.name(input$catvar2))
     return(fit)
   }
 }) 
 
 output$anova <- renderPrint({
   if ( is.null(fit()) ) {return(NULL)}
   else{
     fit=fit()
     interpolate( ~( anova(fit) ), 
            file = "code_twoway.R", 
            mydir = userdir, append = FALSE, save_result = TRUE, eval = TRUE)
   }
 })
 
 output$histplot2 <- renderPlot({
   fit=fit()
   if (is.null(fit)) { return(NULL)} 
   else {
     interpolate(~(  hist(fit$residuals)  ), 
                 file = "code_twoway.R", 
                 mydir = userdir, append = FALSE, save_result = TRUE, eval = TRUE) 
   }
 })
 
 output$normalityplot <- renderPlot({
   fit=fit()
   if (is.null(fit)) { return(NULL)} 
   else {
     interpolate(~(library(car)), file = "code_all.R", 
                 mydir = userdir, append = TRUE, nodupes = TRUE)
     interpolate(~(  car::qqPlot(fit$residuals)  ), 
                 file = "code_twoway.R", 
                 mydir = userdir, append = FALSE, save_result = TRUE, eval = TRUE) 
   }
 })
 
 output$normalitytest <- renderPrint({
   fit=fit()
   if (is.null(fit)) { return(NULL)} 
   else {
     interpolate(~( shapiro.test(fit$residuals)  ), 
                 file = "code_twoway.R", 
                 mydir = userdir, append = FALSE, save_result = TRUE, eval = TRUE)
   }
 })

 output$residualplot <- renderPlot({
   fit=fit()
   if (is.null(fit)) { return(NULL)} 
   else {
     interpolate(~(  plot(fit$fitted.values, fit$residuals)  ), 
                 file = "code_twoway.R", 
                 mydir = userdir, append = FALSE, save_result = TRUE, eval = TRUE) 
   }
 })
 
 output$variancetest1 <- renderPrint({
   if (is.null(fit())) { return(NULL)} 
   else {
     Data=Data()
  interpolate2(~(  cellcount=df%>%group_by(interaction(var2, var3))%>%
                        summarize(n=n())%>%as.data.frame() ), 
        df = quote(Data), var2=as.name(input$catvar1),  var3=as.name(input$catvar2) )
  if ( any(cellcount$n<=1) ) 
  {
    interpolate(~( test1=bartlett.test(var1 ~ var2, data = df) ), 
                df = quote(Data), var1=as.name(input$numvar), 
                var2= as.name(input$catvar1), file = "code_twoway.R", 
                mydir = userdir, append = FALSE, save_result = TRUE, eval = TRUE)
    interpolate(~( test2=bartlett.test(var1 ~ var2, data = df) ), 
                df = quote(Data), var1=as.name(input$numvar), 
                var2= as.name(input$catvar2), file = "code_twoway.R", 
                mydir = userdir, append = FALSE, save_result = TRUE, eval = TRUE)
    interpolate(~( list(test.factor1=test1, test.factor2=test2) ), 
                file = "code_twoway.R", 
                mydir = userdir, append = FALSE, save_result = TRUE, eval = TRUE)
  }else{
    interpolate(~( test1=bartlett.test(var1 ~ var2, data = df) ), 
                df = quote(Data), var1=as.name(input$numvar), 
                var2= as.name(input$catvar1), file = "code_twoway.R", 
                mydir = userdir, append = FALSE, save_result = TRUE, eval = TRUE)
    interpolate(~( test2=bartlett.test(var1 ~ var2, data = df) ), 
                df = quote(Data), var1=as.name(input$numvar), 
                var2= as.name(input$catvar2), file = "code_twoway.R", 
                mydir = userdir, append = FALSE, save_result = TRUE, eval = TRUE)
    interpolate(~( test3=bartlett.test(var1 ~ interaction(var2,var3), data = df) ), 
                df = quote(Data), var1=as.name(input$numvar), 
                var2= as.name(input$catvar1), var3= as.name(input$catvar2),
                file = "code_twoway.R", 
                mydir = userdir, append = FALSE, save_result = TRUE, eval = TRUE)
    interpolate(~( list(test.factor1=test1, test.factor2=test2, test.all=test3) ), 
                file = "code_twoway.R", 
                mydir = userdir, append = FALSE, save_result = TRUE, eval = TRUE)
    
  }
   }
 }) 
 
 output$variancetest2 <- renderPrint({
   if (is.null(fit())) { return(NULL)} 
   else {
     Data=Data()
     interpolate2(~(  cellcount=df%>%group_by(interaction(var2, var3))%>%
                        summarize(n=n())%>%as.data.frame() ), 
                  df = quote(Data), var2=as.name(input$catvar1),  var3=as.name(input$catvar2) )
     if ( any(cellcount$n<=1) ) 
     {
       interpolate(~( test1=car::leveneTest(var1 ~ var2, data = df) ), 
                   df = quote(Data), var1=as.name(input$numvar), 
                   var2= as.name(input$catvar1), file = "code_twoway.R", 
                   mydir = userdir, append = FALSE, save_result = TRUE, eval = TRUE)
       interpolate(~( test2=car::leveneTest(var1 ~ var2, data = df) ), 
                   df = quote(Data), var1=as.name(input$numvar), 
                   var2= as.name(input$catvar2), file = "code_twoway.R", 
                   mydir = userdir, append = FALSE, save_result = TRUE, eval = TRUE) 
       interpolate(~( list(test.factor1=test1, test.factor2=test2) ), 
                   file = "code_twoway.R", 
                   mydir = userdir, append = FALSE, save_result = TRUE, eval = TRUE)
     } else{
       interpolate(~( test1=car::leveneTest(var1 ~ var2, data = df) ), 
                   df = quote(Data), var1=as.name(input$numvar), 
                   var2= as.name(input$catvar1), file = "code_twoway.R", 
                   mydir = userdir, append = FALSE, save_result = TRUE, eval = TRUE)
       interpolate(~( test2=car::leveneTest(var1 ~ var2, data = df) ), 
                   df = quote(Data), var1=as.name(input$numvar), 
                   var2= as.name(input$catvar2), file = "code_twoway.R", 
                   mydir = userdir, append = FALSE, save_result = TRUE, eval = TRUE)
       interpolate(~( test3=car::leveneTest(var1 ~ interaction(var2,var3), data = df) ), 
                   df = quote(Data), var1=as.name(input$numvar), 
                   var2= as.name(input$catvar1), var3= as.name(input$catvar2),
                   file = "code_twoway.R", 
                   mydir = userdir, append = FALSE, save_result = TRUE, eval = TRUE)
       interpolate(~( list(test.factor1=test1, test.factor2=test2, test.all=test3) ), 
                   file = "code_twoway.R", 
                   mydir = userdir, append = FALSE, save_result = TRUE, eval = TRUE)
     }
   }
 }) 
 
 output$variancetest3 <- renderPrint({
   if (is.null(fit())) { return(NULL)} 
   else {
     Data=Data()
     interpolate2(~(  cellcount=df%>%group_by(interaction(var2, var3))%>%
                        summarize(n=n())%>%as.data.frame() ), 
                  df = quote(Data), var2=as.name(input$catvar1),  var3=as.name(input$catvar2) )
     if ( any(cellcount$n<=1) ) 
     {
       interpolate(~( test1=fligner.test(var1 ~ var2, data = df) ), 
                   df = quote(Data), var1=as.name(input$numvar), 
                   var2= as.name(input$catvar1), file = "code_twoway.R", 
                   mydir = userdir, append = FALSE, save_result = TRUE, eval = TRUE)
       interpolate(~( test2=fligner.test(var1 ~ var2, data = df) ), 
                   df = quote(Data), var1=as.name(input$numvar), 
                   var2= as.name(input$catvar2), file = "code_twoway.R", 
                   mydir = userdir, append = FALSE, save_result = TRUE, eval = TRUE)
       interpolate(~( list(test.factor1=test1, test.factor2=test2) ), 
                   file = "code_twoway.R", 
                   mydir = userdir, append = FALSE, save_result = TRUE, eval = TRUE)
     } else {
       interpolate(~( test1=fligner.test(var1 ~ var2, data = df) ), 
                   df = quote(Data), var1=as.name(input$numvar), 
                   var2= as.name(input$catvar1), file = "code_twoway.R", 
                   mydir = userdir, append = FALSE, save_result = TRUE, eval = TRUE)
       interpolate(~( test2=fligner.test(var1 ~ var2, data = df) ), 
                   df = quote(Data), var1=as.name(input$numvar), 
                   var2= as.name(input$catvar2), file = "code_twoway.R", 
                   mydir = userdir, append = FALSE, save_result = TRUE, eval = TRUE)
       interpolate(~( test3=fligner.test(var1 ~ interaction(var2,var3), data = df) ), 
                   df = quote(Data), var1=as.name(input$numvar), 
                   var2= as.name(input$catvar1), var3= as.name(input$catvar2),
                   file = "code_twoway.R", 
                   mydir = userdir, append = FALSE, save_result = TRUE, eval = TRUE)
       interpolate(~( list(test.factor1=test1, test.factor2=test2, test.all=test3) ), 
                   file = "code_twoway.R", 
                   mydir = userdir, append = FALSE, save_result = TRUE, eval = TRUE)  
     }
     }
 }) 

output$mtest1 <- renderPrint({
   if (is.null(fit())) { return(NULL)} 
   else {
     Data=Data()
     interpolate(~( test1=pairwise.t.test(x=df$var1, g=df$var2, p.adj = "bonf") ), 
                 df = quote(Data), var1=input$numvar, 
                 var2= input$catvar1, file = "code_twoway.R", 
                 mydir = userdir, append = FALSE, save_result = TRUE, eval = TRUE)
     interpolate(~( test2=pairwise.t.test(x=df$var1, g=df$var2, p.adj = "bonf") ), 
                 df = quote(Data), var1=input$numvar, 
                 var2= input$catvar2, file = "code_twoway.R", 
                 mydir = userdir, append = FALSE, save_result = TRUE, eval = TRUE)
     interpolate(~( test3=pairwise.t.test(x=df$var1, g=interaction(df$var2,df$var3), p.adj = "bonf") ), 
                 df = quote(Data), var1=input$numvar, 
                 var2= input$catvar1, var3= input$catvar2, file = "code_twoway.R", 
                 mydir = userdir, append = FALSE, save_result = TRUE, eval = TRUE)
     interpolate(~( list(test.factor1=test1, test.factor2=test2, test.all=test3) ), 
                 file = "code_twoway.R", 
                 mydir = userdir, append = FALSE, save_result = TRUE, eval = TRUE) 
   }
 })

output$mtest2 <- renderPrint({
   if (is.null(fit())) { return(NULL)} 
   else {
     Data=Data()
     interpolate(~( test1=pairwise.t.test(x=df$var1, g=df$var2, p.adj = "holm") ), 
                 df = quote(Data), var1=input$numvar, 
                 var2= input$catvar1, file = "code_twoway.R", 
                 mydir = userdir, append = FALSE, save_result = TRUE, eval = TRUE)
     interpolate(~( test2=pairwise.t.test(x=df$var1, g=df$var2, p.adj = "holm") ), 
                 df = quote(Data), var1=input$numvar, 
                 var2= input$catvar2, file = "code_twoway.R", 
                 mydir = userdir, append = FALSE, save_result = TRUE, eval = TRUE)
     interpolate(~( test3=pairwise.t.test(x=df$var1, g=interaction(df$var2,df$var3), p.adj = "holm") ), 
                 df = quote(Data), var1=input$numvar, 
                 var2= input$catvar1, var3= input$catvar2, file = "code_twoway.R", 
                 mydir = userdir, append = FALSE, save_result = TRUE, eval = TRUE)
     interpolate(~( list(test.factor1=test1, test.factor2=test2, test.all=test3) ), 
                 file = "code_twoway.R", 
                 mydir = userdir, append = FALSE, save_result = TRUE, eval = TRUE) 
   }
 })
 
 output$mtest3 <- renderPrint({
   if (is.null(fit())) { return(NULL)} 
   else {
     fit=fit()
     interpolate(~( TukeyHSD(aov(fit), conf.level = clevel) ), 
                 clevel=1-input$alpha, file = "code_twoway.R", 
                 mydir = userdir, append = FALSE, save_result = TRUE, eval = TRUE)
   }
 })
 
 
code <- reactiveFileReader(5000, session, file.path(userdir, "code_all.R"), clean_readlines)
observe({    
   updateAceEditor(session, "code_twoway", 
                   value = paste(code(), collapse = "\n"))
 }) 
 output$clip <- renderUI({
   rclipButton("clipbtn", "Copy-R-code-to-clipboard", input$code_twoway, icon("clipboard", class = "btn btn-success"))
 })
      
 
  })
}
    