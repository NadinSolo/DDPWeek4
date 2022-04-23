
library(shiny)
library(car)
library(graphics)
library(DT)
mtcars<-as.data.frame(mtcars)

ui <- fluidPage(

    # Application title
    titlePanel("Exploratory Analysis Creating Multiple Linear Regression with
               Multiple Independent Variables by Example"),

        sidebarLayout(
        sidebarPanel(
          
          checkboxInput("table", "Show/Hide view mtcars", value=TRUE),
          
            checkboxInput("SummaryModel1", "Show/Hide SummaryModel1", value=TRUE),
       
             checkboxInput("SummaryModel2", "Show/Hide SummaryModel2", value=TRUE),
          
              radioButtons("plot", label = "charts and graphs",
                       choices = list("diagram, linear approximation of dependence
      for each pair of variables, Model1" = 1, "diagnostic charts, Model2 " = 2))
    ),

      mainPanel(
         
          h2(" mtcars "),
          DT::dataTableOutput("table"),
          verbatimTextOutput("SummaryModel1"),
          verbatimTextOutput("SummaryModel2"),
          plotOutput("plot1"),
          plotOutput("plot2")
        )
    )
)

# Define server logic required to draw a histogram

server <- function(input, output, session) {
 
#Table
    output$table =  DT::renderDataTable ({
    if (input$table) print(mtcars)})
  
  #Summary
    #Fitmodel1
    Model1<-lm(mpg~.,mtcars)
    
    #Fitmodel2
    Model2<-lm(mpg~wt + qsec + am, mtcars)
    
    output$SummaryModel1 <- renderPrint({
      if (input$SummaryModel1) print(summary(Model1))
      })
    output$SummaryModel2 <- renderPrint({
      if (input$SummaryModel2) print(summary(Model2)) 
      })
      
   #plot Model1, Model2
        output$plot1<- renderPlot({
        if (input$plot==1)
        print (scatterplotMatrix(~cyl+ disp + hp + drat + vs + wt + qsec + am + gear + carb,
                                 data=mtcars, diag="boxplot" ))
        })
        
        output$plot2<-renderPlot({ 
          if (input$plot==2)
          par(mfrow=(c(2,2)))
          plot(Model2) })
         
  }
  
  
# Run the application 
shinyApp(ui = ui, server = server)
