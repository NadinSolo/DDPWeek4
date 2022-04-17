library('ggplot2')
diamonds

library(shiny)
# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Diamond price prediction"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout (
    sidebarPanel(
      sliderInput("slider Carat",
                  "carat diamond weight:",
                  min = 0.1,
                  max = 0.6,
                  value = 0.23),
      
      
      radioButtons("clarity", label = "Select option: clarity",
                   choices = list("I1" = 1, "SI2" = 2,
                                  "SI1" = 3, "VS2"=4, "VS1"=5, "VVS2"=6,
                                  "VVS1"=7, "IF"=8 ),selected = 2),
      
      checkboxInput("Schow Model", "Schow  Model", value=TRUE)
      
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("plot"),
      h3("Predicted  Price from Model:"),
      textOutput("pred")
    )
  )
)




server <- function(input, output) {
  
  model<-lm(price~carat+clarity, data=diamonds)
  modelpred<-reactive({
    caratInput<-input$sliderCarat
    clarityInput<-input$radioButtonsclarity
    predict(model, newdata=data.frame(carat=caratInput, clarityInput))
    
    
  })
  
  
  output$plot <- renderPlot({
    caratInput<-input$sliderCarat
    clarityInput<-input$radioButtonsclarity
    ggplot(data=diamonds, aes(x=carat, y=price, color=clarity, size=carat)) +
      geom_point()
    #ggplot(data = diamonds, aes(x = carat, y = price,  color = clarity)) +
    # geom_line() + geom_point()
  })
}

# Run the application 
shinyApp(ui = ui, server = server)


