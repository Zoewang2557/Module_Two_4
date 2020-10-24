library(shiny)
library(ggplot2)

bodyfat<-read.csv("BodyFat.csv")


server <- function(input, output) {
  
  f <- function(x) { 
    adipos=x[5]/(0.01*x[6])^2;
    aaa=(-2.727642)-1.143013*x[1]+2.863901*adipos+0.347144*x[2]+0.685274*x[3]-5.288027*x[4]+0.066532*x[1]*x[4]-0.021795*adipos*x[2]
    return(aaa)
  }
  
  output$value=reactive({
    f(c(input$Age,input$Chest,input$Abdomen,input$Wrist,input$Weight,input$Height))
  })
  
  
  output$plot <- renderPlot({
    plot(density(bodyfat$BODYFAT),main="The Bodyfat distribution",xlab="Bodyfat",ylab="Density",lwd=5)
    abline(v=f(c(input$Age,input$Chest,input$Abdomen,input$Wrist,input$Weight,input$Height)),col="red",lwd=5)
    text(f(c(input$Age,input$Chest,input$Abdomen,input$Wrist,input$Weight,input$Height))+3.5,0.01,label="You are Here!",pos=3, col="red",cex=1.5)
  },bg="transparent")
  
  output$txtOutput = renderText({
    paste0(f(c(input$Age,input$Chest,input$Abdomen,input$Wrist,input$Weight,input$Height)), "%")
  })
  
  outputOptions(output, "value", suspendWhenHidden = FALSE) 
  
}