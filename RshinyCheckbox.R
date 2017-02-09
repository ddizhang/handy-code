
ui <- fluidPage(
  fluidRow(
    checkboxInput("check", label = "check", value = TRUE),
    textOutput("checkValue"),
    plotOutput("plot1"),
    plotOutput("plot2"),
    plotOutput("plot3")
  )
)

server <- function(input, output) {
  
  output$checkValue = renderText(input$check)
  
  #with this, no plot is generated
  # output$plot1 = eventReactive(
  #   input$check == TRUE,
  #   {renderPlot(plot(0,0))}
  # )

  #with this, both plots are generated but not react to checkbox
  # observeEvent(
  #   input$check == TRUE,
  #   {output$plot2 = renderPlot(plot(1,1))}
  # )

  output$plot3 = renderPlot(
    if (input$check == TRUE) plot(2,2)
  )
}

shinyApp(ui, server)
