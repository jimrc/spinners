require(shiny)


shinyUI(pageWithSidebar(

  # Application title
  headerPanel("Spinner"),

  sidebarPanel(
    sliderInput("nCat", "Number of pie pieces", min=2, max = 20, value = 2),
    sliderInput("nDraws", "Number of spins", min=1, max=100, value= 10),
    sliderInput("speed", "Speed", min=1, max=30, value=10)
   ),

  mainPanel(
    plotOutput("spinner")
    )
))


