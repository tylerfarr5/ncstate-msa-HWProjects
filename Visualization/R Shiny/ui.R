library( shiny )
shinyUI( fluidPage(   # Define UI for histogram application
  titlePanel( "Distribution Histogram" ),   # App title
  sidebarLayout(   # Sidebar w/slider input for bin width
    sidebarPanel(
      sliderInput(
        "bins",
        "Bin Width:",
        min = 5,
        max = 50,
        value = 20 )
    ),
    mainPanel(   # Plot generated distribution
      plotOutput( "distPlot" ),
      uiOutput( "distInfo")
    )
  )
) )
