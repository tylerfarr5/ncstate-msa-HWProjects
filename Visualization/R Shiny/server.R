library( ggplot2 )
library( shiny )
shinyServer( function( input, output ) {   # Server logic for histogram
  g <- reactive( {
    ggplot( data=chickwts, aes( x=weight ) ) + geom_histogram( binwidth=input$bins, color="white", fill="lightblue", alpha=0.7 ) + scale_y_continuous( breaks=seq( 0, length( chickwts$weight ), by=2 ) ) + ggtitle( "Chicken Weight Distribution" )
  } )
  output$distPlot <- renderPlot( {
    g()
  } )
  output$distInfo <- renderUI( {
    hist_data <- ggplot_build( g() )$data[[ 1 ]]
    min <- hist_data$xmin
    max <- hist_data$xmax
    rng <- range( chickwts$weight )
    bin_s <- paste( "<b>Weight Range:</b>  [", rng[ 1 ], ",", rng[ 2 ], "]<br>" )
    bin_s <- paste( bin_s, "<b>Number of Bins:</b> ", length( min ), "<br>" )
    bin_s <- paste( bin_s, "<b>Left Boundary:</b> ", min[ 1 ], "<br>" )
    bin_s <- paste( bin_s, "<b>Right Boundary:</b> ", max[ length( max ) ], "<br>" )
    HTML( bin_s )
  } )
} )