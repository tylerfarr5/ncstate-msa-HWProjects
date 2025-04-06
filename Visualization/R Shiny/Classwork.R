# R base Graphics
barplot(trees$Height, names.arg = row.names(trees), 
        main = "Tree Height", 
        col = "lightblue", 
        xlab = "ID", 
        ylab = "Height")

##############################################################################
######################## GGplot ##############################################
##############################################################################

x_lbl <- row.names(trees)
x_lbl <- factor(x_lbl, levels = unique(x_lbl))

ggplot(data = trees, aes(x = x_lbl, y = Height)) +
  geom_bar(fill = "lightblue", stat = "identity") +
  xlab("ID") +
  ggtitle("Tree Height")

################################################################################3

#Charts
str(beaver1)

df <- beaver1
df$day <- as.factor(df$day)
df$activ <- factor(df$activ, labels = c("inactive", "active"))
df <- df[order(df$activ),]

ggplot(df, aes(x = day, y = "", fill = activ)) +
  geom_bar(stat = "identity") +
  ylab("Activity") +
  ggtitle("Beaver Activity vs Inactivity")

df <- table(beaver1[,c(1,4)]) #aggreagates data
df <- as.data.frame(df)
levels(df$activ) <- c("inactive", "active")

ggplot(df, aes(x = day, y = Freq, fill = activ)) + 
  geom_bar(stat = "identity", position = "dodge") + 
  ylab("Activity") +
  ggtitle("Beaver Activity vs Inactivity")


#Line charts
x_lbl <- as.numeric(row.names(trees))
x_lbl <- as.numeric( row.names( trees ) )

ggplot( data=trees, aes(x=x_lbl, y=Height) ) + 
  geom_line( colour="red", linetype="dashed", size=1.0 ) + 
  xlab( "ID" ) + 
  ggtitle( "Tree Height" ) 

x_lbl <- row.names( trees )
x_lbl <- factor( x_lbl, levels=unique( x_lbl ) )

ggplot( data=trees, aes(x=x_lbl, y=Height, group=1 ) ) + 
  geom_line( colour="red", linetype="dashed", size=1.0 ) + 
  geom_point( color="red", size=3.0, shape=1 ) + 
  xlab( "ID" ) + 
  ggtitle( "Tree Height" ) 


str( chickwts )
df <- chickwts
cat <- table( df$feed )
idx <- numeric()
for( i in 1:length( cat ) ) {
  idx <- c( idx, 1:cat[ i ] )
}
df$x_lbl <- as.factor( idx )

ggplot( data=df, aes( x=x_lbl, y=weight, group=feed, color=feed ) ) + 
  geom_line( size=1.0 ) + 
  geom_point( size=4.0, shape=20 ) + 
  xlab( "ID" ) + 
  ggtitle( "Chicken Weight by Feed Type" ) 


#pie charts
df <- aggregate( chickwts$weight, by=list( chickwts$feed ), FUN=mean )
names( df ) <- c( "feed", "weight" )
df$feed <- reorder( df$feed, order( -df$weight ) )

ggplot( df, aes( x="", y=weight, fill=feed ) ) + 
  geom_bar( stat="identity", width=0.25 ) + 
  coord_polar( "y", start=0 ) + #command to create pie chart
  ggtitle( "Mean Chicken Weighty by Feed Type" ) 


df <- aggregate( chickwts$weight, by=list( chickwts$feed ), FUN=mean )
names( df ) <- c( "feed", "weight" )
df <- df[ order( -df$weight ), ]
lbl <- paste( df$feed, "\n", round( df$weight / 16.0, 1 ), "lb", sep = "" )
cb_palette <- c( "#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2" )
df$pos <- cumsum( df$weight ) - ( df$weight / 2 )
df$feed <- factor( df$feed, levels = df$feed[ order( df$weight ) ] )

pie <- ggplot( df, aes( x="", y=weight, fill=feed ) )
pie <- pie + geom_bar( colour="black", stat="identity" ) + coord_polar( "y", start=0 )
pie <- pie + theme( axis.ticks=element_blank(), axis.title=element_blank(), axis.text.y=element_blank(), axis.text.x=element_text( colour="black" ), legend.position="none" )
pie <- pie + scale_y_continuous( breaks=df$pos, labels=lbl )
pie <- pie + scale_fill_manual( values=cb_palette )
pie <- pie + ggtitle( "Mean Chicken Weight by Feed Type" )
print( pie ) 


#scatterplot
df <- trees

ggplot( data=df, aes( x=Height, y=Volume ) ) + 
  geom_point( shape=20, size=3.0 ) + 
  geom_smooth(method = "lm") +
  ggtitle( "Tree Height vs Volume" ) 

reg <- lm(Volume ~ Height, data = trees)
summary(reg)

#histograms

#option 1
df <- airquality
df$Temp <- as.factor( df$Temp )

ggplot( data=df, aes( x=Temp ) ) + 
  geom_bar( color="black", fill="palegreen2" ) + 
  xlab( "Temperature F" ) + ylab( "" ) + 
  scale_y_continuous( breaks=c( 1, 3, 5, 7, 9, 11 ) ) + 
  ggtitle( "Temperature Counts" ) 

#option 2
df <- chickwts

ggplot( data=df, aes( x=weight ) ) +
  geom_histogram( binwidth=8, color="black", fill="lightblue", alpha=0.7 ) + 
  ylab( "" ) + 
  ggtitle( "Chicken Weight Counts" ) 


#boxplots
ggplot( data=chickwts, aes( x=feed, y=weight ) ) + 
  geom_boxplot( colour="blue", fill="white", outlier.colour="red", outlier.shape=1 ) 

box <- ggplot( data=iris, aes( x=Species, y=Sepal.Width ) )
box <- box + geom_boxplot( lwd=1, color="black", fill="white" )
box <- box + geom_dotplot( aes( fill=Species ), binaxis="y", stackdir="center", method="histodot", binwidth=0.1, dotsize=0.75 )
print( box ) 


#maps
states <- map_data("state")

ggplot() +
  geom_map(data = states, map = states, aes(map_id = region), fill = "white", color = "black") +
  expand_limits(x = states$long, y = states$lat) +
  coord_map( "albers", lat0=29.5, lat1=49.5 ) #map projection

#chlorpleth map
states <- map_data( "state" )
choropleth <- data.frame( ID = tolower( rownames( state.x77 ) ), pop = state.x77[ , 1 ] )

map <- ggplot() + geom_map( data=states, map=states, aes( map_id=region ), fill="white", colour="black" ) + expand_limits( x=states$long, y=states$lat )
map <- map + geom_map( data=choropleth, map=states, aes( fill=pop, map_id=ID ) )
map <- map + coord_map( "albers", lat0=29.5, lat1=49.5 )
print( map ) 


#dot map
states <- map_data( "state" )
coords <- read.csv( url( "http://www.csc2.ncsu.edu/faculty/healey/msa/shiny/cities-coords.csv" ), header=TRUE, sep="," )
data <- read.csv( url( "http://www.csc2.ncsu.edu/faculty/healey/msa/shiny/cities-data.csv" ), header=TRUE, sep="," )
data <- data[ data$City != "Houston" & data$Year == 2012, ]
points <- merge( coords, data, by.x = c( "City", "State" ), by.y = c( "City", "State" ) )
points$Size <- pmax( points$Population / 500000.0, rep( 5.0, 6 ) )

map <- ggplot() + geom_map( data=states, map=states, aes( map_id=region ), fill="white", colour="black" ) + expand_limits( x=states$long, y=states$lat )
map <- map + geom_point( data=points, aes( x=Longitude, y=Latitude ), colour="blue", size=points$Size, alpha=0.7 )
map <- map + coord_map( "albers", lat0=29.5, lat1=49.5 )
print( map ) 


#############################################################################
######## R Shiny #############################################################
#############################################################################

library(shiny)
