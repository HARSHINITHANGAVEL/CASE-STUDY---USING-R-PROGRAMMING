# Program by Sanjay Ram RR - 21PD32
# R Programming Lab

#install.packages("av")
#install.packages("tidyr")
#install.packages("dplyr")
#install.packages("gifski")
#install.packages("sjmisc")
#install.packages("GGally")
#install.packages("cowplot")
#install.packages("ggplot2")
#install.packages("ggExtra")
#install.packages("gridExtra")
#install.packages("gapminder")
#install.packages("gganimate")
#install.packages("ggcorrplot")
#install.packages("RColorBrewer")

library("av")
library("tidyr")
library("dplyr")
library("gifski")
library("sjmisc")
library("GGally")
library("cowplot")
library("ggplot2")
library("ggExtra")
library("gridExtra")
library("gapminder")
library("gganimate")
library("ggcorrplot")
library("RColorBrewer")


#-------------------------------------------------------------------------------

# Case Study 1 - McDonald's Menu

#-------------------------------------------------------------------------------

Data = read.csv( file.choose(), header = TRUE )               # Reading the data from the csv file and storing it into Data vector

colnames( Data )                                              # Printing the column names of the data
summary ( Data )                                              # Summary of the Data


#-------------------------------------------------------------------------------

Categories    = Data$Category                                 # Storing the values of each column
Items         = Data$Item
Fat           = Data$Total.Fat
Sat_Fat       = Data$Saturated.Fat
Tans_Fat      = Data$Trans.Fat
Cholesterol   = Data$Cholesterol
Calories      = Data$Calories
Sodium        = Data$Sodium
Carbohydrates = Data$Carbohydrates
Diet_Fiber    = Data$Dietary.Fiber
Sugar         = Data$Sugars
Protein       = Data$Protein

#-------------------------------------------------------------------------------
# Count of each Category
#-------------------------------------------------------------------------------

#-----
#Note :
#-----

#  %>% is called the Forward Pipe Operator in R. 
#  It provides a mechanism for chaining commands with a new forward-pipe operator, %>%. 
#  This operator will forward a value, or the result of an expression, into the next function call/expression

#  Distinct() is a function of dplyr package that is used to select distinct or unique rows from the R data frame.

count_of_each_Cat = Data %>% select( Category ) %>% group_by( Category ) %>% mutate( count = n() )
count_of_each_Cat = distinct( count_of_each_Cat )
count_of_each_Cat

plot_cat = count_of_each_Cat %>% ggplot( aes( Category, count) ) + geom_col( fill = "orangered" ) + theme_light() + geom_text( aes( label = count ), color = "black", size = 5 ) + ggtitle( "Amout of Items in each Category" )
plot_cat + theme( axis.text.x = element_text( angle = 45, hjust = 1 ) )

#-------------------------------------------------------------------------------
# Calories per Category
#-------------------------------------------------------------------------------


cal = Data %>% select( Category, Item, Calories ) %>% group_by( Category )
plot_cal = cal %>% ggplot( aes( Calories, Category, fill = Category ) ) + geom_boxplot() + theme_classic() + scale_fill_brewer( palette = "Set3") + ggtitle("Calories per Category")
plot_cal

#-------------------------------------------------------------------------------
# Finding the items with Highest Calories
#-------------------------------------------------------------------------------

maxcal = cal %>% arrange( desc( Calories ) ) %>% head(20) %>% ggplot( aes( Calories, reorder( Item, Calories ), fill = Item ) ) + geom_col() + geom_text( aes( label = Calories ), position = position_stack( vjust = 0.5 ), color = "black", size = 3 ) + theme_light() + ggtitle("TOP 25 Most Calories") + theme( legend.position = "none" )
maxcal

#-------------------------------------------------------------------------------
# Finding the amount of calories contributed by Beverages
#-------------------------------------------------------------------------------

names = c( "Coffee & Tea","Breakfast","Smoothies & Shakes","Chicken & Fish","Beverages","Beef & Pork","Snacks & Sides","Desserts","Salads")

total_cal = 0
bev_cat   = 0
j = 1

for( i in Categories ){
  
  total_cal = total_cal + Calories[j]
  
  if( i == "Coffee & Tea" )
    bev_cat = bev_cat + Calories[j]
  
  else if( i == "Beverages" )
    bev_cat = bev_cat + Calories[j]
  
  j = j + 1 
  
}

rm(i)                                                         # Removing unwanted Variables                                                            
rm(j)                                                         # Removing unwanted Variables

#-------------------------------------------------------------------------------
# Comparison between Grilled and crispy Chicken sandwich
#-------------------------------------------------------------------------------

i = ""
j = 1
k = 1
crispy = 0
grilled = 0

for( i in Items ){
  
  if ( i == "Premium Crispy Chicken Classic Sandwich" )
    crispy[j] = Calories[k]
    j = j + 1
    #
    crispy[j] = Carbohydrates[k]
    j = j + 1
    #
    crispy[j] = Protein[k]
    j = j + 1
    #
    crispy[j] = Fat[k]
    j = j + 1
    
  j = 0
  
  if ( i == "Premium Grilled Chicken Classic Sandwich" )
    grilled[j] = Calories[k]
    j = j + 1
    #
    grilled[j] = Carbohydrates[k]
    j = j + 1
    #
    grilled[j] = Protein[k]
    j = j + 1
    #
    grilled[j] = Fat[k]
    j = j + 1
    
  k = k + 1
}

rm(i)
rm(j)
rm(k)

#-------------------------------------------------------------------------------

ggplot(data = Data, mapping = aes( x = Fat, y = Sat_Fat ) ) + geom_point( color = "blue" ) + theme_dark() + labs( subtitle = "Total fat vs Saturated Fat", x = "Total Fat" , y = "Saturated Fat" )

#-------------------------------------------------------------------------------

paste(  " Average McDonald's value meal's Calories : " , round( ( mean( Calories ) ), digits = 2 ) )
paste( round( ( bev_cat / total_cal ) * 100, digits = 2 ) , " % is contributed by Beverages to Total Calories" )



