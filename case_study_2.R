library("av")
library("tidyr")
library("dplyr")
library("sjmisc")
library("GGally")
library("cowplot")
library("ggplot2")
library("ggExtra")
library("gapminder")
library("gridExtra")
library("gganimate")
library("ggcorrplot")
library("RColorBrewer")

data1<-read.csv("D:\\SEM 3\\R PROGRAMMING\\case study  2\\2018-2010_export.csv")
data2<-read.csv("D:\\SEM 3\\R PROGRAMMING\\case study  2\\2018-2010_import.csv")

#names of bth export and import 
names(data1)
names(data2)

#summary of the export and import 
summary(data1)
summary(data2)

Import_Code      = data2$HSCode                              # Storing the values of each column
Import_Commodity = data2$Commodity
Import_value     = data2$value
Import_value[is.na(Import_value)] <- 0
Import_country   = data2$country
Import_year      = data2$year

Export_Code      = data1$HSCode                              # Storing the values of each column
Export_Commodity = data1$Commodity
Export_value     = data1$value
Export_value[is.na(Export_value)] <- 0
Export_country   = data1$country
Export_year      = data1$year

#boxplot and histogram of imported stocks

qqnorm(Import_value,main="Import Value")
qqline(Import_value)
boxplot(Import_value,main="Import Value")
hist(Import_value,main="Import Value")

#boxplot and histogram of exported stocks

qqnorm(Export_value,main="Export Value")
qqline(Export_value)
boxplot(Export_value,main="Export Value")
hist(Export_value,main="Export Value")

#confidence interval for import value

m<-mean(Import_value)
m
s<-sd(Import_value)
s
n<-length(Import_value)
n
standard_error<-s/(sqrt(n))
error <- standard_error*qt(0.975,df=n-1)
error
left_side <- m-error
right_side <- m + error
print("LEFT SIDE OF THE CONFIDENCE INTERVAL(IMPORT)")
left_side
print("RIGHT SIDE OF THE CONFIDENCE INTERVAL(IMPORT)")
right_side

#confidence interval for export value

m<-mean(Export_value)
m
s<-sd(Export_value)
s
n<-length(Export_value)
n
standard_error<-s/(sqrt(n))
error <- standard_error*qt(0.975,df=n-1)
error
left_side <- m-error
right_side <- m + error
print("LEFT SIDE OF THE CONFIDENCE INTERVAL(EXPORT)")
left_side
print("RIGHT SIDE OF THE CONFIDENCE INTERVAL(EXPORT)")
right_side

#Testing hypothesis
x = rnorm(Import_value)
y = rnorm(Export_value)
t.test(x,y)

#QUESTION 1

VALUE<-max(Export_value)
VALUE
count=1

for(i in Export_value)
{
  if(i==VALUE)
  {
    EXPORTED<-Export_Commodity[count]
  }
  count=count+1
}
print("INDIA EXPORTS THIS PRODUCT AT THE MAXIMUM: ")
EXPORTED
print("VALUE :")
VALUE

#QUESTION 2
VALUE<-max(Import_value)
VALUE
count=1

for(i in Import_value)
{
  if(i==VALUE)
  {
    IMPORTED<-Import_Commodity[count]
  }
  count=count+1
}
print("INDIA EXPORTS THIS PRODUCT AT THE MAXIMUM: ")
IMPORTED
print("VALUE :")
VALUE
print("YES!!! IT IS IMPORTED FROM THE FOREIGN COUNTRIES")

#QUESTION 3

#ggplot
data1 %>% count( HSCode ) %>% arrange( -n ) %>% head(5) %>% 
  ggplot( aes( reorder( HSCode, n ), n ) ) + geom_col( fill = "thistle1" ) + xlab( "COMMODITY HSCODE" ) + ylab( "COUNT" ) + ggtitle( "TOP 5 CMMODITY IN THE EXPORT SECTION" )


data2 %>% count( HSCode ) %>% arrange( -n ) %>% head(5) %>%
  ggplot( aes( reorder( HSCode, n ), n ) ) + geom_col( fill = "tan4" ) + xlab( "COMMODITY HSCODE" ) + ylab( "COUNT" ) + ggtitle( "TOP 5 COMMODITY IN THE IMPORT SECTION" )

data2 %>% filter( year ==  2010 ) %>% count( HSCode ) %>% arrange(-n) %>% head(5) %>%
  ggplot( aes( reorder( HSCode, n ), n ) ) + geom_col( fill = "orangered" ) + xlab( "COMMODITY HSCODE" ) + ylab( "COUNT" ) + ggtitle( "TOP 5 COMMODITY IN THE IMPORT SECTION IN 2010" )

topComImport = data2 %>% filter( HSCode %in% ( data2 %>% count( HSCode ) %>% arrange(-n) %>% head(5) %>% pull( HSCode ) ) )

tb1 = with( topComImport, table( year, HSCode ) )
tb1 = as.data.frame( tb1 )

ggplot( tb1, aes( HSCode, Freq, fill = year) ) + geom_col( position = 'dodge' ) + xlab( "COMMODITY HSCODE" ) + ylab( "COUNT" ) + ggtitle( "5 Top COMMODITIES IN IMPORT SECTION FOR ALL YEARS NEXT TO EACH OTHER" )
