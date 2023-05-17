#Load libraries
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(plyr))
suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(corrplot)) #Enables you to make a correlation plot
suppressPackageStartupMessages(library(gridExtra)) #Puts multiple graphs in grid format

#Load data
climb<- read.csv("../input/mount-rainier-weather-and-climbing-data/climbing_statistics.csv")
weather<- read.csv("../input/mount-rainier-weather-and-climbing-data/Rainier_Weather.csv")


#Look at raw data   
str(climb)
head(climb, n=5)

str(weather)
head(weather, n=5)

climb$DATE<- as.Date(climb$Date,"%m/%d/%Y")
climb$YEAR<- as.numeric(format(climb$DATE,"%Y"))
climb$MONTH<-as.numeric(format(climb$DATE,'%m'))
#Check
head(climb, n=10)

#Extract year and month- for weather data
weather$DATE<-as.Date(weather$Date, format="%m/%d/%Y")
weather$YEAR<-as.numeric(format(weather$DATE, "%Y"))
weather$MONTH<-as.numeric(format(weather$DATE,"%m"))
#Check
head(weather, n=10)

#Let's give the MONTH variable some labels
climb$MONTH<- factor(climb$MONTH,labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "July", "Aug", "Sept", "Oct", "Nov","Dec"))

weather$MONTH<- factor(weather$MONTH,labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "July", "Aug", "Sept", "Oct", "Nov","Dec"))


plot(climb$Attempted, climb$Succeeded, col="purple", main="Attempts and Successes", xlab="Attempt (counts)", ylab="Success (counts)")

SUCOBS<-climb%>%filter(Succeeded>60)
head(SUCOBS, n=10)
#And what the weather looked like for that particular day   
WEATOBS<- weather %>% filter(Date=="7/20/2015")
head(WEATOBS, n=10)

AttChart<-ggplot(data=climb, aes(x=as.factor(MONTH), y=Attempted)) + geom_boxplot(fill="lightblue") +
  ggtitle("Attempts Over Months") + xlab("Month") + ylab("Number of Peolple who Attempted") + theme_minimal() +
  ylim(0,15) + theme(axis.text.x = element_text(angle = 50,  hjust = 1))

SuccChart<-ggplot(data=climb, aes(x=as.factor(MONTH), y=Succeeded)) + geom_boxplot(fill="gold") +
  ggtitle("Successes Over Months") + xlab("Month") + ylab("Number of People who Succeeded") + theme_minimal() +
  ylim(0,15) + theme(axis.text.x = element_text(angle = 50, hjust = 1)) 

#Put all graphs on a grid
grid.arrange(AttChart, SuccChart, nrow= 1)

AttChartY<-ggplot(data=climb, aes(x=as.factor(YEAR), y=Attempted)) + geom_boxplot(fill="lightblue") +
  ggtitle("Attempts Over Years") + xlab("Year") + ylab("Number of Peolple who Attempted") + theme_minimal() +
  ylim(0,15)

SuccChartY<-ggplot(data=climb, aes(x=as.factor(YEAR), y=Succeeded)) + geom_boxplot(fill="gold") +
  ggtitle("Successes Over Years") + xlab("Year") + ylab("Number of People who Succeeded") + theme_minimal() +
  ylim(0,15) 

grid.arrange(AttChartY, SuccChartY, nrow= 1)


SUCCESS<-climb %>% select("Date", "Route", "Succeeded", "Success.Percentage", "YEAR", "MONTH") %>%
  filter(Succeeded>0)

#What routes have had the most climber successes? (top 10)
RouteTab<-rev(sort(table(SUCCESS$Route)))[1:10]
barplot(RouteTab, las=2, col="gold", main="Top 10 Routes with Highest Number of Successful Climbs", ylab="Successes (count)", ylim=c(0,2000))


ggplot(weather, aes(x=MONTH, y=Battery.Voltage.AVG)) + geom_boxplot() + ggtitle("Battery Voltage")
ggplot(weather, aes(x=MONTH, y=Temperature.AVG)) + geom_boxplot() + ggtitle("Temperature")
ggplot(weather, aes(x=MONTH, y=Wind.Speed.Daily.AVG)) + geom_boxplot() + ggtitle("Wind Speed")
ggplot(weather, aes(x=MONTH, y=Wind.Direction.AVG)) + geom_boxplot() + ggtitle("Wind Direction")
ggplot(weather, aes(x=MONTH, y=Solare.Radiation.AVG)) + geom_boxplot() + ggtitle("Solar Radiation")


WEATHERCORP<-weather %>% select("Battery.Voltage.AVG", "Temperature.AVG", "Relative.Humidity.AVG", "Wind.Speed.Daily.AVG", "Wind.Direction.AVG", "Solare.Radiation.AVG")
#Make correlation plot
corrplot(cor(WEATHERCORP), type="lower", method="number")

BatMonth <- aov(Battery.Voltage.AVG ~ MONTH, data=weather)
summary(BatMonth)

TempMonth <- aov(Temperature.AVG ~ MONTH, data=weather)
summary(TempMonth)

HumMonth <- aov(Relative.Humidity.AVG ~ MONTH, data=weather)
summary(HumMonth)

WindSMonth <- aov(Wind.Speed.Daily.AVG ~ MONTH, data=weather)
summary(WindSMonth)

WindDMonth <- aov(Wind.Direction.AVG ~ MONTH, data=weather)
summary(WindDMonth)

SolMonth <- aov(Solare.Radiation.AVG ~ MONTH, data=weather)
summary(SolMonth)