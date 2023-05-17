#install.packages("ggplot2")
#install.packages("ggExtra")
#install.packages("gganimate")
#install.packages("ggcorrplot")

library(corrplot)
library(ggcorrplot)
library(ggplot2)

#To choose a file
#file.choose()

#Location of the csv file in File manager 
data<-read.csv("C:\\Users\\dhivy\\OneDrive\\Documents\\DATA SCIENCE\\SEMESTER 3\\LABS\\Applied Statistics and R Programming Lab\\pulsar_stars.csv")
data

#Extracting the columns
c1<-data$Mean.of.the.integrated.profile
c2<-data$Standard.deviation.of.the.integrated.profile
c3<-data$Excess.kurtosis.of.the.integrated.profile
c4<-data$Skewness.of.the.integrated.profile
c5<-data$Mean.of.the.DM.SNR.curve
c6<-data$Standard.deviation.of.the.DM.SNR.curve
c7<-data$Excess.kurtosis.of.the.DM.SNR.curve
c8<-data$Skewness.of.the.DM.SNR.curve
c9<-data$target_class

#Checking whether the values in the given data set is null or not
#This can be done by checking column wise
is.null(c1)
is.null(c2)
is.null(c3)
is.null(c4)
is.null(c5)
is.null(c6)
is.null(c7)
is.null(c8)
is.null(c9)
head(data)

#Correlation plot
S<-cor(data)
corrplot(S,method="color")

#Histogram for the Target_class row
hist(data$target_class,col='cyan')

#Pair plot
pairs(data)

