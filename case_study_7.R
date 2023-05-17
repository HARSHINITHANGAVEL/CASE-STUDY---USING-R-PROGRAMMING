library(ggplot2)

#1
data = read.csv( file.choose(), header = TRUE )
x<-data$home_team
y<-unique(x)
home<-c(rep(0,length(y)))
home
for(i in 1:length(data$home_team))
{
  if(data$home_score[i]>data$away_score[i])
  {
      z<-which(y==data$home_team[i])[[1]]
      home[z]=home[z]+1; 
  }
}
#print(max(home))
ind<-which(home==max(home))
print("BEST TEAM OF ALL TIME")
print(y[ind])

#2
era1<-c("")
era2<-c("")
era3<-c("")
for(i in 1:length(data$date))
{
  d<-substring(data$date[i],1,2)
  #d2<-substring(data$date[i],7,8)
  if(d=="18")
  {
    era1<-append(era1,data$home_team[i])
  }else if(d=="19"){
    era2<-append(era2,data$home_team[i])
  }else
  {
    era3<-append(era3,data$home_team[i])
  }
}
era1<-unique(era1)
era1
era2<-unique(era2)
era2
era3<-unique(era3)
era3
e1<-c(rep(0,length(era1)))
e2<-c(rep(0,length(era2)))
e3<-c(rep(0,length(era3)))
for(i in 1:length(data$date))
{
  d<-substring(data$date[i],1,2)
  #d2<-substring(data$date[i],7,8)
  if(d=="18")
  {
    if(data$home_score[i]>data$away_score[i])
    {
      j<-which(era1==data$home_team[i])[[1]]
      e1[j]=e1[j]+1
    }
  }else if(d=="19")
  {
    if(data$home_score[i]>data$away_score[i])
    {
      j<-which(era2==data$home_team[i])[[1]]
      e2[j]=e2[j]+1
    }
  }else(d=="20")
  {
    if(data$home_score[i]>data$away_score[i])
    {
      j<-which(era3==data$home_team[i])[[1]]
      e3[j]=e3[j]+1
    }
  }
}
ind<-which(e1==max(e1))
print("DOMINATION OF COUNTRY IN 18 era")
print(era1[ind])
ind<-which(e2==max(e2))
print("DOMINATION OF COUNTRY IN 19 era")
print(era2[ind])
ind<-which(e3==max(e3))
print("DOMINATION OF COUNTRY IN 20 era")
print(era3[ind])

#5
n<-c(rep(0,length(y)))
for(i in 1:length(data$home_team))
{
  if(data$neutral[i]=="FALSE")
  {
    j<-which(y==data$home_team[i])[[1]]
    n[j]=n[j]+1
  }
}
ind<-which(n==max(n))
print("COUNTRY WHICH HOSTED MOST MATCHES WHERE THEY ARE NOT PARTICIPATING")
print(y[ind])

#6
b<-c("")
for(i in 1:length(data$home_team))
{
  if(data$neutral[i]=="TRUE")
  {
    b<-append(b,data$home_team[i])
  }
}
w<-rep(c(0,length(b)))
for(i in 1:length(data$home_team))
{
  if(data$neutral[i]=="TRUE")
  {
    if(data$home_score[i]>data$away_team[i])
    {
      j<-which(b==data$home_team[i])[[1]]
      w[j]=w[j]+1
    }
  }
}
#ind<-which(w==max(w))
total<-sum(w)/length(w)
print("Hosting a major tournament help a country's chances in the tournament")
print(total/100)


#7
team<-c("")
for(i in 1:length(data$home_team))
{
  if(data$tournament[i]=="Friendly")
  {
    team<-append(team,data$home_team[i])
  }
}
team<-unique(team)
tour<-c(rep(0,length(team)))
for(i in 1:length(data$home_team))
{
  if(data$tournament[i]=="Friendly")
  {
    if(data$home_team[i]>data$away_team[i])
    {
      j<-which(team==data$home_team[i])
      tour[j]=tour[j]+1
    }
  }
}
ind<-which(tour==max(tour))
print("TEAM WHICH IS ACTIVE IN FRIENDLY TOURNAMENT")
print(team[ind])

#plots
par(mfrow=c(1,2))
hist(data$home_score)
hist(data$away_score)
ggplot(data, aes(x = data$home_score)) +geom_histogram(binwidth = 5)
ggplot(data, aes(x = data$away_score)) +geom_histogram(binwidth = 5)
ggplot(data,aes(x = home_score, y =away_score)) + geom_point()

