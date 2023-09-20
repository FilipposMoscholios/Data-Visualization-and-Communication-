## LOADING THE DATA

library(readxl)
data= read.csv("C:\\Users\\pipos\\Documents\\My Tableau Prep Repository\\Datasources\\Output-clean-airport-dataset.csv",header = T)
data

data_2004 <- read.csv("2004.csv", header = T)
data_2005 <- read.csv("2005.csv", header = T)
data_2006 <- read.csv("2006.csv", header = T)
data_2007 <- read.csv("2007.csv", header = T)

unique(data_2004$DayofMonth)
df <- rbind(data_2004, data_2005, data_2006, data_2007)
rm(data_2004, data_2005, data_2006, data_2007)

summary(df$DepTime)
df$DepTime[df$DepTime > 2400] <- NA

summary(df$ArrTime)
df$ArrTime[df$ArrTime > 2400] <- NA

summary(df$CRSDepTime)
summary(df$CRSArrTime)

summary(df$ActualElapsedTime)
df$ActualElapsedTime[df$ActualElapsedTime < 0] <- NA


airports = c("JFK", "LGA")
data <- df[which(df$Origin %in% airports),]

data$Month <- as.factor(data$Month)
levels(data$Month) <- c("January", "February", "March", "April", "May", "June", "July", "August", "September", "Octomber", "November", "December")
data$DayOfWeek <- as.factor(data$DayOfWeek)
levels(data$DayOfWeek) <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")
data$FlightNum <- as.character(data$FlightNum)
data$Origin <- as.factor(data$Origin)
data$Dest<- as.factor(data$Dest)
data$Cancelled <- as.factor(data$Cancelled)
levels(data$Cancelled)<- c("No", "Yes")
data$CancellationCode <- as.factor(data$CancellationCode)
levels(data$CancellationCode) <- c("Unknown","Carrier", "Weather", "NAS", "Security")
data$Diverted <- as.factor(data$Diverted)
levels(data$Diverted)<- c("No", "Yes")


## Loading the libraries
library(ggplot2)
library(dplyr)
# install.packages("ggridges")
library(ggridges)
library(viridis)

df_sub_1 <- data[which(data$CancellationCode != "Unknown"),]
p1 <- ggplot(data = df_sub_1, aes(x = Year, group=CancellationCode, fill=CancellationCode)) +
  geom_bar(position = position_dodge(width=1))

p1 <- p1 + facet_wrap(~Origin)
p1


p2 <- ggplot(data = data, aes(x=CRSDepTime, y = factor(DayOfWeek), fill=Origin)) +
  geom_density_ridges(alpha = 0.6, scale = 2)+xlim(400,2400)
p2 <- p2 + theme(
  panel.spacing = unit(0.6, "lines"))
p2

df_sub_3 <- df_sub_1[which(df_sub_1$Origin == "JFK"),]
p3 <- ggplot(data = df_sub_3, aes(x=CRSDepTime, y = factor(Year))) +
  geom_density_ridges(alpha = 0.6, scale = 2)+xlim(400,2400)
p3 <- p3 +  geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01) 
p3 <- p3 +  scale_fill_viridis(name = "Temp. [F]", option = "C")
p3 <- p3 + theme(
  panel.spacing = unit(0.1, "lines"))
p3


p4 <- ggplot(data = data, aes(y=CRSDepTime, x = factor(DayOfWeek), fill=Origin)) +
  geom_violin(position = position_dodge(width=0.9)) + 
  stat_boxplot(position = position_dodge(width=0.9), width=0.2)
p4

p5 <- ggplot(data = data, aes(x=Distance, y = factor(DayOfWeek), fill=Origin)) +
  geom_density_ridges(alpha = 0.6, scale = 2)+xlim(400,2400)
p5<- p5 + theme(
  panel.spacing = unit(0.6, "lines"))
p5

summary(data$DepDelay)

p6 <- ggplot(data = data, aes(x=DayOfWeek, y=DepDelay, color=Origin))
p6 <- p6 + geom_segment(aes(x = DayOfWeek, y = 0, xend = DayOfWeek, yend = DepDelay)) 
p6 <- p6 + geom_point() + coord_flip()
p6
