# Load the data
data_2004 <- read.csv2("2004.csv.bz2", header = T, sep=",")
data_2005 <- read.csv2("2005.csv.bz2", header = T, sep=",")
data_2006 <- read.csv2("2006.csv.bz2", header = T, sep=",")
data_2007 <- read.csv2("2007.csv.bz2", header = T, sep=",")

# BIND DATAFRAMES 
data <- rbind(data_2004, data_2005, data_2006, data_2007)
rm(data_2004, data_2005, data_2006, data_2007)
head(data)
str(data)

#preprocessing

data$Year <- as.factor(data$Year)

data$Month <- as.factor(data$Month)
levels(data$Month) <- c("January", "February", "March", "April", "May", "June", "July", "August", "September", "Octomber", "November", "December")
#data$DayofMonth <- as.factor(data$DayofMonth)
data$DayOfWeek <- as.factor(data$DayOfWeek)
levels(data$DayOfWeek) <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")
data$DepTime[data$DepTime>2400]<- NA
#data$DepTime_grouped <- cut(data$DepTime, breaks = c(0, 600, 1200, 1800, 2400), include.lowest = TRUE, right = FALSE)
#levels(data$DepTime_grouped) <- c("from 00:00 to 6:00", "from 6:00 to 12:00", "from 12:00 to 18:00", "from 18:00 to 24:00")
#data$CRSDepTime_grouped <- cut(data$CRSDepTime, breaks = c(0, 600, 1200, 1800, 2400), include.lowest = TRUE, right = FALSE)
#levels(data$CRSDepTime_grouped) <- c("from 00:00 to 6:00", "from 6:00 to 12:00", "from 12:00 to 18:00", "from 18:00 to 24:00")
data$ArrTime[data$ArrTime>2400]<- NA
#data$ArrTime_grouped <- cut(data$ArrTime, breaks = c(0, 600, 1200, 1800, 2400), include.lowest = TRUE, right = FALSE)
#levels(data$ArrTime_grouped) <- c("from 00:00 to 6:00", "from 6:00 to 12:00", "from 12:00 to 18:00", "from 18:00 to 24:00")
#data$CRSArrTime_grouped <- cut(data$CRSArrTime, breaks = c(0, 600, 1200, 1800, 2400), include.lowest = TRUE, right = FALSE)
#levels(data$CRSArrTime_grouped) <- c("from 00:00 to 6:00", "from 6:00 to 12:00", "from 12:00 to 18:00", "from 18:00 to 24:00")
data$UniqueCarrier <- as.factor(data$UniqueCarrier)
levels(data$UniqueCarrier)
data$FlightNum <- as.character(data$FlightNum)

data$Origin <- as.factor(data$Origin)
data$Dest<- as.factor(data$Dest)

data$Distance <- 1.609344*data$Distance  #to km

data$Cancelled <- as.factor(data$Cancelled)
levels(data$Cancelled)<- c("No", "Yes")

data$CancellationCode <- as.factor(data$CancellationCode)
data[which(data$Cancelled == "No" & data$CancellationCode != ""),]["CancellationCode"] <- ""
levels(data$CancellationCode) <- c("Not Cancelled","Carrier", "Weather", "NAS", "Security") 

data$Diverted <- as.factor(data$Diverted)
levels(data$Diverted)<- c("No", "Yes")

summary(data$AirTime)
summary(data[which(data$Cancelled == "Yes"),]$AirTime)

data$AirTime[data$AirTime <= 0] <- NA

summary(data$CRSElapsedTime)
data$CRSElapsedTime[data$CRSElapsedTime <= 0] <- NA

summary(data$TaxiIn)
summary(data$TaxiOut)
summary(data[which(data$Cancelled == "Yes"),])
summary(data[which(data$Cancelled == "No"),])

data$TaxiIn[data$Cancelled == "Yes"] <- NA
data$TaxiOut[data$Cancelled == "Yes"] <- NA

summary(data$ActualElapsedTime)
data$ActualElapsedTime <- data$TaxiIn+data$TaxiOut+data$AirTime

summary(data$CarrierDelay)
summary(data$WeatherDelay)
summary(data$NASDelay)
summary(data$SecurityDelay)
summary(data$LateAircraftDelay)

data$NASDelay[data$NASDelay < 0] <- NA 


#choose specific airports by origin
airports <- c("ORD", "DAL")
df <- data[which(data$Origin %in% airports),]

write.csv2(data, "data_all_airports.csv",row.names = FALSE)
write.csv2(df, "data_two_airports.csv",row.names = FALSE)


## PLOTS

## Loading the libraries
#install.packages("ggplot2")
library(ggplot2)
# install.packages("ggridges")
library(ggridges)
library(treemapify)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggthemes)
library(tidytable)
library(ggpubr)
library(countrycode)
library(directlabels)
library(viridis)
library(dplyr)
# install.packages("ggbump")
library(ggbump)
#install.packages("ggbeeswarm")
library(ggbeeswarm)
#install.packages("scales")
library(scales)
#install.packages("dbplyr")
library(dbplyr)
#install.packages("tidyverse")
library(tidyverse)


## RIDGE PLOT
data1 <- df %>%
  mutate( DayOfWeek = fct_relevel(DayOfWeek, levels = "Monday", "Tuesday", "Wednesday",
                                  "Thursday", "Friday", "Saturday", "Sunday"))
X = as.POSIXct(strptime( data1$CRSDepTime, format='%H%M'))#, 12, 16
# df_sub_1 <- data1[which(data1$CancellationCode != "Unknown"),]

p1 <- ggplot(data = data1, aes(x=X, y = DayOfWeek, fill=Origin))
p1 <- p1 + geom_density_ridges(alpha = 0.6, scale = 2)
p1 <- p1 + theme(axis.text.x=element_text( colour="black", size=rel(1.1), angle=45, hjust=1, vjust=1),
        axis.text.y=element_text( colour="black", size=rel(1.1)),
        plot.title = element_text(color="black", size=25, face="bold.italic", hjust = 0.5),
        axis.ticks=element_line(size=1),
        panel.grid.minor = element_blank(),
        axis.title.x = element_text(color="black", size=20, face="bold"),
        axis.title.y = element_text(color="black", size=20, face="bold"),
        legend.text=element_text(size=14), legend.title=element_text(size=14),
        legend.position="bottom",
        text = element_text(size=20))+
  scale_x_datetime(breaks = date_breaks("7200 sec"),labels = date_format("%H:%M")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank())+
  #scale_y_discrete(breaks=1:7,labels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))+
  #scale_y_continuous(breaks = round(seq(0,90, by = 10),1))+
  labs( y="Day",
        x="Time",
        title="Distribution of scheduled departure times")
p1 <- p1 + theme(
  panel.spacing = unit(0.6, "lines"))
p1

# BARPLOT WITH CANCELLATION CODES
data2 <- df[which(df$CancellationCode != "Not Cancelled"),]
p2 <- ggplot(data = data2, aes(x = CancellationCode, group=CancellationCode, fill=CancellationCode)) +
  geom_bar(position = position_dodge(width=1))
p2 <- p2 + facet_grid(.~Year)
p2 <- p2 + labs(
  title = "Cancellation reason for Chicago ORD Airport",
  x = "Year",
  y = "frequency",
  fill = "Reason"
)

p2 <- p2 + theme(
  strip.text.x = element_text(size=12, angle=0),
  strip.text.y = element_text(size=15, hjust = 0.5, face="bold"),
  strip.background = element_rect(color="red", fill="palegreen"),
  panel.background = element_rect(fill = "gray96"),
  plot.title = element_text(size = 13, hjust = 0.5, face = "bold"),
  plot.subtitle = element_text(size = 9, color = "blue", face = "bold.italic", hjust = 0.5),
  plot.caption = element_text(size = 8, color = "blue", face = "italic"),
  axis.text.y = element_text(size = 7),
  axis.title.y = element_text(size = 11),
  axis.title.x = element_blank(),
  axis.ticks = element_blank(),
  axis.text.x = element_blank()
)
p2

## PLOT 3


## PLOT 4 SCATTER PLOT WITH ARRAYS
m <- merge(aggregate(data = df, DepDelay~Year+Origin, mean), aggregate(data = df, AirTime~Year, mean))
m
p4 <- ggplot(m, aes(x = DepDelay, y = AirTime, color = Origin))
p4 <- p4 + geom_point()

p4 <- p4 + geom_segment(data = m[which(m$Origin == "DAL"),],
                        aes(xend = c(tail(DepDelay, n = -1), NA), 
                            yend = c(tail(AirTime, n = -1), NA)),
                        arrow = arrow(length = unit(0.3, "cm")),
                        color = "blue2")

p4 <- p4 + geom_segment(data = m[which(m$Origin == "ORD"),],
                        aes(xend = c(tail(DepDelay, n = -1), NA), 
                            yend = c(tail(AirTime, n = -1), NA)),
                        arrow = arrow(length = unit(0.3, "cm")),
                        color = "red2")

p4 <- p4 + geom_point(size = 2)
p4 <- p4 + geom_text(aes(label = Year, x = DepDelay, y =AirTime + 1), color = "darkgreen")
p4 <- p4 + scale_color_manual(values = c("blue2", "red2"))
p4 <- p4 + labs(
  title = "Title",
  x = "Air Time",
  y = "Departure Delay"
)
p4 <- p4 + 
  theme(
    plot.title = element_text(size = 15, face = "bold"),
    plot.subtitle = element_text(size = 10, face = "bold.italic", color = "blue"),
    plot.caption = element_text(size = 7, color = "blue", face = "italic"),
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size= 12),
    legend.title = element_blank(),
    legend.text = element_text(size = 10),
    legend.position = c(0.8, 0.8),
    legend.background = element_rect(color = "transparent", fill = "transparent"),
    legend.key = element_rect(color = "transparent", fill = "transparent")
  )

p4





