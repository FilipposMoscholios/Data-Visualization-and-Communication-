#Install and import libraries
install.packages("ggplot2")
install.packages("skimr")
install.packages("tidyverse")
install.packages("magrittr") 
install.packages("dplyr")   
install.packages("GGally")
install.packages("eurostat")
install.packages("leaflet")
install.packages("sf")
install.packages("rnaturalearth")
install.packages("rnaturalearthdata")
install.packages("patchwork")

library(patchwork)
library(rnaturalearth)
library(rnaturalearthdata)
library(leaflet)
library(eurostat)
library(tidyverse)
library(sf)
library(scales)
library(cowplot)
library(ggthemes)
library("ggrepel")
library(GGally)
library(ggplot2)
library(tidyverse)
library(magrittr) 
library(dplyr)   
library(corrplot)
library(hrbrthemes)
library(lattice)

#Read the data and store it to a dataframe
data <- read.csv(file = 'C:\\***\\hlth_cd_asdr2_1_Data.csv', header = TRUE)#pass path file's 

#data preprocessing by cleaning some useless columns from our dataframe and convert some features as factors
data <- data[,-c(5,6,8)]
data["GEO"][data["GEO"] == "Germany (until 1990 former territory of the FRG)"] <- "Germany"
data["GEO"][data["GEO"] == "European Union - 28 countries (2013-2020)"] <- "EU-28 Countries 2013-2020"
data["GEO"][data["GEO"] == "Czechia"] <- "Czech Rep."

data[, 'TIME'] <- as.factor(data[, 'TIME'])
data[, 'SEX'] <- as.factor(data[, 'SEX'])
data[, 'AGE'] <- as.factor(data[, 'AGE'])
data[, 'GEO'] <- as.factor(data[, 'GEO'])
#afterwards converting to numeric the Value column and deal with the ":" values(no available data), converted in Na values as it must be from documentation
data[, 'Value'] <- as.numeric(data[, 'Value'])
#get some look for our transformed data
#View(data)
#get some exploration and statistics 
summary(data)

#################################################################################################################

## visualizations
###visualization 1 barplot with zoomed barplot because of different scale

df_greece <-filter(data, SEX != "Total" & AGE != 'Total'& GEO=='Greece')
greece_exploratory<-  ggplot(df_greece,aes(TIME,Value))+
  geom_col(aes(TIME,Value,fill=SEX), position = "dodge") +
  geom_text(aes(label = Value, group = SEX),  position = position_dodge(width = .9),vjust=-1) +facet_wrap(~AGE)+
  theme(panel.background = element_rect(fill = "transparent"))+
  theme(legend.position = c(0.60, 0.55))+ labs(title ="Death rate from Dementia per 100k people, Males Vs Females for 65+ and 65- ages for Greece over time",
     x = "Time",y="Death rate")+theme(text = element_text(size = 20))
greece_exploratory_genders_scale <- ggplot(filter(data, SEX != "Total" & AGE == 'Less than 65 years'& GEO=='Greece'),aes(TIME,Value))+
  geom_col(aes(TIME,Value,fill=SEX), position = "dodge",show.legend = FALSE) +
  geom_text(aes(label = Value, group = SEX),  position = position_dodge(width = .93),vjust=-1) +
  theme(panel.background = element_rect(fill = "transparent"))+xlab("")+ylab("")
greece_exploratory <- greece_exploratory + inset_element(greece_exploratory_genders_scale, left = 0.6, bottom = 0.6, right = 1, top = 1)
print(greece_exploratory)

#################################################################################################################

###visualization 2 boxplots with zoomed boxoplots because of different scale 
V2<- data %>%
  filter(SEX == 'Total'&GEO!="EU-28 Countries 2013-2020" &AGE != 'Total') %>%
  mutate(MAX_DEATHS_APPEARANCE=ifelse(TIME=="2018","Most deadly year for 65+","More normal behavior")) %>%
  ggplot(aes(x= TIME, y=Value,fill=MAX_DEATHS_APPEARANCE)) +
  geom_boxplot()+ylab('Death rate')+xlab('Time')+
  scale_fill_manual(values=c("#46B4AF", "#B4464B"))+
  theme(legend.position = c(0.78, 0.48))+
  labs(title="Death rate from Dementia trend line and statistics for all countries for 65+ vs 65- ages, for both gender over time")+
  theme(panel.background = element_rect(fill = "transparent"))+
  theme(text = element_text(size = 15))+facet_wrap(~AGE)+
  stat_summary(
    fun = median,
    geom = 'line',
    aes(group = TIME),
    group=1,
    lwd=0.8,
    position = position_dodge(width = 0.9) 
  )
v2_zoom_65_less <- filter(data, SEX == 'Total'&GEO!="EU-28 Countries 2013-2020" &AGE == 'Less than 65 years') %>%
  ggplot(aes(x= TIME, y=Value,fill="#46B4AF")) +
  geom_boxplot(show.legend = FALSE)+
  scale_fill_manual(values="#46B4AF")+
  theme(panel.background = element_rect(fill = "transparent"))+
  theme(text = element_text(size = 15))+xlab("")+ylab("")+
  stat_summary(
    fun = median,
    geom = 'line',
    aes(group = TIME),
    group=1,
    lwd=0.8,
    position = position_dodge(width = 0.9) 
  )
V2 <- V2 + inset_element(v2_zoom_65_less, left = 0.58, bottom = 0.61, right = 1, top = 1)
print(V2)

#################################################################################################################

## 3 - visualization barplot 

df_sexMF <-filter(data, SEX!='Total')
dtest= df_sexMF %>% mutate(ToHighlight = ifelse( GEO == 'Greece', "yes", "no" ) )
V3<-df_sexMF %>%
  filter(TIME == '2018'&AGE == '65 years or over'& Value!='NA') %>%
  mutate(name = fct_reorder(GEO, Value,.fun='median')) %>%
  mutate(ToHighlight = ifelse( GEO == "Greece", "yes", "no" ))%>%
  ggplot(aes(x= name, y=Value,fill=ToHighlight)) +
  geom_bar(stat="identity", colour = "black",show.legend = FALSE)+
  geom_text(aes(label=Value, y = Value + 20), position = position_dodge(width = 0.9), vjust=0.3, size=4.5)+
  coord_flip()+ theme_minimal() +facet_wrap(~SEX) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                                          panel.background = element_blank())+scale_fill_manual( values = c( "yes"="#4682B4", "no"="tomato" ), guide = "none")+
  xlab('Country')+ylab('Death rate')+
  ggtitle('Death rate from Dementia for each country for 65+ age per gender in the Most Deadly year 2018')+theme(text = element_text(size = 20))
print(V3)

#################################################################################################################

## visualization - 4 violin trellis

v<- data%>%
  filter(SEX!="Total"&GEO!="EU-28 Countries 2013-2020" &AGE == '65 years or over')%>%
  ggplot(aes(SEX, Value, fill = SEX))+
  ggtitle("Death rate from Dementia distribution and statistics per gender for all countries per year for 65+ age group")+
  xlab("")+ylab("Death rate")+
  geom_violin(show.legend=FALSE,trim = TRUE)+ 
  geom_boxplot(width = .2,show.legend=FALSE)+
  facet_wrap(~TIME)+theme(panel.background = element_rect(fill = "transparent"))+
  theme(text = element_text(size = 20))
print(v)

#################################################################################################################

## 5 - Visualization map 

world_map <- ne_countries(scale = 50, returnclass = 'sf')
european_union_countries <- c(unique(filter(data,GEO!='EU-28 Countries 2013-2020')$GEO),
                              as.factor(c("Moldova","Albania","Montenegro","Kosovo","Bosnia and Herz.","Macedonia")))
european_union_countries_map <- world_map %>%  filter(name %in% european_union_countries)
map <- european_union_countries_map %>% left_join(filter(data,AGE=="Total",SEX=="Total"&Value!="NA"), by = c("name" = "GEO"))

V5 <- ggplot(data = map) +
  geom_sf(mapping = aes(fill = Value)) +
  scale_fill_gradient(name = "Death Rate", high = "#990000", low = "#FFCCCC",na.value = "grey") +
  labs(title = "Death rate from dementia in European map for both genders and ages group") +
  geom_sf(data = st_sfc(st_point(c(21.5, 40)), crs = 4326),
          color = '#4682B4',lwd=4)+coord_sf(xlim =  c(-22, 55),ylim = c(33,70))+
  geom_sf_text(aes(label = name), colour = "black",position = position_dodge(0.5),lwd=3)+
  labs(x="",y="")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect("#CCE5FF"))+theme(legend.position = c(0.83, 0.50))+theme(text = element_text(size = 18))

print(V5)

#################################################################################################################

## 6 - visualization heat map 

data_heatmap <- filter(data, SEX=='Total' & AGE=="Total")
VHEATMAP<- ggplot(data_heatmap, aes(TIME, GEO, fill= Value)) + 
  scale_fill_gradient(name = "Death Rate", high = "#990000", low = "#FFCCCC",na.value ="light grey")+
  geom_tile(color = "white",
            linetype = 1)+
  theme(panel.background = element_blank())+
  labs(title="Deathrate from Dementia per 100k people, per year for each country", x="Time",y="Country")+
  geom_text(aes(label = Value), color = "black", size = 4) +
  theme(text = element_text(size = 20))+
  geom_point(aes(size="", shape = NA), colour = "light grey")+
  guides(size=guide_legend("No Data", override.aes=list(shape=15, size = 10)))
print(VHEATMAP)

#################################################################################################################

## 7 - visualization TRELLIS with trendline

V7<-data %>%
  filter( SEX != 'Total' & AGE == 'Total') %>%
  ggplot(aes(x=TIME,y= Value))  +
  stat_summary(fun = "mean", pch = 1, colour = "#417EB7", lwd=0.6)+
  stat_summary(fun = mean, geom = "line", group = 1, colour = "#417EB7",  lwd=0.8)+
  ggtitle('Average death rate trend from Dementia for both males and females over time for both age groups') +
  facet_wrap(~GEO, ncol = 7)+ylab('Death rate')+xlab("")+

  theme(plot.title = element_text(colour = "black", 
                                  hjust = 0.5, vjust = 1, size = 15),
        panel.spacing = unit(0.2, "in"),
        strip.background = element_rect(fill = "#BCBCBC", color = "black", size = 1),
        strip.text = element_text(face = 'bold',colour = "black"),
        legend.position = "right",
        panel.background = element_rect(fill = "transparent"),
        axis.text.x = element_text(angle = -50, vjust = 0.5),
        axis.title = element_text(face = "bold", size = 12),
        legend.title.align = 0.7,
        panel.border = element_rect(fill = "transparent", # Needed to add the border
                                    color = 'black',            # Color of the border
                                    size = 1)) 
print(V7)

#################################################################################################################

## 8 - scatterplot matrix with pearson correlation

df_FIILTER_COUNTRIES <-filter(data, GEO %in%  c("Czech Rep.", "Greece","Latvia", "Lithuania", "Serbia","Estonia"))
df_F_COUNTRIES <- df_FIILTER_COUNTRIES %>% group_by(GEO) %>% filter(SEX == 'Total'&AGE == 'Total')
#df_F_COUNTRIES_FM <- df_F_COUNTRIES %>% group_by(GEO) %>% filter(SEX == 'Total')
TEST <- df_F_COUNTRIES%>% group_by(TIME,GEO)%>%summarise(mean_run = mean(Value))
#View(TEST)
z = TEST  %>% group_by(TIME,GEO)
#z
gr <- z%>%filter(GEO=='Greece')
time <- gr$TIME
czechia <- z%>%filter(GEO=='Czech Rep.')
latvia <- z%>%filter(GEO=='Latvia')
lithuania <- z%>%filter(GEO=='Lithuania')
serbia <- z%>%filter(GEO=='Serbia')
est <- z%>%filter(GEO=='Estonia')

dk <- data.frame(
  TIME = as.factor(time),
  Czechia = log10(as.numeric(czechia$mean_run)),
  Greece = log10(as.numeric(gr$mean_run)),
  Latvia = log10(as.numeric(latvia$mean_run)),
  Lithuania = log10(as.numeric(lithuania$mean_run)),
  Serbia = log10(as.numeric(serbia$mean_run)),
  Estonia = log10(as.numeric(est$mean_run)))

#dk
# Correlation panel
panel.cor <- function(x, y){
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- round(cor(x, y), digits=2)
  txt <- paste0("R = ", r)
  cex.cor <- 1.8/strwidth(txt)
  text(0.5, 0.5, txt, cex = cex.cor * r)
}
# Customize upper panel
upper.panel<-function(x, y){
  points(x,y, pch = c(15,15), col = c("tomato", "#4682B4"),lwd=2.5)
}

# Create the plots
V8 <- pairs(dk[,1:6], 
      lower.panel = panel.cor,
      upper.panel = upper.panel,
      labels = c("Czech Rep.", "Greece","Latvia", "Lithuania", "Serbia","Estonia"),
      main="Log Scale(average Death Rate from dementia) for both genders with pearson correlations among selected countries for both gender and ages groups")

#################################################################################################################

## 9 - visualization trend detection with lineplot of selected countries

Colours_p <- c("red", "#4682B4", "orange", "dark green","black","purple")

V9<-df_FIILTER_COUNTRIES %>%
  filter( SEX != 'Total' & AGE == '65 years or over') %>%
  ggplot(aes(x=TIME,y= Value, color = GEO))  +
  stat_summary(fun = "mean", pch = 1, lwd=0.6,show.legend = FALSE)+
  stat_summary(aes(x = TIME, group = GEO), fun = "mean", geom = "line",  lwd=0.8,show.legend = FALSE)+ scale_color_manual(values = Colours_p)+
  ggtitle('')+theme_minimal()  + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                       panel.background = element_blank())+ 
  theme(panel.background = element_rect(fill = 'light gray', colour = 'black'))+
  labs(title = "Death rate from dementia trend over time for selected countries for 65+ age",
       x="Time",
       y="Average death rate")+theme(text = element_text(size = 18))

V9 = V9 +  annotate(geom = "text", x = 9.4, y = 42, label = "Greece", hjust = "right",col="#4682B4")+
  annotate(geom = "text", x = 0.65, y = 14.7, label = "Greece", hjust = "left",col="#4682B4")+
  annotate(geom = "text", x = 9.5, y = 98, label = "Czech Rep.", hjust = "right",col="red")+
  annotate(geom = "text", x = 0.9, y = 55, label = "Czech Rep.", hjust = "right",col="red")+
  annotate(geom = "text", x = 9.3, y = 68, label = "Latvia", hjust = "right",col="dark orange")+
  annotate(geom = "text", x = 0.9, y = 4.5, label = "Latvia", hjust = "right",col="dark orange")+
  annotate(geom = "text", x = 9.4, y = 120, label = "Lithuania", hjust = "right",col="dark green")+
  annotate(geom = "text", x = 0.9, y = 35, label = "Lithuania", hjust = "right",col="dark green")+
  annotate(geom = "text", x = 9.4, y = 45.5, label = "Serbia", hjust = "right", col="black")+
  annotate(geom = "text", x = 0.9, y = 7.5, label = "Serbia", hjust = "right", col="black")+
  annotate(geom = "text", x = 9.4, y = 65, label = "Estonia", hjust = "right",col="purple")+
  annotate(geom = "text", x = 0.9, y = 60.5, label = "Estonia", hjust = "right",col="purple")

print(V9)

#################################################################################################################

## boxplot comparison with trendline  and highlighted Greece vs other selected countries as group

df_fc_lw_65_gender<- filter(df_FIILTER_COUNTRIES, SEX!="Total" & AGE=="Less than 65 years")

V10<-
  ggplot(df_fc_lw_65_gender,aes(x= TIME, y=Value)) +
  geom_boxplot(data = filter(df_fc_lw_65_gender, GEO != "Greece"),aes(fill=SEX))+
  ylab('Death rate')+xlab('Time')+
  labs(title="Death rate from Dementia Trend and statistics between Selected countries VS Greece per gender, over Time for 65-")+
  theme(panel.background = element_rect(fill = "transparent"))+
  theme(text = element_text(size = 15))+
  stat_summary(
    fun = median,
    geom = 'line',
    lwd=0.8,
    position = position_dodge(width = 0.9), aes(group = SEX, colour =SEX) 
  )
V10<-V10+ geom_boxplot(data = filter(df_fc_lw_65_gender, GEO == "Greece"),aes(fill=SEX),lwd=1.5)+
  theme(legend.position = c(0.51, 0.70))+
  annotate("text", x = 4.75, y = 0.43, label = "Greek Females",colour="#F8766D",lwd=5)+
  annotate("text", x = 5.28, y = 0.39, label = "Greek Males",colour="#00BFC4",lwd=5)+
  annotate("rect", xmin = 4.34, xmax = 5.63, ymin = 0.30, ymax = 0.50,
           alpha = .2,color="black",fill = "dark grey")+theme(text = element_text(size = 18))+
  annotate("text",x = 1.5, y = 0.62, label = "Czech Rep.")+
  annotate("text",x = 1.5, y = 0.66, label = "Latvia")+
  annotate("text",x = 1.5, y = 0.70, label = "Lithuania")+
  annotate("text",x = 1.5, y = 0.74, label =  "Serbia")+
  annotate("text",x = 1.5, y = 0.78, label = "Estonia")+
  annotate("text",x = 1.5, y = 0.82, label = "Selected Countries",lwd=6)

print(V10)

#################################################################################################################
