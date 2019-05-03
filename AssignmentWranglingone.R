library(lubridate)
library(ggplot2)
library(tidyverse)
library(dplyr)
library(digest)
library(evaluate)
HOUR<-"hour.csv" %>%
read_csv(col_names = TRUE)
glimpse(HOUR)
head(HOUR)

#Working on bike_df, show that you can operate on rows: arranging them, getting the top _n_ according to some variable, filtering them (eventually using your own functions).
#hint useful functions: arrange(), top_n(), filter()
#hint to define a function: myfunction <- function(myargument,anotherargument) { bodyofthefunction }

Q2<-HOUR %>% 
  top_n(5000) %>%
  arrange(desc(temp))  %>%
  filter((hr >=0),
         !is.na(temp),
         !is.na(atemp))

is.na.data.frame(Q2)
         


#Task (c): working on columns (12 marks)
#Working on bike_df, show that you can operate on columns: selecting some of them, dropping other, renaming them, reordering them, ...
#hint useful functions: select(), rename(), ...

      
Q3<-Q2%>% 
    select(season,weathersit,hum,temp,dteday,atemp,windspeed,cnt,hr,everything()) %>%
    select(1:9) %>% 
    rename(Weather=weathersit,
           Temperature=temp,
           Season=season,
           Total_Rentals = cnt,
           Normalwindspeed = windspeed,
           Humidity = hum,
           ActualFeelTemp = atemp,
           Dates=dteday,
           Hour= hr
           )
Q3
#Task (d): groups and not (12 marks)
#Working on bike_df, show that you can produce new variables (columns) and modify existing one; show also that you can operate on the dataframe 
#as a whole and on groups. See Readme.txt to get an understanding of the variable scaling applied.
#hint useful functions: mutate(), group_by(), summarise(), tally(), ...
Q4<-Q3 %>%
    mutate(Temperature=round(Temperature*41,1),
         Normalwindspeed=round(Normalwindspeed*67,1),
         ActualFeelTemp=round(ActualFeelTemp*50,1),
         Humidity=round(Humidity*100,1),
         WeatherName = case_when(
           Weather == 1 ~ "Clear",
           Weather == 2 ~ "Cloudy",
           Weather == 3 ~ "Wet",
           Weather == 4 ~ "Severe"),
         SeasonName = case_when(
           Season == 1 ~ "Spring",
           Season == 2 ~ "Summer",
           Season == 3 ~ "Fall",
           Season == 4 ~ "Winter"),
         Year = lubridate::year(Dates), 
         Month = lubridate::month(Dates), 
         Day = lubridate::day(Dates)) %>%
  group_by(Hour,Year,Month,Day,WeatherName,Total_Rentals,SeasonName) %>%
  summarize(Avg.Temperature=mean(Temperature+ActualFeelTemp)/2) %>%
  glimpse()
Q4
#Q5. The data is well structured in terms of the columns having headings, 
# which are legit. the values of the variables do not overlap the headings,
#
Q6_1 <-ggplot(Q4,aes(Avg.Temperature,Total_Rentals))+
  geom_point(aes(col=WeatherName))+
  coord_cartesian(xlim=c(0,50), ylim=c(200,1000))+
  scale_colour_brewer(palette = "Set1")+
  labs(title ="Average Temperature Vs Total_Rentals ", subtitle = "From the hours/bike dataset" 
  ,y = "Total number of cycle rentals",x ="Average Temperature",caption = "2011/2012 Washington Bike Sharing ")+
  theme_set(theme_light())+
  theme(legend.position = "right")
Q6_1


Q6_2 <- read.csv("Hour.csv")
Q6_2$day <- wday(ymd(Q6_2$dteday), label = TRUE)
ggplot(Q6_2, aes(hr,cnt,colour = day)) + geom_smooth(ce=FALSE, fill=NA) +
ggtitle("Hourly Trend of bikes rented on weekdays and weekends") + scale_x_continuous( breaks = seq(0,24,by=1))+
  scale_colour_brewer(palette = "Dark2")+
  labs(title ="Hour Vs Total_Rentals ", subtitle = "From the Hours/Bike dataset" 
       ,y = "Total number of cycle rentals",x ="Hour",caption = "2011/2012 Washington Bike Sharing ")+
  theme_set(theme_light())+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  theme(legend.position = "right")
Q6_2

Q6.3<-qplot( x=SeasonName , y=Total_Rentals , data=Q4 , geom=c("boxplot","jitter") , fill=SeasonName)+
  ggtitle(" Trend of bikes rented by season")+
  labs(title ="SeasonName Vs Total_Rentals ", subtitle = "From the Hours/Bike dataset" 
  ,y = "Total number of cycle rentals",x ="Season",caption = "2011/2012 Washington Bike Sharing ")+
  theme_set(theme_light())+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  theme(legend.position = "right")
Q6.3


Q6.4<-qplot( x=WeatherName , y=Total_Rentals , data=Q4 , geom=c("boxplot","jitter") , fill=WeatherName)+
  ggtitle(" Trend of bikes rented by season")+
  labs(title ="WeatherName Vs Total_Rentals ", subtitle = "From the Hours/Bike dataset" 
       ,y = "Total number of cycle rentals",x ="Weather",caption = "2011/2012 Washington Bike Sharing ")+
  theme_set(theme_light())+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  theme(legend.position = "right")
Q6.4


         
         
         
         
       
