##PLOTS##

##################################
########  AVERAGES  ##############
##################################


#####LIGHTS################LIGHTS#####LIGHTS###########LIGHTS###########LIGHTS######
#####LIGHTS################LIGHTS#####LIGHTS###########LIGHTS###########LIGHTS######


db<-read.csv("AVLights.csv")
dayLabs<-c("Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday") 
db$Weekday <- factor(db$Weekday, levels= dayLabs)
db<-db[order(db$Weekday), ]



#Plot aggregation on date
PlotLights <- ggplot(data=db, aes(x=Weekday, y=TOTAL)) +
  geom_bar(stat="identity",fill="lightsteelblue2")+
  geom_text(aes(y=db$TOTAL, label=TOTAL), vjust=-0.5, 
            color="gray17", size=5)+
  #scale_x_date(date_labels="%d %b",date_breaks  ="2 day")+
  coord_cartesian(ylim=c(1,12)) +
  scale_y_continuous(breaks=seq(0,12,2))+
  theme_classic()+
  labs(title="", x="", y = "Value") +
  theme(plot.title = element_text(color="lightsteelblue2", size=22, face="bold",hjust=0.5),
        axis.text.x = element_text(size=15,colour="grey20"),
        axis.text.y = element_text(size=15,colour="grey20"),
        axis.title.y = element_text(size=15,colour="grey20"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),#Background Transparant
        panel.background = element_rect(fill = "transparent",colour = NA),#######Background Transparant
        plot.background = element_rect(fill = "transparent",colour = NA))
PlotLights

######Movement######Movement######Movement######Movement######Movement######Movement######
#Movement######Movement######Movement######Movement######Movement######Movement#####Movement

db<-read.csv("AVMOVEMENT.csv")
dayLabs<-c("Sun","Mon","Tue","Wed","Thu","Fri","Sat") 
db$DayOfTheWeek <- factor(db$DayOfTheWeek, levels= dayLabs)
db<-db[order(db$DayOfTheWeek), ]
db$TOTAL <- db$TOTAL/4

#Plot aggregation on date
PlotMovement<- ggplot(data=db, aes(x=DayOfTheWeek, y=TOTAL)) +
  geom_bar(stat="identity",fill="lightsteelblue2")+
  geom_text(aes(y=db$TOTAL, label=TOTAL), vjust=-0.5, 
            color="gray17", size=5)+
  #scale_x_date(date_labels="%d %b",date_breaks  ="2 day")+
  coord_cartesian(ylim=c(10,100)) +
  scale_y_continuous(breaks=seq(10,100,10))+
  theme_classic()+
  labs(title="", x="", y = "Value") +
  theme(plot.title = element_text(color="lightsteelblue2", size=22, face="bold",hjust=0.5),
        axis.text.x = element_text(size=15,colour="grey20"),
        axis.text.y = element_text(size=15,colour="grey20"),
        axis.title.y = element_text(size=15,colour="grey20"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),#Background Transparant
        panel.background = element_rect(fill = "transparent",colour = NA),#######Background Transparant
        plot.background = element_rect(fill = "transparent",colour = NA))
PlotMovement

###### POWER ###### POWER ###### POWER ###### POWER ###### POWER ###### POWER ######
# POWER ###### POWER ###### POWER ###### POWER ###### POWER ###### POWER ##### POWER

db<-read.csv("NEWAVPOW.csv")
dayLabs<-c("Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday") 
db$Weekday <- factor(db$Weekday, levels= dayLabs)
db<-db[order(db$Weekday), ]

#Plot aggregation on date
PlotPower<- ggplot(data=db, aes(x=Weekday, y=IsItOn)) +
  geom_bar(stat="identity",fill="lightsteelblue2")+
  #geom_text(aes(y=db$IsItOn, label=IsItOn), vjust=-0.5, 
  #          color="gray17", size=3.1)+
  #scale_x_date(date_labels="%d %b",date_breaks  ="2 day")+
  coord_cartesian(ylim=c(0.1,1)) +
  scale_y_continuous(breaks=seq(0,0,0))+
  theme_classic()+
  labs(title="TVs On (Average)", x="", y = "NO                                                                       YES") +
  theme(plot.title = element_text(color="lightsteelblue2", size=22, face="bold",hjust=0.5),
        axis.text.x = element_text(size=15,colour="grey20"),
        axis.text.y = element_text(size=15,colour="grey20"),
        axis.title.y = element_text(size=15,colour="grey20"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),#Background Transparant
        panel.background = element_rect(fill = "transparent",colour = NA),#######Background Transparant
        plot.background = element_rect(fill = "transparent",colour = NA))
PlotPower

#####COFFEE################COFFEE#####COFFEE###########COFFEE###########COFFEE######
#####COFFEE################COFFEE#####COFFEE###########COFFEE###########COFFEE######


db<-read.csv("AVOFCOFFEE.csv")
dayLabs<-c("Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday") 
db$WeekDay <- factor(db$WeekDay, levels= dayLabs)
db<-db[order(db$WeekDay), ]



#Plot aggregation on date
PlotCoffee <- ggplot(data=db, aes(x=WeekDay, y=Total)) +
  geom_bar(stat="identity",fill="lightsteelblue2")+
  geom_text(aes(y=db$Total, label=Total), vjust=-0.5, 
            color="gray20", size=5)+
  #scale_x_date(date_labels="%d %b",date_breaks  ="2 day")+
  coord_cartesian(ylim=c(1,15)) +
  scale_y_continuous(breaks=seq(1,15,2))+
  theme_classic()+
  labs(title="", x="", y = "Coffees") +
  theme(axis.text.x = element_text(size=15,colour="grey20"),
        axis.text.y = element_text(size=15,colour="grey20"),
        axis.title.y = element_text(size=15,colour="grey20"),
        plot.title = element_text(color="lightsteelblue2", size=22, face="bold",hjust=0.5),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),#Background Transparant
        panel.background = element_rect(fill = "transparent",colour = NA),#######Background Transparant
        plot.background = element_rect(fill = "transparent",colour = NA))
PlotCoffee


##################################
########  APRIL  ##############
##################################

#MOVEMENT################MOVEMENT##########MOVEMENT###########MOVEMENT###########MOVEMENT
##########MOVEMENT################MOVEMENT##########MOVEMENT###########MOVEMENT###########
#MOVEMENT################MOVEMENT##########MOVEMENT###########MOVEMENT###########MOVEMENT


db<-read.csv("MovementFri-Sun.csv")
db$Date <- as.Date(db$Date)



#Just April Size 3.8 5.1
PlotMovementsSmall <- ggplot(data=db, aes(x=Date, y=Movements)) +
  geom_bar(stat="identity",fill="lightsteelblue3")+
  geom_text(aes(y=db$Movements, label=Movements), vjust=-0.5, 
            color="gray17", size=4)+
  scale_x_date(date_breaks  ="1 day", date_labels="%d %b")+
  coord_x_date(xlim = c("2017-04-07", "2017-04-13"), ylim = c(10, 70))+
  scale_y_continuous(breaks=seq(10,70,10))+
  theme_classic()+
  labs(title="", x="", y = "") +
  theme(plot.title = element_text(color="lightsteelblue2", size=22, face="bold",hjust=0.5),
        axis.text.x = element_text(size=12,colour="grey20"),
        axis.text.y = element_text(size=12,colour="grey20"),
        axis.title.y = element_text(size=15,colour="grey20"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),#Background Transparant
        panel.background = element_rect(fill = "transparent",colour = NA),#######Background Transparant
        plot.background = element_rect(fill = "transparent",colour = NA))
PlotMovementsSmall

###All Dates
db<-read.csv("AllMovementsGrouped.csv")
db$Date <- as.Date(db$Date)

#Whole Dataset SIZE 8-13
PlotMovementsAll <- ggplot(data=db, aes(x=Date, y=Movements)) +
  geom_bar(stat="identity",fill="lightsteelblue2")+
  geom_text(aes(y=db$Movements, label=Movements), vjust=-0.5, 
            color="gray17", size=4)+
  scale_x_date(date_breaks  ="2 day", date_labels="%d %b")+
  coord_x_date(xlim = c("2017-03-07", "2017-04-13"), ylim = c(10, 220))+
  scale_y_continuous(breaks=seq(10,220,10))+
  theme_classic()+
  labs(title="", x="", y = "Movements") +
  theme(plot.title = element_text(color="lightsteelblue2", size=22, face="bold",hjust=0.5),
        axis.text.x = element_text(size=12,colour="grey20"),
        axis.text.y = element_text(size=15,colour="grey20"),
        axis.title.y = element_text(size=15,colour="grey20"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),#Background Transparant
        panel.background = element_rect(fill = "transparent",colour = NA),#######Background Transparant
        plot.background = element_rect(fill = "transparent",colour = NA))
PlotMovementsAll

#LIGHTS##############LIGHTS#######LIGHTS#########LIGHTS###########LIGHTS######LIGHTS######LIGHTS
#######LIGHTS##############LIGHTS########LIGHTS#########LIGHTS###########LIGHTS######LIGHTS######
#LIGHTS##############LIGHTS#######LIGHTS#########LIGHTS###########LIGHTS######LIGHTS######LIGHTS

db<-read.csv("NumOfHoursPerDayTotal.csv")
db$date <- as.Date(db$date)



#Just April Size 3.8 5.1
PlotLightsSmall <- ggplot(data=db, aes(x=date, y=HoursLightsOn)) +
  geom_bar(stat="identity",fill="lightsteelblue3")+
  geom_text(aes(y=db$HoursLightsOn, label=HoursLightsOn), vjust=-0.5, 
            color="gray17", size=4)+
  scale_x_date(date_breaks  ="1 day", date_labels="%d %b")+
  coord_x_date(xlim = c("2017-04-07", "2017-04-13"), ylim = c(1, 12))+
  scale_y_continuous(breaks=seq(1,12,2))+
  theme_classic()+
  labs(title="", x="", y = "") +
  theme(plot.title = element_text(color="lightsteelblue2", size=22, face="bold",hjust=0.5),
        axis.text.x = element_text(size=12,colour="grey20"),
        axis.text.y = element_text(size=12,colour="grey20"),
        axis.title.y = element_text(size=15,colour="grey20"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),#Background Transparant
        panel.background = element_rect(fill = "transparent",colour = NA),#######Background Transparant
        plot.background = element_rect(fill = "transparent",colour = NA))
PlotLightsSmall


#Whole Dataset SIZE 5-13
PlotLightsAll <- ggplot(data=db, aes(x=date, y=HoursLightsOn)) +
  geom_bar(stat="identity",fill="lightsteelblue2")+
  geom_text(aes(y=db$HoursLightsOn, label=HoursLightsOn), vjust=-0.5, 
            color="gray17", size=4)+
  scale_x_date(date_breaks  ="2 day", date_labels="%d %b")+
  coord_x_date(xlim = c("2017-03-07", "2017-04-13"), ylim = c(1, 13))+
  scale_y_continuous(breaks=seq(1,13,2))+
  theme_classic()+
  labs(title="", x="", y = "Hours") +
  theme(plot.title = element_text(color="lightsteelblue2", size=22, face="bold",hjust=0.5),
        axis.text.x = element_text(size=12,colour="grey20"),
        axis.text.y = element_text(size=15,colour="grey20"),
        axis.title.y = element_text(size=15,colour="grey20"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),#Background Transparant
        panel.background = element_rect(fill = "transparent",colour = NA),#######Background Transparant
        plot.background = element_rect(fill = "transparent",colour = NA))
PlotLightsAll

#COFFEE##############COFFEE#######COFFEE#########COFFEE###########COFFEE######COFFEE#######COFFEE
#######COFFEE##############COFFEE########COFFEE#########COFFEE###########COFFEE######COFFEE######
#COFFEE##############COFFEE#######COFFEE#########COFFEE###########COFFEE######COFFEE#######COFFEE

db<-read.csv("AllCoffeeGrouped.csv")
db$Date <- as.Date(db$Date)



#Just April Size 3.8 5.1
PlotCoffeeSmall <- ggplot(data=db, aes(x=Date, y=Coffees)) +
  geom_bar(stat="identity",fill="lightsteelblue3")+
  geom_text(aes(y=db$Coffees, label=Coffees), vjust=-0.5, 
            color="gray17", size=4)+
  scale_x_date(date_breaks  ="1 day", date_labels="%d %b")+
  coord_x_date(xlim = c("2017-04-07", "2017-04-13"), ylim = c(2, 25))+
  scale_y_continuous(breaks=seq(2,25,2))+
  theme_classic()+
  labs(title="", x="", y = "") +
  theme(plot.title = element_text(color="lightsteelblue2", size=22, face="bold",hjust=0.5),
        axis.text.x = element_text(size=12,colour="grey20"),
        axis.text.y = element_text(size=12,colour="grey20"),
        axis.title.y = element_text(size=15,colour="grey20"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),#Background Transparant
        panel.background = element_rect(fill = "transparent",colour = NA),#######Background Transparant
        plot.background = element_rect(fill = "transparent",colour = NA))
PlotCoffeeSmall


#Whole Dataset SIZE 8-13
PlotCoffeeAll <- ggplot(data=db, aes(x=Date, y=Coffees)) +
  geom_bar(stat="identity",fill="lightsteelblue2")+
  geom_text(aes(y=db$Coffees, label=Coffees), vjust=-0.5, 
            color="gray17", size=4)+
  scale_x_date(date_breaks  ="2 day", date_labels="%d %b")+
  coord_x_date(xlim = c("2017-03-07", "2017-04-13"), ylim = c(2, 25))+
  scale_y_continuous(breaks=seq(2,25,2))+
  theme_classic()+
  labs(title="", x="", y = "Coffees") +
  theme(plot.title = element_text(color="lightsteelblue2", size=22, face="bold",hjust=0.5),
        axis.text.x = element_text(size=12,colour="grey20"),
        axis.text.y = element_text(size=15,colour="grey20"),
        axis.title.y = element_text(size=15,colour="grey20"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),#Background Transparant
        panel.background = element_rect(fill = "transparent",colour = NA),#######Background Transparant
        plot.background = element_rect(fill = "transparent",colour = NA))
PlotCoffeeAll

#POWER##############POWER#######POWER#########POWER###########POWER######POWER#######POWER
#######POWER############POWER########POWER#########POWER###########POWER######POWER######
#POWER##############POWER#######POWER#########POWER###########POWER######POWER#######POWER


db<-read.csv("AllPowerGrouped.csv")
db$Date <- as.Date(db$Date)
db$TvOnLenghtThousands <- round(db$TvOnLenght/1000, 2)


#Just April Size 3.8 5.1
PlotPowerSmall <- ggplot(data=db, aes(x=Date, y=TvOnLenghtThousands)) +
  geom_bar(stat="identity",fill="lightsteelblue3")+
  geom_text(aes(y=db$TvOnLenghtThousands, label=TvOnLenghtThousands), vjust=-0.5, 
            color="gray17", size=4)+
  scale_x_date(date_breaks  ="1 day", date_labels="%d %b")+
  coord_x_date(xlim = c("2017-04-07", "2017-04-13"), ylim = c(1, 22))+
  scale_y_continuous(breaks=seq(1,22,2.5))+
  theme_classic()+
  labs(title="", x="", y = "") +
  theme(plot.title = element_text(color="lightsteelblue2", size=22, face="bold",hjust=0.5),
        axis.text.x = element_text(size=12,colour="grey20"),
        axis.text.y = element_text(size=12,colour="grey20"),
        axis.title.y = element_text(size=15,colour="grey20"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),#Background Transparant
        panel.background = element_rect(fill = "transparent",colour = NA),#######Background Transparant
        plot.background = element_rect(fill = "transparent",colour = NA))
PlotPowerSmall


#Whole Dataset SIZE 8-13
PlotPowerAll <- ggplot(data=db, aes(x=Date, y=TvOnLenghtThousands)) +
  geom_bar(stat="identity",fill="lightsteelblue2")+
  geom_text(aes(y=db$TvOnLenghtThousands, label=TvOnLenghtThousands), vjust=-0.5, 
            color="gray17", size=3.5)+
  scale_x_date(date_breaks  ="2 day", date_labels="%d %b")+
  coord_x_date(xlim = c("2017-03-07", "2017-04-13"), ylim = c(1, 22))+
  scale_y_continuous(breaks=seq(1,22,2.5))+
  theme_classic()+
  labs(title="", x="", y = "Lenght in thousands") +
  theme(plot.title = element_text(color="lightsteelblue2", size=22, face="bold",hjust=0.5),
        axis.text.x = element_text(size=12,colour="grey20"),
        axis.text.y = element_text(size=15,colour="grey20"),
        axis.title.y = element_text(size=15,colour="grey20"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),#Background Transparant
        panel.background = element_rect(fill = "transparent",colour = NA),#######Background Transparant
        plot.background = element_rect(fill = "transparent",colour = NA))
PlotPowerAll

#Weather######Weather#######Weather#########Weather###########Weather######Weather#######Weather
#######Weather###########Weather########Weather#########Weather###########Weather######Weather######
#Weather#######Weather#######Weather#########Weather###########Weather######Weather#######Weather

db<-read.csv("Weather.csv")
db$Date <- as.Date(db$Date)

#Plot aggregation on date 3 - 10
PlotWeather <- ggplot(data=db, aes(x=Date, y=TotalProp)) +
  geom_bar(stat="identity",fill="lightsteelblue2")+
  geom_text(aes(y=db$TotalProp, label=TotalProp), vjust=-0.5, 
            color="gray17", size=5)+
  scale_x_date(date_labels="%d %b",date_breaks  ="1 day")+
  coord_cartesian(ylim=c(0.05,1)) +
  scale_y_continuous(breaks=seq(0.1,1,0.2))+
  theme_classic()+
  labs(title="", x="", y = "Value") +
  theme(plot.title = element_text(color="lightsteelblue2", size=22, face="bold",hjust=0.5),
        axis.text.x = element_text(size=15,colour="grey20"),
        axis.text.y = element_text(size=15,colour="grey20"),
        axis.title.y = element_text(size=15,colour="grey20"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),#Background Transparant
        panel.background = element_rect(fill = "transparent",colour = NA),#######Background Transparant
        plot.background = element_rect(fill = "transparent",colour = NA))
PlotWeather


#SCOREBOARD########SCOREBOARD#######SCOREBOARD#########SCOREBOARD###########SCOREBOARD######SCOREBOARD#######POWER
#######SCOREBOARD###########SCOREBOARD########SCOREBOARD#########SCOREBOARD###########SCOREBOARD######SCOREBOARD######
#SCOREBOARD#########SCOREBOARD#######SCOREBOARD#########SCOREBOARD###########SCOREBOARD######SCOREBOARD#######POWER

#1 April Week

library(reshape2) 
ScoreBoardPlot <- ScoreBoard
ScoreBoardPlot$Coffee <- ScoreBoard$CPCoffee*ScoreBoard$RainMultiplier
ScoreBoardPlot$Power <- ScoreBoard$CPPower*ScoreBoard$RainMultiplier
ScoreBoardPlot$Light <- ScoreBoard$CPLight*ScoreBoard$RainMultiplier
ScoreBoardPlot$Movements <- ScoreBoard$CPMovements*ScoreBoard$RainMultiplier


#Remove useless columns
ScoreBoardPlot <- ScoreBoardPlot[,c(1,8:11)]

#Turn Around and rename
ScoreBoardPlot <- melt(ScoreBoardPlot, id.vars=c("Weekday"))
names(ScoreBoardPlot) <- c("Weekday","Sensor","CleaningPoints")


#Plot
library(ggplot2)
ggplot(data=ScoreBoardPlot, aes(x=Weekday, y=CleaningPoints, fill=Sensor), break(31)) +
  geom_bar(stat="identity")+
  coord_cartesian(ylim=c(25,300))+
  scale_fill_brewer()+
  geom_text(aes(label=CleaningPoints), size = 4, color="gray17", position = position_stack(vjust = 0.83))+
  labs(title="", x="", y = "Cleaning Points")+
  theme_classic() + 
  scale_y_continuous(breaks=seq(25,300,25))+
  theme(legend.background = element_rect(fill="white",size=0.3, linetype="solid",colour ="lightsteelblue3"),
        legend.text=element_text(size=12),
        legend.title=element_text(size=15),
        legend.position = c(.95, .95),
        legend.justification = c("right", "top"),
        legend.box.just = "right",plot.title = element_text(color="lightsteelblue2", size=22, face="bold",hjust=0.5),
        axis.text.x = element_text(size=12,colour="grey20"),
        axis.text.y = element_text(size=15,colour="grey20"),
        axis.title.y = element_text(size=15,colour="grey20"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),#Background Transparant
        panel.background = element_rect(fill = "transparent",colour = NA),#######Background Transparant
        plot.background = element_rect(fill = "transparent",colour = NA))

#####Averages
db<-read.csv("SCOREBOARD.CSV")

library(reshape2) 

db$Coffee <- db$Cleaning_Points.2*db$multiplier2
db$Power <- db$CleaningPoints*db$multiplier2
db$Light <- db$Cleaning_Points*db$multiplier2
db$Movements <- db$Cleaning_Points.1*db$multiplier2

#Remove useless columns
ScoreBoardPlot <- db[,c(2,10:13)]

#Turn Around and rename
ScoreBoardPlot <- melt(ScoreBoardPlot, id.vars=c("Weekday"))
names(ScoreBoardPlot) <- c("Weekday","Sensor","CleaningPoints")

#Order
levels(ScoreBoardPlot$Weekday)
dayLabs<-c("Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday") 
ScoreBoardPlot$Weekday <- factor(ScoreBoardPlot$Weekday, levels= dayLabs)
levels(ScoreBoardPlot$Weekday)
attributes(ScoreBoardPlot$Weekday)


#Plot
library(ggplot2)
ggplot(data=ScoreBoardPlot, aes(x=Weekday, y=CleaningPoints, fill=Sensor), break(31)) +
  geom_bar(stat="identity")+
  coord_cartesian(ylim=c(25,300))+
  scale_fill_brewer()+
  geom_text(aes(label=CleaningPoints), size = 4, color="gray17", position = position_stack(vjust = 0.7))+
  labs(title="", x="", y = "Cleaning Points")+
  theme_classic() + 
  scale_y_continuous(breaks=seq(25,300,25))+
  theme(legend.background = element_rect(fill="white",size=0.3, linetype="solid",colour ="lightsteelblue3"),
        legend.text=element_text(size=12),
        legend.title=element_text(size=15),
        legend.position = c(.95, .95), legend.justification = c("right", "top"), legend.box.just = "right",
        plot.title = element_text(color="lightsteelblue2", size=22, face="bold",hjust=0.5),
        axis.text.x = element_text(size=12,colour="grey20"),
        axis.text.y = element_text(size=15,colour="grey20"),
        axis.title.y = element_text(size=15,colour="grey20"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),#Background Transparant
        panel.background = element_rect(fill = "transparent",colour = NA),#######Background Transparant
        plot.background = element_rect(fill = "transparent",colour = NA))



levels(ScoreBoardPlot$Weekday)

## Reorder fullname based on the the sum of the other columns
ScoreBoardPlot$Weekday <- factor(ScoreBoardPlot$Weekday, levels= dayLabs)

## Examine the new factor order
levels(ScoreBoardPlot$Weekday)
attributes(ScoreBoardPlot$Weekday)



