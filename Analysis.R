######################
#### April Data ######
######################

##################################
########   movement ##############
##################################
library(jsonlite)
library(splitstackshape)

movement <- fromJSON("movement.json")

#Bind
#Select usefull data and create a dataframe
  MovementNew <- data.frame(cbind(movement$rows$doc$movement,movement$rows$doc$value,movement$rows$doc$sensorType,movement$rows$doc$date))

#As we need numeric data in value for a next step we converted it
  MovementNew$X1 <- as.numeric(as.character(MovementNew$X1))
  MovementNew$X2 <- as.numeric(as.character(MovementNew$X2))

#Combine Movement & Value to eliminate null values
  MovementNew$Comb <- ifelse(is.na(MovementNew$X1),MovementNew$X2 ,MovementNew$X1)

#Remove 0s, as 0s are sent automatically every 15sec even without any movement
#Just values for the sensortype "movement", as "coffee" is not correct (after a long detection of the values and dates)
  MovementNew <- subset(MovementNew, Comb %in% c('1'))
  MovementNew <- subset(MovementNew, X3 %in% c('movement'))
  
#Split the date into Date & Time and create a new column with date and time together
  MovementNew <- cSplit(MovementNew, c("X4"), "T")
  MovementNew <- cSplit(MovementNew, c("X4_2"), ".")
  MovementNew$DateTime <- as.POSIXct(paste(MovementNew$X4_1, MovementNew$X4_2_1), format="%Y-%m-%d %H:%M:%S")

#Change value from Factor into Date
  MovementNew$X4_1 <- as.Date(as.character(MovementNew$X4_1))

#Insert new column with weekdays
  MovementNew$WeekDay <- weekdays(MovementNew$X4_1)

#Removing useless columns and rename
  MovementNew <- MovementNew[,c(4,5,6,9,8)]
names(MovementNew) <- c("ValueCombined","Date","Time","WeekDay","DateTime")

#Creation of new Dataframe with the aggregation on Date and cleaning points
  library(dplyr)
  MovementGrouped <- data.frame(
  MovementNew %>%
    group_by(Date, WeekDay) %>%
    summarise(Movements = n()))
  MovementGrouped$Movements <- MovementGrouped$Movements/4
  MovementGrouped$CleaningPoints <- MovementGrouped$Movements * 2
  
#Exclude March dates
  MovementGrouped <- MovementGrouped[20:23,2:4]

#As we miss Friday, Saturday and Sunday we decided to import the before created weighted averages of those weekdays and include them here
  AverageMovements<-read.csv("AVMOVEMENT.csv")
  AverageMovements <- AverageMovements[,2:4]
  AverageMovements$Weekday <- ifelse(AverageMovements$DayOfTheWeek == "Sun","Sunday" ,
                                    ifelse(AverageMovements$DayOfTheWeek == "Mon","Monday" ,
                                    ifelse(AverageMovements$DayOfTheWeek == "Tue","Tuesday" ,
                                    ifelse(AverageMovements$DayOfTheWeek == "Wed","Wednesday",
                                    ifelse(AverageMovements$DayOfTheWeek == "Thu","Thursday" ,
                                    ifelse(AverageMovements$DayOfTheWeek == "Fri","Friday" ,"Saturday"))))))
  AverageMovements$Cleaning_Points <- AverageMovements$Cleaning_Points/4
  AverageMovements$TOTAL <- AverageMovements$TOTAL/4
  AverageMovements <- AverageMovements[c(1,6,7),c(4,2,3)]
  names(AverageMovements) <- c("WeekDay","Movements","CleaningPoints")

#Merge average of weekday and April dates, sort, include dates again
MovementTotal <- rbind(MovementGrouped, AverageMovements)
dayLabs<-c("Friday","Saturday","Sunday","Monday","Tuesday","Wednesday","Thursday") 
MovementTotal$WeekDay <- factor(MovementTotal$WeekDay, levels= dayLabs)
MovementTotal<-MovementTotal[order(MovementTotal$WeekDay), ]
MovementTotal$Date <- c("2017-04-07","2017-04-08","2017-04-09","2017-04-10","2017-04-11","2017-04-12","2017-04-13")
MovementTotal$Date <- as.Date(MovementTotal$Date)


###########################################################################################################################
##Coffee################Coffee##########Coffee#################Coffee##########################Coffee######################
###########################################################################################################################

Coffee <- fromJSON("coffee.json")

#Select usefull data and create a dataframe
  CoffeeNew <- data.frame(cbind(Coffee$rows$doc$value, Coffee$rows$doc$sensorType, Coffee$rows$doc$date))
  
#Split the string values and then remove "trash" columns created while splitting
  CoffeeNew <- cSplit(CoffeeNew, c("X1"), " ")
  CoffeeNew <- cSplit(CoffeeNew, c("X1_3"), ",") 
  CoffeeNew <- CoffeeNew[,c(2,3,7)]
  
#As we need numeric data in value for a next step we converted it
  CoffeeNew$X1_1 <- as.numeric(as.character(CoffeeNew$X1_1))
  CoffeeNew$X1_3_1 <- as.numeric(as.character(CoffeeNew$X1_3_1))
  
#Combonation of the two value columns
  CoffeeNew$Comb <- ifelse(is.na(CoffeeNew$X1_1),CoffeeNew$X1_3_1 ,CoffeeNew$X1_1)
  
#Split the date into Date & Time and create a new column with date and time together
  CoffeeNew <- cSplit(CoffeeNew, c("X3"), "T")
  CoffeeNew <- cSplit(CoffeeNew, c("X3_2"), ".")
  CoffeeNew$DateTime <- as.POSIXct(paste(CoffeeNew$X3_1, CoffeeNew$X3_2_1), format="%Y-%m-%d %H:%M:%S")
  
#Change value from Factor into Date
  CoffeeNew$X3_1 <- as.Date(as.character(CoffeeNew$X3_1))
  
#Insert new column with weekdays
  CoffeeNew$WeekDay <- weekdays(CoffeeNew$X3_1)
  
#Remove not needed columns and rename
  CoffeeNew <- CoffeeNew[,c(3,4,5,8,7)]
  names(CoffeeNew) <- c("CombValue","Date","Time","WeekDay","DateTime")

#Max per Date (The sensor adds one 1 per coffe, that's why we take the highest number as it's the last insertion = number of coffees that day)
  library(dplyr)
  CoffeeGrouped <- data.frame(
  CoffeeNew %>%
    group_by(Date, WeekDay) %>%
    summarise(Coffees = max(CombValue)))
  CoffeeGrouped$CleaningPoints <- CoffeeGrouped$Coffees * 5

#Creation of the final vector for relevant week
  FinalCoffee <- CoffeeGrouped[17:23,]
  CoffeeVector <- as.vector(FinalCoffee$CleaningPoints) #Creation of the final vector consisting of the cleaning points per day
  CoffeeVector  


  
####################### Power - TV ####################################################### Power - TV ################################
####################### Power - TV ####################################################### Power - TV ################################
####################### Power - TV ####################################################### Power - TV ################################
  library(jsonlite)
  library(splitstackshape)
  Power <- fromJSON("power.json")

#Select usefull data and create a dataframe
  PowerNew <- data.frame(cbind(Power$rows$doc$power, Power$rows$doc$date)) 
  
#As we need numeric data in power for the next step  we converted it
  PowerNew$X1 <- as.numeric(as.character(PowerNew$X1))
  
#Create a new binary coloumn wether power is used or not (After analyising the power value we recognized
#that the value must at least be higher than 1 to consider the TV as on)
  PowerNew$TvOn <- ifelse(PowerNew$X1 <= 1,0 ,1)
  
#1.Split the date into Date and Time
#2.Change value from Factor into Date
#3.Create Column for weekday
  PowerNew <- cSplit(PowerNew, c("X2"), "T")
  PowerNew$X2_1 <- as.Date(as.character(PowerNew$X2_1))
  PowerNew <- cSplit(PowerNew, c("X2_2"), ".")
  PowerNew$WeekDay <- weekdays(PowerNew$X2_1)
  
#Removing useless columns and rename
  PowerNew <- PowerNew[,c(1,2,3,4,6)]
  names(PowerNew) <- c("Power","TvOn","Date","Time","Weekday") #Rename columns
  
#Creation of new Dataframe with the aggregation on Date and cleaning points
  library(dplyr)
  PowerGrouped <- data.frame(
    PowerNew %>%
      group_by(Date, Weekday) %>%
      summarise(TvOnLenght = sum(TvOn),
                TvOnBinary = max(TvOn),
                MaxPower = round(max(Power), digits = 2),
                MinPower = round(min(Power), digits = 2),
                AvgPower = round(mean(Power), digits = 2)))
  PowerGrouped$CleaningPoints <- PowerGrouped$TvOnBinary *25
  
  
#Creation of the final vector with binary values (1=TVs used, 0=TVs not used)
  FinalPower <- PowerGrouped[25:31,]
  PowerVector <- as.vector(FinalPower$CleaningPoints) #Creation of the final vector with binary values (1=TVs used, 0=TVs not used)
  PowerVector
  
  
  
  
####################### Weather ######################## Weather ############################### Weather ################################
# Weather ######################## Weather ############################### Weather ################################ Weather #############
####################### Weather ######################## Weather ############################### Weather ################################
  
Weather <- fromJSON("weather.json")
  
#Select usefull data and create a dataframe
  WeatherNew <- data.frame(cbind(Weather$fcst_valid_local, Weather$day$pop_phrase, Weather$night$pop_phrase)) 
  
#Split the date into Date and Time  
  WeatherNew <- cSplit(WeatherNew, c("X1"), "T") 
  
#Reduce phrase to the numeric propability value 
  WeatherNew <- cSplit(WeatherNew, c("X2"), " ")
  WeatherNew <- cSplit(WeatherNew, c("X2_4"), "%")
  WeatherNew <- cSplit(WeatherNew, c("X3"), " ")
  WeatherNew <- cSplit(WeatherNew, c("X3_4"), "%")
  
#Remove not needed columns  
  WeatherNew <- WeatherNew[,c(1,6,11)] 
  
#Devide number (e.g. 90 for 90%) by 100 to get e.g. 0,9
  WeatherNew$X2_4_1 <- WeatherNew$X2_4_1/100
  WeatherNew$X3_4_1 <- WeatherNew$X3_4_1/100
  
#If in both Day & Night there is apropability -> take the average,
#If just day or night -> take the one with propability
#If none -> 0
  WeatherNew$TotalProp <- ifelse(is.na(ifelse(is.na((WeatherNew$X2_4_1+WeatherNew$X3_4_1)/2),
                              ifelse(is.na(WeatherNew$X2_4_1),WeatherNew$X3_4_1 ,WeatherNew$X2_4_1),
                              (WeatherNew$X2_4_1+WeatherNew$X3_4_1)/2)),
                              0,
                              ifelse(is.na((WeatherNew$X2_4_1+WeatherNew$X3_4_1)/2),
                                     ifelse(is.na(WeatherNew$X2_4_1),WeatherNew$X3_4_1 ,WeatherNew$X2_4_1),
                                     (WeatherNew$X2_4_1+WeatherNew$X3_4_1)/2))

#Insert new column with weekdays
  WeatherNew$Weekday <- weekdays(as.Date(WeatherNew$X1_1)) 
  
#Remove not needed columns and just relevant week
  WeatherNew <- WeatherNew[4:10,c(1,5,4)]
  
#Rename columns
  names(WeatherNew) <- c("Date","Weekday","TotalProp")
  

#Create a new column with the rain propability multiplied by 3 for the later calculation of the total points
  WeatherNew$RainMultiplier <- WeatherNew$TotalProp*3 

#Place 1 for null values (days where it should not rain) for the later calculation of the total points
  WeatherNew$RainMultiplier <- ifelse(WeatherNew$RainMultiplier ==0 , 1 ,WeatherNew$RainMultiplier)

WeatherVector <- as.vector(WeatherNew$RainMultiplier) #Creation of the final vector with the multipliers
WeatherVector


####################### Lights ########################Lights############################### Lights ################################
####################### Lights ########################Lights############################### Lights ################################
####################### Lights ########################Lights############################### Lights ################################

library(jsonlite)
library(splitstackshape)

Lights <- fromJSON("lights.json")

#Create Dataframe
  LightsNew <- data.frame(cbind(Lights$rows$doc$value, Lights$rows$doc$light_value, Lights$rows$doc$date))

#As we need numeric data in value for a next step we converted it
  LightsNew$X1 <- as.numeric(as.character(LightsNew$X1))
  LightsNew$X2 <- as.numeric(as.character(LightsNew$X2))
  
#Combine Movement & Value to eliminate null values
  LightsNew$ValueComb <- ifelse(is.na(LightsNew$X1),LightsNew$X2 ,LightsNew$X1)
  
#Setting a threshhold of 250 that indicates if the lights are on or not 
  LightsNew$LightsOn <- ifelse(LightsNew$ValueComb <= 250,0 ,1)
  
#Split the date into Date & Time and create a new column with date and time together  
  LightsNew <- cSplit(LightsNew, c("X3"), "T")
  LightsNew <- cSplit(LightsNew, c("X3_2"), ".")
  LightsNew$DateTime <- as.POSIXct(paste(LightsNew$X3_1, LightsNew$X3_2_1), format="%Y-%m-%d %H:%M:%S")

#Round date to one hour as we count the hours the light was on in that particular hour
  LightsNew$DateTimeHours <- round_date(LightsNew$DateTime, '1 hours')
  LightsNew$X3_1 <- as.Date(as.character(LightsNew$X3_1))
  
#Insert new column with weekdays
  LightsNew$WeekDay <- weekdays(LightsNew$X3_1)

#Removing useless columns and rename
  LightsNew <- LightsNew[,c(5,6,8,9,10,3,4)]
  names(LightsNew) <- c("date","Time","DateTime","DateTimeHours","Weekday","ValueComb","LightsOn")

#Creation of new Dataframe with the aggregation on the hour of each day
  library(dplyr)
  LightGrouped <- data.frame(
  LightsNew %>%
    group_by(DateTimeHours, date, Weekday) %>%
    summarise(Power = round(sum(ValueComb), digits = 2),
              Signals = n(),
              MinPower = round(min(ValueComb), digits = 2),
              MaxPower = round(max(ValueComb), digits = 2)))
  
#Summing up the power per hour and then devide it by the number of signals, if the value is higher than 250 we assume the light was on
#After plenty of testings we figured out that this is a good approach 
  LightGrouped$LightOn <- ifelse(LightGrouped$Power/LightGrouped$Signals <= 250, 0,1)

#Creation of new Dataframe with the aggregation on Date and cleaning points
NumOfHoursPerDay <- data.frame(
  LightGrouped %>%
    group_by(date, Weekday) %>%
    summarise(HoursLightsOn = round(sum(LightOn), digits = 2)))
NumOfHoursPerDay$CleaningPoints <- NumOfHoursPerDay$HoursLightsOn *5

####################### SCOREBOARD ######################## SCOREBOARD ############################### SCOREBOARD ################################
# SCOREBOARD ######################## SCOREBOARD ############################### SCOREBOARD ################################ SCOREBOARD 
####################### SCOREBOARD ######################## SCOREBOARD ############################### SCOREBOARD ################################

#Reduce different sensor dataframes to relevant dates
ScoreCoffee <- CoffeeGrouped[17:23,]
ScorePower <- PowerGrouped[25:31, c(1,2,4,8)]
ScoreMovements <- MovementTotal[,c(4,1,2,3)]
ScoreLight <- NumOfHoursPerDay[24:30,]
ScoreWeather <- WeatherNew[,c(1,3,4)]

#Sort Weather to same order like the sensortypes (starting at Friday)
dayLabs<-c("Friday","Saturday","Sunday","Monday","Tuesday","Wednesday","Thursday") 
ScoreWeather$Weekday <- factor(ScoreWeather$Weekday, levels= dayLabs)
ScoreWeather<-ScoreWeather[order(ScoreWeather$Weekday), ]

#Bind to ScoreBoard
ScoreBoard <- data.frame(cbind(ScoreCoffee, ScorePower$CleaningPoints, ScoreMovements$CleaningPoints, ScoreLight$CleaningPoints, ScoreWeather$RainMultiplier))

#Clean and Name
ScoreBoard <- ScoreBoard[,c(2,4:8)]
names(ScoreBoard) <- c("Weekday","CPCoffee","CPPower","CPMovements","CPLight","RainMultiplier")

#Include Total Cleaning Points
ScoreBoard$CPsTotal <- (ScoreBoard$CPCoffee + ScoreBoard$CPPower + ScoreBoard$CPMovements + ScoreBoard$CPLight) * ScoreBoard$RainMultiplier

dayLabs<-c("Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday") 
ScoreBoard$Weekday <- factor(ScoreBoard$Weekday, levels= dayLabs)
ScoreBoard<-ScoreBoard[order(ScoreBoard$Weekday), ]

CleaningPointsVector <- as.vector(ScoreBoard$CPsTotal) #Creation of the final vector with the multipliers
CleaningPointsVector


CleaningPointsVector <- as.vector(db$RESULT2)

########################################################################################################################################
#############################################################################################################################################
#### All Data -> Weekday Average ############################################################################################################
#############################################################################################################################################
########################################################################################################################################

##################################
########   movement ##############
##################################

library(jsonlite)
library(splitstackshape)

json_file<-"movement.json"
movement<-fromJSON("C:/Users/pa/Downloads/IoT/movement.json")
movement_data<-as.data.frame(movement)


date_human=as.data.frame(movement$rows$doc$date)
sensorType=as.data.frame(movement$rows$doc$sensorType)
movementcol=as.data.frame(movement$rows$doc$movement)
value=as.data.frame(movement$rows$doc$value)

##BIND
Movement<-data.frame(cbind(value,movementcol,sensorType,date_human))
names(Movement)<-c("value", "movement", "SensorType", "date_human")
Movement<-cSplit(Movement, c("date_human"), c("T"))
Movement<-cSplit(Movement, c("date_human_2"), c("."))

Movement$date_human_2_2<-NULL
Movement$value<-NULL
Movement$movement<-NULL

Movement <- Movement[c(5,1,2,3,4)]

write.csv(Movement, file = "Movement.csv")


#MERGING VALUE AND MOVEMENT COLUMN ERASING NAs
Movement$merged = Movement$value  # your new merged column start with value
Movement$merged[!is.na(Movement$movement)] = Movement$movement[!is.na(Movement$movement)]  # merge with movement


#Deleting the rows with SensorType=coffee
Movement<-Movement[!(Movement$SensorType=="coffee"),]

#MERGE DATE AND TIME
Movement$DateTime = paste(Movement$date_human_1, Movement$Round_Time, sep=" ")

library(sqldf)

Movement$date_human_2_1<-NULL
Movement$X<-NULL

Movement<-Movement[c(2,3,4,1,5)]

#Per day sum(merged) from all weeks 
DailyMovement <- sqldf('select date_human_1 as Date, Weekday, sum(merged) as DailyTotal
                       from Movement
                       group by date_human_1')

write.csv(DailyMovement,file = "DailyMovement.csv")


#WEIGHTS IN EXCEL IN DailyMovement -- RE-IMPORT
#GETTING AVERAGES PER DAY
AVMOVEMENT<-sqldf('select DayOfTheWeek, sum(Total) as Summary
                  from TotalMovement
                  group by DayOfTheWeek')

AVMOVEMENT$FREQ<-c(4,4,3,4,5,4,4)
AVMOVEMENT$TOTAL<- AVMOVEMENT$Summary/AVMOVEMENT$FREQ

#drop freq summ
AVMOVEMENT$Summary<-NULL
AVMOVEMENT$FREQ<-NULL


#SET THE DAYS IN ORDER
AVMOVEMENT<-AVMOVEMENT[c(4,2,6,7,5,1,3),] 

#ROUND UP THE Wmean
AVMOVEMENT$TOTAL <- round(AVMOVEMENT$TOTAL,digits=1)

#EXTRACTING VECTOR OF AVERAGES
VECTOROFAVMOVEMENT<-AVMOVEMENT[, 2]
VECTOROFAVMOVEMENT

#EXTRACTING VECTOR FOR WEEK OF APRIL 9/04/2017 TO 15/04/2017
lastmoveweek<-DailyMovement[(20:26),3]
lastmoveweek


#CLEANING POINTS COLUMN
AVMOVEMENT$Clean_Points<-c(0,0,0,0,0,0,0)

AVMOVEMENT <- sqldf('select DayOfTheWeek, TOTAL, (TOTAL*2) as Cleaning_Points
                    from AVMOVEMENT
                    group by DayOfTheWeek')

write.csv(AVMOVEMENT,file = "AVMOVEMENT.csv")


#-----------------------------------------------------------------------------------

#ADDING ROWS TO A TABLE#NOT NECESSARY HERE
TotalMovement5 <- rbind(TotalMovement5,data.frame(Date = "2017-04-15", Total = "0"))
TotalMovement5 <- TotalMovement5[order(TotalMovement5$Date),]

#EXTRACTING EACH WEEK
WEEK5 <- TotalMovement[c(29:35),]
WEEK1 <- TotalMovement[c(1:7),]
WEEK2 <- TotalMovement[c(8:14),]
WEEK3 <- TotalMovement[c(15:21),]
WEEK4 <- TotalMovement[c(22:28),]




########################
#######LIGHTS###########
########################
library(jsonlite)
library(splitstackshape)

json_file<-"lights.json"
lights<-fromJSON("C:/Users/pa/Downloads/IoT/lights.json")
lights_data<-as.data.frame(lights)


date=as.data.frame(lights$rows$doc$date)
sensorType=as.data.frame(lights$rows$doc$sensorType)
value=as.data.frame(lights$rows$doc$light_value)
zone=as.data.frame(lights$rows$doc$zone)
light_value=as.data.frame(lights$rows$doc$value)


##BIND
GoodLights<-data.frame(cbind(value,light_value,zone,sensorType,date))
names(GoodLights)<-c("value","light_value", "Zone", "SensorType", "date")
GoodLights<-cSplit(GoodLights, c("date"), c("T"))
GoodLights<-cSplit(GoodLights, c("date_2"), c("."))
write.csv(GoodLights, file = "GoodLights.csv")

#MERGING VALUE AND light_value COLUMN ERASING NAs
GoodLights$mergedValue = GoodLights$value  # your new merged column start with value
GoodLights$mergedValue[!is.na(GoodLights$light_value)] = GoodLights$light_value[!is.na(GoodLights$light_value)]  # merge with movement

#DELETING VALUE, LIGHT_VALUE
GoodLights$value<-NULL
GoodLights$light_value<-NULL
GoodLights$date_2_2<- NULL
GoodLights$X <-NULL
GoodLights <- GoodLights[c(5,1,2,3,4)]

#MERGE DATE AND TIME
GoodLights$DateTime = paste(GoodLights$date_1, GoodLights$Rounded_Time, sep=" ")

#DELETE SEPARATE DATE AND TIME COLUMNS
GoodLights$date_1<-NULL
GoodLights$date_2_1<-NULL
GoodLights$Rounded_Time<-NULL
GoodLights$mergedValue <- NULL

#EXTRACTING THE HOUR#not needed
GoodLights<-cSplit(GoodLights, c("date_1"), c("/"))
names(GoodLights)<-c("value", "Zone", "SensorType", "date", "code", "Hour", "Minute", "Second")

#IMPORTING GOODLIGHTS AFTER ROUNDING THE TIME

#AGGREGATING PER DAY PER HOUR POWER, COUNT BEEPS, POWER ON/OFF
TotalLights<-sqldf('select DateTime,Weekday, sum(value) as DailyLights, count(*) as Beeps
                   from GoodLights
                   group by DateTime')

#importing TotalLights after adding powerOn


#omitting NA's# BIG NO NO
TotalLights <- na.omit(TotalLights)
TotalLights$X<-NULL

write.csv(TotalLights, file = "TotalLights.csv")

TotalLights<-TotalLights[c(5,1,6,3,4,2)]

#ADDED DAYS IN EXCEL, RE-IMPORT AND SPLIT DAY TIME
TotalLights<-cSplit(TotalLights, c("DateTime"), c(" "))


#COUNT HOURS PER DAY
LightsOn <- sqldf('select DateTime_1 as Date,Weekday,sum(PowerOn) as HoursLightsAreOn
                  from TotalLights
                  group by DateTime_1')

write.csv(LightsOn, file = "LightsOn.csv")

#WEIGHTS IN EXCEL IN LIGHTSON -- RE-IMPORT
#GETTING AVERAGES PER DAY
AVLIGHTS<-sqldf('select Weekday, sum(HoursLightsAreOn) as summary
                from LightsOn
                group by Weekday')
AVLIGHTS$FREQ<-c(4,4,4,4,5,4,4)
AVLIGHTS$TOTAL<-AVLIGHTS$summary/AVLIGHTS$FREQ

#DROP SUM FREQ AND EXPORT
AVLIGHTS$summary<-NULL
AVLIGHTS$FREQ<-NULL

#ROUND UP THE HOURS
AVLIGHTS$Wmean <- round(AVLIGHTS$Wmean,digits=1)

#SET THE DAYS IN ORDER
AVLIGHTS<-AVLIGHTS[c(4,2,6,7,5,1,3),] 


#EXTRACTING VECTOR OF AVERAGES
VECTOROFAVLIGHTS<-AVLIGHTS[, 2]
VECTOROFAVLIGHTS


#EXTRACTING VECTOR FOR WEEK OF APRIL 9/04/2017 TO 15/04/2017
lastlightweek<-LightsOn[(26:32),3]
lastlightweek
LightsOn$X<-NULL

#CLEANING POINTS COLUMN
AVLIGHTS$Clean_Points<-c(0,0,0,0,0,0,0)

AVLIGHTS <- sqldf('select Weekday, TOTAL, (TOTAL*5) as Cleaning_Points
                  from AVLIGHTS
                  group by Weekday')

write.csv(AVLIGHTS,file = "AVLIGHTS.csv")






################################
#########   POWER      #########
################################

library(jsonlite)
library(splitstackshape)

json_file<-"power.json"
power<-fromJSON("C:/Users/pa/Downloads/IoT/power.json")
power_data<-as.data.frame(power)


date=as.data.frame(power$rows$doc$date)
sensorType=as.data.frame(power$rows$doc$sensorType)
power=as.data.frame(power$rows$doc$power)



##BIND
GoodPower<-data.frame(cbind(date,sensorType,power))
names(GoodPower)<-c("date","sensorType", "Power")
GoodPower<-cSplit(GoodPower, c("date"), c("T"))
GoodPower<-cSplit(GoodPower, c("date_2"), c("."))
write.csv(GoodPower, file = "GoodPower.csv")



#DELETING VALUE, LIGHT_VALUE
GoodLights$value<-NULL
GoodLights$light_value<-NULL
GoodLights$date_2_2<- NULL
TotalPower$X <-NULL
GoodLights <- GoodLights[c(5,1,2,3,4)]

#MERGE DATE AND TIME
GoodPower$DateTime = paste(GoodPower$date_1, GoodPower$Rounded_Time, sep=" ")

#DELETE SEPARATE DATE AND TIME COLUMNS
PowerOn$X<-NULL
TotalPower$X.2<-NULL




#IMPORTING GOODLIGHTS AFTER ROUNDING THE TIME

#AGGREGATING PER DAY PER HOUR POWER, COUNT BEEPS, POWER ON/OFF
TotalPower<-sqldf('select DateTime,Weekday, sum(Power) as DailyPower, count(*) as Beeps, min(Power) as MiniPower, max(Power) as MaxPower
                  from GoodPower
                  group by DateTime')
write.csv(TotalPower,file = "TotalPower.csv")


TotalPower <- sqldf('select ')

#importing TotalPower after adding powerOn


#ADDED DAYS IN EXCEL, RE-IMPORT AND SPLIT DAY TIME
TotalPower<-cSplit(TotalPower, c("DateTime"), c(" "))


#ADDING ROWS TO A TABLE#
PowerOn <- rbind(PowerOn,data.frame(Date = "04/15/2017", Weekday="Saturday", HoursPowerIsOn = "0"))

NEWAVPOW <-sqldf('select Weekday, count(*), sum(HoursPowerIsOn) as DemoArea
                 from PowerOn
                 group by Weekday')

write.csv(NEWAVPOW,file = "NEWAVPOW.csv")

#COLUMN "ISITON" IS ADDED AS AN AVERAGE OF COUNT OF 1 OVER TIMES A DAY APPEARS IN THE DATASET. IF GREATER THAN 50% THEN ISITON=1 THEN 25 POINTS 

NEWAVPOW<-NEWAVPOW[c(4,2,6,7,5,1,3),] 

NEWAVPOWVECTOR<-NEWAVPOW[, 5]
NEWAVPOWVECTOR

#------------------THAT'S OUT--------------------------------------------
#COUNT HOURS PER DAY
PowerOn <- sqldf('select DateTime_1 as Date,Weekday, max(PowerOn) as HoursPowerIsOn
                 from TotalPower
                 group by DateTime_1')

write.csv(PowerOn, file = "PowerOn.csv")

#WEIGHTS IN EXCEL IN POWERSON -- RE-IMPORT
#GETTING AVERAGES PER DAY
AVPOWER<-sqldf('select Weekday, sum(HoursPowerIsOn*Weights) as Wmean
               from PowerOn
               group by Weekday')
#-------------------------------------------------------------
#ROUND UP THE HOURS
AVPOWER$Wmean <- round(AVPOWER$Wmean,digits=1)

#SET THE DAYS IN ORDER
AVPOWER<-AVPOWER[c(4,2,6,7,5,1,3),] 


#EXTRACTING VECTOR OF AVERAGES
VECTOROFAVPOWER<-AVPOWER[, 2]
VECTOROFAVPOWER

#COMPARE TO WEEK OF APRIL
#EXTRACTING VECTOR FOR WEEK OF APRIL 9/04/2017 TO 15/04/2017
lastpowerweek<-PowerOn[(27:33),3]
lastpowerweek


#CLEANING POINTS COLUMN
AVPOWER$Clean_Points<-c(0,0,0,0,0,0,0)

AVPOWER <- sqldf('select Weekday, Wmean, (Wmean*25) as Cleaning_Points
                 from AVPOWER
                 group by Weekday')
write.csv(AVPOWER,file = "AVPOWER.csv")

################################
########   coffee ##############
################################

library(jsonlite)
library(splitstackshape)

json_file<-"coffee.json"
coffee<-fromJSON("C:/Users/pa/Downloads/IoT/coffee.json")
coffee_data<-as.data.frame(coffee)


date=as.data.frame(coffee$rows$doc$date)
sensorType=as.data.frame(coffee$rows$doc$sensorType)
value=as.data.frame(coffee$rows$doc$value)

##BIND
COFFEE<-data.frame(cbind(value,sensorType,date))
names(COFFEE)<-c("value", "SensorType", "date")
COFFEE<-cSplit(COFFEE, c("date"), c("T"))
COFFEE<-cSplit(COFFEE, c("date_2"), c("."))



COFFEE$date_2_2<-NULL

write.csv(COFFEE, file = "GoodCoffee.csv")

GoodCoffee<-GoodCoffee[c(3,4,5,6,2,1)] 

#ASSUME SENSORTYPE IS CORRECT
#DROP THE EXTRA ROWS WITH {d: {coffees: 2, "sensorType": "coffee"}}
GoodCoffee<-GoodCoffee[!c(1:25),]

#Round Time and extract day name

TotalCoffee <- sqldf('select date_1, WeekDay, max(value) as Total
                     from GoodCoffee
                     GROUP BY date_1')

#GETTING RID OF THE UGLY VALUE BY CREATING A NEW TABLE
TotalCoffees <- TotalCoffee[c(9:23),]
#ADDING THE MISSING ROWS
TotalCoffees <- rbind(TotalCoffees,data.frame(date_1 = "3/16/2017", Total = "8"))
TotalCoffees <- TotalCoffees[order(TotalCoffees$date_1),]
TotalCoffees$WeekDay<-TotalCoffee$WeekDay

#Adding the days of the week
TotalCoffees$Day <- weekdays(as.Date(TotalCoffees$date_1))
TotalCoffees<-TotalCoffees[c(1,3,2)] 

write.csv(TotalCoffees, file = "TotalCoffees.csv")

#weights in excel 
#GETTING AVERAGES PER DAY
AVERCOFFEES<-sqldf('select date_1, WeekDay, (Total*Weights)  as Wmean
                   from TotalCoffees
                   group by date_1')


#WEIGHTS IN EXCEL IN TOTALCOFFEES -- RE-IMPORT
#GETTING AVERAGES PER DAY
AVOFCOFFEE<-sqldf('select WeekDay, (sum(Total)/count(WeekDay)) as Total
                  from TotalCoffees
                  group by WeekDay')

AVOFCOFFEE$Total <- round(AVOFCOFFEE$Total,digits=2)


#SET THE DAYS IN ORDER
AVOFCOFFEE<-AVOFCOFFEE[c(4,2,6,7,5,1,3),] 


#EXTRACTING VECTOR OF AVERAGES
VECTOROFAVCOFFEES<-AVOFCOFFEE[, 2]
VECTOROFAVCOFFEES


#EXTRACTING VECTOR FOR WEEK OF APRIL 9/04/2017 TO 15/04/2017
lastcoffeeweek<-TotalCoffees[(17:23),3]
lastcoffeeweek
lastcoffeeweek<-lastcoffeeweek[c(5,6,7,1,2,3,4)]

#CLEANING POINTS COLUMN
AVOFCOFFEE$Clean_Points<-c(0,0,0,0,0,0,0)

AVOFCOFFEE <- sqldf('select Weekday, Total, (Total*5) as Cleaning_Points
                    from AVOFCOFFEE
                    group by Weekday')

write.csv(AVOFCOFFEE,file = "AVOFCOFFEE.csv")


###########################################
#########       WEATHER     ###############
###########################################


library(jsonlite)
library(splitstackshape)

json_file<-"weather.json"
weather<-fromJSON("C:/Users/pa/Downloads/IoT/weather.json")
weather_data<-as.data.frame(weather)


night_chance=as.data.frame(weather$night$pop_phrase)
day_chance=as.data.frame(weather$day$pop_phrase)
date=as.data.frame((weather$fcst_valid_local))

##BIND
weather<-data.frame(cbind(date,night_chance,day_chance))
names(weather)<-c("date", "nightProb", "dayProb")
weather<-cSplit(weather, c("date"), c("T"))
weather<-cSplit(weather, c("date_2"), c("+"))
weather$date_2_2<-NULL
weather$date_2_1<-NULL
weather$day <- weekdays(as.Date(weather$date_1))
weather$prob <- c(0.5,0.9,0.3,0.8,0,0,0,0,0.5,0.6,0.3)
weather$date_1<-NULL
weather<-sqldf('select day, "avg(prob)" as aver
               from weather
               group by day')
weather<-sqldf('select day, (aver*3) as multiplier
               from weather
               group by day')
#SET THE DAYS IN ORDER
weather<-weather[c(4,2,6,7,5,1,3),] 
weather$multiplier<-c(1.65,1,1,1,0.75,2.10,1.35)

library(sqldf)
#SCOREBOARD

SCOREBOARD<-data.frame(cbind(AVLIGHTS,AVMOVEMENT,AVOFCOFFEE,NEWAVPOW))
SCOREBOARD$ <-NULL

SCOREBOARD$POINTS<-c(0,0,0,0,0,0,0)
SCOREBOARD$POINTS <- SCOREBOARD$Cleaning_Points + SCOREBOARD$Cleaning_Points.1+SCOREBOARD$Cleaning_Points.2+SCOREBOARD$CleaningPoints

#SET THE DAYS IN ORDER
SCOREBOARD<-SCOREBOARD[c(4,2,6,7,5,1,3),] 

#ADDING WEATHER MULTIPLIER
SCOREBOARD$MULTIPLIER <- weather$multiplier

#RESULT
SCOREBOARD$RESULT <- SCOREBOARD$POINTS*SCOREBOARD$MULTIPLIER

write.csv(SCOREBOARD, file = "SCOREBOARD.CSV")


#LOOK INTO PRESENCE IN THE OFFICE
#movement

PresenceMovement<-sqldf('select Round_Time, sum(merged)
      from Movement
      group by Round_Time')

PresenceMovement <- PresenceMovement[order(PresenceMovement$`sum(merged)`),]
write.csv(PresenceMovement, file = "PresenceMovement.csv")

#coffee


