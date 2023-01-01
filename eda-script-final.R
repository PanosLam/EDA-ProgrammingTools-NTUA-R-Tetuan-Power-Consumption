# Import libraries
library(data.table) # data table
library(lubridate) # time
library(ggplot2) # plot
library(ggcorrplot) # plot
library(dplyr) # for the %>% operator
library(ggalt)
library(ggfortify)

# Declare auxiliary functions
getSeason <- function(month) {
  if (month %in% c(12, 1, 2)) {
    "winter"
  } else if (month %in% c(3, 4, 5)) {
    "spring"
  } else if (month %in% c(6, 7, 8)) {
    "summer"
  } else if (month %in% c(9, 10, 11)) {
    "autumn"
  } else {
    "unknown"
  }
}

encodeSeason <- function(season) {
  if (season == "winter") {
    1
  } else if (season == "spring") {
    2
  } else if (season == "summer") {
    3
  } else if (season == "autumn") {
    4
  } else {
    5
  }
}

decodeMonth <- function(month) {
  if (month == 1) {
    "January"
  } else if (month == 2) {
    "February"
  } else if (month == 3) {
    "March"
  } else if (month == 4) {
    "April"
  } else if (month == 5) {
    "May"
  } else if (month == 6) {
    "June"
  } else if (month == 7) {
    "July"
  } else if (month == 8) {
    "August"
  } else if (month == 9) {
    "September"
  } else if (month == 10) {
    "October"
  } else if (month == 11) {
    "November"
  } else {
    "December"
  }
}

decodeZone <- function(zoneId) {
  if (zoneId == 1) {
    "Zone1"
  } else if (zoneId == 2) {
    "Zone2"
  } else {
    "Zone3"
  }
}

encodeZone <- function(zoneName) {
  if (zoneName == "Zone1") {
    1
  } else if (zoneName == "Zone2") {
    2
  } else {
    3
  }
}

decodeDoW <- function(doW) {
  if (doW == 1) {
    "Monday"
  } else if (doW == 2) {
    "Tuesday"
  } else if (doW == 3) {
    "Wednesday"
  } else if (doW == 4) {
    "Thursday"
  } else if (doW == 5) {
    "Friday"
  } else if (doW == 6) {
    "Saturday"
  } else if (doW == 7) {
    "Sunday"
  }
}

# Set constants
DATASET_PATH <- "./PhD/courses/semester-1/ProgrammingTools/R/Tetuan_City_power_consumption.csv"
# Retrieved from https://holidayapi.com/countries/ma-01/2017
BANK_HOLIDAYS_PATH_OFC <- "./PhD/courses/semester-1/ProgrammingTools/R/tetouan-bank-holidays-2017-only-official.csv"


# Read dataset sample to determine the data table column classes
dataSample <- read.table(DATASET_PATH, nrows=100, sep=",", header = TRUE)
officialBankHolidays <- read.csv(BANK_HOLIDAYS_PATH_OFC, header = F)

# Read the data by specifying each column type and make sure it is a data table
classes <- sapply(dataSample, class)
data <- read.table(DATASET_PATH, colClasses = classes, sep=",", header = TRUE)
setDT(data, keep.rownames=T)


###############################
####### Create Features #######
###############################

dateTimeSplitList <- strsplit(data$DateTime, " +")

# create new date and time columns.
data[, "DateStr"] = sapply(dateTimeSplitList, "[[", 1)
data[, "Time"] = sapply(dateTimeSplitList, "[[", 2)

# Create a column to see if the date was a bank holiday
data[, "isBankHoliday"] = rep(1, nrow(data))
data$isBankHoliday = ifelse(data$DateStr %in% officialBankHolidays[1]$V1, 1, 0)  # kept as numeric for the correlation map

# Convert Date column to date type, so to extract the month, the day of the week and the day of the month
data$Date <- as.Date(data$DateStr, format = "%m/%d/%Y")


data[, "DoW"] = wday(data$Date, week_start=1) # kept as numeric for the correlation map
data[, "Month"] = as.numeric(format(data$Date, "%m")) # kept as numeric for the correlation map
data[, "DoM"] = as.numeric(format(data$Date, "%d")) # kept as numeric for the correlation map
data[, "Hour"] <- as.numeric(format(as_datetime(data$DateTime, format = "%m/%d/%Y %H:%M"),'%H'))

# Produce info if the day was during the weekend or it was a workday
data[, "isWeekend"] = rep(1, nrow(data))
data$isWeekend = ifelse(data$DoW %in% c(6, 7), 1, 0) # 6: Sat, 7: Sun

# Determine the season of the year for the data sample
data[, "season"] = sapply(data$Month, getSeason) # convert to numeric for the correlation map
data[, "seasonF"] = sapply(data$season, encodeSeason)


################################
### Create Correlation plots ###
################################

dataOnlyNumeric <- data[, -c("rn", "DateStr", "DateTime", "Date", "Time", "season")]

# Correlogram
corr <- round(cor(dataOnlyNumeric), 1)
ggcorrplot(corr, hc.order = TRUE,
           type = "lower",
           lab = TRUE,
           lab_size = 3,
           method="circle",
           colors = c("tomato2", "white", "springgreen3"),
           title="Correlogram of Tetuan Power Consumption",
           ggtheme=theme_bw)


####################################
########## Expand Dataset ##########
####################################

dataFiltered = data[, -c("rn", "DateTime", "DateStr", "Time", "season")]
dataFiltered$DateF <- as.numeric(factor(data$Date))
dataFiltered$TimeF <- as.numeric(factor(data$Time))

# Expand dataset and have the consumption in one column and another column with the zone
# This will increase the dataset suze by 200% but will help us with the plotting
dataZone1 <- dataFiltered[, -c("Zone.2..Power.Consumption", "Zone.3..Power.Consumption")]
setnames(dataZone1, "Zone.1.Power.Consumption", "ZoneConsumption")
dataZone1$Zone <- rep(1, nrow(dataZone1))

dataZone2 <- dataFiltered[, -c("Zone.1.Power.Consumption", "Zone.3..Power.Consumption")]
setnames(dataZone2, "Zone.2..Power.Consumption", "ZoneConsumption")
dataZone2$Zone <- rep(2, nrow(dataZone2))

dataZone3 <- dataFiltered[, -c("Zone.1.Power.Consumption", "Zone.2..Power.Consumption")]
setnames(dataZone3, "Zone.3..Power.Consumption", "ZoneConsumption")
dataZone3$Zone <- rep(3, nrow(dataZone3))

dataZoneLevelWithDate <- rbind(dataZone1, dataZone2, dataZone3)
dataZoneLevelWithDate$DateF <- as.numeric(factor(dataZoneLevelWithDate$Date))
dataZoneLevelWithDate$TimeF <- as.numeric(factor(dataZoneLevelWithDate$Time))

dataZoneLevel <- dataZoneLevelWithDate[, -c("Date")]
dataZoneLevel[, "ZoneName"] <- factor(sapply(dataZoneLevel$Zone, decodeZone))

###############################
######### Facet Plots #########
###############################

# Temperature Vs Consumption Facet per Zone
gg <- ggplot(dataZoneLevel, aes (x = Temperature , y = ZoneConsumption )) + 
      geom_point(aes(color=ZoneName)) +
      stat_summary(aes(group=ZoneName),fun=mean,geom="line") +
      facet_grid(~Zone) 
plot(gg)


# Facet per zone per Hour consumption
facetBoxPlotLabelsHour <- seq(0, 23)
facetBoxPlotBreaksHour <- seq(0, 23)
gg <- ggplot(dataZoneLevel, aes (x = Hour , y = ZoneConsumption )) + 
      geom_boxplot(varwidth=T, fill="plum", aes(group=Hour)) +
      stat_summary(aes(group=ZoneName),fun=mean,geom="line") +
      facet_grid(~ZoneName) +
      scale_x_continuous(labels = facetBoxPlotLabelsHour, breaks = facetBoxPlotBreaksHour)
plot(gg)

# Facet per zone per Month consumption
facetBoxPlotBreaksMonth <- seq(1, 12)
dataZoneLevel[, "MonthName"] <- sapply(dataZoneLevel$Month, decodeMonth)
gg <- ggplot(dataZoneLevel, aes (x = Month , y = ZoneConsumption )) + 
      geom_boxplot(varwidth=T, fill="plum", aes(group=Month)) +
      stat_summary(aes(group=ZoneName),fun=mean,geom="line", color="blue") +
      facet_grid(~ZoneName) +
      scale_x_continuous(labels = unique(dataZoneLevel$MonthName), breaks = facetBoxPlotBreaksMonth) +
      theme(axis.text.x = element_text (angle=65, vjust =0.6))
plot(gg)


# Facet per zone per DoW consumption
facetBoxPlotBreaksDoW <- seq(1, 7)
dataZoneLevel[, "DoWName"] <- sapply(dataZoneLevel$DoW, decodeDoW)
gg <- ggplot(dataZoneLevel, aes (x = DoW , y = ZoneConsumption )) + 
  geom_boxplot(varwidth=T, fill="plum", aes(group=DoWName)) +
  stat_summary(aes(group=ZoneName),fun=mean,geom="line", color="blue") +
  facet_grid(~ZoneName) +
  scale_x_continuous(labels = unique(dataZoneLevel$DoWName), breaks = facetBoxPlotBreaksDoW) +
  theme(axis.text.x = element_text (angle=65, vjust =0.6))
plot(gg)

###############################
########## Box Plots ##########
###############################

gg <- ggplot(dataZoneLevel, aes (x = Hour , y = Temperature )) + 
      geom_boxplot(varwidth=T, fill="plum", aes(group=Hour)) +
      scale_x_continuous(labels = facetBoxPlotLabelsHour, breaks = facetBoxPlotBreaksHour)
plot(gg)

gg <- ggplot(dataZoneLevel, aes (x = Month , y = Temperature )) + 
      geom_boxplot(varwidth=T, fill="plum", aes(group=MonthName)) +
      scale_x_continuous(labels = unique(dataZoneLevel$MonthName), breaks = facetBoxPlotBreaksMonth) +
      theme(axis.text.x = element_text (angle=65, vjust =0.6))
plot(gg)


gg <- ggplot(dataZoneLevel, aes (x = Hour , y = Humidity )) + 
      geom_boxplot(varwidth=T, fill="plum", aes(group=Hour)) +
      scale_x_continuous(labels = facetBoxPlotLabelsHour, breaks = facetBoxPlotBreaksHour)
plot(gg)

gg <- ggplot(dataZoneLevel, aes (x = Month , y = Humidity )) + 
      geom_boxplot(varwidth=T, fill="plum", aes(group=MonthName)) +
      scale_x_continuous(labels = unique(dataZoneLevel$MonthName), breaks = facetBoxPlotBreaksMonth) +
      theme(axis.text.x = element_text (angle=65, vjust =0.6))
plot(gg)


g <- ggplot(dataZoneLevel, aes(ZoneName, ZoneConsumption)) + 
     geom_boxplot(varwidth=T, fill="plum") +
     labs(title="Box plot",
          subtitle="Zone power consumption by zone",
          x="Zone",
          y="Zone consumption")
plot(g)
ld <- layer_data(g)

###########################
####### Violin Plot ####### 
###########################
g <- ggplot(dataZoneLevel, aes(ZoneName, ZoneConsumption)) + 
     geom_violin () +
     labs(title="Violin plot",
         subtitle="Zone vs Power consumption",
         x="Zone", 
         y="Power Consumption")
plot(g)

############################
######## Bar Charts ######## 
############################


# Average consumption PER ZONE PER SEASON
dataZoneLevel[, "season"] = sapply(dataZoneLevel$Month, getSeason)
avgConsumptionPerSeasonPerZone = dataZoneLevel[, .(mean(ZoneConsumption)) , by = .(season, ZoneName)]
setnames(avgConsumptionPerSeasonPerZone, "V1", "AvgConsumption")
gg <- ggplot(avgConsumptionPerSeasonPerZone, aes (x = season, y = AvgConsumption , fill = ZoneName )) +
      geom_bar(position = "dodge", stat = "identity") +
      labs(title="Average Power Consumption per Season per Zone")
plot(gg)


# Average consumption PER MONTH PER ZONE
avgConPerMonPerZoneData = dataZoneLevel[, c("ZoneName", "Month", "ZoneConsumption")]
avgConPerMonPerZoneData[, "MonthName"] = sapply(avgConPerMonPerZoneData$Month, decodeMonth)

avgConPerMonPerZoneData = avgConPerMonPerZoneData[, .(mean(ZoneConsumption)) , by = .(Month, ZoneName)]
setnames(avgConPerMonPerZoneData, "V1", "AvgConsumption")

# It is aggregated per month per zone, so first 12 rows
# contain all discrete months in the proper order 
months <- sapply(avgConPerMonPerZoneData$Month, decodeMonth)

gg <- ggplot(avgConPerMonPerZoneData, aes (x = Month, y = AvgConsumption , fill = ZoneName )) +
      geom_bar(position = "dodge", stat = "identity") + 
      geom_smooth(method="loess", se=F) +
      scale_x_continuous(breaks=seq(1, 12), labels=months[1:12]) +
      theme(axis.text.x = element_text (angle=65, vjust =0.6)) +
      labs(title="Average Power Consumption per Month per Zone")
plot(gg)



