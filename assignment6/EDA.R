sf <- read.csv("~/Git/datasci_course_materials/assignment6/sanfrancisco_incidents_summer_2014.csv", stringsAsFactors=FALSE)
df <- sf

str(df)
summary(df)

##########
#Numerical
##########
x <- df$IncidntNum
x <- df$X
x <- df$Y
x <- df$PdId
#----------
hist(x)
boxplot(x)

##########
# Character
##########
x <- df$Category
x <- df$Descript
x <- df$DayOfWeek
x <- df$PdDistrict
x <- df$Resolution
x <- df$Address
#----------
length(unique(x))
unique(x)
df$Category <- as.factor(x)

##########
# Date
##########
x <- df$Date
x <- df$Time
#----------
str(x)
summary(x)
#----------
df$Time <- paste0(df$Date,df$Time)
df$Date <- as.Date(x, "%m/%d/%Y")
df$Time <- as.POSIXct(df$Time,format="%H:%M")
##########
# List
##########
x <- df$Location
#----------
