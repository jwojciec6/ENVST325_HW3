# ENVST 325 HW 3
# John Wojciechowski
#2/17/2026
install.packages(c("ggplot2", "dplyr"))
library(ggplot2)
library(dplyr)
install.packages("lubridate")
library(lubridate)

datCO2 <- read.csv("/cloud/project/activity03/annual-co-emissions-by-region.csv")
cc <- read.csv("/cloud/project/activity03/climate-change.csv")

colnames(datCO2)

colnames(datCO2)[4] <- "CO2"

colnames(datCO2)

# convert the entity names to factor and store a variable with levels for
# easy reference
datCO2$Entity <- as.factor(datCO2$Entity)

# make a vector of all levels
name.Ent <- levels(datCO2$Entity)

name.Ent

plot(datCO2$Year, datCO2$CO2)

# new data frame for US
US <- datCO2[datCO2$Entity == "United States",]
# new data frame for Mexico
ME <- datCO2[datCO2$Entity == "Mexico",]

plot(US$Year, # x data
     US$CO2, # y data
     type = "b", #b = points and lines
     pch = 19, # symbol shape
     ylab = "Annual fossil fuel emissions (billons of tons CO2)", #y axis label
     xlab = "Year", #x axis label
     yaxt = "n") # turn off y axis
# add y axis
# arguments are axis number (1 bottom, 2 left, 3 top, 4 right)
# las = 2 changes the labels to be read in horizontal direction
axis(2, seq(0,6000000000, by=2000000000), #location of ticks
     seq(0,6, by = 2), # label for ticks
     las=2 )
# add mexico to plot ----
# add points
points(ME$Year, # x data
       ME$CO2, # y data
       type = "b", #b = points and lines
       pch = 19, # symbol shape,
       col= "darkgoldenrod3")

#Prompt One

#reassign the data type 
cc$Entity <- as.factor(cc$Entity)

#New data frame for Northern Hemipshere

NH <- cc %>%
  filter(cc$Entity == "Northern Hemisphere")

#Change Date to Numeric value 

NH$Day <- as.Date(NH$Day)

#New data frame for southern hemisphere

SH <- cc %>%
  filter(cc$Entity == "Southern Hemisphere")

#Change Date to Numeric Value 

SH$Day <- as.Date(SH$Day)

#Base R plot  

plot(NH$Day, NH$temperature_anomaly,
     type = "b",
     pch = 19,
     xlab = "Year",
     ylab = "Degrees Celsius", 
     main = "Temperature Changes")

points(SH$Day, # x data
            SH$temperature_anomaly, # y data
            type = "b", #b = points and lines
            pch = 19, # symbol shape,
            col= "darkgoldenrod3")
legend("topleft",
       c("Northen Hemisphere", "Southern Hemisphere"),
       col=c("black", "darkgoldenrod3"),
       pch=19, bty= "n")

#Ggplot implementation

BH <- full_join(NH, SH)

ggplot(data = BH, aes(x = Day, y = temperature_anomaly, color = Entity)) +
  geom_point() +
  labs(x="Year", y="degrees Celsius", title ="Temperature Anomalies Northern and Southern Hemispheres") +
  scale_color_manual(values = c("black", "red"))


##Prompt number 2  

