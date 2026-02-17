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
US <- datCO2 %>%
  filter(datCO2$Entity == "United States")

Canada <- datCO2 %>%
  filter(datCO2$Entity == "Canada")

Mexico <- datCO2 %>%
  filter(datCO2$Entity == "Mexico")

total_us <- sum(US$CO2)

total_mexico <- sum(Mexico$CO2)

total_canada <- sum(Canada$CO2)

totals <- data.frame(
  Country = c("United States", "Canada", "Mexico"),
  total_CO2 = c(total_us, total_canada, total_mexico))

#divide total emissions by 1 billion 
totals$total_CO2 <- totals$total_CO2 /1000000000

ggplot(totals, aes(x = Country, y = total_CO2)) +
  geom_col(fill = "steelblue") +
  labs(y = "Total fossil fuel emissions (billions of tons CO2)",
       x = "Country",
       title ="Total CO2 emissions by country")

##Question 1

Poland <- datCO2 %>%
  filter(datCO2$Entity == "Poland")

##Divides emissions by 1 million
Poland$CO2 <- Poland$CO2 / 1000000

ggplot(Poland, aes(x = Year, y = CO2),)+
  geom_point(color = "blue") +
  labs(x = "Year", y = "CO2 Emmsions (millions of tons)", title =
         "Poland CO2 Emissions by Year")

##Question 2

#creating seprate data frames with only world data
world_temp <- cc %>%
  filter(cc$Entity == "World")

world_CO2 <- datCO2 %>%
  filter(datCO2$Entity == "World")

#formatting the date column for proper graph implementation
world_temp$Day <- ymd(world_temp$Day)

ggplot(world_temp, aes(x = Day, y = temperature_anomaly)) +
  geom_point(color = "blue") +
  labs(x = "Year", y = "Temperature Anomaly °C", 
       title = "World Temperature Anomalies")

#convert to billions of tons 
world_CO2$CO2 <- world_CO2$CO2 / 1000000000

ggplot(world_CO2, aes(x = Year, y = CO2)) +
  geom_line(color = "blue") +
  labs(x = "Year", y = "CO2 Emissions (billions of tons)", 
       title = "World CO2 Emissions per Year")


##Question 3

#need for labels without legend to look good.
install.packages("ggrepel")
library(ggrepel)

nuclear <- read.csv("/cloud/project/activity03/nuclear-energy-generation.csv")

#combine nations data needed into one data frame
nuclear_nations <- nuclear %>%
  filter(nuclear$Entity == "France"|
         nuclear$Entity == "Japan"|
         nuclear$Entity == "Sweden"|
         nuclear$Entity == "United Kingdom"|
         nuclear$Entity == "Germany")

#gets last year to use for labeling
last_year <- nuclear_nations %>%
  group_by(Entity) %>%
  slice_max(Year)

ggplot(nuclear_nations) +
  aes(x = Year, y = Nuclear, color = Entity) +
  geom_point() +
  geom_line() +
  #creates labels
  geom_label_repel(aes(label = Entity),
                   data = last_year,
                   nudge_x = 3,
                   nudge_y = 15,
                   segment.color = "gray50",
                   fontface = "bold",
                   size = 3) +
  labs(x = "Year", y = "Terawatt-hours (TWh)",
       title ="Nuclear power generation") +
  scale_color_manual(values = c("steelblue", "firebrick4","darkgoldenrod4", "aquamarine4", "darkorchid4" )) +
  theme(legend.position = "none") + 
  #extendeds graph so labels can be read
  xlim(NA, 2035)
