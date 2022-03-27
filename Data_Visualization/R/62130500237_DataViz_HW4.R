
#Nuttavadee Autsavapanakit 62130500237
#HW4

# To do:
# Now try to find the top 10 cities and 10 states with the worst AQI
# of various pollutants.
# Applying a variety of plots like
# - line graph
# - histogram
# - boxplot (you may need to further process outliers)
# - stacked bar plot
# - faceted plot

# Do screen captures of the output graphs and provide your insight about them

library(dplyr)
library(ggplot2)

pol <- read.csv("pollution_us_2000_2016.csv") #import data
#count cities
city_col <- unique(pol[,c("City")])
city_col #144 cities

state_col <- unique(pol[,c("State")])
state_col #47 states

# Check NO2.AQI range value
y <- pol$NO2.AQI
range(y) #0-132


#10 cities with the worst NO2.AQI
cNO2 <- pol[,c("City","NO2.AQI")]
#group same cities using mean value
meancNO2 <- cNO2 %>%
  group_by(City) %>%
  summarise(
    NO2.AQI = mean(NO2.AQI, na.rm = T)
  ) 
#sort the data
sortcNO2 <- order(meancNO2[, "NO2.AQI"])
sortedcNO2 <- meancNO2[sortcNO2, , drop = FALSE]
worstcNO2 <- tail(sortedcNO2,10) #get last 10 rows

format_cNO2 <- format(round(worstcNO2$NO2.AQI, 2), nsmall = 2)

# plot and see
ggplot(worstcNO2, aes(x= reorder(City, -NO2.AQI), y=NO2.AQI, fill=City)) +
  geom_bar(stat="identity") + scale_fill_brewer(palette = "Paired") + 
  geom_text(aes(x = City, y = NO2.AQI, label = format_cNO2, vjust = -0.3)) +
  labs(x = "City", y = "NO2.AQI(Mean)", 
       title = "10 cities with the worst NO2.AQI")


#10 cities with the worst O3.AQI
cO3 <- pol[,c("City","O3.AQI")]
meancO3 <- cO3 %>%
  group_by(City) %>%
  summarise(
    O3.AQI = mean(O3.AQI, na.rm = T)
  ) 
#find 10 cities with worst O3.AQI
sortcO3 <- order(meancO3[, " O3.AQI"])
sortedcO3 <- meancO3[sortcO3, , drop = FALSE]
worstcO3 <- tail(sortedcO3,10) #get last 10 rows
cO3List <- worstcO3$City
cO3List
#boxplot
box_cO3 <- pol[pol$City == "Norristown" | pol$City == "Paducah" |
                 pol$City == "Lancaster" | pol$City == "Ashland" | 
                 pol$City == "Victorville" | pol$City == "Fresno" |
                 pol$City == "Roosevelt" | pol$City == "Fontana" |
                 pol$City == "Rubidoux" | pol$City == "Winston-Salem" , 
                   c("City", "O3.AQI")]
boxplot(O3.AQI ~ City, data=box_cO3, 
        horizontal = FALSE , col =  c("cyan",rgb(.4,.3,0,0.5)))




#10 cities with the worst SO2.AQI
cSO2 <- pol[,c("City","SO2.AQI")]
meancSO2 <- cSO2 %>%
  group_by(City) %>%
  summarise(
    SO2.AQI = mean(SO2.AQI, na.rm = T)
  ) 
#sort the data
sortcSO2 <- order(meancSO2[, "SO2.AQI"])
sortedcSO2 <- meancSO2[sortcSO2, , drop = FALSE]
worstcSO2 <- tail(sortedcSO2,10) #get last 10 rows
cSO2List <- worstcSO2$City
cSO2List

sc_cSO2 <- pol[pol$City == "Winston-Salem" | pol$City == "Ashland" |
                 pol$City == "St. Ann" | pol$City == "New Castle" | 
                 pol$City == "Greensburg" | pol$City == "Henderson" |
                 pol$City == "Detroit" | pol$City == "Seven Corners" |
                 pol$City == "Beaver Falls" | pol$City == "Reading"
               , c("City","SO2.AQI")]
#plotting
ggplot(sc_cSO2, aes(x=City, y=SO2.AQI)) +
  geom_point(shape=1) +
  labs(x = "City", y = "SO2.AQI", 
       title = "10 cities with the worst SO2.AQI")



#10 cities with the worst CO.AQI
cCO <- pol[,c("City","CO.AQI")]
#group same cities using mean value
meancCO <- cCO %>%
  group_by(City) %>%
  summarise(
    CO.AQI = mean(CO.AQI, na.rm = T)
  ) 
#sort the data
sortcCO <- order(meancCO[, "CO.AQI"])
sortedcCO <- meancCO[sortcCO, , drop = FALSE]
worstcCO <- tail(sortedcCO,10) #get last 10 rows

format_cCO <- format(round(worstcCO$CO.AQI, 2), nsmall = 2)

# plot and see
ggplot(worstcCO, aes(x= reorder(City, -CO.AQI), y=CO.AQI, fill=City)) +
  geom_bar(stat="identity") + scale_fill_brewer(palette = "Set3") + 
  geom_text(aes(x = City, y = CO.AQI, label = format_cCO, vjust = -0.3)) +
  labs(x = "City", y = "CO.AQI(Mean)", 
       title = "10 cities with the worst CO.AQI")



#10 states with the worst NO2.AQI
sNO2 <- pol[,c("State","NO2.AQI")]
#group same states using mean value
meansNO2 <- sNO2 %>%
  group_by(State) %>%
  summarise(
    NO2.AQI = mean(NO2.AQI, na.rm = T)
  ) 
#sort the data
sortsNO2 <- order(meansNO2[, "NO2.AQI"])
sortedsNO2 <- meansNO2[sortsNO2, , drop = FALSE]
worstsNO2 <- tail(sortedsNO2,10) #get last 10 rows

format_sNO2 <- format(round(worstsNO2$NO2.AQI, 2), nsmall = 2)

# plot and see
ggplot(worstsNO2, aes(x= reorder(State, -NO2.AQI), y=NO2.AQI, fill=State)) +
  geom_bar(stat="identity") + scale_fill_brewer(palette = "Paired") + 
  geom_text(aes(x = State, y = NO2.AQI, label = format_sNO2, vjust = -0.3)) +
  labs(x = "State", y = "NO2.AQI(Mean)", 
       title = "10 States with the worst NO2.AQI")


#10 states with the worst O3.AQI
sO3 <- pol[,c("State","O3.AQI")]
meansO3 <- sO3 %>%
  group_by(State) %>%
  summarise(
    O3.AQI = mean(O3.AQI, na.rm = T)
  ) 
#find 10 states with worst O3.AQI
sortsO3 <- order(meansO3[, "O3.AQI"])
sortedsO3 <- meansO3[sortsO3, , drop = FALSE]
worstsO3 <- tail(sortedsO3,10) #get last 10 rows
sO3List <- worstsO3$State
sO3List
#boxplot
box_sO3 <- pol[pol$State == "Nevada" | pol$State == "Oklahoma" |
                 pol$State == "New Mexico" | pol$State == "Wyoming" | 
                 pol$State == "Indiana" | pol$State == "Missouri" |
                 pol$State == "Utah" | pol$State == "Kentucky" |
                 pol$State == "North Carolina" | pol$State == "Tennessee" , 
               c("State", "O3.AQI")]
boxplot(O3.AQI ~ State, data=box_sO3, 
        horizontal = TRUE , col =  c("cyan",rgb(.4,.3,0,0.5)))



#10 states with the worst SO2.AQI
sSO2 <- pol[,c("State","SO2.AQI")]
meansSO2 <- sSO2 %>%
  group_by(State) %>%
  summarise(
    SO2.AQI = mean(SO2.AQI, na.rm = T)
  ) 
sortsSO2 <- order(meansSO2[, "SO2.AQI"])
sortedsSO2 <- meansSO2[sortsSO2, , drop = FALSE]
worstsSO2 <- tail(sortedsSO2,10) #get last 10 rows
sSO2List <- worstsSO2$State
sSO2List

sc_sSO2 <- pol[pol$State == "District Of Columbia" | pol$State == "Illinois" |
                 pol$State == "New York" | pol$State == "Alaska" | 
                 pol$State == "Indiana" | pol$State == "Ohio" |
                 pol$State == "Pennsylvania" | pol$State == "Michigan" |
                 pol$State == "Missouri" | pol$State == "Kentucky"
               , c("State","SO2.AQI")]
#plotting
ggplot(sc_sSO2, aes(x=State, y=SO2.AQI)) +
  geom_point(shape=1) +
  labs(x = "State", y = "SO2.AQI", 
       title = "10 states with the worst SO2.AQI")



#10 states with the worst CO.AQI
sCO <- pol[,c("State","CO.AQI")]
meansCO <- sCO %>%
  group_by(State) %>%
  summarise(
    CO.AQI = mean(CO.AQI, na.rm = T)
  ) 
sortsCO <- order(meansCO[, "CO.AQI"])
sortedsCO <- meansCO[sortsCO, , drop = FALSE]
worstsCO <- tail(sortedsCO,10) #get last 10 rows

format_sCO <- format(round(worstsCO$CO.AQI, 2), nsmall = 2)

# plotting
ggplot(worstsCO, aes(x= reorder(State, -CO.AQI), y=CO.AQI, fill=State)) +
  geom_bar(stat="identity") + scale_fill_brewer(palette = "Set3") + 
  geom_text(aes(x = State, y = CO.AQI, label = format_sCO, vjust = -0.3)) +
  labs(x = "State", y = "CO.AQI(Mean)", 
       title = "10 states with the worst CO.AQI")


