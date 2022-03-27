
# Mar. 2022
# By PM

library(dplyr)
library(ggplot2)

# read in a CSV file
gdp <- read.csv("GDPperCap.csv")
country_code <- read.csv("Metadata_Country.csv")

can_tha_mex_wld <- gdp[gdp$Country.Name == "Canada" | gdp$Country.Name == "Thailand"
                   | gdp$Country.Name == "Mexico" | gdp$Country.Name == "World", ]

# initial a new data frame
ccode <- as.data.frame(NULL)

# add each row with, from cross table information with same country code
for (i in can_tha_mex_wld$Country.Code){
  cc <- country_code[country_code$Country.Code==i, ] 
  ccode <- rbind(ccode,cc)  # binding by row
}

# combine some columns of the two data frames
can_tha_mex_wld <- cbind(can_tha_mex_wld,ccode$Region)  # binding by column
can_tha_mex_wld <- cbind(can_tha_mex_wld,ccode$IncomeGroup)

# transpose the data frame
new_df <- as.data.frame(t(can_tha_mex_wld))

# select some rows
nd <- new_df[-c(1:11,45,46),]

# see some simple plots
# prepare data for x, y axes
can_gdpCap <- new_df[-c(1:11,45,46),1]  
tha_gdpCap <- new_df[-c(1:11,45,46),2]
mex_gdpCap <- new_df[-c(1:11,45,46),3]
wld_gdpCap <- new_df[-c(1:11,45,46),4]

yrs <- c(1987:2019)

plot(yrs, can_gdpCap)
plot(yrs, tha_gdpCap)
plot(yrs, mex_gdpCap)
plot(yrs, wld_gdpCap)

plot(yrs, can_gdpCap, type = "b", col= "dark red",
     main = "Canada's, Thailand's, Mexico's, and World's GDP per capita",
     sub = "Red -> Canada; Green -> Thailand; Blue -> Mexico; Purple -> World",
     xlab = "Years", ylab = "GDP per capita ($US)",ylim= c(1000,55000))

lines(yrs, tha_gdpCap, col = "dark green")
points(yrs, tha_gdpCap, col = "dark green")

lines(yrs, mex_gdpCap, col = "dark blue")
points(yrs, mex_gdpCap, col = "dark blue")

lines(yrs, wld_gdpCap, col = "Purple")
points(yrs, wld_gdpCap, col = "Purple")


#Combine 2 table together (gdp,country_code)
newgdp <- gdp[-c(109),] #'Not classified' row

newccode <- as.data.frame(NULL)

for (i in newgdp$Country.Code){
  newcc <- country_code[country_code$Country.Code==i, ] 
  newccode <- rbind(newccode,newcc)  # binding by row
}

newgdp <- cbind(newgdp,newccode$Region)  
newgdp <- cbind(newgdp,newccode$IncomeGroup)

data <- newgdp[,-c(2:44)]

colnames(data)[2] <- "Region"
colnames(data)[3] <- "IncomeGroup"

#count number of country
count_c <- data %>% group_by(Region) %>% summarise(count_country = n())

x <- c(count_c$count_country)
labels <- c(count_c$Region)


# Plot pie chart.
pie_labels <- paste0(round(100 * x/sum(x), 2), "%")
pie(x, labels = pie_labels, fill = "Region")

pie(x,labels = count_c$Region)


unique(country_code$Country.Code)
unique(data$IncomeGroup)
unique(data$Region)


# Stack bar chart
count_g <- data %>% group_by(Region,IncomeGroup) %>% summarise(count_income = n())
ggplot(count_g, aes(fill=IncomeGroup, y=count_income, x=Region)) + 
  geom_bar(position='stack', stat='identity')+
  labs(x = "Region", y = "IncomeGroup", 
       title = "Group of income in each region")



#boxplot
gdp3y<- newgdp[,-c(2:41,46)]
colnames(gdp3y)[5] <- "Region"
df <- data.frame(Country.name =gdp3y[,1], Region = gdp3y[,5] , Means=rowMeans(gdp3y[,2:4], na.rm = TRUE))

box_gdp <- df[df$Region == "Latin America & Caribbean" | df$Region == "South Asia" |
                 df$Region == "Sub-Saharan Africa" | df$Region == "Europe & Central Asia" | 
                 df$Region == "" | df$Region == "Middle East & North Africa" |
                 df$Region == "East Asia & Pacific" | df$Region == "North America" , 
               c("Region", "Means")]
boxplot(Means ~ Region, data=box_gdp, 
        horizontal = FALSE , col =  c("cyan",rgb(.4,.3,0,0.5)))



