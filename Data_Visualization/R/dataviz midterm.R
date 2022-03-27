library(dplyr)
library(ggplot2)
library(tidyverse)

data <- read.csv("csvData.csv")
data <- csvData

death <- head(data,10)
# plot and see
ggplot(death, aes(x= reorder(country, -totalDeaths), y=totalDeaths, fill=country)) +
  geom_bar(stat="identity") + scale_fill_brewer(palette = "Paired") + 
  geom_text(aes(x = country, y = totalDeaths, label = totalDeaths, vjust = -0.3)) +
  labs(x = "Country", y = "totalDeaths", 
       title = "10 country with most total number of deaths")

x <- c(death$totalDeaths)
labels <- c("China","Russia","Ukraine", "Poland", "Germany", 
            "Japan", "India", "Belarus", "Hungary", "Greece")

# Plot the chart.
pie(x,labels)
