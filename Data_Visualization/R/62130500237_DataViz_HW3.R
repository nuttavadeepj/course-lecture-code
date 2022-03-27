
#HW3
#Nuttavadee Autsavapanakit 62130500237

library(tidyverse) #for DS DA
library(socviz) #for visualize
library(gapminder) #for dataset

gapminder
gap <- gapminder


#Thai & ASEAN
asean <- gap[gap$country == "Malaysia" | gap$country == "Myanmar" | gap$country == "Cambodia" 
             | gap$country == "Loas" | gap$country == "Singapore" | gap$country == "Vietnam" 
             | gap$country == "Brunei" | gap$country == "Philippines" 
             | gap$country == "Indonesia" | gap$country == "Thailand",]

#GDP
ggplot(data=asean, mapping = aes(x=year, y=gdpPercap, color=country)) +
  geom_point() +
  geom_smooth(se=FALSE) +
  labs(x = "Year",
       y = "GDP per capita",
       title = "GDP per capita trends of Thailand compare to ASEAN")

#Life Expextancy
ggplot(data=asean, mapping = aes(x=year, y=lifeExp, color=country)) +
  geom_point() +
  geom_smooth(se=FALSE) +
  labs(x = "Year",
       y = "Life expectancy",
       title = "Life expectancy trends of Thailand compare to ASEAN")

#Population Size
ggplot(data=asean, mapping = aes(x=year, y=pop, color=country)) +
  geom_point() +
  geom_smooth(se=FALSE) +
  labs(x = "Year",
       y = "Population",
       title = "Population trends of Thailand compare to ASEAN")


#Thai & Asia
asia <- gap[gap$country == "Thailand" | gap$continent == "Asia",]

#GDP
ggplot(data=asia, mapping = aes(x=year, y=gdpPercap, color=country)) +
  geom_smooth(se=FALSE) +
  labs(x = "Year",
       y = "GDP per capita",
       title = "GDP per capita trends of Thailand compare to ASIA")

#Life Expextancy
ggplot(data=asia, mapping = aes(x=year, y=lifeExp, color=country)) +
  geom_smooth(se=FALSE) +
  labs(x = "Year",
       y = "Life expectancy",
       title = "Life expectancy trends of Thailand compare to ASIA")

#Population Size
ggplot(data=asia, mapping = aes(x=year, y=pop, color=country)) +
  geom_smooth(se=FALSE) +
  labs(x = "Year",
       y = "Population",
       title = "Population trends of Thailand compare to ASIA")


#Thai & Europe
eu <- gap[gap$country == "Thailand" | gap$continent == "Europe",]

#GDP
ggplot(data=eu, mapping = aes(x=year, y=gdpPercap, color=country)) +
  geom_smooth(se=FALSE) +
  labs(x = "Year",
       y = "GDP per capita",
       title = "GDP per capita trends of Thailand compare to Europe")

#Life Expextancy
ggplot(data=eu, mapping = aes(x=year, y=lifeExp, color=country)) +
  geom_smooth(se=FALSE) +
  labs(x = "Year",
       y = "Life expectancy",
       title = "Life expectancy trends of Thailand compare to Europe")

#Population Size
ggplot(data=eu, mapping = aes(x=year, y=pop, color=country)) +
  geom_smooth(se=FALSE) +
  labs(x = "Year",
       y = "Population",
       title = "Population trends of Thailand compare to Europe")


#Thai & South Americas

south <- gap[gap$country == "Argentina" | gap$country == "Bolivia" | gap$country == "Brazil" 
             | gap$country == "Chile" | gap$country == "Colombia" | gap$country == "Ecuador" 
             | gap$country == "Guyana" | gap$country == "Paraguay" | gap$country == "Thailand"  
             | gap$country == "Peru" | gap$country == "Suriname" 
             | gap$country == "Uruguay" | gap$country == "Venezuela",]

#GDP
ggplot(data=south, mapping = aes(x=year, y=gdpPercap, color=country)) +
  geom_smooth(se=FALSE) +
  labs(x = "Year",
       y = "GDP per capita",
       title = "GDP per capita trends of Thailand compare to South Americas")

#Life Expextancy
ggplot(data=south, mapping = aes(x=year, y=lifeExp, color=country)) +
  geom_smooth(se=FALSE) +
  labs(x = "Year",
       y = "Life expectancy",
       title = "Life expectancy trends of Thailand compare to South Americas")

#Population Size
ggplot(data=south, mapping = aes(x=year, y=pop, color=country)) +
  geom_smooth(se=FALSE) +
  labs(x = "Year",
       y = "Population",
       title = "Population trends of Thailand compare to South Americas")

#Thai & Afica

af <- gap[gap$country == "Thailand" | gap$continent == "Africa",]

#GDP
ggplot(data=af, mapping = aes(x=year, y=gdpPercap, color=country)) +
  geom_smooth(se=FALSE) +
  labs(x = "Year",
       y = "GDP per capita",
       title = "GDP per capita trends of Thailand compare to Afica")

#Life Expextancy
ggplot(data=af, mapping = aes(x=year, y=lifeExp, color=country)) +
  geom_smooth(se=FALSE) +
  labs(x = "Year",
       y = "Life expectancy",
       title = "Life expectancy trends of Thailand compare to Afica")

#Population Size
ggplot(data=af, mapping = aes(x=year, y=pop, color=country)) +
  geom_smooth(se=FALSE) +
  labs(x = "Year",
       y = "Population",
       title = "Population trends of Thailand compare to Afica")

#Thai and India
ti <- gap[gap$country == "Thailand" | gap$country == "India",]

s <- ggplot(data = ti, mapping = aes(x =gdpPercap, y=lifeExp, size = pop, color = country))
s <- s + geom_point() +
  geom_smooth(method = "lm", se = TRUE) + 
  scale_x_log10(labels=scales::dollar) +
  labs(x = "GDP per capita", y = "Life expectancy", 
       title = "GDPPercap and Life Expectancy Thailand-India",
       subtitle = "Data points are country-years",
       caption = "Source: Gapminder.")
s

#Thailand and China
tc <- gap[gap$country == "Thailand" | gap$country == "China",]

u <- ggplot(data = tc, mapping = aes(x =gdpPercap, y=lifeExp, size = pop, color = country))
u <- u + geom_point() +
  geom_smooth(method = "lm", se = TRUE) + 
  scale_x_log10(labels=scales::dollar) +
  labs(x = "GDP per capita", y = "Life expectancy", 
       title = "GDPPercap and Life Expectancy Thailand-China",
       subtitle = "Data points are country-years",
       caption = "Source: Gapminder.")
u

#Thai, China, India, Asia, Europe, and Africa
continentData <- gap %>% group_by(continent,year)
medContinent <- continentData %>% summarise(gdpPercap = median(gdpPercap),
                                            lifeExp = median(lifeExp),
                                            pop = median(pop))
meanContinent <- continentData %>% summarise(gdpPercap = mean(gdpPercap),
                                             lifeExp = mean(lifeExp),
                                             pop = mean(pop))

ThChIn <- gap[gap$country == "Thailand" | gap$country == "China" | gap$country == "India",]
sThChIn <- select(ThChIn, -continent)
colnames(sThChIn)[colnames(sThChIn) == "country"] <- "continent" #rename column
combineDataMed <- rbind(medContinent, sThChIn)

#GDP
ggplot(data=combineDataMed, mapping = aes(x=year, y=gdpPercap, color=continent)) +
  geom_point() +
  geom_smooth(se=FALSE) +
  labs(x = "Year",
       y = "GDP per capita",
       title = "GDP per capita trends of Thailand compare to others",
       subtitle = "Thai, China, India, Asia, Europe, Africa, Americas, and Oceania")

#Life Expextancy
ggplot(data=combineDataMed, mapping = aes(x=year, y=lifeExp, color=continent)) +
  geom_point() +
  geom_smooth(se=FALSE) +
  labs(x = "Year",
       y = "Life expectancy",
       title = "Life expectancy trends of Thailand compare to others",
       subtitle = "Thai, China, India, Asia, Europe, Africa, Americas, and Oceania")

#Population Size
ggplot(data=combineDataMed, mapping = aes(x=year, y=pop, color=continent)) +
  geom_point() +
  geom_smooth(se=FALSE) +
  labs(x = "Year",
       y = "Population",
       title = "Population trends of Thailand compare to others",
       subtitle = "Thai, China, India, Asia, Europe, Africa, Americas, and Oceania")



