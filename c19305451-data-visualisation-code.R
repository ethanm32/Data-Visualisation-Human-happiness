library(tidyverse)
library(ggplot2)
library(dplyr)
library(leaflet)
library(data.table)
#install.packages("plotly")
library(plotly)
#install.packages("maps")
library(maps)

happinessdf <- read.csv("happiness.csv")
happinessdf

dfCountries <- read.csv("countries.csv")
dfCountries

dfContinents <- read.csv("continents.csv")
dfContinents

#ensuring datasets can be merged
names(happinessdf)[names(happinessdf) == "Country.name"] <- "country"

names(dfContinents)[names(dfContinents) == "Country"] <- "country"

#merging datasets by country column
merged_df = merge(happinessdf, dfCountries, by = c("country"))
merged_df

dfHappinessCountries = merge(merged_df, dfContinents, by = c("country"))



#removing unnecessary columns
dfHappinessCountries <- select(dfHappinessCountries, -Positive.affect, -Negative.affect, -country_code, -Generosity)

#making sure only the years after 2011 are in the dataset
dfHappinessCountries <- subset(dfHappinessCountries, Year >= 2011)

#grouping by country and year
dfHappinessCountries <- dfHappinessCountries %>%
  group_by(country) %>%
  arrange(country, Year)

#main data set that is used for graphs to pull averages etc. from
dfHappinessCountries



#getting the average happiness for each country
happiness_by_country <- dfHappinessCountries %>% 
  group_by(country, latitude, longitude) %>% 
  summarise(Average_Happiness = mean(Life.Ladder),
            Average_GDP = mean(Log.GDP.per.capita),
            Average_Life_Exp = mean(Healthy.life.expectancy.at.birth))


#ensuring only one of each country is used
happiness_by_country <- happiness_by_country %>% 
  distinct(country, .keep_all = TRUE)

#ensuring green and red are the opposite ends of the happiness scale 
colours <- scales::col_numeric(c("red", "green"), domain = c(min(happiness_by_country$Average_Happiness), max(happiness_by_country$Average_Happiness)))


#creates map using leaflet
leaflet(data = happiness_by_country) %>% 
  addTiles() %>% 
  setView(lng = 0, lat = 0, zoom = 2) %>% 
  addCircleMarkers(lng = ~longitude, lat = ~latitude, 
                   radius = ~sqrt(Average_Happiness)*2, 
                   color = ~colours(Average_Happiness), 
                   fillOpacity = 0.8,
                   popup = paste("<b>", happiness_by_country$country, "</b><br>",
                                 "Log GDP per capita: ", happiness_by_country$Average_GDP, "<br>",
                                 "Life Expectancy: ", round(happiness_by_country$Average_Life_Exp), " years<br>",
                                 "Average Happiness: ", happiness_by_country$Average_Happiness, 2)) %>% 
  addLegend(position = "bottomright", pal = colours, values = happiness_by_country$Average_Happiness,
          title = "Average Happiness") 


#this filters out any countries with nas in the categories given to clean up the data set
na_countries <- dfHappinessCountries %>%
  filter(is.na(Log.GDP.per.capita) | is.na(Perceptions.of.corruption) | is.na(Freedom.to.make.life.choices) | is.na(Confidence.in.national.government)) %>%
  select(country) %>% 
  distinct()

#gets rid of the nas based on the filter
dfHappinessCountries <- dfHappinessCountries %>% 
  filter(!country %in% na_countries$country)



#gets the average happiness of each continent over the years 
avg_continent_happiness <- dfHappinessCountries %>% 
  group_by(Continent, Year) %>% 
  summarize(avg_life_ladder = mean(Life.Ladder, na.rm = TRUE))


#creates a plot that shows the average happiness over the years
ggplot(avg_continent_happiness, aes(x = Year, y = avg_life_ladder, group = Continent, color = Continent)) +
  geom_line(linewidth = 1.5) +
  labs(x = "Year", y = "Average Happiness", title = "Average Happiness by Continent and Year") + 
  scale_x_continuous(limits = c(2011, 2021), breaks = seq(2011, 2021, 2))


#creates a average country happiness data frame from the main data frame that has all the following columns to be used for graphs
avg_country_happiness <- dfHappinessCountries %>% 
  group_by(country,Continent) %>% 
  summarise(Average_Happiness = mean(Life.Ladder,na.rm = TRUE),
            Average_GDP = mean(Log.GDP.per.capita,na.rm = TRUE),
            Average_Life_Exp = mean(Healthy.life.expectancy.at.birth,na.rm = TRUE),
            Average_Freedom = mean(Freedom.to.make.life.choices,na.rm = TRUE),
            Average_Corruption = mean(Perceptions.of.corruption, na.rm = TRUE),
            Average_Social = mean(Social.support, na.rm = TRUE))


#makes a scatter plot for average happiness vs life expectancy
life_expectancy_happiness <- ggplot(avg_country_happiness, aes(x = Average_Happiness, y = Average_Life_Exp, fill = Continent,text = paste("country:", country))) +
  geom_point(size = 3, shape = 21) +
  labs(x = "Average Happiness", y = "Average Life Expectancy", title = "Relationship between Happiness and Life Expectancy by Country 2011-2021") +
  scale_x_continuous(limits = c(3, 8), expand = c(0, 0)) +
  theme_minimal()

#makes it so users can click dots and see the country name
fig_life_happiness <- ggplotly(life_expectancy_happiness)

fig_life_happiness


#makes a scatter plot for gdp vs happiness
gdp_happiness <- ggplot(avg_country_happiness, aes(x = Average_Happiness, y = Average_GDP, fill = Continent,text = paste("country:", country))) +
  geom_point(size = 3, shape = 21) +
  labs(x = "Average Happiness", y = "Average GDP per capita(Log scale)", title = "Relationship between Happiness and GDP by Country 2011-2021") +
  scale_x_continuous(limits = c(3, 8), expand = c(0, 0)) +
  theme_minimal()

fig_gdp_happiness <- ggplotly(gdp_happiness)

fig_gdp_happiness

#makes a scatter plot for freedom vs corruption
freedom_corruption <- ggplot(data = avg_country_happiness, aes(x = Average_Freedom, y = Average_Corruption, color = Average_Happiness,text = paste("country:", country))) +
  geom_point(size = 3) +
  scale_color_gradient(low = "red", high = "green") +
  labs(x = "Freedom", y = "Corruption", color = "Happiness", title = "Relationship between Freedom and Corruption in a Country based on happiness 2011-2021") +
  theme_minimal()

fig_freedom_corruption <- ggplotly(freedom_corruption)
fig_freedom_corruption


#makes a bubble chart for social support vs GDP with average happiness as the size of the bubbles
ggplot(avg_country_happiness, aes(x = Average_GDP, y = Average_Social, size = Average_Happiness, color = Continent,text = paste("country:", country))) +
  geom_point(alpha = 0.7) +
  scale_x_log10() + 
  scale_size(range = c(1,20)) + 
  labs(x = "GDP per capita(log scale)", y = "Social Support", title = "Chart to show correlation of GDP and Social Support in countries by happiness, coloured by continents 2011-2021") + 
  theme_minimal()



