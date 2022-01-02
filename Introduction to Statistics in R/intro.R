library(dplyr)
library(ggplot2)
library(tidyr)
library(tidyverse)
library(data.table)
food_consumption<-readRDS("food_consumption.rds")
seller_1<-readRDS("seller_1.rds")

world_happiness_sugar<-readRDS("world_happiness_sugar.rds")


# Filter for Belgium
belgium_consumption <- food_consumption %>%
  filter(country=="Belgium")

# Filter for USA
usa_consumption <- food_consumption %>%
  filter(country=="USA")

# Calculate mean and median consumption in Belgium
mean(belgium_consumption$consumption)
median(belgium_consumption$consumption)

# Calculate mean and median consumption in USA
mean(usa_consumption$consumption)
median(usa_consumption$consumption)

food_consumption %>%
  # Filter for Belgium and USA
  filter(country %in% c("Belgium", "USA")) %>%
  # Group by country
  group_by(country) %>%
  # Get mean_consumption and median_consumption
  summarize(mean_consumption = mean(consumption),
      median_consumption = median(consumption))
