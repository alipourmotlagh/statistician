library(dplyr)
library(ggplot2)
library(tidyr)
library(tidyverse)
library(data.table)
food_consumption<-readRDS("food_consumption.rds")
seller_1<-readRDS("seller_1.rds")
amir_deals<-seller_1
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

food_consumption %>%
  # Filter for rice food category
  filter(food_category=="rice") %>%
  # Create histogram of co2_emission
  ggplot(aes(x=co2_emission)) +
    geom_histogram()

food_consumption %>%
  # Filter for rice food category
  filter(food_category=="rice") %>% 
  # Get mean_co2 and median_co2
  summarize(mean_co2 = mean(co2_emission),
            median_co2 = median(co2_emission))

# Calculate the quartiles of co2_emission
quantile(food_consumption$co2_emission)

# Calculate the quintiles of co2_emission
quantile(food_consumption$co2_emission,probs=seq(0,1,0.2))

# Calculate the deciles of co2_emission
quantile(food_consumption$co2_emission,probs=seq(0,1,0.1))

# Calculate variance and sd of co2_emission for each food_category
food_consumption %>% 
  group_by(food_category) %>% 
  summarize(var_co2 = var(co2_emission),
     sd_co2 = sd(co2_emission))

# Plot food_consumption with co2_emission on x-axis
ggplot(food_consumption,aes(co2_emission)) +
  # Create a histogram
  geom_histogram() +
  # Create a separate sub-graph for each food_category
  facet_wrap(~ food_category)

# Calculate total co2_emission per country: emissions_by_country
emissions_by_country <- food_consumption %>%
  group_by(country) %>%
  summarize(total_emission = sum(co2_emission))

# Compute the first and third quantiles and IQR of total_emission
q1 <- quantile(emissions_by_country$total_emission, 0.25)
q3 <- quantile(emissions_by_country$total_emission, 0.75)
iqr <- q3 - q1

# Calculate the lower and upper cutoffs for outliers
lower <- q1 - 1.5 * iqr
upper <- q3 + 1.5 * iqr

# Filter emissions_by_country to find outliers
emissions_by_country %>%
  filter(total_emission>upper | total_emission<lower)

# Calculate probability of picking a deal with each product
amir_deals %>%
  count(product) %>%
  mutate(prob = n/sum(n))
# Set random seed to 31
set.seed(31)

# Sample 5 deals without replacement
amir_deals %>%
  sample_n(5)
# Set random seed to 31
set.seed(31)

# Sample 5 deals with replacement
amir_deals %>%
  sample_n(5,replace=TRUE)

group_id<-c('A','B','C','D','E','F','G','H','I','J')
group_size<-c(2,4,6,2,2,2,3,2,4,2)
restaurant_groups<-data.frame(group_id,group_size)
restaurant_groups
# Create a histogram of group_size
ggplot(restaurant_groups,aes(group_size)) +
  geom_histogram(bins=5)

# Create probability distribution
size_distribution <- restaurant_groups %>%
  # Count number of each group size
  count(group_size) %>%
  # Calculate probability
  mutate(probability = n / sum(n))

size_distribution

# Calculate expected group size
expected_val <- sum(size_distribution$group_size *
                    size_distribution$probability)
expected_val


# Calculate probability of picking group of 4 or more
size_distribution %>%
  # Filter for groups of 4 or larger
  filter(group_size>=4) %>%
  # Calculate prob_4_or_more by taking sum of probabilities
  summarize(prob_4_or_more = sum(probability))


# Min and max wait times for back-up that happens every 30 min
min <- 0
max <- 30

# Calculate probability of waiting less than 5 mins
prob_less_than_5 <- punif(5,min=min,max=max)
prob_less_than_5


# Calculate probability of waiting more than 5 mins
prob_greater_than_5 <- punif(5,min=min,max=max,lower.tail=FALSE)
prob_greater_than_5

# Calculate probability of waiting 10-20 mins
prob_between_10_and_20 <- punif(20,min=min,max=max)-punif(10,min=min,max=max)
prob_between_10_and_20

w<-1:1000
w
wait_times<-data.frame(w)
wait_times
# Set random seed to 334
set.seed(334)
# Generate 1000 wait times between 0 and 30 mins, save in time column
wait_times %>%
  mutate(time = runif(1000, min = 0, max = 30)) 

wait_times %>%
  mutate(time = runif(1000, min = 0, max = 30)) %>%
  # Create a histogram of simulated times
  ggplot(aes(time)) +
  geom_histogram(bins=30)


# Set random seed to 10
set.seed(10)

# Simulate a single deal
rbinom(1,1,0.3)


# Simulate 1 week of 3 deals
rbinom(1,3,0.3)

# Simulate 52 weeks of 3 deals
deals <- rbinom(52,3,0.3)

# Calculate mean deals won per week
mean(deals)

# Probability of closing 3 out of 3 deals
dbinom(3,3,0.3)

# Probability of closing <= 1 deal out of 3 deals
pbinom(3,3,0.3)