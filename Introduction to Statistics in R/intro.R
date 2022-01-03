library(dplyr)
library(ggplot2)
library(tidyr)
library(tidyverse)
library(data.table)
food_consumption<-readRDS("food_consumption.rds")
seller_1<-readRDS("seller_1.rds")
amir_deals<-seller_1
world_happiness_sugar<-readRDS("world_happiness_sugar.rds")
world_happiness<-world_happiness_sugar

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
pbinom(1,3,0.3)

# Probability of closing > 1 deal out of 3 deals
pbinom(1,3,0.3, lower.tail=FALSE)
1-pbinom(1,3,0.3)

# Expected number won with 30% win rate
won_30pct <- 3*0.3
won_30pct

# Expected number won with 25% win rate
won_25pct <- 3*0.25
won_25pct

# Expected number won with 35% win rate
won_35pct <- 3*0.35
won_35pct

# Histogram of amount with 10 bins
ggplot(amir_deals,aes(amount))+geom_histogram(bins=10)

# Probability of deal < 7500
pnorm(7500,mean=5000,sd=2000)

# Probability of deal > 1000
pnorm(1000,mean=5000,sd=2000,lower.tail=FALSE)

# Probability of deal between 3000 and 7000
pnorm(7000,mean=5000,sd=2000)-pnorm(3000,mean=5000,sd=2000)

# Calculate amount that 75% of deals will be more than
qnorm(0.75, mean=5000,sd=2000, lower.tail=FALSE)

new_sales<-1:36
new_sales<-data.frame(new_sales)
new_sales

# Calculate new average amount
new_mean <- 5000*1.2

# Calculate new standard deviation
new_sd <- 2000*1.3

# Simulate 36 sales
new_sales <- new_sales %>% 
  mutate(amount = rnorm(36,mean=new_mean,sd=new_sd))

# Create histogram with 10 bins
ggplot(new_sales,aes(amount))+geom_histogram(bins=10)

# Create a histogram of num_users
ggplot(amir_deals,aes(num_users))+geom_histogram(bins=10)

# Set seed to 104
set.seed(104)

# Sample 20 num_users with replacement from amir_deals
sample(amir_deals$num_users, 20, replace=TRUE) %>%
  # Take mean
  mean()
# Repeat the above 100 times
sample_means <- replicate(100, sample(amir_deals$num_users, size = 20, replace = TRUE) %>% mean())

# Create data frame for plotting
samples <- data.frame(mean = sample_means)

# Histogram of sample means
ggplot(samples,aes(mean)) +
  geom_histogram(bins=10)

library(data.table)

all_deals <- read.table("all_deals.txt",sep="\t", col.names=c("index","product","num_users") )
all_deals <- read.csv("all_deals.txt", col.names=c("index","product","num_users") )
all_deals <- fread("all_deals.txt", col.names=c("index","product","num_users") )
all_deals<-data.frame(all_deals)
head(all_deals)
all_deals<-all_deals%>%select(product,num_users)
head(all_deals)


# Set seed to 321
set.seed(321)

# Take 30 samples of 20 values of num_users, take mean of each sample
sample_means <- replicate(30, sample(all_deals$num_users, 20) %>% mean())

# Calculate mean of sample_means
mean(sample_means)

# Calculate mean of num_users in amir_deals
mean(amir_deals$num_users)

# Probability of 5 responses
dpois(5,lambda=4)
# Probability of 5 responses from coworker
dpois(5,lambda=5.5)
# Probability of 2 or fewer responses
ppois(2,lambda=4)
# Probability of > 10 responses
ppois(10,lambda=4,lower.tail=FALSE)
#What's the probability it takes Amir less than an hour to respond to a lead?
#To further evaluate Amir's performance, you want to know how much time it takes him to respond to a lead after he opens it. On average, it takes 2.5 hours for him to respond. In this exercise, you'll calculate probabilities of different amounts of time passing between Amir receiving a lead and sending a response.
#rate=1/lambda 
#here rate = 1/2.5
#then Rate=0.4
# Probability response takes < 1 hour
pexp(1,rate=0.4)

# Probability response takes > 4 hours
pexp(4,rate=0.4,lower.tail=FALSE)
# Probability response takes 3-4 hours
pexp(4,rate=0.4)-pexp(3,rate=0.4)

# Add a linear trendline to scatterplot
ggplot(world_happiness, aes(life_exp, happiness_score)) +
  geom_point() +
  geom_smooth(method="lm",se=FALSE)
cor(world_happiness$life_exp,world_happiness$happiness_score)

# Scatterplot of gdp_per_cap and life_exp
ggplot(world_happiness, aes(gdp_per_cap, life_exp)) +
  geom_point()

# Correlation between gdp_per_cap and life_exp
cor(world_happiness$gdp_per_cap,world_happiness$life_exp)


# Scatterplot of happiness_score vs. gdp_per_cap
ggplot(world_happiness,aes(gdp_per_cap,happiness_score))+geom_point()

# Calculate correlation
cor(world_happiness$gdp_per_cap,world_happiness$happiness_score)

# Create log_gdp_per_cap column
world_happiness <- world_happiness %>%
  mutate(log_gdp_per_cap = log(gdp_per_cap))

# Scatterplot of log_gdp_per_cap vs. happiness_score
ggplot(world_happiness, aes(log_gdp_per_cap,happiness_score)) +
  geom_point()

# Calculate correlation
cor(world_happiness$log_gdp_per_cap, world_happiness$happiness_score)

# Scatterplot of grams_sugar_per_day and happiness_score
ggplot(world_happiness,aes(grams_sugar_per_day,happiness_score))+geom_point()

# Correlation between grams_sugar_per_day and happiness_score
cor(world_happiness$grams_sugar_per_day,world_happiness$happiness_score)
