#Day_2 stats
#April 13 2018
#Luvuyo Kani
#Discussing data visuallisations and distribution


# Load libraries ----------------------------------------------------------

library(tidyverse)



# Manual caltulations -----------------------------------------------------

#Generating random data
r_dat <- data.frame(dat = rnorm(n = 600, mean = 372, sd = 50),
                    sample = "A") #random normal data 

#Quick visualisations
ggplot(data = r_dat, aes(x = dat))+
  geom_density()

#The mean
#The mean: is the sum of all the points divided by the number of points

r_dat %>% 
  summarise(r_sum = sum(dat),
            r_n = n(),
            r_mean = r_sum/r_n,
            r_mean_func = mean(dat))

#The median
r_dat$dat[(length(r_dat$dat)+1)/2]

#Or use tidy
r_dat %>% 
  arrange(dat) %>% 
  slice(n()/2)


#Or the tidy automagic way
r_dat %>% 
  summarise(r_median = median(dat))


#variance
#The variance is the sum each of the values, minus the  mean squared, divided by the count of samples minus 1

r_dat %>%  
  mutate(r_error = dat-mean(dat),
         r_error_square = r_error*r_error) %>% 
  summarise(r_squared_sum = sum(r_error_square),
            r_var = r_squared_sum/n()-1)

            
#The standard deviation 
r_dat %>% 
  summarise(r_var = var(dat),
            r_sd = sqrt(r_var),
            r_sd_func = sd(dat))





# Exercise 1 --------------------------------------------------------------

summary(ChickWeight$weight)

ChickWeight %>% 
  summarise(min_weight = min(weight),
            quart_1 = quantile(weight,0.25),
            med_weight = median(weight),
            mean_weight = mean(weight),
            quart_3 = quantile(weight, 0.75),
            max_weight = max(weight))


# Chapter 4 Graphical Displays --------------------------------------------
#Loading packages
library(tidyverse)
library(ggpubr)
library(RColorBrewer)
library(viridis)

#Load our SA time data
sa_time <- read_csv("SA_time.csv")


#Edit our data
sa_time <- sa_time %>% 
  mutate(human = seq(1,n(), 1),
         geo = c(rep(c("Cape Town", "George", "PE"), times = 6),
                 rep("Joburg", 2)))

sa_long <- sa_time %>% 
  gather(key = "time_type", value = minutes, -human, -geo)

# Qualitative -------------------------------------------------------------
#Create a count of qualitative values

sa_count <- sa_long %>%
  count(time_type) %>% 
  mutate(prop = n/sum(n))


#Stacked bar graph
ggplot(data = sa_count, aes(x = "", y = n, fill = time_type)) +
  geom_bar(width = 1, stat = "identity") +
  labs(title = "Stacked bar graph", subtitle = "cumulative sum",
       x = NULL, y = "Count") +
  theme_minimal()

ggplot(data = sa_count, aes(x = "", y = prop, fill = time_type)) +
  geom_bar(width = 1, stat = "identity") +
  scale_y_continuous(breaks = c(0.00, 0.33, 0.66, 1.00)) +
  labs(title = "Stacked bar graph", subtitle = "relative proportions",
       x = NULL, y = "Proportion") +
  theme_minimal()

#stacked proportion bar graph 

ggplot(data = sa_count, aes(x = "", y = prop, fill = time_type)) +
  geom_bar(width = 1, stat = "identity") +
  scale_y_continuous(breaks = c(0.00, 0.33, 0.66, 1.00)) +
  labs(title = "Stacked bar graph", subtitle = "relative proportions",
       x = NULL, y = "Proportion") +
  theme_minimal()

#a pie chart 

ggplot(data = sa_count, aes(x = "", y = n, fill = time_type)) +
  geom_bar(width = 1, stat = "identity") +
  labs(title = "pie Chart", subtitle = "but why though", 
       x = NULL, y= NULL) +
  coord_polar("y", start = 0) +
  theme_minimal()


# Quantitative continuous data --------------------------------------------

#Histograms
ggplot(data = sa_long, aes(x  = minutes))+
  geom_histogram()


#Lets get rid of that one outlier
sa_clean <- sa_long %>% 
  filter(minutes < 300) 

#Try again
ggplot(data = sa_clean, aes(x = minutes))+
  geom_histogram(aes(fill = time_type), posistion = "dodge")+
  facet_wrap(~time_type, ncol = 1, scales = "free_x")


#Relative proportion histogram
ggplot(data = sa_clean, aes(x = minutes))+
  geom_histogram(aes(y = ..density.. ,fill = time_type),
                 position = "dodge", binwidth = 1)+
  facet_wrap(~time_type, ncol = 1, scales = "free_x")

#boxplots
ggplot(data = sa_clean, aes(x = time_type, y = minutes))+
  geom_boxplot(aes(fill = time_type))

#Notched boxplot
ggplot(data = sa_clean, aes(x = time_type, y = minutes))+
  geom_boxplot(aes(fill = time_type, notch = TRUE))

#Calculate summary stats for plotting over the boxplots
sa_summary_stats <- sa_clean %>% 
  group_by(time_type) %>% 
  summarise(time_type_mean = mean(minutes))

#plot these means over the boxplots
ggplot(data = sa_clean, aes(x = time_type, y = minutes))+
  geom_boxplot(aes(fill = time_type), notch = TRUE)+
  geom_point(data = sa_summary_stats, size = 6, shape = 18,
             aes(y = time_type_mean, colour = "goldenrod"))



# Relationships -----------------------------------------------------------

#A basic scatterplot
ggplot(data = sa_time,aes(y = now_now, x = just_now)) +
  geom_point() +
  coord_equal(xlim = c(0, 60), ylim = c(0, 60)) +
  labs(x = "Just Now (minutes)", y = "Now Now (minutes)")

#adding trend lines 

ggplot(data = sa_time,aes(y = now_now, x = just_now)) +
  geom_point(aes(colour = geo)) +
  geom_smooth(aes(colour = geo), method = "lm") +
  coord_equal(xlim = c(0, 60), ylim = c(0, 60)) +
  labs(x = "Just Now (minutes)", y = "Now Now (minutes)")
