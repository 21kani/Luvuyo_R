#Day_R_12/04/19
#12 April 2018
#Luvuyo Kani
#Biostat stat course
#Practice of some of the concepts that we will encounter


# Load packages -----------------------------------------------------------
library(tidyverse)

# Intergers ---------------------------------------------------------------
integer <- as.integer(seq(5,14, by =1)) #discrete values like how many members does a family have

# Looking at the summary of our integers
summary(integer)


# Continuous --------------------------------------------------------------
#generate a sequence of numeric values
numeric_r <- seq(23, 43, length.out = 10) #Generating a seq of numeric values 
#length.out describes the desired length of the sequence


# Dates -------------------------------------------------------------------

#Arithmetic with dates
as.Date("2005-12-31")- as.Date("2005-12-31")
#or for example
dates_r <- seq(as.Date("2005-12-16"), as.Date("2005-12-25"), by = "day")
#this is a sequence of dates between two dates by each day.
summary(dates_r)


# Dataframes --------------------------------------------------------------
#Creating the baser dataframe
df_r <- data.frame(integers = integer,
                   numeric = numeric_r,
                   dates = dates_r)
#upgrade to tibble
df_r <- as_tibble(df_r)
summary(df_r)

#the strings of data/sequences need to be the same length 

# Categories --------------------------------------------------------------

#Eletronic devices
ele_r <- as.factor(c("laptops","desktops", "cell phones"))


#People
people_r <- as.factor(c("funny", "beautiful", "beanies"))

#Colours
colours_r <- as.factor(c("red", "blue"))


# Ordinal data ------------------------------------------------------------

#here we still have qualitative data but with some sort of order

colour_qual <- ordered(c("blue", "green", "yellow", "orange", "red"),
                          levels = c("blue", "green", "yellow","orange","red"))



# Binary ------------------------------------------------------------------

#These are generrally represented as : True of false
binary_r <- c(TRUE, FALSE, TRUE, TRUE)
summary(binary_r)

# Character data ----------------------------------------------------------

sites_r <- c("Yztervarkpunt", "Betty's Bay", "Gansbaai", "Sea Point") 



# Missing values ----------------------------------------------------------

chicks_nest <- c(3, 2, 0, 10, 5, 6, 8, 2, 4, NA)#10 nests but the 10th one was not recorded because it started raining
summary(chicks_nest)


# Viewing data ------------------------------------------------------------

#just name the dataframe
ChickWeight

summary(ChickWeight) #summarizes the dataset
  
# to view the first/last few records of a dataset we use head and tail functions respectively.
#for example
head(ChickWeight, 7) #first 7 records of the dataset
tail(ChickWeight, 7) #last 7 records of the dataset



# Descriptive statistics --------------------------------------------------
#First create a dataframe
Chicks <- as_tibble(ChickWeight)

#Counting the data
Chicks %>%
  summarise(n())
#or
nrow(Chicks)



# Measures of central tendency --------------------------------------------

#Caltulate mean weight
Chicks %>% 
  summarise(mean_wt = mean(weight)) 

#Be more specific
Chicks %>% 
  filter(Time == 21) %>%  #double == sign becuse is a logical argument not a numeric
  group_by(Diet) %>% 
  summarise(mean_wt = weight), median_wt = median(weight))


# Visualise the density of the data ---------------------------------------
ggplot(data = filter(Chicks, == 21),
       aes(x = weight, fill = Diet))+
  geom_density(alpha = 0.4)


# Skewness ----------------------------------------------------------------

#Caltulate the numeric value
#First load packages
library(e1071)


# Compare differnces in mean and median against skewness ------------------
Chicks %>% 
  filter(Time == 21) %>%  #double == sign becuse is a logical argument not a numeric
  group_by(Diet) %>% 
  summarise(mean_wt = weight,
            median_wt = median(weight),
            skew_wt = skewness(weight))
#median is the middle of the data
#mean is the core of the data


# Kurtosis ----------------------------------------------------------------

#Caltulate the kurtosis of the tails of a distributions
Chicks %>% 
  filter(Time == 21) %>%  #double == sign becuse is a logical argument not a numeric
  group_by(Diet) %>% 
  summarise(mean_wt = weight,
            median_wt = median(weight),
            skew_wt = skewness(weight),
            kurtosis_wt = kurtosis(weight))




# Variation ---------------------------------------------------------------

#Below is a summary of many different statisticasl properties
wt_summary <- Chicks %>% 
  filter(Time == 21) %>% 
  group_bt(Diet) %>% 
  summarise(wt_mean = mean(weight),
            wt_median = median(weight),
            wt_var = var(weight),
            wt_sd = sd(weight),
            wt_min = min(weight),
            wt_quart1 = quantile(weight, p 0.25),
            wt_quart3 = quantile(weight),p 0.75))



