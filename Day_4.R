#Day_3 script
#Luvuyo Kani
#Anovas
#19 April 2018

library(tidyverse)
library(ggplot2)



# t-test ------------------------------------------------------------------


# First grab the data
chicks <- as_tibble(ChickWeight)

# Then subset out only the sample sets to be compared
chicks_sub <- chicks %>% 
  filter(Diet %in% c(1, 2), Time == 21)

#t-test
t.test(weight ~ Diet, data = chicks_sub)



# 1 way ANOVA -------------------------------------------------------------

#Research question: is there a difference in chicken mass attained after 
#21 days after the chickens having been fed four different diets?

#Null hypothesis: There is no difference in chicken mass at 21 days after having 
#been fed 1 of 4 different diets. 

chicks_21 <- chicks %>% 
  filter(Time == 21)

chicks.aov1 <- aov(weight ~ Diet, data = chicks_21)
summary(chicks.aov1)

ggplot(chicks_21, aes(x = Diet, y = weight))+
  geom_boxplot(aes(fill = Diet), notch = TRUE)

#Tukey HSD test

TukeyHSD(chicks.aov1)

#Boxplot
ggplot(chicks_21, aes(x = Diet, y = weight, fill = Diet))+
  geom_boxplot(notch = TRUE, colour = "grey50")+
  geom_segment(aes(x = Diet, xend = Diet, y = weight, yend = weight+2))

#segments showing confidence interval


 chicks_tukey <- as.data.frame(TukeyHSD(aov(weight ~ Diet, data = chicks_21))$Diet)

 chicks_tukey$pairs <- as.factor(row.names(chicks_tukey))
 
 ggplot(chicks_tukey,aes(x = pairs, y = diff))+
   geom_point()+
   geom_errorbar(aes(ymin = lwr, 
                     ymax = upr))+
   geom_hline(yintercept = 0, linetype = "dashed")
  
 

# Multiple factor ANOVA ---------------------------------------------------

#H0: There is no change in chicken mass (kg) from day 1  to 21.
 
 chicks_1_21 <- ChickWeight %>% 
   filter(Time %in% c(0,21))
 
 ggplot(data = chicks_1_21,aes(x = Time, y = weight))+
   geom_boxplot(notch = T, aes(fill = as.factor(Time)))

 #Perform a Tukey post-hoc test
 TukeyHSD((aov(weight ~ as.factor(Time), data = chicks_1_21)))

 #Look at the confidence intervals
 plot(TukeyHSD((aov(weight ~ as.factor(Time), data = chicks_1_21))))
 
 #Look at day 0 and 21 for both Time and diet
 summary(aov(weight ~ Diet + as.factor(Time), data = ChickWeight))

 #note the increase in the degrees of freedom for the time factor
 #but no increase for the d.f for diet
 
 #now look at the interactions BETWEEN factors
 summary(aov(weight ~ Diet * as.factor(Time), data =  filter(ChickWeight, Time)))


#Create a line graph to help explain this concept
 #first create mean values by time and  diet
 chicks_mean <- ChickWeight %>% 
   group_by(Diet, Time) %>% 
   summarise(weight_mean = mean(weight, na.rm = TRUE))
 ggplot()









