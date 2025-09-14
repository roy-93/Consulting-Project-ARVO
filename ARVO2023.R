# Packages
library(dplyr)
library(tidyverse)
library(ggplot2)
library(ggExtra)

# Main Dataset
data<-read.csv(file="ARVO2023.csv", header=TRUE)
data <- data.frame(data)


# Make new variable with stimulus (control=0, others=1) 
data$stimulus<-substr(data$trial_location,12,12)
data$stimulus <- sub("^$", "0", data$stimulus)# Replace the blanks with 0 (control=0)

# Make new variable with location (1T, 2T, 4T, 8T)
data$location <- substr(data$trial_location,1,2)

# Make character variable as factor
data2<-data %>% mutate(Hx_fac=as.factor(Hx)) %>% mutate(Sex_fac=as.factor(Sex)) %>%

            mutate(trial_location_fac=as.factor(trial_location)) %>%
            
            mutate(stimulus_fac=as.factor(stimulus)) %>% mutate(location_fac=as.factor(location))

attach(data2)


# Descriptive Statistics
str(data2)
summary(data2)

newdata <- data2 %>% select(ID, Hx_fac, Sex_fac, stimulus_fac, location_fac, Amplitude, Implicit.time, Recovery..)

rename(newdata, Implicit.time = IT)

#colnames(newdata) <- c(ID, Hx, Sex, stimulus, location, amplitude, IT, Recovery)

#write.csv(newdata, file="ARVO_new.csv", sep="", row.names = FALSE)

# # Plot Average amplitude for each location and each group
summary1 <- newdata %>% group_by(Hx_fac, location_fac, stimulus_fac) %>%
             summarise(n = n(), mean = mean(Amplitude, na.rm = TRUE), SD= sd(Amplitude, na.rm = TRUE)) 
                                                   
summary2 <- newdata %>% group_by(Hx_fac, location_fac, stimulus_fac) %>%
  summarise(n = n(), mean = mean(Implicit.time, na.rm = TRUE), SD= sd(Implicit.time, na.rm = TRUE)) 


summary3 <- newdata %>% group_by(Hx_fac, location_fac, stimulus_fac) %>%
  summarise(n = n(), mean = mean(Recovery.., na.rm = TRUE), SD= sd(Recovery.., na.rm = TRUE)) 

ggplot(data=summary1, mapping = aes(x = location_fac, y = mean)) + 
  geom_point(aes(color = Hx_fac)) +
  facet_wrap(~Hx_fac) +
  theme_bw() + ggtitle("Average Amplitude in each location for each Group")

# Plot Average amplitude for each stimulus and each group
summary2 <- newdata %>% group_by(Hx_fac,location_fac,stimulus_fac) %>% summarise(n = n(), mean = mean(Amplitude, na.rm = TRUE)) 
#min = min(Amplitude, na.rm = TRUE),sd = sd(Amplitude, na.rm = TRUE), max = max(Amplitude, na.rm = TRUE))

ggplot(data=summary2, mapping = aes(x = location_fac, y = mean)) + 
  geom_point(aes(color = Hx_fac)) + geom_line()+
  facet_wrap(~c(Hx_fac)) +
  theme_bw() + ggtitle("Average Amplitude in each location for each Group")

ggplot(data=summary2, mapping = aes(x = stimulus_fac, y = mean)) + 
  geom_point(aes(color = Hx_fac)) + geom_line()+
  facet_wrap(~c(Hx_fac)) +
  theme_bw() + ggtitle("Average Amplitude in each stimulus for each Group")


data2 %>% group_by(Hx, stimulus_fac, location_fac) %>% summarise(n = n(), min = min(Implicit.time, na.rm = TRUE), mean = mean(Implicit.time, na.rm = TRUE), 
                                          sd = sd(Implicit.time, na.rm = TRUE), max = max(Implicit.time, na.rm = TRUE))

# Marginal Distribution of each variable
par(mfrow=c(2,2))
hist(data$Amplitude)
hist(data$Implicit.time)
hist(data$Recovery..)

# Comparisoon of amplitude on each location
ggplot(data=newdata, mapping = aes(x = location_fac, y = Amplitude)) + 
  geom_boxplot() +
  theme_bw() + ggtitle("Boxplots of amplitude by location")

# Comparison of implicit time on each location
ggplot(data=newdata, mapping = aes(x = location_fac, y = Implicit.time)) + 
  geom_boxplot() +
  theme_bw() + ggtitle("Boxplots of implicit time by location")

# Comparison of recovery in each location
ggplot(data=newdata, mapping = aes(x = location_fac, y = Recovery..)) + 
  geom_boxplot() +
  theme_bw() + ggtitle("Boxplots of recovery by location")


# Comparison of amplitude in each location and stimulus
ggplot(data=newdata, mapping = aes(x = location_fac, y = mean(Amplitude))) + 
  geom_boxplot(aes(color = stimulus_fac)) +
  facet_wrap(~stimulus_fac) +
  theme_bw() + ggtitle("Boxplots of amplitude by location levels by Stimulus")

# Comparison of amplitude in each location and stimulus
ggplot(data=newdata, mapping = aes(x = location, y = Implicit.time)) + 
  geom_boxplot(aes(color = stimulus_fac)) +
  facet_wrap(~stimulus_fac) +
  theme_bw() + ggtitle("Boxplots of Implitude time by location levels by Stimulus")

# Comparison of amplitude in each location and stimulus
ggplot(data=newdata, mapping = aes(x = location, y = Recovery..)) + 
  geom_boxplot(aes(color = stimulus_fac)) +
  facet_wrap(~stimulus_fac) +
  theme_bw() + ggtitle("Boxplots of Recovery by location levels by Stimulus")


table(data2$trial_location, useNA = "always")

hist(newdata$Amplitude, xlab = "Amplitude", main = "Histogram of Amplitude")

hist(newdata$Implicit.time, xlab = "Implicit-time", main = "Histogram of Implicit-time")

hist(newdata$Recovery.., xlab = "Recovery", main = "Histogram of Recovery")

