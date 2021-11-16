library(tidyverse)
library (ggpubr)
setwd('C:/Users/krist/OneDrive/Documents/tobacco easy data practice')
tobacco <- read.csv("Researcher Exercise_Data Set.csv")
glimpse(tobacco)

names(tobacco)

tobacco %>% 
  select(smoke, sex)%>%
  filter(sex == "female") %>%
  summarize(mean=mean(smoke))

tobacco %>% 
  select(smoke, sex)%>%
  filter(sex == "female") %>%
  summarize(sd=sd(smoke))


tobacco %>%
  select (smoke, sex) %>%
  filter(sex == "male") %>%
  summarize(mean=mean(smoke))

tobacco %>%
  select (smoke, sex) %>%
  filter(sex == "male") %>%
  summarize(sd=sd(smoke))

tobacco %>% 
  select(smoke, sex)%>%
  filter(sex == "female") %>%
  summarize(mean=mean(smoke))

tobacco %>% 
  select(smoke, sex)%>%
  filter(sex == "female") %>%
  summarize(sd=sd(smoke))


tobacco %>%
  select (overweight, sex) %>%
  filter(sex == "male") %>%
  summarize(mean=mean(overweight))

tobacco %>%
  select (overweight, sex) %>%
  filter(sex == "male") %>%
  summarize(sd=sd(overweight))

tobacco %>%
  select (overweight, sex) %>%
  filter(sex == "female") %>%
  summarize(sd=sd(overweight))

tobacco %>%
  select (overweight, sex) %>%
  filter(sex == "female") %>%
  summarize(mean=mean(overweight))

tobacco %>%
  ggplot( aes(x=smoke, y=overweight))+
  geom_point(alpha =0.5)+
  geom_smooth(method = 'lm', se=FALSE)
  labs (title= 'Association of Smoking versus Overweight')
  
  cor.test(tobacco$smoke, tobacco$overweight, method = "pearson")

  lmodel <- lm(sqrt(overweight) ~ sqrt(smoke), data = tobacco)
lmodel$coefficients  
summary(lmodel)

tobacco$predicted <-predict(lmodel)
tobacco$residuals <-residuals(lmodel)

tobacco %>%
  ggplot(aes(x= smoke, y= overweight))+
  geom_segment(aes(xend = smoke, yend = predicted))+
  geom_point() +
  geom_point(aes(y = predicted), shape = 1)


