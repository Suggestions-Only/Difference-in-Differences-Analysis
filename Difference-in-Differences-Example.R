##useful numbers====
options(scipen = 999)

##data loading and cleaning====
did <- read.csv("C:/Users/Owner/Desktop/Victoria Data - Long.csv")

did$Treatment <- factor(did$Treatment,
                        levels = c(2,1),
                        labels = c("Placebo","Drug"))

did$Time <- factor(did$Time,
                   levels = c(1,2),
                   labels = c("Time 1","Time 2"))

##difference in differences model
did_model <- lm(Attitude ~ Treatment*Time, data = did)
summary(did_model)

##plot a graph====
##looks like a difference even if it doesn't reach statistical significance
library(tidyverse)
did_summary <- did %>%
  group_by(Treatment, Time) %>%
  na.omit() %>%
  summarise(Attitude = mean(Attitude))

ggplot(did_summary, aes(x = Time, y = Attitude, color = Treatment)) +
  geom_point() +
  geom_line(aes(group = Treatment))
