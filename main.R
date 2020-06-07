# Title     : TODO
# Objective : TODO
# Created by: jakobpovsic
# Created on: 07/06/2020

library(tidyverse)
library(ggplot2)

data <- read.csv("data/gun-violence-data_01-2013_03-2018.csv")
pop_data <- read.csv("data/datasets_28951_36892_State Populations.csv")



df1 <- data%>%
  group_by(state)%>%
  summarise(total_n_killed=sum(n_killed), total_n_injured=sum(n_injured), n_total = n())%>%
  rowwise()%>%
  mutate(im_ratio=total_n_killed/total_n_injured)%>%
  mutate(pop=pop_data[pop_data$State == state, ]$X2018.Population)%>%
  mutate(killed_per_capita=(total_n_killed / pop) * 100000, injured_per_capita = (total_n_injured / pop) * 100000, total_n_per_capita= (n_total / pop) * 100000)%>%
  arrange(desc(im_ratio))

ggplot(df1) +
  geom_col(aes(x = reorder(state, im_ratio), y = im_ratio)) +
  labs(y = 'Ratio between murders and injuries', x = 'State') +
  coord_flip()

ggplot(df1) +
  geom_col(aes(x = reorder(state, total_n_per_capita), y = total_n_per_capita)) +
  labs(y = 'Reports per 100,000', x = 'State') +
  coord_flip()

ggplot(df1) +
  geom_col(aes(x = reorder(state, injured_per_capita), y = injured_per_capita)) +
  labs(y = 'Injuries per 100,000', x = 'State') +
  coord_flip()


df2 <- data%>%
  mutate(suicide=grepl('suicide', incident_characteristics) & n_killed == 1 & n_injured == 0)%>%
  group_by(state)%>%
  summarise(n_suicides = sum(suicide), n_total = sum(n_killed))%>%
  mutate(total_suicide_ratio = n_suicides / n_total)

ggplot(df2) +
  geom_col(aes(x = reorder(state, total_suicide_ratio), y = total_suicide_ratio)) +
  labs(y = 'Ratio of suicides in all gun violence reports', x = 'State') +
  coord_flip()

ggplot(df2) +
  geom_col(aes(x = reorder(state, n_total), y = n_total)) +
  geom_col(aes(x = reorder(state, n_total), y = n_suicides), fill='red') +
  coord_flip()


df4 <- data%>%
  group_by(date)%>%
  summarise(n_killed = sum(n_killed), n_injured = sum(n_injured), n_reports = n())

ggplot(df4, aes(x = date, group=1)) +
  geom_line(aes(y = n_killed, color='Murders')) +
  geom_line(aes(y = n_injured, color='Injuries')) +
  geom_line(aes(y = n_reports, color='Incidents'))
