#load libraries
library(ggplot2)
library(dplyr)
library(data.table)
a<-data.table(read.csv("../BCAD Data/total_corp_housing.csv"))

##step 1:  line graph for owners that have value >200
#I attempted to use this script from : 
#https://stackoverflow.com/questions/53125393/ggplot-label-the-top-n-lines

#names<-a$py_owner_name

#labels <- 
#  a %>% 
#  group_by(py_owner_name) %>% 
#  filter(any(value - 200 > 1)) %>% 
#  top_n(1300, value) %>% 
#  with(annotate('label', label = py_owner_name, x = year, y = value - 1))

#ggplot(a, aes(x=year, y=value-1, color= py_owner_name)) + 
#  geom_line(size = 1) +
#  labels
#too big to process, and never saw a plot

###

#rewriting script from above
#a_filtered <- a %>%
#  select(py_owner_name, value, year) %>%
#  filter(value> 200) %>%
#  group_by(year) %>%
#  top_n(10, value)

#plot
#ggplot(a_filtered, aes(x = year, y = value, color = py_owner_name)) + 
#  geom_line() + 
#  labs(x = "Year", y = "Value") +
#  theme_classic() +
#  scale_x_continuous(limits = c(2018, 2022), expand = c(0,0)) +
#  scale_y_continuous(limits = c(0, max(a_filtered$value)), expand = c(0,0)) +
#  theme(legend.position = "bottom")
#^above code produced a plot but it counted the top 10 owners, in each year. What i need to do is tract from 2018, the top 10 and track each year how those same names values changed

#Rewriting code to filter for 2018's top 10 names, over 100 properties owned. 
a_filtered <- a %>%
  filter(year == 2018 & value > 100) %>%
  top_n(10, value) %>%
  filter(year >= 2018 & year <= 2022) %>%
  group_by(py_owner_name) %>%
  mutate(Rank = row_number())

#rewrite plot from above
ggplot(a_filtered, aes(x = year, y = value, color = py_owner_name, group = Rank)) + 
  geom_line() + 
  labs(x = "Year", y = "Value") +
  theme_classic() +
  scale_x_continuous(limits = c(2018, 2022), expand = c(0,0)) +
  scale_y_continuous(limits = c(0, max(a_filtered$value)), expand = c(0,0)) +
  theme(legend.position = "right")
