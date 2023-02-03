library(ggplot2)
library(dplyr)
a<-data.table(read.csv("../BCAD Data/total_corp_housing.csv"))



##step 1:  line graph for owners that have value >200
#I attempted to use this script from : 
#https://stackoverflow.com/questions/53125393/ggplot-label-the-top-n-lines

names<-a$py_owner_name

labels <- 
  a %>% 
  group_by(py_owner_name) %>% 
  filter(any(value - 200 > 1)) %>% 
  top_n(1300, value) %>% 
  with(annotate('label', label = py_owner_name, x = year, y = value - 1))

ggplot(a, aes(x=year, y=value-1, color= py_owner_name)) + 
  geom_line(size = 1) +
  labels
#too big to process, and never saw a plot

###




