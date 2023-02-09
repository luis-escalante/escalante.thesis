#load libraries
library(ggplot2)
library(dplyr)
library(data.table)
a<-data.table(read.csv("../BCAD Data/total_corp_housing.csv"))


a_subset1 <- a %>% 
  group_by(year) %>% 
  top_n(10, value)

#add a column that couns name occurances for each tractce
# a[, count := .N, by = c("py_owner_name", "TRACTCE", "year")] #didn't work - 

#unique_a = unique(a, by = c("TRACTCE", "year")) #attemp2
#unique_a[, count := uniqueN(py_owner_name), by = c("TRACTCE", "year")] #didn't work, or get what i need

#subset_a = a[, .N, by = c("TRACTCE", "year")] #this works

#for each year, count total geoid's in each Census tract  
#subset_b = a[, .N, by = c("GEOID", "TRACTCE", "year")] #No data for 2020. It's 2018, 2019, 2021, and 2022  


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
#a_filtered <- a %>%
#  filter(year == 2018 & value > 100) %>%
#  top_n(10, value) %>%
#  filter(year >= 2018 & year <= 2022) %>%
#  group_by(py_owner_name) %>%
#  mutate(Rank = row_number())

#rewrite plot from above
#ggplot(a_filtered, aes(x = year, y = value, color = py_owner_name, group = Rank)) + 
#  geom_line() + 
#  labs(x = "Year", y = "Value") +
#  theme_classic() +
#  scale_x_continuous(limits = c(2018, 2022), expand = c(0,0)) +
#  scale_y_continuous(limits = c(0, max(a_filtered$value)), expand = c(0,0)) +
#  theme(legend.position = "right")




#aplot1<-ggplot(a_subset1, aes(x = py_owner_name, y = value, color = year)) +
#  geom_point() +
#  ggtitle("Top 10 Values for Each Year") +
#  xlab("Py Owner Name") +
#  ylab("Value")
#aplot1 #too messy it has names on bottom. 
#rm(aplot1)

#aplot1<-ggplot(a_subset1, aes(x = py_owner_name, y = value)) +
 # geom_point() +
  # ggtitle("Top 10 Values for Each Year") +
  #xlab("Py Owner Name") +
  #ylab("Value") +
  #facet_wrap(~ year, ncol = 1, scales = "free")
#aplot1

#aplot1<-ggplot(a_subset1, aes(x = py_owner_name, y = value, fill = py_owner_name)) +
#  geom_bar(stat = "identity") +
 # ggtitle("Top 10 Values for Each Year") +
  #xlab("Py Owner Name") +
  #ylab("Value") +
  #facet_wrap(~ year, ncol = 1, scales = "free") +
  #scale_fill_brewer(palette = "Set1")
#aplot1  #too big n 


##Bar graphs that work!!

a_2018<-ggplot(a_subset1[a_subset1$year == "2018",], aes(x = py_owner_name, y = value, fill = py_owner_name)) +
  geom_bar(stat = "identity") +
  ggtitle("Corporate Landlords with Highest Properties in 2018") +
  xlab("Py Owner Name") +
  ylab("Value") +
  scale_fill_brewer(palette = "Set1")
a_2018 #this ended up consolidating Continental Homes of Texas LP- if you look at a_subset1 there are 2 entries of Continental; kind of want to use different colors.


a_2019<-ggplot(a_subset1[a_subset1$year == "2019",], aes(x = py_owner_name, y = value, fill = py_owner_name)) +
  geom_bar(stat = "identity") +
  ggtitle("Corporate Landlords with Highest Properties in 2019") +
  xlab("Py Owner Name") +
  ylab("Value") +
  scale_fill_brewer(palette = "Set1")
a_2019


a_2020<-ggplot(a_subset1[a_subset1$year == "2020",], aes(x = py_owner_name, y = value, fill = py_owner_name)) +
  geom_bar(stat = "identity") +
  ggtitle("Corporate Landlords with Highest Properties in 2020") +
  xlab("Py Owner Name") +
  ylab("Value") +
  scale_fill_brewer(palette = "Set1")
a_2020


a_2021<-ggplot(a_subset1[a_subset1$year == "2021",], aes(x = py_owner_name, y = value, fill = py_owner_name)) +
  geom_bar(stat = "identity") +
  ggtitle("Corporate Landlords with Highest Properties in 2021") +
  xlab("Py Owner Name") +
  ylab("Value") +
  scale_fill_brewer(palette = "Set1")
a_2021 #consolidated multiple entries for Lennar homes


a_2022<-ggplot(a_subset1[a_subset1$year == "2022",], aes(x = py_owner_name, y = value, fill = py_owner_name)) +
  geom_bar(stat = "identity") +
  ggtitle("Corporate Landlords with Highest Properties in 2022") +
  xlab("Py Owner Name") +
  ylab("Value") +
  scale_fill_brewer(palette = "Set1")
a_2022 #more consolidating

ggsave("top_2018.png", plot = a_2018, width = 8, height = 6, units = "in", dpi = 300)
ggsave("top_2019.png", plot = a_2019, width = 8, height = 6, units = "in", dpi = 300)
ggsave("top_2020.png", plot = a_2020, width = 8, height = 6, units = "in", dpi = 300)
ggsave("top_2021.png", plot = a_2021, width = 8, height = 6, units = "in", dpi = 300)
ggsave("top_2022.png", plot = a_2022, width = 8, height = 6, units = "in", dpi = 300)



