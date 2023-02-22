#bind total corp with each year's XY Values from the individual years' csv files
library(data.table)
corp_18<- read.csv("../BCAD Data/appraisal_corp_2018.csv")
corp_19<- read.csv("../BCAD Data/appraisal_corp_2019.csv")
corp_20<- read.csv("../BCAD Data/appraisal_corp_2020.csv")
corp_21<- read.csv("../BCAD Data/appraisal_corp_2021.csv")
corp_22<- read.csv("../BCAD Data/appraisal_corp_2022.csv")

all_appraisalcorp <- rbind(corp_18, corp_19, corp_20, corp_21, corp_22)
#doesnt work because corp_20 has 1 less column ... why?
names(corp_19)
names(corp_20) # corp_20 is missing the column "prop_id" 



#rbind all the data
# stack all the appraisal data ontop of each other
# datatable new corp = 
#   data[,py_owner_name_std_lag:=shift(py_owner_name_std,type=“lag”)]
#merge totalcorp names to appraisal names ; shift creates variable that has name and previous year's name (the lag)
##this helps compare both names S