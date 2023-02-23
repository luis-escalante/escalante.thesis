library(ggplot2)
library(dplyr)
library(data.table)
library(readxl)
library(stringi)
library(foreign)
library(sf)
G<-fread("../BCAD Data/total_corp_housing.csv") #with geoid variable
H<-fread("../BCAD Data/sum_corp_housing.csv") #without the geoid variable

G[,GEOID:=as.character(GEOID)]

G<-G[,.(TotalCorp=sum(TotalCLProp,na.rm=T)),by=.(year,GEOID)]

bexar<-tigris::tracts(state = "TX",county = "Bexar")

bexar<-merge(bexar,G[year==2022,],by="GEOID",all.x=T,sort=F)


library(mapview)

mapview(bexar,zcol="TotalCorp")


