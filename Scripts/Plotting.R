#LOAD library(terra) #https://rspatial.org/spatial-terra/3-vectordata.html
s<-vect("D:/UTSA Fall 2022/Thesis Fall 2022/WingDownload/2018 GIS Data/2018_GIS_Public_Data/BCAD_2018_043018.shp")

library(data.table)
a<-as.data.table(s)
head(a)
corps<-c("LLC","LTD","LP","INC","TRUST","CORP","HOMES","ASSOC","PROPERTIES","ESTATE","PROPERTY MANAGEMENT")
matches <- grep(paste(corps,collapse="|"), 
                a$Owner, value=F)

####ignore above this line####



install.packages("sf")
library(sf)
library(foreign)
library(mapview)
library(ggplot2)
library(classInt)

#letters to match with the data 

#2018 - q / Q
#2019 - w / W
#2020 - e / E
#2021 - r / R
#2022 - t / T

2018
q<-read.dbf("C:/Users/ydj154/OneDrive - University of Texas at San Antonio/Escalante Thesis/WingDownload/2018 GIS Data/2018_GIS_Public_Data/BCAD_2018_043018.dbf")

#Create corps object of all corp entity name matches
qcorps<-c("LLC","LTD","LP","INC","TRUST","CORP","HOMES","ASSOC","PROPERTIES","ESTATE","PROPERTY MANAGEMENT")
qmatches <- grep(paste(qcorps,collapse="|"), 
                q$Owner, value=F)


#read and add the matches of corporate entities to this new data set.
q1<-read_sf("C:/Users/ydj154/OneDrive - University of Texas at San Antonio/Escalante Thesis/WingDownload/2018 GIS Data/2018_GIS_Public_Data/BCAD_2018_043018.shp")
q2<-q2[grepl(paste(qcorps,collapse = "|"),  q2$Owner),]
q2$corp<-as.numeric(qmatches)

#simple plot, may not be necessary
ggplot()+
  geom_sf(data = q2)

#load mapview
mapview(q2)

#Creating simplefeature with filtered corp landlords
q1_sf<-st_as_sf(s1[!is.na(s$Latitude),],coords = c("Longitude","Latitude"),crs=2278)
q1_sf$corp<-0
q1_sf[grepl(paste(qcorps,collapse="|"), q$Owner),]$corp<-1

q1_sf2<-merge(q1_sf,total_corp_housing[,.(Owner,value)],by="Owner")
Q_sf_Bcorp<-q1_sf2[q1_sf2$corp==1 & q1_sf2$value>30,]
mapview(Q_sf_Bcorp,zcol="value",at=c(30,100,200,500,700))

# put b_tracts here
q_b_tracts<-tigris::tracts(county="Bexar",state = "TX",cb = T,year = 2020)

crsuggest::suggest_crs(q_b_tracts)

Q_sf_Bcorp<-st_transform(Q_sf_Bcorp,crs = 2847)
q_b_tracts<-st_transform(q_b_tracts,crs=2847)

Q_sf_Bcorp<-st_join(Q_sf_Bcorp,q_b_tracts[,c("GEOID","TRACTCE")])

Q_d<-data.table(Q_sf_Bcorp)
Q_d<-Q_d[,.(num_corp=.N),by=.(GEOID)]

q_b_tracts_corp<-merge(q_b_tracts,Q_d,by="GEOID")

#VIEW MAP OF POINTS AND CENSUS TRACTS
library(classInt)
bk<-classIntervals(q_b_tracts_corp$num_corp,n = 4,style = "quantile")

mapview(q_b_tracts_corp,zcol="num_corp",at=bk$brks)+
  mapview(Q_sf_Bcorp,zcol="value",at=c(30,100,200,500,700))


#2019 W
w<-read.dbf("C:/Users/ydj154/OneDrive - University of Texas at San Antonio/Escalante Thesis/WingDownload/2018 GIS Data/2018_GIS_Public_Data/BCAD_2018_043018.dbf")

#Create corps object of all corp entity name matches
wcorps<-c("LLC","LTD","LP","INC","TRUST","CORP","HOMES","ASSOC","PROPERTIES","ESTATE","PROPERTY MANAGEMENT")
wmatches <- grep(paste(wcorps,collapse="|"), 
                w$Owner, value=F)


#read and add the matches of corporate entities to this new data set.
#update directory 
w1<-read_sf("C:/Users/ydj154/OneDrive - University of Texas at San Antonio/Escalante Thesis/WingDownload/2018 GIS Data/2018_GIS_Public_Data/BCAD_2018_043018.shp")
w2<-w2[grepl(paste(wcorps,collapse = "|"),  w2$Owner),]
w2$corp<-as.numeric(wmatches)

#simple plot, may not be necessary
ggplot()+
  geom_sf(data = w2)

#load mapview
mapview(w2)

#Creating simplefeature with filtered corp landlords
w1_sf<-st_as_sf(w1[!is.na(s$Latitude),],coords = c("Longitude","Latitude"),crs=2278)
w1_sf$corp<-0
w1_sf[grepl(paste(wcorps,collapse="|"), q$Owner),]$corp<-1

w1_sf2<-merge(s1_sf,total_corp_housing[,.(Owner,value)],by="Owner")
W_sf_Bcorp<-w1_sf2[w1_sf2$corp==1 & w1_sf2$value>30,]
mapview(W_sf_Bcorp,zcol="value",at=c(30,100,200,500,700))

# put b_tracts here
w_b_tracts<-tigris::tracts(county="Bexar",state = "TX",cb = T,year = 2020)

crsuggest::suggest_crs(q_b_tracts)

W_sf_Bcorp<-st_transform(W_sf_Bcorp,crs = 2847)
w_b_tracts<-st_transform(w_b_tracts,crs=2847)

W_sf_Bcorp<-st_join(W_sf_Bcorp,w_b_tracts[,c("GEOID","TRACTCE")])

W_d<-data.table(W_sf_Bcorp)
W_d<-W_d[,.(num_corp=.N),by=.(GEOID)]

w_b_tracts_corp<-merge(w_b_tracts,W_d,by="GEOID")

#VIEW MAP OF POINTS AND CENSUS TRACTS
library(classInt)
bk<-classIntervals(q_b_tracts_corp$num_corp,n = 4,style = "quantile")

mapview(w_b_tracts_corp,zcol="num_corp",at=bk$brks)+
  mapview(W_sf_Bcorp,zcol="value",at=c(30,100,200,500,700))
