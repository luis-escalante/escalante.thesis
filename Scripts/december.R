library(sf)

gis<-read_sf("../WingDownload/2018 GIS Data/2018_GIS_Public_Data/BCAD_2018_043018.shp")
st_is_valid(gis)
ctr<-st_centroid(gis) #figure out why centroid is not working
b_tracts<-tigris::tracts(county="Bexar",state = "TX",cb = T,year = 2020)

gis<-st_transform(gis,crs = st_crs(b_tracts))

gis2<-st_join(x = gis,y=b_tracts) #error

#gis2<-gis[,.(prop_id,x,y,CT)] #what we need to merge data sets
#^In arcgis i calculated longitude, and latitude (X,Y) centroids of each polygon. Following this weblink:https://support.esri.com/en/technical-article/000009381
##page on polygon zm https://pro.arcgis.com/en/pro-app/latest/tool-reference/data-management/feature-to-point.htm
#successful in calculating the centroids for 2018, will continue for rest of the data years
#exports will be find in BCAD Data folder as 'xy',i,'.csv


####
library(ggplot2)
bcad2018<-read.csv(file ="BCAD DATA/xy2018.csv")
rm(bcad2018)
#ggplot(data = world) +
#  geom_sf() +
#  geom_point(data = bcasd2018, aes(x = x, y = y))

####


library(readxl)
library(stringi)
library(data.table)

###DATA LOADING#
file_list<-dir("BCAD Data/")
total_corp_housing<-NULL
f3_fields<-read_xlsx("BCAD DATA/Appraisal Export Layout - 8.0.25.xlsx",range = "A55:F486",col_names = T)
types<-gsub(pattern = "\\s*\\([^\\)]+\\)",replacement = "",x = f3_fields$Datatype)
for(i in 2018:2022){
  print(paste("i am in",2018))
  DT<- fread(paste0("BCAD DATA/",i,"_APPRAISAL_INFO.TXT"), header = FALSE, sep = "\n")
  DT<-DT[ , lapply(seq_len(length(f3_fields$Start)), function(ii) {
    stringi::stri_sub(V1, f3_fields$Start[ii], f3_fields$End[ii])
  })]
  names(DT)<-f3_fields$`Field Name` # adding names
  Apprsl<-DT[trimws(prop_type_cd)=="R", .(py_owner_id,prop_id,geo_id,prop_type_cd,py_owner_name,
                                          py_addr_line1,py_addr_line2,
                                          py_addr_city,py_addr_state,py_addr_zip,py_addr_zip_cass,
                                          tract_or_lot,appraised_val,assessed_val,mortage_co_name,
                                          mortgage_acct_id,hs_exempt,market_value,ht_exempt)] #removed mortgage_acct_id, and mortgage_co_name as the data is not necessary
  
  
  
  corps<-c("LLC","LTD","LP","INC","TRUST","CORP","HOMES","ASSOC","PROPERTIES","ESTATE","PROPERTY MANAGEMENT")
  matches1 <- grep(paste(corps,collapse="|"), 
                  Apprsl$py_owner_name, value=F)
  
  Apprsl[matches1,corp:=1]#this denotes a yes/no on if a row is corplandlord or not.
  #Apprsl[,.(t_corp=sum(corp,na.rm = T),per_corp=sum(corp,na.rm = T)/.N),by=.(CT)]
  total_corp_housingsss<-rbind(total_corp_housingsss,Apprsl[corp==1,.(value=sum(corp),year=i),by=.(py_owner_name)])#this filters corplandlords
  
                      #Apprs2<-subset(Apprsl,py_owner_name%in%c("LLC","LTD","LP","INC","TRUST","CORP","HOMES","ASSOC","PROPERTIES","ESTATE","PROPERTY MANAGEMENT")) #attempting a new way to filter CLs
                      #df1<-Apprsl[!(matches %in% corps),]#another attempt
                      #Apprsl2<- as.data.table(Apprsl) #not sure if this is needed but it worked for the line 64
                      #Apprslcorps<-Apprsl[grepl(paste(corps,collapse = "|"),  Apprsl$py_owner_name),] #this subsets all 'corporate landlords' into another dt. It works great!
                      #df2<-Apprsl[Apprsl$py_owner_name%like%"LLC","LTD","LP","INC","TRUST","CORP","HOMES","ASSOC","PROPERTIES","ESTATE","PROPERTY MANAGEMENT",]
  
  
  fwrite(x = DT,file = paste0("appraisal_",i,".csv"))
}


xy2018 <- fread("BCAD DATA/xy2018.csv")# Loading the CSV file
#xy2019 <- fread("BCAD DATA/xy2019.csv")

#result <- merge(dt, csv_file, by = prop_id)# binding the data.table and csv file by the common column


#install.packages(c("sp", "raster", "rgdal"))
#library (sp)
#library (rgdal)
#library (raster)

#my_spdf <- readOGR(dsn="D:\\UTSA Fall 2022/Thesis Fall 2022/corporate_singlefams/BCAD Data",layer="BCADshape_2018",verbose=FALSE)

#my_spdf <- readOGR("D:/UTSA Fall 2022/Thesis Fall 2022/corporate_singlefams/BCAD Data/BCADshape_2018")#kept getting issues with reading the file

file.exist("D:/UTSA Fall 2022/Thesis Fall 2022/corporate_singlefams/BCAD Data/BCADshape_2018.shp")

#shape <- readOGR(dsn = path.expand("BCADshape_2018.shp"), 
#                 layer = "BCADshape_2018")#the file still would not read; had to readjust path, 
#when reading shapefiles in R, the shapefile itself has to be with the other files like .shx and so on. otherwise R will continue to say that the file is unreadable


#install.packages("terra") #trying new package to read the shapefile
library(terra) #https://rspatial.org/spatial-terra/3-vectordata.html
#f <- system.file("BCAD Data/BCADshape_2018.shp", package="terra")
#r <- rast(f)#its not a raster data
#r
#getwd()
#setwd("D:/UTSA Fall 2022/Thesis Fall 2022/corporate_singlefams/BCAD Data/")
#getwd()
s<-vect("D:/UTSA Fall 2022/Thesis Fall 2022/WingDownload/2018 GIS Data/2018_GIS_Public_Data/BCAD_2018_043018.shp")
plot(s)
library(sf)
library(foreign)
s1<-read.dbf("D:/UTSA Fall 2022/Thesis Fall 2022/WingDownload/2018 GIS Data/2018_GIS_Public_Data/BCAD_2018_043018.dbf")
s2<-read_sf("D:/UTSA Fall 2022/Thesis Fall 2022/WingDownload/2018 GIS Data/2018_GIS_Public_Data/BCAD_2018_043018.shp")
s2<-s2[grepl(paste(corps,collapse = "|"),  s2$Owner),]
s2$corp<-as.numeric(matches)
library(ggplot2)
ggplot()+
  geom_sf(data = s2)
library(mapview)

mapview(s2)


s1_sf<-st_as_sf(s1[!is.na(s1$Latitude),],coords = c("Longitude","Latitude"),crs=2278)
s1_sf$corp<-0
s1_sf[grepl(paste(corps,collapse="|"), a$Owner),]$corp<-1

s1_sf2<-merge(s1_sf,total_corp_housing[,.(Owner,value)],by="Owner")
s1_sf_Bcorp<-s1_sf2[s1_sf2$corp==1 & s1_sf2$value>30,]
mapview(s1_sf_Bcorp,zcol="value",at=c(30,100,200,500,700))

# put b_tracts here

crsuggest::suggest_crs(b_tracts)

s1_sf_Bcorp<-st_transform(s1_sf_Bcorp,crs = 2847)
b_tracts<-st_transform(b_tracts,crs=2847)

s1_sf_Bcorp<-st_join(s1_sf_Bcorp,b_tracts[,c("GEOID","TRACTCE")])

s1_d<-data.table(s1_sf_Bcorp)
s1_d<-s1_d[,.(num_corp=.N),by=.(GEOID)]

b_tracts_corp<-merge(b_tracts,s1_d,by="GEOID")

library(classInt)
bk<-classIntervals(b_tracts_corp$num_corp,n = 4,style = "quantile")
mapview(b_tracts_corp,zcol="num_corp",at=bk$brks)+
  mapview(s1_sf_Bcorp,zcol="value",at=c(30,100,200,500,700))


a<-as.data.table(s)
head(a)
corps<-c("LLC","LTD","LP","INC","TRUST","CORP","HOMES","ASSOC","PROPERTIES","ESTATE","PROPERTY MANAGEMENT")
matches <- grep(paste(corps,collapse="|"), 
                a$Owner, value=F)
#a[matches,corp:=1]#this denotes a yes/no on if a row is corplandlord or not.
#Apprsl[,.(t_corp=sum(corp,na.rm = T),per_corp=sum(corp,na.rm = T)/.N),by=.(CT)]
#total_corp_housing<-rbind(total_corp_housing,a[corp==1,.(value=sum(corp),year=2018),by=.(Owner)])#this filters corplandlords

BCADshp<-a[grepl(paste(corps,collapse = "|"),  a$Owner),] #this subsets all 'corporate landlords' into another dt. It works great!

a[matches,corp:=1]
total_corp_housing<-rbind(total_corp_housing,a[corp==1,.(value=sum(corp),year=2018),by=.(Owner)])


#plot(BCADshapeCorps)
#par("mar")
#plot(BCADshapeCorps)+

#s1<-vect(BCADshapeCorps)
