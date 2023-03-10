library(readxl)
library(stringi)
library(data.table)
library(foreign)
library(sf)
setwd("C:/Users/ydj154/OneDrive - University of Texas at San Antonio/Documents/escalante.thesis")
###DATA LOADING#
file_list<-dir("../BCAD Data/")
total_corp_housing<-NULL
agg_corp_housing<-NULL
f3_fields<-read_xlsx("../BCAD DATA/Appraisal Export Layout - 8.0.25.xlsx",range = "A55:F486",col_names = T)
types<-gsub(pattern = "\\s*\\([^\\)]+\\)",replacement = "",x = f3_fields$Datatype)

shp_list_folder<-dir("../BCAD Data/")
shp_list_folder<-shp_list_folder[grepl(pattern = "_GIS",x = shp_list_folder)]
shp_list_file<-c("BCAD_2018_Public_Parcels_with_Attributes_Sept_05.shp","BCAD_2019_with_Attributes_August16.shp","BCAD_2020_with_Attributes_Public2020August17.shp",
                 "BCAD_2021_with_Attributes_Public2021September02.shp","BCAD_2022_with_Attributes_Public2022August08.shp")

#write an object that writes the variable names in each year's .dbf file
#need to make a list that has the year, and the list of names. 
q_var_list<-data.frame(v_2018=c("prop_id","X", "Y"),
                       v_2019=c("prop_id","X", "Y"),
                       v_2020=c("prop_id","X", "Y"),  
                       v_2021=c("prop_id","X", "Y"),
                       v_2022=c("prop_id","X", "Y"))
                                                                        
#identify names of corporate landlords
corps<-c("LLC","LTD","LP","INC","TRUST","CORP","HOMES","ASSOC","PROPERTIES","ESTATE","PROPERTY MANAGEMENT")

#tracts loading
q_b_tracts<-tigris::tracts(county="Bexar",state = "TX",cb = T,year = 2020)

#crsuggest::suggest_crs(q_b_tracts)

q_b_tracts<-st_transform(q_b_tracts,crs=2847)



for(i in 2018:2022){
  print(paste("i am in",i))
  # Step 1: building appraisal datasets and corp data
  {
  DT<- fread(paste0("../BCAD DATA/",i,"_APPRAISAL_INFO.TXT"), header = FALSE, sep = "\n")
  DT<-DT[ , lapply(seq_len(length(f3_fields$Start)), function(ii) {
    stringi::stri_sub(V1, f3_fields$Start[ii], f3_fields$End[ii])
  })]
  names(DT)<-f3_fields$`Field Name` # adding names
  Apprsl<-DT[trimws(prop_type_cd)=="R", .(py_owner_id,prop_id,geo_id,prop_type_cd,py_owner_name, partial_owner,
                                          py_addr_line1,py_addr_line2,
                                          py_addr_city,py_addr_state,py_addr_zip,py_addr_zip_cass,
                                          tract_or_lot,appraised_val,assessed_val,hs_exempt,market_value,ht_exempt)] #removed mortgage_acct_id, and mortgage_co_name as the data is not necessary
  
  rm(DT);gc()
  matches1 <- grep(paste(corps,collapse="|"), 
                  Apprsl$py_owner_name, value=F)
  
  Apprsl[matches1,corp:=1]#this denotes a yes/no on if a row is corplandlord or not.
  Apprsl[,prop_id_num:=as.numeric(prop_id)] #making prop_id a number, removing the zeroes
  Apprsl[,py_owner_id_num:=as.numeric(py_owner_id)] #individual property OWNER ids
  Apprsl[,mark_val:=as.numeric(market_value)]#making market_value  a number, removing the zeroes
  }
  #Step 2: adding lat long by year
  {
  # if(i!=2020){ #"prop_id" column is missing in corp_2020
  #   q<-read.dbf(paste0("../BCAD Data/",shp_list_folder[i-2017],"/",shp_list_file[i-2017])) #Reads addreses to get lat long
  #   q<-q[grepl(paste(corps,collapse = "|"),  q$Owner),]
  #   q<-q[, q_var_list[,i-2017]]
  # }else{
    q<-st_read(paste0("../BCAD Data/",shp_list_folder[i-2017],"/",shp_list_file[i-2017]))
    q<-q[grepl(paste(corps,collapse = "|"),  q$Owner),]
    q<-q[st_is_valid(q),]
    coords_q<-st_centroid(q) #error for 2018 shapefile, invalid number of points
    coords_q<-st_coordinates(coords_q)
    q<-data.table(st_drop_geometry(q[,c("prop_id","Owner")]),stringsAsFactors = F)
    q<-cbind(q,coords_q)
    #q<-read.dbf(paste0("../BCAD Data/",shp_list_folder[i-2018],"/",shp_list_file[i-2018])) #Reads addreses to get lat long
        rm(coords_q)
  # }
   
      
  Apprsl2<-merge(Apprsl[corp==1,],q,by.x = "prop_id_num",by.y = q_var_list[1,i-2017],all.x = T,sort = F) 
  rm(q);gc()
  
  #Step 2.1: adding tract and geoid with spatial join
  nas<-as.vector(is.na(Apprsl2[,q_var_list[2,i-2017],with=F]))
  Apprsl2_sf<-st_as_sf(Apprsl2[!nas,],coords = c(q_var_list[2,i-2017],q_var_list[3,i-2017]),crs=2278)
  Apprsl2_sf<-st_transform(Apprsl2_sf,crs = 2847)
  Apprsl2_sf<-st_join(Apprsl2_sf,q_b_tracts[,c("GEOID","TRACTCE")])
  Apprsl2_sf$X<-st_coordinates(Apprsl2_sf)[,1]
  Apprsl2_sf$Y<-st_coordinates(Apprsl2_sf)[,2]
  Apprsl2_sf<-data.table(st_drop_geometry(Apprsl2_sf),stringsAsFactors = F)
  
    }
  
  #Step3: aggregating corps stats
  {
    #total_corp_housing1<-rbind(total_corp_housing,Apprsl[corp==1,.(value=sum(corp),year=i),by=.(py_owner_name)])#this filters corplandlords for APPRSL 
    total_corp_housing<-rbind(total_corp_housing,Apprsl2_sf[corp==1,.(TotalCLProp=sum(corp,na.rm = T),AvgMktVal=mean(mark_val,na.rm = T),year=i),by=.(py_owner_name,py_addr_city,py_addr_state,GEOID)])#this filters corplandlords for APPRSL #,,,TRACTCE,py_owner_id_num
    sum_corp_housing<-rbind(agg_corp_housing,Apprsl2[corp==1,.(TotalCLProp=sum(corp,na.rm = T),AvgMktVal=mean(mark_val,na.rm = T),year=i),by=.(py_owner_name,py_addr_city,py_addr_state)])#removing GEOID
    rm(Apprsl2_sf);gc()  
    }
  fwrite(x=total_corp_housing,file = "../BCAD Data/total_corp_housing.csv")
  fwrite(x=sum_corp_housing,file = "../BCAD Data/sum_corp_housing.csv") # only has 2022 (?) why is that
  #fwrite(x = Apprsl2,file = paste0("../BCAD Data/appraisal_corp_",i,".csv")) 
  #fwrite(x = Apprsl,file = paste0("../BCAD Data/appraisal_",i,".csv")) 
}
