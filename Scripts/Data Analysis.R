library(readxl)
library(stringi)
library(data.table)
library(foreign)
library(sf)

###DATA LOADING#
file_list<-dir("BCAD Data/")
total_corp_housing<-NULL
f3_fields<-read_xlsx("../BCAD DATA/Appraisal Export Layout - 8.0.25.xlsx",range = "A55:F486",col_names = T)
types<-gsub(pattern = "\\s*\\([^\\)]+\\)",replacement = "",x = f3_fields$Datatype)

shp_list_folder<-dir("../BCAD Data/")
shp_list_folder<-shp_list_folder[grepl(pattern = "_GIS",x = shp_list_folder)]
shp_list_file<-c("BCAD_2018_043018.dbf","BCAD_2019_with_Attributes_August16.dbf")

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
  Apprsl<-DT[trimws(prop_type_cd)=="R", .(py_owner_id,prop_id,geo_id,prop_type_cd,py_owner_name,
                                          py_addr_line1,py_addr_line2,
                                          py_addr_city,py_addr_state,py_addr_zip,py_addr_zip_cass,
                                          tract_or_lot,appraised_val,assessed_val,mortage_co_name,
                                          mortgage_acct_id,hs_exempt,market_value,ht_exempt)] #removed mortgage_acct_id, and mortgage_co_name as the data is not necessary
  
  
  
  matches1 <- grep(paste(corps,collapse="|"), 
                  Apprsl$py_owner_name, value=F)
  
  Apprsl[matches1,corp:=1]#this denotes a yes/no on if a row is corplandlord or not.
  }
  #Step 2: adding lat long by year
  {
  q<-read.dbf(paste0("../BCAD Data/",shp_list_folder[i-2017],"/",shp_list_file[i-2017])) #Reads addreses to get lat long
  q<-q[grepl(paste(corps,collapse = "|"),  q$Owner),]
  Apprsl[,prop_id_num:=as.numeric(prop_id)]
  Apprsl2<-merge(Apprsl[corp==1,],q[,c("PropID","Longitude","Latitude")],by.x = "prop_id_num",by.y = "PropID",all.x = T,sort = F)
  
  #Step 2.1: adding tract and geoid with spatial join
  Apprsl2_sf<-st_as_sf(Apprsl2[!is.na(Apprsl2$Latitude),],coords = c("Longitude","Latitude"),crs=2278)
  Apprsl2_sf<-st_transform(Apprsl2_sf,crs = 2847)
  Apprsl2_sf<-st_join(Apprsl2_sf,q_b_tracts[,c("GEOID","TRACTCE")])
  Apprsl2_sf$X<-st_coordinates(Apprsl2_sf)[,1]
  Apprsl2_sf$Y<-st_coordinates(Apprsl2_sf)[,2]
  Apprsl2<-st_drop_geometry(Apprsl2_sf)
    }
  
  #Step3: aggregating corps stats
  {
    total_corp_housing<-rbind(total_corp_housing,Apprsl[corp==1,.(value=sum(corp),year=i),by=.(py_owner_name)])#this filters corplandlords for APPRSL 
    total_corp_housing2<-rbind(total_corp_housing,Apprsl2[corp==1,.(value=sum(corp),year=i),by=.(py_owner_name)])#this filters corplandlords for APPRSL12
    
  }
  
  fwrite(x=total_corp_housing,file = "../BCAD Data/total_corp_housing.csv")
  fwrite(x = Apprsl2,file = paste0("../BCAD Data/appraisal_corp_",i,".csv"))
  fwrite(x = Apprsl,file = paste0("../BCAD Data/appraisal_",i,".csv"))
}


