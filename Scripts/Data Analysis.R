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


