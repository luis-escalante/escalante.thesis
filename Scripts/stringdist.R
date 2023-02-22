#install.packages("stringdist")
library(stringdist)
library(data.table)

a<-fread("../BCAD Data/total_corp_housing.csv",stringsAsFactors = F)


new_total_corp<-NULL

#i<-4475
#y<-2018
#for(y in 2018:2019){       (#used on 16 February)
for(y in 2018:2022){ #CHANGE YEAR
  names_corps<-a[!duplicated(py_owner_name) & year==y,]
  names_corps[,py_owner_name_std:=trimws(py_owner_name)]
  for(i in 1:dim(names_corps)[1]){
    names_corps[,stringdist_Var:=stringdist(a = names_corps$py_owner_name_std[i],b =py_owner_name_std,method = "lv")]
    ini<-startsWith(names_corps$py_owner_name_std,prefix = substr(x = names_corps$py_owner_name_std[i],start = 1,stop = 5))
    names_corps[stringdist_Var<=5 & ini==T,new_corp_name:=names_corps$py_owner_name_std[i]]
  }  
  new_total_corp<-rbind(new_total_corp,names_corps)
  fwrite(x = new_total_corp,file = "../BCAD Data/total_corp_housing_std.csv")
  
}


uniqueN(names_corps$py_owner_name_std)
uniqueN(names_corps$new_corp_name)

names_corps_unique<-unique(names_corps)
# https://cran.r-project.org/web/packages/stringdist/stringdist.pdf <-read up on stringdist package


library(ggplot2)
names_corps_sum<-names_corps[,.(value=sum(TotalCLProp,na.rm=T)),by=.(year,new_corp_name)]
setkeyv(names_corps_sum,c("year","value"))

K<-ggplot(data = tail(names_corps_sum,20))+
  geom_bar(aes(x=reorder(new_corp_name,value),y=value),stat = "identity")+
  coord_flip()+
  labs(title = "Top 20 Corporate Landlords by Total Properties Owned in Bexar County in 2018-2022", #CHANGE YEAR
       y = "Corporate Landlord Names",
       x = "Number of Total Properties")

  ggsave("Outputs/Top 20_2018_2022.png", width=12, height=9) #CHANGE YEAR and WIDTH HEIGHT BASED ON PLOT.
  




