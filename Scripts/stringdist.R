names_corps<-data.table(unique(total_corp_housing$py_owner_name),stringsAsFactors = F)

install.packages("stringdist")
library(stringdist)

i<-4884
for(i in 1:dim(names_corps)[1]){
  names_corps[,V2:=trimws(V1)]
  names_corps[,stringdist_Var:=stringdist(a = names_corps$V2[i],b =V2,method = "lv")]
  names_corps[stringdist_Var<=5,new_corp_name:=names_corps$V2[i]]
  #names_corps[startsWith(x = names_corps$V1,prefix = substr(x = names_corps$V1[i],start = 1,stop = 5)),new_corp_name:=names_corps$V1[i]]  
}

uniqueN(names_corps$new_corp_name)

names_corps_unique<-unique(names_corps)
# https://cran.r-project.org/web/packages/stringdist/stringdist.pdf <-read up on stringdist package
