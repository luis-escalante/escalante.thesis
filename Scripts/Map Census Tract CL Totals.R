
#previous code from december 2022
bk<-classIntervals(q_b_tracts_corp$num_corp,n = 4,style = "quantile")

mapview(q_b_tracts_corp,zcol="num_corp",at=bk$brks)+
  mapview(Q_sf_Bcorp,zcol="value",at=c(30,100,200,500,700))
#end


library(mapview)
b<-st_as_sf(subset_b)
mapview(subset_b)


#merged_dt<- merge(subset_b, Apprsl2_sf, by = "GEOID")

Apprsl2_sf_2 <- st_as_sf(Apprsl2, coords = c("X", "Y"), crs = 2278)
total_corp_housing_sf2 <- st_as_sf(merge(total_corp_housing, Apprsl2_sf_2, by = "GEOID"),coords = c("X", "Y"), crs = 2278)
