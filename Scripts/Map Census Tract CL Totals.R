
#previous code from december 2022
bk<-classIntervals(q_b_tracts_corp$num_corp,n = 4,style = "quantile")

mapview(q_b_tracts_corp,zcol="num_corp",at=bk$brks)+
  mapview(Q_sf_Bcorp,zcol="value",at=c(30,100,200,500,700))
#end

