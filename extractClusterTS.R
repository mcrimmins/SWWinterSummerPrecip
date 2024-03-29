# extract time series from cluster zones
# MAC 06/07/23

library(raster)

# load cluster map
classMap<-raster("./data/classMap5.grd")
  clusterN<-nrow(levels(classMap)[[1]])

# dates 1895-2022 PRISM data
dates=seq(as.Date("1895-01-01"), as.Date("2022-12-31"), by="month")
dates<-as.data.frame(dates)
dates$month<-as.numeric(format(dates$dates, "%m"))
dates$year<-as.numeric(format(dates$dates, "%Y"))
length(unique(dates$year))

# update scratch dir with PRISM data 1895-2022 monthly precip/mean temp
# use ~/RProjects/PRISMDownload/monthyDownloadPRISM.R
# process to subset using ~/RProjects/WinterSummerPrecip/processPRISM.R
prec<-stack("/scratch/crimmins/PRISM/monthly/processed/SW/SWmonthlyPRISM_prec_1895_2022.grd")
tmean<-stack("/scratch/crimmins/PRISM/monthly/processed/SW/SWmonthlyPRISM_tmean_1895_2022.grd")
# add in PET from monthlyHargreavesPETraster.R
pet<-stack("/scratch/crimmins/PRISM/monthly/processed/SW/SWmonthlyPRISM_hargreaves_1895_2022.grd")
# new vars
tdmean<-stack("/scratch/crimmins/PRISM/monthly/processed/SW/SWmonthlyPRISM_tdmean_1895_2022.grd")
vpdmax<-stack("/scratch/crimmins/PRISM/monthly/processed/SW/SWmonthlyPRISM_vpdmax_1895_2022.grd")
tmax<-stack("/scratch/crimmins/PRISM/monthly/processed/SW/SWmonthlyPRISM_tmax_1895_2022.grd")
tmin<-stack("/scratch/crimmins/PRISM/monthly/processed/SW/SWmonthlyPRISM_tmin_1895_2022.grd")

# set stack  
gridVar<-prec
gridVar<-crop(gridVar, extent(classMap))

clusterN<-nrow(levels(classMap)[[1]])  
# get zonal time series of climate values from clusters
climTS<-as.data.frame(t(zonal(gridVar,classMap, fun="mean",na.rm=TRUE)))
climTS<-climTS[2:nrow(climTS),]
colnames(climTS)<-paste0("cluster",seq(1,clusterN,1))  
# add in dates
climTS<-cbind.data.frame(dates,climTS)
# cut seasons
# dates$seas = cut(dates$month,c(0,3,6,9,12))
# levels(dates$seas) = c("JFM","AMJ","JAS","OND")
climTS$seas<-cut(climTS$month,c(0,3,6,9,12))
levels(climTS$seas) = c("JFM","AMJ","JAS","OND")

# write out csv
write.csv(climTS,file='./data/csv_5cluster/Cluster5_monthly_min_temp_PRISM_1895-2022.csv', row.names=FALSE)

# check climo
# library(dplyr)
# 
# test<-climTS %>% group_by(month) %>%
#                  summarize(moVAR=mean(cluster2))
# 
# plot(test$month,test$moVAR)
