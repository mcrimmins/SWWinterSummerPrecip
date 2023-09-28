# fire stats by precip cluster
# MAC 06/05/23

library(raster)
library(ggplot2)

# load cluster map
classMap<-raster("./data/classMap6.grd")

#####
# load cropped FOD 
load("~/RProjects/BurnPeriodResearch/data/swFOD.RData")
# work with  FOD dataframe only
fc<-fc@data
# remove factors
fc[,] <- lapply(fc, function(x) type.convert(as.character(x), as.is = TRUE))
# adjust dates on fc DF  
fc$DISCOVERY_DATE<-as.Date(fc$DISCOVERY_DATE,"%m/%d/%Y")
fc$CONT_DATE<-as.Date(fc$CONT_DATE,"%m/%d/%Y")
fc$DISCOVERY_MONTH<-as.numeric(format(fc$DISCOVERY_DATE, "%m"))
fc$CONT_MONTH<-as.numeric(format(fc$CONT_DATE, "%m"))
fc$DISCOVERY_YEAR<-as.numeric(format(fc$CONT_DATE, "%Y"))
#####

# extract raster value to fire points
fc$fireCluster<-extract(classMap,fc[,c("LONGITUDE","LATITUDE")])
# 

ggplot(fc, aes(fireCluster,DISCOVERY_MONTH, group=fireCluster))+
  geom_boxplot(varwidth = TRUE)+
  ggtitle("Wildfire Discovery Month by Cluster Region")

ggplot(fc, aes(fireCluster,CONT_MONTH, group=fireCluster))+
  geom_boxplot(varwidth = TRUE)+
  ggtitle("Wildfire Containment Month by Cluster Region")

ggplot(fc, aes(DISCOVERY_YEAR,DISCOVERY_DOY, color=as.factor(fireCluster), group=DISCOVERY_YEAR))+
  geom_boxplot(varwidth = TRUE)+
  ggtitle("Wildfire Discovery Day of Year by Cluster Region")+
  geom_hline(yintercept = 182)+
  geom_hline(yintercept = 91)+
  facet_wrap(.~fireCluster)


ggplot(fc, aes(fireCluster,DISCOVERY_DOY, group=fireCluster))+
  geom_boxplot(varwidth = TRUE)+
  ggtitle("Wildfire Discovery Day of Year by Cluster Region")+
  geom_hline(yintercept = 182)+
  geom_hline(yintercept = 91)


ggplot(fc, aes(fireCluster,CONT_DOY, group=fireCluster))+
  geom_boxplot(varwidth = TRUE)+
  ggtitle("Wildfire Containment Day of Year by Cluster Region")+
  geom_hline(yintercept = 182)+
  geom_hline(yintercept = 91)


