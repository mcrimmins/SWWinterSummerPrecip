# data processing and figures for SW Climate Seasonality manuscript
# MAC 03/19/24

library(raster)
library(dplyr)
library(rasterVis)
library(viridis)
library(ggplot2)
library(cowplot)

# set rasteroptions
rasterOptions(progress = 'text')

##### load data -----
# try other polygons...MLRAs, ecoregions?
states <- getData('GADM', country='United States', level=1)
states <- subset(states, NAME_1 %in% c("Arizona","New Mexico","Texas","Colorado","Utah","Nevada","California"))
countries<-map_data("world")
countries<-subset(countries, region %in% c("Mexico"))

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

# nClimGrid precip - use processnClimGrid.R
#prec<-stack("/scratch/crimmins/climgrid/processed/sw/SWmonthly_nClimGrid_prec_1895_2022.grd")
#####

##### set up data ----
# monthly averages - prec
monthSum<- stackApply(prec, dates$month, fun = sum)
moAvgPrec<-monthSum/length(unique(dates$year))
names(moAvgPrec)<-month.abb

# use monthly scaled data to perc of annual total
moScalePrec<-calc(moAvgPrec, fun=function(x){(x/sum(x))*100})
names(moScalePrec)<-paste0(month.abb,"Prec")
plot(moScalePrec, colNA="red")

# set grid for clustering
clusterGrid<-moScalePrec

# crop to smaller area? AZ/NM 
clusterGrid<-crop(clusterGrid, extent(-114.88,-102.9,31.22,37.08))
#####

# supp material figure on monthly perc of annual total
#cols <- colorRampPalette(RColorBrewer::brewer.pal(9,"YlGn"))
#rasterVis::levelplot(clusterGrid, col.regions=cols,  main="Monthly % of Annual Total Precip")+
#  latticeExtra::layer(sp.polygons(states, fill=NA, col = "grey40"))

##### evaluate cluster N ---- supp material ----
# # # # find optimal cluster number
clusterN=10
wss<-c(NA, seq(2,clusterN,1))
minwss<-c(NA, seq(2,clusterN,1))
totss<-c(NA, seq(2,clusterN,1))
btwss<-c(NA, seq(2,clusterN,1))
for (i in 2:clusterN){
  set.seed(1234)
  unC <- unsuperClass(clusterGrid, nSamples = 250000, nClasses = i, nStarts = 25, nIter = 1000)
  wss[i]<-unC$model$tot.withinss
  minwss[i]<-min(unC$model$withinss)
  totss[i]<-unC$model$totss
  btwss[i]<-unC$model$betweenss
  print(i)
}
# save cluster diag vars
save(clusterN,wss,minwss,totss,btwss,
     file="./data/moScale_FindClusterDiagnostics.RData")
load("./data/moScale_FindClusterDiagnostics.RData")

plot(2:clusterN, wss[2:clusterN], type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares", ylim=c(min(wss[2:clusterN]),max(wss[2:clusterN])))
plot(3:clusterN, diff(wss[2:clusterN])*-1, type="b", xlab="Number of Clusters",
     ylab="Diff Within groups sum of squares")#, ylim=c(min(diff(wss[2:clusterN])),max(diff(wss[2:clusterN]))))
plot(2:clusterN, btwss[2:clusterN], type="b", xlab="Number of Clusters",
     ylab="Between groups sum of squares",  ylim=c(min(btwss[2:clusterN]),max(btwss[2:clusterN])))
plot(2:clusterN, minwss[2:clusterN], type="b", xlab="Number of Clusters",
     ylab="Min groups sum of squares",  ylim=c(min(minwss[2:clusterN]),max(minwss[2:clusterN])))
plot(3:clusterN, diff(minwss[2:clusterN])*-1, type="b", xlab="Number of Clusters",
     ylab="Diff Min groups sum of squares")#,ylim=c(min(minwss[2:clusterN]),max(minwss[2:clusterN])))
plot(2:clusterN, btwss[2:clusterN]/totss[2:clusterN], type="b", xlab="Number of Clusters",
     ylab="BSS/TSS Ratio")#,  ylim=c(min(totss[2:clusterN]),max(totss[2:clusterN])))
#####



##### develop clustering -----
# clustering
library(RStoolbox)
library(parallel)
library(rasterVis)

rsOpts(verbose=TRUE)

# cluster data
ptm <- proc.time()
  set.seed(1234)
  clusterN<-5
  #unC <- unsuperClass(clusterGrid, nSamples = 5000000, nClasses = clusterN, nStarts = 25, nIter = 1000, norm = FALSE, output = 'distances') # or allGDD
  unC <- unsuperClass(clusterGrid, clusterMap=FALSE, nClasses = clusterN, nStarts = 50, nIter = 5000, norm = FALSE, output = 'classes', algorithm = "Hartigan-Wong") # or allGDD
  #unC <- unsuperClass(clusterGrid, clusterMap=FALSE, nClasses = clusterN, nStarts = 50, nIter = 5000, norm = FALSE, output = 'distances') # or allGDD
proc.time() - ptm

classMap<-as.factor(unC$map)
#classMap<-unC$map
rat <- levels(classMap)[[1]]
# cluster names
 rat[["cluster"]]<-(c("Colorado Plateau","Great Plains","Lower Gila/Colorado","Upper Gila","Rio Grande"))

#rat[["cluster"]] <- as.character(seq(1, clusterN, by=1))
levels(classMap) <- rat 
#####


##### make cluster map using sp
# convert raster to df
rast_df <- as.data.frame(classMap, xy = TRUE)
# drop NA values
rast_df<-na.omit(rast_df)
# state polygons
stateDF <- fortify(states, region = "NAME_1")

p1<-ggplot(data = rast_df) +
  geom_raster(aes(x = x, y = y, fill = class_cluster)) +
  #scale_fill_manual(values=c("grey","#7570B3","#B2DF8A","#8DD3C7","#80B1D3"), name="region")+
  scale_fill_manual(values=c('#7fc97f','#beaed4','#fdc086','#dede73','#386cb0'), name="region")+
  #scale_fill_manual(values=c('#66c2a5','#fc8d62','#8da0cb','#e78ac3','#a6d854'), name="region")+
  geom_polygon(data=stateDF, aes(x = long, y = lat, group = group),fill=NA, color="grey17", size=0.1)+
  geom_polygon(data = countries, aes(x = long, y = lat, group = group), fill="grey88", color="grey88", size=0.1)  +
  #coord_equal()+
  coord_fixed(xlim=c(-114.88,-102.9), ylim=c(31.22,37.08), ratio = 1)+
  scale_x_continuous(expand=c(0,0)) +
  scale_y_continuous(expand=c(0,0)) +
  xlab("")+
  ylab("")+
  theme_bw() +
  theme(
    legend.position = "bottom"
  )
#####

##### plot monthly cluster values
# cluster centers - months
moCluster<-as.data.frame(unC$model$centers)
colnames(moCluster)<-seq(1,12,1)
#moCluster$cluster<-seq(1,nrow(moCluster),1)
moCluster$cluster<-c("Colorado Plateau","Great Plains","Lower Gila/Colorado","Upper Gila","Rio Grande")
moCluster <- tidyr::gather(moCluster, month, precip, 1:12, factor_key=TRUE)
moCluster$month<-as.numeric(moCluster$month)

# plot monthly data
p2<-ggplot(moCluster, aes(month,precip, color=as.factor(cluster)))+
  geom_line()+
  geom_point(size=2)+
  scale_color_manual(values=c('#7fc97f','#beaed4','#fdc086','#dede73','#386cb0'), name="region")+
  scale_x_continuous(breaks=seq(1,12,1))+
  #ggtitle("K-mean cluster centers - Percent of annual total by month")+
  ylab("% of annual total precip")+
  theme_bw()+
  theme(
    legend.position = "bottom"
  )

# combined figure
plot_grid(p1, p2, ncol=1)
#####


