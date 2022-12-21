# Testing seasonality clustering
# MAC 12/15/22

library(raster)

# set rasteroptions
rasterOptions(progress = 'text')

# load data
# watershed boundaries
huc4<-rgdal::readOGR(dsn = "~/RProjects/SOMs/monsoonPrecip/shapes/wbdhu4_a_us_september2019/wbdhu4_a_us_september2019.gdb", layer="WBDHU4")
SWhucs<-subset(huc4, HUC4 %in% c("1504","1506","1502","1302"))
# try other polygons...MLRAs, ecoregions?
states <- getData('GADM', country='United States', level=1)

# dates 1895-2020 PRISM data
dates=seq(as.Date("1895-01-01"), as.Date("2020-12-31"), by="month")
dates<-as.data.frame(dates)
dates$month<-as.numeric(format(dates$dates, "%m"))
dates$year<-as.numeric(format(dates$dates, "%Y"))
length(unique(dates$year))

# update scratch dir with PRISM data 1895-2020 monthly precip/mean temp
# use ~/RProjects/PRISMDownload/monthyDownloadPRISM.R
# process to subset using ~/RProjects/WinterSummerPrecip/processPRISM.R
prec<-stack("/scratch/crimmins/PRISM/monthly/processed/SW/SWmonthlyPRISM_prec_1895_2020.grd")

# nClimGrid precip - use processnClimGrid.R
#prec<-stack("/scratch/crimmins/climgrid/processed/sw/SWmonthly_nClimGrid_prec_1895_2020.grd")

# subset precip?
prec<-subset(prec, which(dates$year>=1991 & dates$year<=2021))


# monthly averages
monthSum<- stackApply(prec, dates$month, fun = sum)
moAvgPrec<-monthSum/length(unique(dates$year))

# rescaled monthly averages
#moScale<-calc(moAvgPrec, fun=function(x){(x-mean(x))/sd(x)})
moScale<-calc(moAvgPrec, fun=function(x){(x/sum(x))*100})
#moClusterScale<-calc(moAvgPrec, scale)

clusterGrid<-moScale

# clustering
library(RStoolbox)
library(parallel)
library(rasterVis)

rsOpts(verbose=TRUE)

# cluster data
ptm <- proc.time()
set.seed(1234)
  clusterN<-9
  unC <- unsuperClass(clusterGrid, nSamples = 5000000, nClasses = clusterN, nStarts = 25, nIter = 1000, norm = FALSE) # or allGDD
proc.time() - ptm

# random colors -- https://stackoverflow.com/questions/15282580/how-to-generate-a-number-of-most-distinctive-colors-in-r
qual_col_pals = RColorBrewer::brewer.pal.info[RColorBrewer::brewer.pal.info$category == 'qual',]
col_vector = unlist(mapply(RColorBrewer::brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))
darkcols<-sample(col_vector, clusterN)

classMap<-as.factor(unC$map)
rat <- levels(classMap)[[1]]
# cluster names
# rat[["cluster"]]<-rev(c("Eastside","Oro Valley","Rita Ranch","Gates Pass",
#                         "Twin Peaks","Saddlebrook","Foothills","Central"))

rat[["cluster"]] <- as.character(seq(1, clusterN, by=1))
levels(classMap) <- rat 

# plot classified map
rasterVis::levelplot(classMap, col.regions=darkcols, par.settings=list(panel.background=list(col="white")),
                     margin=FALSE, main="Precip Season Clusters")+
  latticeExtra::layer(sp.polygons(states, fill=NA, col = "grey40"))+
  # layer(sp.polygons(huc4, fill=NA, col = "black"))
  latticeExtra::layer(sp.polygons(SWhucs, fill=NA, col = "black",lwd=0.5))
  # lty=2, http://www.sthda.com/english/wiki/line-types-in-r-lty 

# cluster centers
moCluster<-as.data.frame(unC$model$centers)
colnames(moCluster)<-seq(1,12,1)
moCluster$cluster<-seq(1,nrow(moCluster),1)
moCluster <- tidyr::gather(moCluster, month, precip, 1:12, factor_key=TRUE)
moCluster$month<-as.numeric(moCluster$month)

# plot monthly data
library(ggplot2)

ggplot(moCluster, aes(month,precip))+
  geom_bar(stat = "identity")+
  scale_x_continuous(name="Month", breaks=c(seq(1,12,1)))+
  ylab("% of annual total")+
  facet_wrap(.~cluster)+
  ggtitle("K-mean cluster centers - Percent of annual total by month")+
  theme_bw()

# CLUSTER Diagnostics
#cluster correlations
library(corrplot)
centers<-as.data.frame(t(unC$model$centers))
res <- cor(centers, method = "pearson")
corrplot(res, type = "upper", order = "original",
         tl.col = "black", tl.srt = 45, title="Intercluster Pearson Corrs")

# # # # find optimal cluster number
clusterN=20
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
      file="moScale_FindClusterDiagnostics.RData")
# #load("./fixed/zProd_FindClusterDiagnostics.RData")
 
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
 
 







