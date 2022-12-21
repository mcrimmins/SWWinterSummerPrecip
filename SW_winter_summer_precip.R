# prep data for SW winter-summer precip analysis
# MAC 08/23/21

library(raster)
library(tidyverse)
library(ggplot2)
library(dplyr)
library(rasterVis)
library(viridis)

# set rasteroptions
rasterOptions(progress = 'text')

# load and summarize climate indices
load("~/RProjects/WinterSummerPrecip/climInd.RData")
climIndAnn<-climInd %>% group_by(year) %>%
                        summarize(annONI = mean(ONI))
climIndAnn$annONIroll<-(zoo::rollapply(climIndAnn$annONI, 30, mean, na.rm = TRUE, align="right", fill=NA))


# update scratch dir with PRISM data 1895-2020 monthly precip/mean temp
# use ~/RProjects/PRISMDownload/monthyDownloadPRISM.R
# process to subset using ~/RProjects/WinterSummerPrecip/processPRISM.R
 prec<-stack("/scratch/crimmins/PRISM/monthly/processed/SW/SWmonthlyPRISM_prec_1895_2020.grd")

# nClimGrid precip - use processnClimGrid.R
#prec<-stack("/scratch/crimmins/climgrid/processed/sw/SWmonthly_nClimGrid_prec_1895_2020.grd")

# load data
# watershed boundaries
huc4<-rgdal::readOGR(dsn = "~/RProjects/SOMs/monsoonPrecip/shapes/wbdhu4_a_us_september2019/wbdhu4_a_us_september2019.gdb", layer="WBDHU4")
SWhucs<-subset(huc4, HUC4 %in% c("1504","1506","1502","1302"))
# try other polygons...MLRAs, ecoregions?

#####
#####  make maps
# summarize to annual totals
dates<-seq(as.Date("1895-01-01"), as.Date("2020-12-31"), by="month")
years<-as.integer(format(dates,"%Y"))
sumPrec<- stackApply(prec, years, fun = sum) 
avgPrec <- calc(sumPrec, mean)

states <- getData('GADM', country='United States', level=1)

nlev <- 200
# at <- seq(from = cellStats(avgPrec, "min"),
#              to = cellStats(avgPrec, "max"),
#              length.out = nlev + 1)
at<-c(seq(0,1200,100),1800)
my.cols <- viridis_pal(option = "D")(nlev)
avgMap<-levelplot(avgPrec, contour=FALSE, margin=FALSE, at=at,
                   col.regions=my.cols,
                   #par.settings=mapTheme,
                   #par.settings = list(region=c("lightblue", "blue","green","green4","yellow","red", "red4"),
                   #                   axis.line = list(col = textCol[panel.number()])),
                   scales=list(draw=TRUE),
                   main="Annual Average Precipitation (mm)")+
          layer(sp.polygons(states, col = 'gray40', lwd=1))+
          layer(sp.polygons(SWhucs, col = 'black', lwd=2))

# create 3-month season factors for correlations
dates<-as.data.frame(seq(as.Date("1895-01-01"), as.Date("2020-12-31"), by="month"))
colnames(dates)<-"date"
dates$month<-as.integer(format(dates$date,"%m"))
dates$seas <- cut(dates$month,c(0,3,6,9,12))
levels(dates$seas) = c("JFM","AMJ","JAS","OND")
dates$seas_yr<-paste0(dates$seas,"-",format(dates$date,"%Y"))
seasSumPrec<- stackApply(prec, dates$seas_yr, fun = sum)
length(seq(1,nlayers(seasSumPrec)-3,by=4))
# correlate two months
winter_summer_prec_corr<-corLocal(subset(seasSumPrec,seq(1,nlayers(seasSumPrec)-3,by=4)),
                                  subset(seasSumPrec,seq(3,nlayers(seasSumPrec)-1,by=4)),
                                  test=TRUE, method="pearson")
my.at <- seq(-0.5, 0.5, 0.1)
corrMap<-levelplot(winter_summer_prec_corr[[1]], par.settings = RdBuTheme, margin=FALSE, at=my.at,
          main="Pearson Correlation JFM v JAS precipitation")+ 
  layer(sp.polygons(states, col = 'gray40', lwd=1))+
  layer(sp.polygons(SWhucs, col = 'black', lwd=2))
#####


# extract zonal time series
# precTS<-as.data.frame(t(zonal(prec, SWhucs, 'mean'))) # rasterized polygons
precTS<-as.data.frame(t(raster::extract(prec, SWhucs, 'mean'))) # polygons
colnames(precTS)<-unique(SWhucs$NAME)

# calc rolling three month sums
precTSsum<-as.data.frame(zoo::rollapply(precTS[,1:4], 3, sum, na.rm = TRUE, align="right", fill=NA))

# add in date fields
# dates 1895-2017 PRISM data
precTSsum$date<-seq(as.Date("1895-01-01"), as.Date("2020-12-31"), by="month")
precTSsum$month<-as.integer(format(precTSsum$date, "%m"))
precTSsum$year <- as.integer(format(precTSsum$date, "%Y"))

# create 3-month season factors
precTSsum$seas = cut(precTSsum$month,c(0,3,6,9,12))
levels(precTSsum$seas) = c("JFM","AMJ","JAS","OND")

# gather into long format
precTSsumLong <- gather(precTSsum, HUC, sum, 1:4, factor_key=TRUE)

# subset two seasons
precTSsumLong <- subset(precTSsumLong, month %in% c(3,9))

# percent rank for each season
precTSsumLong<-precTSsumLong %>%
                group_by(HUC, seas) %>% 
                mutate(percrank=round(rank(sum)/length(sum),2))

# spread for scatter plots
temp<-precTSsumLong[,c("year","seas","HUC","percrank")]
temp <- spread(temp, seas, percrank)

# scatterplot of winter/summer relationships 3-month windows
p<-ggplot(temp, aes(x=JFM, y=JAS, name=year)) +
  geom_point()+
  geom_smooth(method=lm, color="red", se=FALSE)+
  facet_wrap(.~HUC)+
  geom_quantile(quantiles = c(0.1,0.5,0.9))+
  theme_bw()
p<-plotly::ggplotly(p)

# bar plots of seasonal precip
p2<-ggplot(precTSsumLong, aes(x=year, y=sum, fill=seas)) + 
  geom_bar(stat="identity",position="dodge")+
  facet_wrap(.~HUC)+
  theme_bw()
p2<-plotly::ggplotly(p2)


# correlations between watersheds
library("PerformanceAnalytics")
temp<-subset(precTSsum, seas=="JAS")
my_data <- temp[, c(1:4)]
JAScorr<-chart.Correlation(my_data, histogram=TRUE, pch=19)

# chi-square test of percentile exceedances
# create percentile categories
precTSsumLong$percrank_cat = cut(precTSsumLong$percrank,c(0,0.33,0.66,1))
levels(precTSsumLong$percrank_cat) = c("Dry","Normal","Wet")
temp<-precTSsumLong[,c(3,4,5,8)]
temp<-spread(temp, seas, percrank_cat)

countsTable<-temp%>%
  group_by(HUC,JFM, JAS)%>%
  summarize(n=n())%>%
  spread(JAS,n) %>%
  knitr::kable()

##### 
# uneven comparison periods
# 6-month ONDJFM winter
# 1-month July/summer

# extract zonal time series
# precTS<-as.data.frame(t(zonal(prec, SWhucs, 'mean'))) # rasterized polygons
precTS<-as.data.frame(t(raster::extract(prec, SWhucs, 'mean'))) # polygons
colnames(precTS)<-unique(SWhucs$NAME)
  #precTS$date<-seq(as.Date("1895-01-01"), as.Date("2020-12-31"), by="month")


# winter seas
precTSwinter<-as.data.frame(zoo::rollapply(precTS[,1:4], 6, sum, na.rm = TRUE, align="right", fill=NA))
# add in date fields
# dates 1895-2017 PRISM data
precTSwinter$date<-seq(as.Date("1895-01-01"), as.Date("2020-12-31"), by="month")
precTSwinter$month<-as.integer(format(precTSwinter$date, "%m"))
precTSwinter$year <- as.integer(format(precTSwinter$date, "%Y"))
  # subset to winter season
  precTSwinter<-subset(precTSwinter, month==3)
  precTSwinter$seas<-"ONDJFM"

# gather into long format
precTSwinterLong <- gather(precTSwinter, HUC, sum, 1:4, factor_key=TRUE)
precTSwinterLong <- precTSwinterLong %>%
  group_by(HUC, seas) %>% 
  mutate(winter_percrank=round(rank(sum)/length(sum),2))
precTSwinterLong$winter_percrank<-ifelse(is.na(precTSwinterLong$sum)==TRUE, NA, precTSwinterLong$winter_percrank)

# summer seas
# dates 1895-2017 PRISM data
#precTSsummer<-precTS
precTSsummer<-as.data.frame(zoo::rollapply(precTS[,1:4], 1, sum, na.rm = TRUE, align="right", fill=NA))
precTSsummer$date<-seq(as.Date("1895-01-01"), as.Date("2020-12-31"), by="month")
precTSsummer$month<-as.integer(format(precTSsummer$date, "%m"))
precTSsummer$year <- as.integer(format(precTSsummer$date, "%Y"))
  # subset to summer season
  precTSsummer<-subset(precTSsummer, month==9)
  precTSsummer$seas<-"JAS"

# gather into long format
precTSsummerLong <- gather(precTSsummer, HUC, sum, 1:4, factor_key=TRUE)
precTSsummerLong <- precTSsummerLong %>%
  group_by(HUC, seas) %>% 
  mutate(summer_percrank=round(rank(sum)/length(sum),2))
precTSsummerLong$summer_percrank<-ifelse(is.na(precTSsummerLong$sum)==TRUE, NA, precTSsummerLong$summer_percrank)

# correlations between watersheds
# library("PerformanceAnalytics")
# my_data <- precTSsummer[, c(1:4)]
# chart.Correlation(my_data, histogram=TRUE, pch=19)


# combine into common dataframe
precComb<-merge(precTSsummerLong, precTSwinterLong, by=c("year","HUC"))

# scatterplot of winter/summer relationships 3-month windows
p3<-ggplot(precComb, aes(x=winter_percrank, y=summer_percrank, name=year)) +
  geom_point()+
  #geom_smooth(method=lm)+
  facet_wrap(.~HUC)+
  geom_quantile(quantiles = c(0.1,0.5,0.9))+
  theme_bw()+
  ggtitle("ONDJFM vs JAS precipitation")

p3<-plotly::ggplotly(p3)

## rolling correlations
runCorr<- precComb %>%
        group_by(HUC) %>%
          mutate(cor = zoo::rollapplyr(
          data = cbind(sum.x, sum.y),
          width = 15,
          FUN = function(w) cor(w[, 1], w[, 2]),
          by.column = FALSE,
          fill = NA, align="center"))
temp<-merge(runCorr,climIndAnn,by="year")

# plot corrs
p4<-ggplot(runCorr,aes(date.x,cor))+
      geom_line()+
      facet_wrap(.~HUC)+
      geom_hline(yintercept = 0)+
      ylim(-0.8,0.8)+
      ggtitle("15 yr running centered correlation - ONDJFM vs Sept")
  #geom_line(data=temp, aes(date.x,annONIroll), color="red")
p4<-plotly::ggplotly(p4)

## Trying different period sums within group_by
  leng1<-6; endMo1<-3 # seas 1
  leng2<-3; endMo2<-9 # seas 2
  precTSfull<-precTS
  precTSfull$date<-seq(as.Date("1895-01-01"), as.Date("2020-12-31"), by="month")
  precTSfull$month<-as.integer(format(precTSfull$date, "%m"))
  precTSfull$year <- as.integer(format(precTSfull$date, "%Y"))
  # diff time periods?
  #precTSfull<-subset(precTSfull, year>1950)
  
  precTSfullLong <- gather(precTSfull, HUC, sum, 1:4, factor_key=TRUE)
  precTSfullLong <- precTSfullLong %>% group_by(HUC) %>%
                    mutate(period1=zoo::rollapply(sum, leng1, sum, na.rm = TRUE, align="right", fill=NA),
                           period2=zoo::rollapply(sum, leng2, sum, na.rm = TRUE, align="right", fill=NA))
  precTSfullLong <- precTSfullLong %>% group_by(HUC, year) %>%
                    filter(month %in% c(endMo1,endMo2))
  precTSfullLong<-precTSfullLong[,c(-5)]
    temp1<-spread(precTSfullLong, month, period1)
      temp1<-na.omit(temp1[,c(1,2,3,5)])   
    temp2<-spread(precTSfullLong, month, period2)
      temp2<-na.omit(temp2[,c(1,2,3,6)])  
    temp1<-merge(temp1,temp2[,c(2,3,4)], by=c("year","HUC"))
    colnames(temp1)[c(4,5)]<-c("per1","per2")
  # get correlation by HUC
    corHUC<-temp1 %>% group_by(HUC) %>%
                    summarize(HUCcor=cor(per1,per2))

# correlations between months
    
    precTSmo <- gather(precTSfull, HUC, sum, 1:4, factor_key=TRUE)
    precTSmo <- precTSmo[,-1]
    precTSmo <- spread(precTSmo, month, sum)
    
    moCorr <- precTSmo  %>% 
              group_by(HUC) %>%
              summarize(corr = cor(`7`, `9`, method = "sp"))
    
    
#####

    write.csv(precTS, file = "HUC_precipitation.csv")    
    
rmarkdown::render("notebook.Rmd", output_file="SWUSprecip_notebook.html")