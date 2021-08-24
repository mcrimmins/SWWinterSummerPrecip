# prep data for SW winter-summer precip analysis
# MAC 08/23/21

library(raster)

# update scratch dir with PRISM data 1895-2020 monthly precip/mean temp
# use ~/RProjects/PRISMDownload/monthyDownloadPRISM.R



# load data
# watershed boundaries
huc4<-rgdal::readOGR(dsn = "~/RProjects/SOMs/monsoonPrecip/shapes/wbdhu4_a_us_september2019/wbdhu4_a_us_september2019.gdb", layer="WBDHU4")
SW_hucs<-subset(huc4, HUC4 %in% c("1504","1506","1502","1302"))

