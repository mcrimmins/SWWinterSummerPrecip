# process monthly PRISM data for SW winter-summer precip analysis
# adapted from /LivenehDrought/processMonthlyPRISM.R
# MAC 08/23/21

library(raster)
library(prism)

# set rasteroptions
rasterOptions(progress = 'text')

# dates 1895-2017 PRISM data
dates=seq(as.Date("1895-01-01"), as.Date("2020-12-31"), by="month")

# precip
options(prism.path = "/scratch/crimmins/PRISM/monthly/precip") 
prec <- pd_stack(prism_archive_ls())
#writeRaster(prec,filename="/scratch/crimmins/PRISM/monthly/processed/monthlyPRISM_prec_1895_2017.grd", overwrite=TRUE )

# tmean
options(prism.path = "/scratch/crimmins/PRISM/monthly/tmean") 
tmean <- pd_stack(prism_archive_ls())
#writeRaster(tmean,filename="/scratch/crimmins/PRISM/monthly/processed/monthlyPRISM_tmean_1895_2017.grd", overwrite=TRUE )

# SW domain
# -115.576172,30.543339,-101.997070,38.582526

# western US only
e <- extent(-115.5, -102, 30.5, 39)
prec <- crop(prec, e)	
tmean <- crop(tmean, e)	
#tmax <- crop(tmax, e)	
#tmin <- crop(tmin, e)	

names(prec)<-dates
names(tmean)<-dates

writeRaster(prec,filename="/scratch/crimmins/PRISM/monthly/processed/SW/SWmonthlyPRISM_prec_1895_2020.grd", overwrite=TRUE )
writeRaster(tmean,filename="/scratch/crimmins/PRISM/monthly/processed/SW/SWmonthlyPRISM_tmean_1895_2020.grd", overwrite=TRUE )
#writeRaster(tmax,filename="/scratch/crimmins/PRISM/monthly/processed/west/WESTmonthlyPRISM_tmax_1895_2017.grd", overwrite=TRUE )
#writeRaster(tmin,filename="/scratch/crimmins/PRISM/monthly/processed/west/WESTmonthlyPRISM_tmin_1895_2017.grd", overwrite=TRUE )
