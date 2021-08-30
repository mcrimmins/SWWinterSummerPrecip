# download nClimGrid data
# MAC 08/30/21
# get data from https://www.ncei.noaa.gov/data/nclimgrid-monthly/access/
# https://www.ncei.noaa.gov/access/metadata/landing-page/bin/iso?id=gov.noaa.ncdc:C00332

library(RCurl)
library(raster)

# get precip grid ~ 1 GB
URL <- "https://www.ncei.noaa.gov/data/nclimgrid-monthly/access/nclimgrid_prcp.nc"
download.file(URL, destfile = "/scratch/crimmins/climgrid/netcdf/nclimgrid_prcp.nc", method="curl")