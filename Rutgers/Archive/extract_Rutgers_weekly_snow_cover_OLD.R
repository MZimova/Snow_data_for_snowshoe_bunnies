# Extract  date and snow cover from Rutgers weekly data
# Alex Bryan/Marketa Zimova 
# NetCDF file with weekly snow cover extent
# 1 degree resolution, 1966-2017
# 8/2017

library(ncdf4)
library(lubridate)
camsNH <-read.csv("/Users/marketzimova/Documents/WORK/DISSERTATION/3 Camera Traps Study/camera locations/NH/hare_locs_NH_VT_Rileybatch.csv", header=T,sep=",")
camera_names=c("CLNA1","CLNA10","CLNA2","CLNA4","CLNA6","CLNA7","CLNA8","CLTC4","Jeff1","Jeff2","Jeff3","Jeff4","Jeff5","Kil10","Kil2","Kil3","Kil5","Kil6","Kil7","Kil8","Kil9","Kins2","Kins3","Kins4","Nul10","Nul11","Nul13","Nul14","Nul15","Nul16","Nul17","Nul18","Nul19","Nul20","Nul9","VB1","VB2","VB3","VB4","VB5","VB6","VB7","VB8")

#load data
setwd('//Users/marketzimova/Documents/WORK/DISSERTATION/3 Camera Traps Study/analysis SNOW/Rutgers/weekly')
ncin <- nc_open("nhsce_v01r01_19661004_20170501.nc")
ncin

# extract variables
time <-ncvar_get(ncin, "time") #time <- get.var.ncdf(ncin, "time") 
# days since 1966-10-03
date <- as.Date("1966-10-03") + time # convert time to date format
lat <- ncvar_get(ncin, "latitude") #lat <- get.var.ncdf(ncin, "lat")
lon <- ncvar_get(ncin, "longitude") #lon <- get.var.ncdf(ncin, "lon")
snow <- ncvar_get(ncin, "snow_cover_extent") #swe <- get.var.ncdf(ncin, "swe")

<<<<<<< HEAD
photos <-read.csv("/Users/marketzimova/Documents/WORK/DISSERTATION/3 Camera Traps Study/data/NH_hare_data2.csv", header=T,sep=",")
photos$date<-mdy(photos$Date)

# loop through cameras
icam=which(camsNH$Camera == 'Kins2') # one camera at a time
=======
# loop through cameras
icam=which(camsNH$Camera == 'CLNA2') # one camera at a time
>>>>>>> origin/master
#for (icam in 1:length(camera_names))
{
  # find Rutgers cell nearest to each camera location
  camera <- which(camsNH$Camera == camera_names[icam])
  photos_cam <- photos[photos$Camera == camera_names[icam], ]
  dlat  <- abs(lat-camsNH[camera,]$Lat)
  dlon  <- abs(lon-camsNH[camera,]$Lon)
  dlatlon  <- dlat + dlon
  ilatlon  <- which(dlatlon == min(dlatlon))  # 1-dimensional lat/lon value
  ilat  <- ceiling(ilatlon/nrow(lat))         # lat value in 2 dimensions
  ilon  <- ilatlon - ((ilat - 1) * nrow(lat)) # lon value in 2 dimensions
  #snow[ilon,ilat,]                      # snow wat eqv in 3 dimensions
<<<<<<< HEAD
  print(paste(camera_names[icam],ilon,ilat))
=======
>>>>>>> origin/master
  
  color = 'lightblue4'
  #if (elevation > 300) {color = 'lightblue3'} 
  #if (elevation > 400) {color = 'lightblue2'} 
  #if (elevation > 500) {color = 'lightblue1'}
  
<<<<<<< HEAD
  year=2015 # one year at a time
=======
  year=2014 # one year at a time
>>>>>>> origin/master
  #for (year in 1980:2016) # all years
  {
    
    time_subset <- which(date >= paste0(year,"-07-01") & date < paste0(year+1,"-07-01"))
    plot(snow[ilon,ilat,time_subset] ~ date[time_subset], col = 'red', pch = 2, cex = 2.)
<<<<<<< HEAD
    points(photos_cam$Snow/100 ~ photos_cam$date, col = color)
=======
    points(camera_snow, add=T, col = color)
>>>>>>> origin/master
    
  } # end year loop
  
} # end camera loop
<<<<<<< HEAD
head(photos)
=======

>>>>>>> origin/master
