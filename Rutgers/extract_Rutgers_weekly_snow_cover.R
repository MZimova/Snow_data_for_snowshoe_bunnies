# Extract  date and snow cover from Rutgers weekly data
# Alex Bryan/Marketa Zimova 
  # NetCDF file with weekly snow cover extent
  # 1 degree resolution, 1966-2017
# 6/2017

library(ncdf4)
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

# find Rutgers cell nearest to camera location
  #one camera at a time
# camera <- which(camsNH$Camera == "Kil6")
# dlat <- abs(lat-camsNH[camera,]$Lat)
# dlon <- abs(lon-camsNH[camera,]$Lon)
# dlatlon <- dlat + dlon
# ilatlon <- which(dlatlon == min(dlatlon))  # 1-dimensional lat/lon value
# ilat <- ceiling(ilatlon/nrow(lat))         # lat value in 2 dimensions
# ilon <- ilatlon - ((ilat - 1) * nrow(lat)) # lon value in 2 dimensions
# snow[ilon,ilat,]                    # snow wat eqv in 3 dimensions
# 
# #save data set
# data <- data.frame(Date=date, Snow=snow[ilon,ilat,], Camera="Kil6")
# write.csv(data,"weeklyRutgers.csv")

#### PLEASE HELP W FUNCTION
# find Rutgers cell nearest to each camera location
  # loop through cameras
for (icam in 1:length(camera_names))
{
  camera <- which(camsNH$Camera == camera_names[icam])
  dlat[icam]  <- abs(lat-camsNH[camera,]$Lat)
  dlon[icam]  <- abs(lon-camsNH[camera,]$Lon)
  dlatlon[icam]  <- dlat + dlon
  ilatlon[icam]  <- which(dlatlon == min(dlatlon))  # 1-dimensional lat/lon value
  ilat[icam]  <- ceiling(ilatlon/nrow(lat))         # lat value in 2 dimensions
  ilon[icam]  <- ilatlon - ((ilat - 1) * nrow(lat)) # lon value in 2 dimensions
  snow[ilon,ilat,]                      # snow wat eqv in 3 dimensions
}

# save data (after 2014 only)
data <- data.frame(Date=date, Name=camera_names, Snow=snow[ilon,ilat,], Lat=ilat, Lon=ilon)
datasubset <-subset(data, data$Date > "2014-01-01")


