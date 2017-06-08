# Extract NetCDF SWE data from Daymet tiles
# Alex Bryan/Marketa Zimova 
# 5/2017

library(ncdf4)

#load data
setwd('///Volumes/Seagate/Daymet') 
#setwd('C:/Users/ABRYAN/Documents/proj-active/snowshoe_hare')

# open camera locations data
camsNH <-read.csv("/Users/marketzimova/Documents/WORK/DISSERTATION/3 Camera Traps Study/camera locations/NH/hare_locs_NH_VT_Rileybatch.csv", header=T,sep=",")
#camsNH <-read.csv("data/hare_locs_NH_VT_Rileybatch.csv", header=T,sep=",")

load("tile_locs.RData") # load tile lat/lon info for matching tiles w/ cameras

#find which tile has each camera (one camera at a time)
#camera <- which(camsNH$Camera == "Kil6")
#(tile_id <- tile_ids[which(lats[,1] < camsNH$Lat[camera] & lats[,2] > camsNH$Lat[camera]
#                           & lons[,1] < camsNH$Lon[camera] & lons[,2] > camsNH$Lon[camera])])

#find which tile has each camera for all cameras at once 
camera_names=c("CLNA1","CLNA10","CLNA2","CLNA4","CLNA6","CLNA7","CLNA8","CLTC4","Jeff1","Jeff2","Jeff3","Jeff4","Jeff5","Kil10","Kil2","Kil3","Kil5","Kil6","Kil7","Kil8","Kil9","Kins2","Kins3","Kins4","Nul10","Nul11","Nul13","Nul14","Nul15","Nul16","Nul17","Nul18","Nul19","Nul20","Nul9","VB1","VB2","VB3","VB4","VB5","VB6","VB7","VB8")
tiles <- numeric(length(camera_names))
for (icam in 1:length(camera_names))
{
  icamsNH <- which(camsNH$Camera == camera_names[icam])
  tiles[icam] <- tile_ids[which(lats[,1] < camsNH$Lat[icamsNH] & lats[,2] > camsNH$Lat[icamsNH]
                                & lons[,1] < camsNH$Lon[icamsNH] & lons[,2] > camsNH$Lon[icamsNH])]
}
data <- data.frame(Name=camera_names, Tile=tiles)


#open climate data
# Kil 6, Kil 8, Kins 2, Jef 1, Jef 2, Zeal 3 are in 12115; Sand 1,Sand 2 are in 11935
ncin <- nc_open(paste0(tile_id,"_2003_2004_swe.nc"))
ncin

# extract variables
time <-ncvar_get(ncin, "time") #time <- get.var.ncdf(ncin, "time") 
# days since 1980-01-01 00:00:00 UTC
date <- as.Date("1980-01-01") + time # convert time to date format
lat <- ncvar_get(ncin, "lat") #lat <- get.var.ncdf(ncin, "lat")
lon <- ncvar_get(ncin, "lon") #lon <- get.var.ncdf(ncin, "lon")
swe <- ncvar_get(ncin, "swe") #swe <- get.var.ncdf(ncin, "swe")

# find daymet cell nearest to camera location
dlat <- abs(lat-camsNH[camera,]$Lat)
dlon <- abs(lon-camsNH[camera,]$Lon)
dlatlon <- dlat + dlon
ilatlon <- which(dlatlon == min(dlatlon))  # 1-dimensional lat/lon value
ilat <- ceiling(ilatlon/nrow(lat))         # lat value in 2 dimensions
ilon <- ilatlon - ((ilat - 1) * nrow(lat)) # lon value in 2 dimensions
swe[ilon,ilat,]                    # snow wat eqv in 3 dimensions

#save to dataframe
data <- data.frame(Date=date, SWE=swe[ilon,ilat,], Camera="Sand 1")
data1 <- data.frame(Date=date, SWE=swe[ilon,ilat,], Camera="Sand 2")
#data2 <- data.frame(Date=date, SWE=swe[ilon,ilat,], Camera="Zeal 3")
#data3 <- data.frame(Date=date, SWE=swe[ilon,ilat,], Camera="Jeff1")
#data4 <- data.frame(Date=date, SWE=swe[ilon,ilat,], Camera="Jeff2")
#data5 <- data.frame(Date=date, SWE=swe[ilon,ilat,], Camera="Zeal 3")
#data6 <- data.frame(Date=date, SWE=swe[ilon,ilat,], Camera="Kil8")
#data7 <- data.frame(Date=date, SWE=swe[ilon,ilat,], Camera="Kil8")

# combine and save to csv
morecams20132014<-rbind(data, data1)
write.csv(morecams20132014, file = "moreDaymet_bestcamsNH_2013_2014.csv")

