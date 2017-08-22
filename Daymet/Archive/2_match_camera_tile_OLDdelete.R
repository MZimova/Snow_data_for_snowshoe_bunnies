# Match cameras with Daymet tiles and grid points
# Alex Bryan
# 6/2017

# Packages
library(ncdf4)

# Set wd and load data
setwd('///Volumes/Seagate/Daymet') 
#setwd('C:/Users/ABRYAN/Documents/proj-active/snowshoe_hare')

# Open camera locations data
camsNH <-read.csv("/Users/marketzimova/Documents/WORK/DISSERTATION/3 Camera Traps Study/camera locations/Colorado/Camera_Locations_2010_2015_reprojected_EPSG4326_MASTER.csv", header=T,sep=",")
#camsNH <-read.csv("data/hare_locs_NH_VT_Rileybatch.csv", header=T,sep=",")

# Load tile lat/lon info for matching tiles w/ cameras
  # RData file created using 1_find_tile_Daymet.R code
load("tile_locs.RData") 

########################################################################################################
# Find which tile has each camera 

#1. one camera at a time
#camera <- which(camsNH$Camera == "Ivan1042015")
#(tile_id <- tile_ids[which(lats[,1] < camsNH$Lat[camera] & lats[,2] > camsNH$Lat[camera]
#                           & lons[,1] < camsNH$Lon[camera] & lons[,2] > camsNH$Lon[camera])])

#2. all cameras at once 
camera_names <-names(table(camsNH$Camera))
#camera_names=c("CLNA1","CLNA10","CLNA2","CLNA4","CLNA6","CLNA7","CLNA8","CLTC4","Jeff1","Jeff2","Jeff3","Jeff4","Jeff5","Kil10","Kil2","Kil3","Kil5","Kil6","Kil7","Kil8","Kil9","Kins2","Kins3","Kins4","Nul10","Nul11","Nul13","Nul14","Nul15","Nul16","Nul17","Nul18","Nul19","Nul20","Nul9","VB1","VB2","VB3","VB4","VB5","VB6","VB7","VB8")
#camera_names=c("SWTB0132016" ,"SWTB0152015" ,"SWTB0162015" ,"SWTB0162016" ,"SWTB0172015" ,"SWTB0182015")

tiles <- numeric(length(camera_names))
for (icam in 1:length(camera_names))
{
  icamsNH <- which(camsNH$Camera == camera_names[icam])
  tiles[icam] <- tile_ids[which(lats[,1] < camsNH$Lat[icamsNH] & lats[,2] > camsNH$Lat[icamsNH]
                                & lons[,1] < camsNH$Lon[icamsNH] & lons[,2] > camsNH$Lon[icamsNH])]
}
tiles
# Create dataframe with camera name and corresponding tile
(cam_tiles <- data.frame(Name=camera_names, Tile=tiles))

############################################################################################
# Find which grid cells (within the appropriate tile) are nearest each camera
# This loop opens each tile that contains cameras only once 
#you'll want this once you start dealing with multiple tiles; for now, it will only read in 12115 as you had)
for (tile_id in unique(cam_tiles$Tile))
{
  #open climate data, one year at a time
  # Kil6, Kil8, Kins2, Jef1, Jef2, Zeal3 are in 12115; Sand1,Sand2 are in 11935
  ncin <- nc_open(paste0(tile_id,"_2003_2004_swe.nc"))
  #ncin <- nc_open("12115_2003_2004_swe.nc")
  #ncin <- nc_open(paste0('data/daymet/',tile_id,"_2015_2016_swe.nc"))
  
  # extract variables
  lat <- ncvar_get(ncin, "lat") 
  lon <- ncvar_get(ncin, "lon") 

  # Find daymet cell nearest to each camera location
  # loop through cameras
  # also works w/more than 1 tile
  for (icam in which(cam_tiles$Tile == tile_id)) 
  {
    camera <- which(camsNH$Camera == camera_names[icam])
    dlat <- abs(lat-camsNH[camera,]$Lat)
    dlon  <- abs(lon-camsNH[camera,]$Lon)
    dlatlon <- dlat + dlon
    ilatlon  <- which(dlatlon == min(dlatlon))  # 1-dimensional lat/lon value
    ilat  <- ceiling(ilatlon/nrow(lat))         # lat value in 2 dimensions
    ilon  <- ilatlon - ((ilat - 1) * nrow(lat)) # lon value in 2 dimensions

    cam_tiles$lat_index[icam] <- ilat
    cam_tiles$lon_index[icam] <- ilon
    
  } # end camera loop
  
} # end tile loop

# Save the cam_tile information for loading into winter_metrics
save(cam_tiles, file="cam_tiles.RData")
load("cam_tiles.RData")

