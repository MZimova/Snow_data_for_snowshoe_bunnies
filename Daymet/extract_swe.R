# Extract NetCDF SWE data from Daymet tiles
# Alex Bryan/Marketa Zimova 
# 5/2017

library(ncdf4)

# Load data
setwd('///Volumes/Seagate/Daymet') 
#setwd('C:/Users/ABRYAN/Documents/proj-active/snowshoe_hare')

# Load camera-tile matching information
load("cam_tiles.RData") 

# Loop over cameras and years; and use Tile, lat_index and lon_index columns in cam_tiles 
#to get the appropriate tile and pixel matching the camera:
swe_df <- NULL
for (tile in unique(cam_tiles$Tile))
{
  #for (year in 2015:2015)
  for (year in 2012:2013)
  {
    
    # Read in one daymet file, corresponding to one tile and year
    ncin <- nc_open(paste0(tile,"_",year,"_",year+1,"_swe.nc"))
    #ncin <- nc_open(paste0('data/daymet/',tile,"_",year,"_",year+1,"_swe.nc"))
    time <-ncvar_get(ncin, "time")
    date <- as.Date("1980-01-01") + time
    swe <- ncvar_get(ncin, "swe")
    
    # Loop over only those cameras within the tile above (will eventually
    # get through all of the cameras as it goes through the tiles)
    for (camera in which(cam_tiles$Tile == tile))
    {
      
      # get Daymet grid cell indices for this camera
      ilat <- cam_tiles$lat_index[camera]
      ilon <- cam_tiles$lon_index[camera]
      
      # used rep to copy the camera 365x to match the length of date and swe; and rbind to append new rows with each loop
      swe_df <- rbind(swe_df,
                      data.frame(
                        name = rep(cam_tiles$Name[camera],length(date)), 
                        date = date, 
                        swe = swe[ilon,ilat,]
                      ))
      #winter_metric_1 <- function_for_winter_metric_1(swe[cam_tiles$lon_index,cam_tiles$lat_index,])
    } # end camera loop
  } # end year loop
} # end tile loop

# Create csv with Camera name, Date and SWE
data <- write.csv(swe_df, file = "xxx.csv")

