# Alex Bryan 
# Function that extracts lat and lon bounds for each Daymet tile
# June 2018


tile_ids <- c(11376,11377,
              11556,11557,
              11935,
              12115,
              12453,12454,
              12631,12632,12633,
              12811,12812,12813)

library(ncdf4)

setwd('///Volumes/Seagate/Daymet')
#setwd('C:/cygwin/home/abryan/data/obs/daymet')
#setwd('C:/Users/ABRYAN/Documents/proj-active/snowshoe_hare')

lats <- matrix(NA, length(tile_ids), 2) # ncol = 2, nrow = length(tiles)
lons <- matrix(NA, length(tile_ids), 2)
for (itile in 1:length(tile_ids))
{
  # Open the file and read in the data
  tile_id <- tile_ids[itile]
  ncin <- nc_open(paste0(tile_id,"_2003_2004_swe.nc")) #was in the older version
  #ncin <- nc_open(paste0("data/daymet/",tile_id,"_2015_2016_swe.nc")) #added in new version
  lat <- ncvar_get(ncin, "lat")
  lon <- ncvar_get(ncin, "lon")
  swe <- ncvar_get(ncin, "swe")
  
  # Determine which lat and lon values are not missing (each data file
  # trims the corners due to the map projection)
  lat_na <- lat
  lon_na <- lon
  for (jy in 1:nrow(lat))
  {
    for (ix in 1:ncol(lat))
    {
      #  if there are no (0) not-missing values (i.e. all values are missing)
      if (sum(!is.na(swe[jy,ix,])) == 0)
      {
        lat_na[jy,ix] <- NA
        lon_na[jy,ix] <- NA
      }
    }
  }
  lats[itile,] <- round(range(lat_na, na.rm=T))
  lons[itile,] <- round(range(lon_na, na.rm=T))
}

save(tile_ids, lats, lons, file = "tile_locs.RData")

# ### TESTING
# 
# itile=5
# ncin <- nc_open(paste0("data/daymet/",tile_ids[itile],"_2015_2016_swe.nc"))
# lat5 <- ncvar_get(ncin, "lat") #lat <- get.var.ncdf(ncin, "lat")
# lon5 <- ncvar_get(ncin, "lon") #lon <- get.var.ncdf(ncin, "lon")
# swe5 <- ncvar_get(ncin, "swe")
# 
# itile=6
# ncin <- nc_open(paste0("data/daymet/",tile_ids[itile],"_2015_2016_swe.nc"))
# lat6 <- ncvar_get(ncin, "lat") #lat <- get.var.ncdf(ncin, "lat")
# lon6 <- ncvar_get(ncin, "lon") #lon <- get.var.ncdf(ncin, "lon")
# swe6 <- ncvar_get(ncin, "swe")
# 
# range(lat5[,51])
# lat6[,250]
# ilatlon <- which(lat5 == lat6[213,250]) # 10994 = which(lon5 == lon6[214,250]) # 10994
# ilon <- ceiling(ilatlon/nrow(lat5))         # lat value in 2 dimensions
# ilat <- ilatlon - ((ilon - 1) * nrow(lat5)) # lon value in 2 dimensions
# 
# lat5[ilatlon]
# lon5[ilatlon]
# lat5[ilat,ilon]
# lon5[ilat,ilon]
# lat6[213,250]
# lon6[213,250]
# swe5[ilat,ilon,1:5]
# swe6[213,250,]
# lat5[1,1]
# lat6[214,250]
# 
# swe6[213,250,]
# sum(!is.na(swe6[jy,ix,])) == 0
# 
# lat6_na <- lat6 # matrix(NA, 214, 250)
# lon6_na <- lon6 # matrix(NA, 214, 250)
# for (jy in 1:nrow(lat6))
# {
#   for (ix in 1:ncol(lat6))
#   {
#     #  if there are no (0) not-missing values (i.e. all values are missing)
#     if (sum(!is.na(swe6[jy,ix,])) == 0)
#     {
#       lat6_na[jy,ix] <- NA
#       lon6_na[jy,ix] <- NA
#     }
#   }
# }
# 
# lat5_na <- lat5 # matrix(NA, 214, 250)
# lon5_na <- lon5 # matrix(NA, 214, 250)
# for (jy in 1:nrow(lat5))
# {
#   for (ix in 1:ncol(lat5))
#   {
#     #  if there are no (0) not-missing values (i.e. all values are missing)
#     if (sum(!is.na(swe5[jy,ix,])) == 0)
#     {
#       lat5_na[jy,ix] <- NA
#       lon5_na[jy,ix] <- NA
#     }
#   }
# }
# 
# range(lat5_na, na.rm=T)
# range(lon5_na, na.rm=T)
# range(lat6_na, na.rm=T)
# range(lon6_na, na.rm=T)
# 
# round(range(lat5_na, na.rm=T))
# round(range(lon5_na, na.rm=T))
# round(range(lat6_na, na.rm=T))
# round(range(lon6_na, na.rm=T))
# 
# lat5[2:216,2:250] <- NA
# lon5[2:216,2:250] <- NA
# lat6[2:213,2:249] <- NA
# lon6[2:213,2:249] <- NA
# plot(lat5 ~ lon5, type='p', pch='.:',ylim=range(lat5,lat6,na.rm=T))
# points(lat6 ~ lon6, type='p', pch='.:', col='red')
# points(lat6_na ~ lon6_na, type='p', pch='.:', col='blue')
# points(lat5_na ~ lon5_na, type='p', pch='.:', col='green')
# 
# filled.contour(swe5[,,1])
