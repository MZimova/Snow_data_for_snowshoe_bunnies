# Extract  date and snow cover from Rutgers weekly data
# Alex Bryan/Marketa Zimova 
# NetCDF file with weekly snow cover extent
# 1 degree resolution, 1966-2017
# 8/2017

# Packages
library(ncdf4)

# Load camera data
#setwd('//Users/marketzimova/Documents/WORK/DISSERTATION/3 Camera Traps Study/analysis SNOW/Rutgers/weekly')
setwd('C:/Users/ABRYAN/Documents/proj-active/snowshoe_hare')
# delete camsNH <-read.csv("/Users/marketzimova/Documents/WORK/DISSERTATION/3 Camera Traps Study/camera locations/NH/hare_locs_NH_VT_Rileybatch.csv", header=T,sep=",")
#camsNH <-read.csv("data/hare_locs_NH_VT_Rileybatch.csv", header=T,sep=",")
cameras <-read.csv("data/NH_hare_data2.csv", header=T,sep=",")
camera_names <-names(table(cameras$Camera)) #all cameras
# camera_names=c("CLNA1","CLNA10","CLNA2","CLNA4","CLNA6","CLNA7","CLNA8","CLTC4","Jeff1","Jeff2","Jeff3","Jeff4","Jeff5","Kil10","Kil2","Kil3","Kil5","Kil6","Kil7","Kil8","Kil9","Kins2","Kins3","Kins4","Nul10","Nul11","Nul13","Nul14","Nul15","Nul16","Nul17","Nul18","Nul19","Nul20","Nul9","VB1","VB2","VB3","VB4","VB5","VB6","VB7","VB8") #few good ones
cameras$date<-as.Date(cameras$Date, format = "%m/%d/%y")

# Load Rutgers data
ncin <- nc_open("data/nhsce_v01r01_19661004_20170605.nc")
ncin

# extract variables
days <- ncvar_get(ncin, "time") #time <- get.var.ncdf(ncin, "time") # days since 1966-10-03
tunits <- ncatt_get(ncin,"time","units")
dates <- as.Date(unlist(strsplit(tunits$value, " "))[3]) + days

lat <- ncvar_get(ncin, "latitude") #lat <- get.var.ncdf(ncin, "lat")
lon <- ncvar_get(ncin, "longitude") #lon <- get.var.ncdf(ncin, "lon")
snow <- ncvar_get(ncin, "snow_cover_extent") 


# loop through cameras
#icam=which(cameras$Camera == 'Kins2') # one camera at a time
#snow_df <- NULL
snow_year_df <- NULL
for (icam in 1:length(camera_names))
{
  # find Rutgers cell nearest to each camera location
  # cameras with a particular name don't move, right?  There are multiple
  # lines with the same camera name -- choosing the first because can only
  # have one to pick nearest cell and run statistics
  camera <- which(cameras$Camera == camera_names[icam])[1]
  camera_obs <- cameras[cameras$Camera == camera_names[icam], ]
  dlat  <- abs(lat-cameras[camera,]$Lat)
  dlon  <- abs(lon-cameras[camera,]$Lon)
  dlatlon  <- dlat + dlon
  ilatlon  <- which(dlatlon == min(dlatlon))  # 1-dimensional lat/lon value
  ilat  <- ceiling(ilatlon/nrow(lat))         # lat value in 2 dimensions
  ilon  <- ilatlon - ((ilat - 1) * nrow(lat)) # lon value in 2 dimensions
  #snow[ilon,ilat,]                      # snow wat eqv in 3 dimensions
  print(paste(camera_names[icam],ilon,ilat))
  
  #color = 'lightblue4'
  #if (camera_obs$Elevation > 300) {color = 'lightblue4'};if (camera_obs$Elevation > 500) {color = 'lightblue3'}, if (camera_obs$Elevation > 700) {color = 'lightblue2'};if (camera_obs$Elevation > 900) {color = 'lightblue1'}
  
  #year=2016 # one year at a time
  # note you can go as far back as 1967 if you want
  # 1981 corresponds to the 1980-1981 winter
  for (year in 1981:2016) # all years
  {
    
    iyear   <- dates >= as.Date(paste0(year-1,'-07-01')) & dates <= as.Date(paste0(year  ,'-06-30'))
    ifall   <- dates >= as.Date(paste0(year-1,'-07-01')) & dates <= as.Date(paste0(year-1,'-12-31'))
    ispring <- dates >= as.Date(paste0(year  ,'-01-01')) & dates <= as.Date(paste0(year  ,'-06-30'))
    
    # Snow metrics for each camera for EACH YEAR (1980-2016) and EACH SEASON (fall: Jul through Dec, spring: Jan through Jun)
    
    # 1. Number of snow days in the fall and in the spring
    # = Total # days when snow cover > 0
    # You previously had:               
    snowdays_year   <- snow[ilon, ilat, iyear  ] > 0
    snowdays_fall   <- snow[ilon, ilat, ifall  ] > 0
    snowdays_spring <- snow[ilon, ilat, ispring] > 0
    nsnowdays_year   <- sum(snowdays_year  ) * 7  # weeks --> days conversion
    nsnowdays_fall   <- sum(snowdays_fall  ) * 7  # weeks --> days conversion
    nsnowdays_spring <- sum(snowdays_spring) * 7  # weeks --> days conversion
    
    # 2. First snow-on and snow -off date each year
    # = First date in the fall when snow>0 and stays >0 for at least 7 days 
    # = First date in the spring when snow<1 and stays <1 for at least 7 days 
    # You previously had:  
    dates_year <- dates[iyear]
    first_snowday <- as.Date(paste0('1980-',format(dates_year[which(snowdays_year)[1]], '%m-%d')))
    last_snowday  <- as.Date(paste0('1980-',format(dates_year[which(snowdays_year)[length(which(snowdays_year))]], '%m-%d')))
    
    
    # 3. Number of snow melts for each season
    # = Number of times snow flips between >0 and < 0 (i assume you mean "= 0") ;-)
    snow_melts <- length(rle(snowdays_year>0)$lengths)
    
    # A few other metrics:
    # longest stretch with snow on the ground
    longest_streak <- max(rle(snowdays_year>0)$lengths[rle(snowdays_year>0)$values==T])
    
    snow_year_df <- rbind(snow_year_df,
                          data.frame(
                            Camera         = camera_names[icam], 
                            Lon            = ilon,
                            Lat            = ilat,
                            Year           = year, 
                            N_snow_ann     = nsnowdays_year,
                            N_snow_fall    = nsnowdays_fall,
                            N_snow_spring  = nsnowdays_spring,
                            First_snow     = first_snowday,
                            Last_snow      = last_snowday,
                            Melts          = snow_melts,
                            Longest_streak = longest_streak
                          ))
    
  } # end year loop

  # I don't think you need this -- you just need the calculated metrics, right?
  # snow_df <- rbind(snow_df,
  #                  data.frame(
  #                    Camera = rep(camera_names[icam],length(date)), 
  #                    Lon = rep(ilon,length(date)),
  #                    Lat = rep(ilat,length(date)),
  #                    Date = date, 
  #                    Snow = snow[ilon,ilat,]
  #                  ))
  
} # end camera loop

camera='CLNA1'
camera_metrics <- snow_year_df[snow_year_df$Camera == camera, ]

plot(N_snow_ann ~ Year, data = camera_metrics)

# add trendline (if significant)
regression <- lm(N_snow_ann ~ Year, data = camera_metrics)
f <- summary(regression)$fstatistic  # F-statistic
p <- pf(f[1],f[2],f[3],lower.tail=F) # p-value (< 0.05 = 95% CI is significant)
if (p < 0.05) {
  lines(camera_metrics$Year, regression$coefficients[2] * camera_metrics$Year + regression$coefficients[1]) # y = mx + b
  mtext(sprintf("%+.2f days/decade (p = %.2f)", regression$coefficients[2]*10, p), side = 1, line = -1.5, adj = 0.05, cex = 0.7)
} else {
  mtext(sprintf("p = %+.2f", p), side = 1, line = -1.5, adj = 0.05)
}

plot(N_snow_fall    ~ Year, data = camera_metrics)
plot(N_snow_spring  ~ Year, data = camera_metrics)
plot(First_snow     ~ Year, data = camera_metrics)
plot(Last_snow      ~ Year, data = camera_metrics)
plot(Melts          ~ Year, data = camera_metrics)
plot(Longest_streak ~ Year, data = camera_metrics)


# CAN YOU PLS FIX CODE BELOW SO IT PRINTS OUT DF W/ ALL CAMS 1980-2016?
#   I think you had done this right -- I just switched out with new data frame
# Create df with all cameras and snow
                print(paste(camera_names[icam],ilon,ilat))
#data <- write.csv(snow_df, file = "Rutgers_NH_1980_2016.csv")
data <- write.csv(snow_year_df, file = "Rutgers_NH_1980_2016.csv", row.names = F)

