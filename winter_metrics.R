# Create winter metrics with DayMet SWE data
# Alex Bryan/Marketa Zimova 
# 5/2017
# just ideas/archive for now


#load data
setwd('///Volumes/Seagate/Daymet') 
data <-read.csv("/Users/marketzimova/Documents/WORK/DISSERTATION/3 Camera Traps Study/camera locations/NH/data.csv", header=T,sep=",")

#extract winter metrics
# 1. total winter SWE 
totswe <- function(x, na.rm=T) {sum(x>=0, na.rm=TRUE)} # cumulative sum of SWE
# 2. number of days with snow cover (SWE>0)
totswe <- function(x, na.rm=T) {sum(x>0, na.rm=TRUE)} ; totswe(swe)
# 3. number snow melts

#some other functions
#cwdp <- function(x, na.rm=T) {max(rle(na.omit(x)>0)$lengths[rle(na.omit(x)>0)$values==T], na.rm=T)} # cons wet days (intensity)
#gsl  <- function(x, na.rm=T) {max(rle(na.omit(x)>32)$lengths[rle(na.omit(x)>32)$values==T], na.rm=T)} # growing season length
#cwdf <- function(x, na.rm=T) {sum(rle(na.omit(x)>0)$values==T & rle(na.omit(x)>0)$lengths>=3, na.rm=T)} # cons wet days (freq of 3+-day streaks)
#c85p <- function(x, na.rm=T) {max(rle(na.omit(x)>=85)$lengths[rle(na.omit(x)>=85)$values==T], na.rm=T)} # heat waves (intensity)
#c85f <- function(x, na.rm=T) {sum(rle(na.omit(x)>=85)$values==T & rle(na.omit(x)>=85)$lengths>=3, na.rm=T)} # heat waves (freq of 5-day streaks)

# get july - june
# loop over days, counting each day where swe > 0
# first define your function - this gives your longest streak
snow_cover_streak <- function(x, na.rm=T) 
{max(rle(na.omit(x)==0)$lengths[rle(na.omit(x)==0)$values==T], na.rm=T)} # cons snowless days
snow_cover_streak(swe)
#
streak <- numeric(2016-1980+1)
for (iyear in 1980:2016)
{
  # streak for first year
  streak[iyear-1979] <- snow_cover_streak(swe[2016])
}

# find the length of the 95% window (or set a specific date range?)
pdf <- density(swe,na.rm=T); plot(pdf)



#iwinter <- which(time > as.Date(paste0(year,"-07-01")) & time < as.Date(paste0(year+1,"-06-30")))
# aggregate by year
# tmp1 <- aggregate(swe ~ year, data = df, funs[ifun], na.rm=T, na.action=NULL)
# ina  <- aggregate(eval(parse(text=vars[ivar])) ~ year, data = df, function(x) {sum(is.na(x))}, na.action=NULL)
# tmp1$ina <- ina[,2]
# imiss <- count(df, 'year')
# yall <- merge(tmp1, imiss, by="year")
# yall[,2][yall$ina >= 35 | yall$freq <= 330] <- NA
# dfann <- yall[,c(1,2)]
# names(dfann)[length(dfann)] <- "obs"



#####
#extract SWE data from Daymet
#for (year in 1980:2015)
#for (year in 1980:1980)
#year=1980
#{
#ncin <- open.ncdf(paste0("C:/cygwin/home/abryan/data/obs/daymet/",tile,"_",year,"_",year+1,"_swe.nc"))
#  "swe.nc" #1980-2016
#ncin <- nc_open(paste0("//Volumes/Seagate/Daymet",tile,"_",year,"_",year+1,"_swe.nc"))

