# Plots to compare Daymet SWE and data from Camera traps (photos csv) and from snow depth Data from Alexej (depth csv)
# 5/2017
# Marketa Zimova

########### Packages
library(ggplot2)
library(lubridate)
library(plyr)
library(gridExtra)
library(beepr)

########### Load data
setwd('//Users/marketzimova/Documents/WORK/DISSERTATION/3 Camera Traps Study/analysis SNOW')

########### Daymet snow data
# NH: actually not for all cameras right now-- need to fix Daymet extract code!
  #daymet <-read.csv("Daymet_allNHcameras_2012_2016.csv", header=T,sep=",")
#CO: 
  daymet <-read.csv("Daymet/data/Daymet_allCO_2010_2016.csv", header=T,sep=",")

########### Observations
# CO % snow cover from random daily pics
  photos <-read.csv("camera trap snow data/daily_photos_CO.csv", header=T,sep=",")
# NH snow depth data from Alexej
  #depth <-read.csv("Snowpack Data 2014-2016 Alexej_Riley hare cams only.csv", header=T,sep=",")
# NH % snow cover from hare pics only
  #photos <-read.csv("/Users/marketzimova/Documents/WORK/DISSERTATION/3 Camera Traps Study/data/NH_hare_data2.csv", header=T,sep=",")

########### Prepare dataset
# fix dates
#depth$Date<-mdy(depth$Date);depth$Year<-year(depth$Date);depth$fYear<-factor(depth$Year);depth$Julian<-yday(depth$Date)
daymet$Date<-mdy(daymet$Date); photos$Date<-mdy(photos$Date)
# merge datasets
merged <- merge(x=daymet, y=photos[,c("Date","Snow","Any_snow","Camera")], by = c("Camera","Date"), all.x = TRUE, sort = FALSE)
# add more variables
merged$Year<-year(merged$Date); merged$fYear<-factor(merged$Year); merged$Julian<-yday(merged$Date)
merged$any<-NA; merged$any[merged$Any_snow==1]<-5 #snow presence
merged$none<-NA; merged$none[merged$Any_snow==0]<--5 #snow absence
str(merged)
names(table(merged$Camera))



########### Plot Daymet vs. % snow cover for each camera and save to pdf 
  myplots <- lapply(unique(merged$Camera), 
                    function(id) 
                      ggplot(subset(merged, Camera == id)) +
                      geom_line(aes(Date, SWE)) +
                      geom_point(aes(Date, Snow), color="blue") +
                      geom_point(aes(Date, any), color="red",shape=20) +
                      geom_point(aes(Date, none), color="red",shape=20) +
                      ylim(-5,400) + 
                      xlim(range(merged$Date)) +
                      facet_wrap(~Camera))
  #do.call(gridExtra::grid.arrange, myplots[1:12])
  #library(grid)
  #grid.arrange(rectGrob(), rectGrob())
  ml <- marrangeGrob(myplots, nrow=1, ncol=1)
  ## non-interactive use, multipage pdf
  ggsave("multipage.pdf", ml)
  beep()
  
  

# Test, not work right
  # library(ggplus) #devtools::install_github("guiastrennec/ggplus")
  # myplot <- ggplot(merged) + 
  #   geom_line(data=merged, aes(Date, SWE)) +
  #   geom_point(data=merged, aes(Date, Snow), color="red")
  # pdf('multiple_page_plot.pdf')
  # facet_multiple(plot = myplot, facets = 'Camera', ncol = 5, nrow = 5)
  # dev.off() 
  # beep()
  
  # ggplot(subset(merged, Camera == "Ivan0032010")) +
  #   geom_line(aes(Date, SWE)) +
  #   geom_point(aes(Date, Snow), color="blue") +
  #   geom_point(aes(Date, any), color="red",shape=20) +
  #   geom_point(aes(Date, none), color="red",shape=20) +
  #   ylim(-5,400)  
  #   xlim(range(merged$Date)) 
  
# Archived:
  #merged <- merge(photos, daymet, by = c("Camera","Date"), all = TRUE, sort = FALSE)
  #binded <-rbind.fill(depth,daymet) # rbind by rows despite uneven # of columns
  #write.csv(binded, file = "binded.csv") #
  #best <- subset(binded, Camera==c("Kil 8", "Jeff 1", "Jeff 2", "Kins 2", "Kil 6"))         
  
  ##### Plots to compare Daymet vs. observed date
# daymet vs. DEPTH for each site
a<-ggplot() +
  geom_point(data=depth[depth$Camera %in% c("Kil8"),], aes(Date, Depth)) +
  geom_point(data=daymet[daymet$Camera %in% c("Kil8"),], aes(Date, SWE), color="blue") +
  xlab ("Date") + ylab ("Depth or SWE") + ggtitle("Kill8, 335 m")

b<-ggplot() +
  geom_point(data=depth[depth$Camera %in% c("Kil6"),], aes(Date, Depth)) +
  geom_point(data=daymet[daymet$Camera %in% c("Kil6"),], aes(Date, SWE), color="blue") +
  geom_point(data=photos[photos$Camera %in% c("Kil6"),], aes(Date, Snow), color="red") +
  xlab ("Date") + ylab ("Depth or SWE") + ggtitle("Kil6, 429 m")

c<-ggplot() +
  geom_point(data=depth[depth$Camera %in% c("SAND1"),], aes(Date, Depth)) +
  geom_point(data=daymet[daymet$Camera %in% c("Sand1"),], aes(Date, SWE), color="blue") +
  xlab ("Date") + ylab ("Depth or SWE") + ggtitle("Sand1, 896 m")

d<-ggplot() +
  geom_point(data=depth[depth$Camera %in% c("SAND2"),], aes(Date, Depth)) +
  geom_point(data=daymet[daymet$Camera %in% c("Sand2"),], aes(Date, SWE), color="blue") +
  xlab ("Date") + ylab ("Depth or SWE") + ggtitle("Sand 2, 1021 m")

e<-ggplot() +
  geom_point(data=depth[depth$Camera %in% c("ZEAL3"),], aes(Date, Depth)) +
  geom_point(data=daymet[daymet$Camera %in% c("Zeal3"),], aes(Date, SWE), color="blue") +
  xlab ("Date") + ylab ("Depth or SWE")  + ggtitle("Zeal3, 1246 m")

f<-ggplot() +
  geom_point(data=depth[depth$Camera %in% c("Jeff2"),], aes(Date, Depth)) +
  geom_point(data=daymet[daymet$Camera %in% c("Jeff2"),], aes(Date, SWE), color="blue") +
  xlab ("Date") + ylab ("Depth or SWE")  + ggtitle("Jeff2, 1027 m")

g<-ggplot() +
  geom_point(data=depth[depth$Camera %in% c("Jeff1"),], aes(Date, Depth)) +
  geom_point(data=daymet[daymet$Camera %in% c("Jeff1"),], aes(Date, SWE), color="blue") +
  xlab ("Date") + ylab ("Depth or SWE") + ggtitle("Jeff1, 1058 m")

grid.arrange(a,b,c,d,e,f,g, ncol=2)

################################################################################################

unique(depth$Camera)
# snow depth (circle) and cover (triangle) at all cameras (facet by camera, color by year)
ggplot() +
  facet_wrap(~ Camera) +
  geom_point(data=depth, aes(Julian, Depth,color=depth$fYear)) +
  geom_point(data=photos, aes(Julian, Snow,color=photos$fYear), shape=2)
  #xlim(0, 150)

