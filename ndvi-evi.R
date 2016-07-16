########################################################
###### COMPARING GROUND-BASED VEGETATION MEASURES ###### 
######### TO REMOTELY SENSED VEGETATION INDICES ######## 
############ NSERP KJB  July 2016  #####################
########################################################

## WD
wd_workcomp <- "C:\\Users\\kristin.barker\\Documents\\GitHub\\RemoteSensing"
wd_laptop <- "C:\\Users\\kjbark3r\\Documents\\GitHub\\RemoteSensing"
	if (file.exists(wd_workcomp)) {
	  setwd(wd_workcomp)
	} else {
	  if(file.exists(wd_laptop)) {
		setwd(wd_laptop)
	  } else {
		  cat("Are you SURE you got that file path right?\n")
		  }
	  }
rm(wd_workcomp, wd_laptop)

## PACKAGES
library(dplyr)
library(tidyr) #gather

## DATA

# remote sensing
remote <- read.csv(file = "NSERP_AllPlots_NDVI-EVI.csv", as.is = TRUE) 
remote$Date <- as.Date(as.POSIXct((remote$Date+21600000)/1000, origin="1970-01-01", tz = "UTC"))
remote$PlotVisit <- paste(remote$PlotID, ".", remote$Date, sep = "")

# herbaceous biomass
biomass <- read.csv(file = "biomass-phenology.csv", as.is = TRUE)
  biomass$VisitDate <- as.POSIXlt(biomass$Date)
  biomass$VisitDOY <- biomass$VisitDate$yday  
  biomass <- select(biomass, -c(PlotID, Date))

# ndvi, long form
ndvi <- remote %>%
  filter(Type == "Phenology") %>%
  select(PlotVisit, starts_with("ndvi."), starts_with("doy.")) %>%
  gather(RemoteDate, NDVI, -PlotVisit)
ndvi$Date <- as.POSIXlt(substr(ndvi$PlotVisit, 5, 14))
ndvi$VisitDOY <- ndvi$Date$yday  



# evi, long form
  
# all together now (phenology plots only)
phen <- filter(remote, Type == "Phenology") 
phen <- merge(phen, biomass)



## STILL TO DO
#add latitude to remote sensing data
#make data long rather than wide
#relate plotvisit date to remote sensing date
