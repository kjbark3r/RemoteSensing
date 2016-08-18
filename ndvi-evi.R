########################################################
###### COMPARING GROUND-BASED VEGETATION MEASURES ###### 
######### TO REMOTELY SENSED VEGETATION INDICES ######## 
############# NSERP KJB  Jul-Aug 2016  #################
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
remote <- read.csv(file = "NSERP_AllPlots_NDVI-EVI.csv", as.is = TRUE) %>%
  filter(Type == "Biomass" | Type == "Phenology")
remote$Date <- as.Date(as.POSIXct((remote$Date+21600000)/1000, origin="1970-01-01", tz = "UTC"))
remote$PlotVisit <- paste(remote$PlotID, ".", remote$Date, sep = "")

# biomass estimations from all vegetation plots
veg <- read.csv(file = "biomass.csv", as.is = TRUE)
  veg$VisitDate <- as.POSIXlt(veg$Date)
  veg$VisitDOY <- veg$VisitDate$yday 
  veg$VisitDate <- as.POSIXct(veg$VisitDate)
  veg <- select(veg, -c(PlotID, Date))

# ndvi, long form
ndvi <- remote %>%
  select(PlotVisit, starts_with("ndvi."), starts_with("doy.")) %>%
  gather(RemoteDate, NDVI, -PlotVisit)
ndvi$VisitDate <- as.POSIXlt(sub("(.*)[.](.*)", "\\2", ndvi$PlotVisit))
ndvi$VisitDOY <- ndvi$VisitDate$yday  
ndvi$VisitDate <- as.POSIXct(ndvi$VisitDate)
ndvi$RemoteDate <- as.POSIXlt(substr(ndvi$RemoteDate, 6, 13), format = "%Y%m%d")
ndvi$RemoteDOY <- ndvi$RemoteDate$yday
ndvi$RemoteDate <- as.POSIXct(ndvi$RemoteDate)  
ndvi <- ndvi %>%
  mutate(doydiff = abs(RemoteDOY-VisitDOY)) %>%
  group_by(PlotVisit) %>%
  arrange(doydiff) %>%
  slice(which.min(doydiff)) %>%
  ungroup()

# evi, long form
evi <- remote %>%
  select(PlotVisit, starts_with("evi."), starts_with("doy.")) %>%
  gather(RemoteDate, EVI, -PlotVisit)
evi$VisitDate <- as.POSIXlt(sub("(.*)[.](.*)", "\\2", evi$PlotVisit))
evi$VisitDOY <- evi$VisitDate$yday  
evi$VisitDate <- as.POSIXct(evi$VisitDate)
evi$RemoteDate <- as.POSIXlt(substr(evi$RemoteDate, 5, 12), format = "%Y%m%d")
evi$RemoteDOY <- evi$RemoteDate$yday
evi$RemoteDate <- as.POSIXct(evi$RemoteDate)  
evi <- evi %>%
  mutate(doydiff = abs(RemoteDOY-VisitDOY)) %>%
  group_by(PlotVisit) %>%
  arrange(doydiff) %>%
  slice(which.min(doydiff))  %>%
  ungroup() %>%
  subset(select = c(PlotVisit, EVI))

latlong <- read.csv(file = "NSERP_VegPlots.csv")
latlong$Date <- as.Date(latlong$Date, format = "%m/%d/%Y")
latlong <- latlong %>%
  mutate(PlotVisit = paste(PlotID, ".", Date, sep="")) %>%
  select(PlotVisit, Latitude, Longitude)
  
remote.veg <- full_join(ndvi, evi, by = "PlotVisit") %>%
  select(-c(VisitDate, VisitDOY)) %>%
  full_join(veg, by = "PlotVisit") %>%
  full_join(latlong, by = "PlotVisit")

# remove "outliers" (a.k.a. huge sagebrush)
no.out <- remote.veg[!remote.veg$PlotVisit == "401.2014-07-18",]
no.out <- no.out[!no.out$PlotVisit == "376.2014-07-17",]

# EXPORT
write.csv(remote.veg, file="remote-plot.csv", row.names=F)
write.csv(no.out, file = "remote-plot-noout.csv", row.names=F)
