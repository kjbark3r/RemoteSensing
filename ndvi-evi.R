#####################################################
###### DATA PREP - REMOTELY SENSED VEG INDICES ###### 
############# NSERP KJB  Jul-Aug 2016  ##############
#####################################################

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

# remotely sensed data -  raw file from Google Engine
remote <- read.csv(file = "NSERP_AllPlots_NDVI-EVI.csv", as.is = TRUE) %>%
  filter(Type == "Biomass" | Type == "Phenology") # veg plots only
remote$Date <- as.Date(as.POSIXct((remote$Date+21600000)/1000, origin="1970-01-01", tz = "UTC"))
remote$PlotVisit <- paste(remote$PlotID, ".", remote$Date, sep = "")

# ndvi, long form
ndvi <- remote %>%
  select(PlotVisit, starts_with("ndvi."), starts_with("doy.")) %>%
  gather(RemoteDate, NDVI, -PlotVisit)
ndvi$VisitDate <- as.POSIXlt(sub("(.*)[.](.*)", "\\2", ndvi$PlotVisit))
ndvi$VisitDOY <- ndvi$VisitDate$yday  
ndvi$VisitDate <- as.POSIXct(ndvi$VisitDate)
ndvi$RemoteDate <- as.POSIXlt(sub("(.*)[.](.*)", "\\2", ndvi$RemoteDate), format = "%Y%m%d")
ndvi$RemoteDOY <- ndvi$RemoteDate$yday
ndvi$RemoteDate <- as.POSIXct(ndvi$RemoteDate)
ndvi14 <- ndvi %>%
  filter(VisitDate < "2015-01-01", RemoteDate < "2015-01-01") %>%
  mutate(doydiff = abs(RemoteDOY-VisitDOY)) %>%
  group_by(PlotVisit) %>%
  arrange(doydiff) %>%
  slice(which.min(doydiff)) %>% # pick ndvi value closest to doy of plot visit
  ungroup() 
ndvi15 <- ndvi %>%    
  filter(VisitDate >= "2015-01-01", RemoteDate >= "2015-01-01") %>%
  mutate(doydiff = abs(RemoteDOY-VisitDOY)) %>%
  group_by(PlotVisit) %>%
  arrange(doydiff) %>%
  slice(which.min(doydiff)) %>% # pick ndvi value closest to doy of plot visit
  ungroup() 
ndvi <- bind_rows(ndvi14, ndvi15)
rm(ndvi14, ndvi15)

# evi, long form
evi <- remote %>%
  select(PlotVisit, starts_with("evi."), starts_with("doy.")) %>%
  gather(RemoteDate, EVI, -PlotVisit)
evi$VisitDate <- as.POSIXlt(sub("(.*)[.](.*)", "\\2", evi$PlotVisit))
evi$VisitDOY <- evi$VisitDate$yday  
evi$VisitDate <- as.POSIXct(evi$VisitDate)
evi$RemoteDate <- as.POSIXlt(sub("(.*)[.](.*)", "\\2", evi$RemoteDate), format = "%Y%m%d")
evi$RemoteDOY <- evi$RemoteDate$yday
evi$RemoteDate <- as.POSIXct(evi$RemoteDate)
evi14 <- evi %>%
  filter(VisitDate < "2015-01-01", RemoteDate < "2015-01-01") %>%
  mutate(doydiff = abs(RemoteDOY-VisitDOY)) %>%
  group_by(PlotVisit) %>%
  arrange(doydiff) %>%
  slice(which.min(doydiff)) %>% # pick evi value closest to doy of plot visit
  ungroup() 
evi15 <- evi %>%    
  filter(VisitDate >= "2015-01-01", RemoteDate >= "2015-01-01") %>%
  mutate(doydiff = abs(RemoteDOY-VisitDOY)) %>%
  group_by(PlotVisit) %>%
  arrange(doydiff) %>%
  slice(which.min(doydiff)) %>% # pick evi value closest to doy of plot visit
  ungroup() 
evi <- bind_rows(evi14, evi15)
rm(evi14, evi15)

# plot info (to add lat/longs)
latlong <- read.csv(file = "NSERP_VegPlots.csv")
latlong$Date <- as.Date(latlong$Date, format = "%m/%d/%Y")
latlong <- latlong %>%
  mutate(PlotVisit = paste(PlotID, ".", Date, sep="")) %>%
  select(PlotVisit, Latitude, Longitude)

## FINAL DATAFRAME

# ndvi & evi per plot visit
rmt <- ndvi %>%
  select(-RemoteDate) %>%
  full_join(evi, by = "PlotVisit") %>%
  full_join(latlong, by = "PlotVisit") %>% 
  select(PlotVisit, RemoteDate, NDVI, EVI, Latitude, Longitude)  
write.csv(rmt, file="remote-plot.csv", row.names=F)
