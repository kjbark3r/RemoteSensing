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

## DATA
  # remote sensing
remote <- read.csv(file = "NSERP_AllPlots_NDVI-EVI.csv", as.is = TRUE) 
remote$Date <- as.Date(as.POSIXct((remote$Date+0.1)/1000, origin="1970-01-01"))
remote$PlotVisit <- paste(remote$PlotID, ".", remote$Date, sep = "")
  # herbaceous biomass
biomass <- read.csv(file = "biomass-phenology.csv", as.is = TRUE)
  biomass$VisitDate <- as.POSIXlt(biomass$Date)
  biomass$VisitDOY <- biomass$VisitDate$yday  
  biomass <- select(biomass, -c(PlotID, Date))

  # both (phenology plots only)
veg <- remote %>%
    filter(Type == "Phenology") %>%
###CODE FAILS HERE. I suspect it has something to do with the date conversion
  #from Brady's file, because the issue seems to be that many plot visits don't
  #match up (62 do; 47 don't)
      # same <- as.data.frame(intersect(veg$PlotVisit, biomass$PlotVisit)
      # diff <- as.data.frame(setdiff(veg$PlotVisit, biomass$PlotVisit))
    full_join(biomass, by = "PlotVisit")

## STILL TO DO
#add latitude to remote sensing data
#remove duplicate columns from joined data
#make data long rather than wide
#relate plotvisit date to remote sensing date
