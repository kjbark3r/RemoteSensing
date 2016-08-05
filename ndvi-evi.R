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

## RELATIONSHIPS

## visualizations

plot(Biomass ~ NDVI, data = remote.veg)
# yeesh. remove sagebrush just for now.
par(mfrow=c(2,2))
plot(Biomass ~ NDVI, data = no.out)
plot(ForageBiomass ~ NDVI, data = remote.veg)
plot(Biomass ~ EVI, data = no.out)
plot(ForageBiomass ~ EVI, data = remote.veg)

par(mfrow=c(2,2))
hist(remote.veg$NDVI)
hist(remote.veg$EVI)
hist(no.out$Biomass)
hist(remote.veg$ForageBiomass)

#transforming...
hist(remote.veg$NDVI)
hist(sqrt(remote.veg$NDVI))
  #meh, regular data's probably ok here
hist(log(remote.veg$EVI))
hist(sqrt(remote.veg$EVI))
hist(1/sqrt(remote.veg$EVI))
hist(-1/sqrt(remote.veg$EVI))
  #ok apparently i should use the negative version
  #bc it preserves the direction of relationships

hist(no.out$Biomass)
hist(log(no.out$Biomass))
hist(remote.veg$ForageBiomass)
hist(log(remote.veg$ForageBiomass))    
  #ahh, beautiful

# FINAL TRANSFORMATIONS #
par(mfrow=c(2,2))
hist(remote.veg$NDVI)
hist(-1/sqrt(remote.veg$EVI))
hist(log(no.out$Biomass))
hist(log(remote.veg$ForageBiomass)) 

# plots with transformed data
par(mfrow=c(2,2))
plot(log(Biomass) ~ NDVI, data = no.out)
plot(log(ForageBiomass) ~ NDVI, data = remote.veg)
plot(Biomass ~ -1/sqrt(EVI), data = no.out)
plot(log(ForageBiomass) ~ -1/sqrt(EVI), data = remote.veg)


## regressions

##NDVI
all.bio <- lm(log(Biomass) ~ NDVI, data=no.out); summary(all.bio)
for.bio <- lm(log(ForageBiomass+.05) ~ NDVI, data=remote.veg); summary(for.bio)
# wow, forage biomass is way worse than all biomass


  
lm1 <- c(herb$coefficients[1], summary(herb)$adj.r.squared, summary(herb)$sigma)
lm2 <- c(herb.for$coefficients[1], summary(herb.for)$adj.r.squared, summary(herb.for)$sigma)
lm3 <- c(forb$coefficients[1], summary(forb)$adj.r.squareforb, summary(forb)$sigma)
lm4 <- c(forb.for$coefficients[1], summary(forb.for)$adj.r.squared, summary(forb.for)$sigma)
lm5 <- c(grass$coefficients[1], summary(grass)$adj.r.squared, summary(grass)$sigma)
lm6 <- c(grass.for$coefficients[1], summary(grass.for)$adj.r.squared, summary(grass.for)$sigma)

tprep <- rbind(lm1, lm2, lm3, lm4, lm5, lm6)
tab <- as.data.frame(tprep, row.names = c("All Herb", "Forage Herb", "All Forb", "Forage Forb", 
                         "All Grass", "Forage Grass"))
#tab <- rename(tab, NDVIcoeff = NDVI)
tab <- rename(tab, AdjRsquared = V2)
tab <- rename(tab, StdError = V3)
View(tab)

##EVI
herb <- lm(sqrt(HerbBiomass) ~ EVI, data=remote.phen); summary(herb)
  herb.for <- lm(sqrt(ForageHerbBiomass) ~ EVI, data=remote.phen); summary(herb.for)
forb <- lm(sqrt(ForbBiomass) ~ EVI, data=remote.phen); summary(forb)
  forb.for <- lm(sqrt(ForageForbBiomass) ~ EVI, data=remote.phen); summary(forb.for)
grass <- lm(sqrt(GrassBiomass) ~ EVI, data=remote.phen); summary(grass)
  grass.for <- lm(sqrt(ForageGrassBiomass) ~ EVI, data=remote.phen); summary(grass.for)
  
lm1 <- c(herb$coefficients[1], summary(herb)$adj.r.squared, summary(herb)$sigma)
lm2 <- c(herb.for$coefficients[1], summary(herb.for)$adj.r.squared, summary(herb.for)$sigma)
lm3 <- c(forb$coefficients[1], summary(forb)$adj.r.squareforb, summary(forb)$sigma)
lm4 <- c(forb.for$coefficients[1], summary(forb.for)$adj.r.squared, summary(forb.for)$sigma)
lm5 <- c(grass$coefficients[1], summary(grass)$adj.r.squared, summary(grass)$sigma)
lm6 <- c(grass.for$coefficients[1], summary(grass.for)$adj.r.squared, summary(grass.for)$sigma)

tprep <- rbind(lm1, lm2, lm3, lm4, lm5, lm6)
tab2 <- as.data.frame(tprep, row.names = c("All Herb", "Forage Herb", "All Forb", "Forage Forb", 
                         "All Grass", "Forage Grass"))
#tab <- rename(tab, NDVIcoeff = NDVI)
tab2 <- rename(tab2, AdjRsquared = V2)
tab2 <- rename(tab2, StdError = V3)
View(tab2)

############
## ADDING ELEV AND LANDCOVER TO MODELS

#did landcov in arcmap; couldn't get it to work in r 
landcov <- read.csv(file = "plots-landcov.txt") 
landcov$Date <- as.Date(landcov$Date, format = "%m/%d/%Y")
landcov <- landcov %>%
  subset(Type == "Phenology") %>%
  mutate(PlotVisit = paste(PlotID, ".", Date, sep = "")) %>%
  subset(PlotVisit != "326.2015-09-23") %>%
  rename(CovClass = RASTERVALU) %>%
  dplyr::select(PlotVisit, CovClass) %>%
  mutate(LandCov = ifelse(CovClass == 0, "Mask", 
                                     ifelse(CovClass == 1, "Badlands",
                                     ifelse(CovClass == 2, "Riparian",
                                     ifelse(CovClass == 3, "Forest",
                                     ifelse(CovClass == 4, "Shrub",
                                     ifelse(CovClass == 5, "Sagebrush",
                                     ifelse(CovClass == 6, "Grassland",
                                     ifelse(CovClass == 7, "Agricultural",
                                            NA)))))))))

all.phen <- remote.phen %>%
  subset(PlotVisit != "326.2015-09-23") %>% #wrong lat/long on this plot
  full_join(landcov, by = "PlotVisit")

library(raster)
  
elevSE <- raster("C:/Users/kristin.barker/Documents/ArcGIS/Backgrounds/NED/NEDn46w114/grdn46w114_13")
elevSW <- raster("C:/Users/kristin.barker/Documents/ArcGIS/Backgrounds/NED/NEDn46w115/grdn46w115_13")
elevNE <- raster("C:/Users/kristin.barker/Documents/ArcGIS/Backgrounds/NED/NEDn47w114/grdn47w114_13")
elevNW <- raster("C:/Users/kristin.barker/Documents/ArcGIS/Backgrounds/NED/NEDn47w115/grdn47w115_13")
#landcov <- raster("C:/Users/kristin.barker/Documents/NSERP/GIS/HabitatTypes/MSDI_Reclass_MTtiff/MSDI_RC_MT.tif")
xy <- data.frame("x"=all.phen$Longitude, "y"=all.phen$Latitude)

all.phen$elevNE <- extract(elevNE, xy); all.phen$elevNE[is.na(all.phen$elevNE)] <- 0
all.phen$elevNW <- extract(elevNW, xy); all.phen$elevNW[is.na(all.phen$elevNW)] <- 0
all.phen <- all.phen %>%
  mutate(Elevm = elevNE+elevNW) %>%
  dplyr::select(-c(elevNW, elevNE))

###########
##MODELS
herb1 <- lm(sqrt(HerbBiomass) ~ EVI, data=all.phen); summary(herb)
herb2 <- lm(sqrt(HerbBiomass) ~ EVI+Elevm, data=all.phen); summary(herb2)
herb3 <- lm(sqrt(HerbBiomass) ~ EVI+LandCov, data=all.phen); summary(herb3)
herb4 <- lm(sqrt(HerbBiomass) ~ EVI+Elevm+LandCov, data=all.phen); summary(herb4)

lm1 <- c(herb1$coefficients[1], summary(herb1)$adj.r.squared, summary(herb1)$sigma)
lm2 <- c(herb2$coefficients[1], summary(herb2)$adj.r.squared, summary(herb2)$sigma)
lm3 <- c(herb3$coefficients[1], summary(herb3)$adj.r.squared, summary(herb3)$sigma)
lm4 <- c(herb4$coefficients[1], summary(herb4)$adj.r.squared, summary(herb4)$sigma)

tprep <- rbind(lm1, lm2, lm3, lm4)
tab2 <- as.data.frame(tprep, row.names = c("EVI", "EVI+Elevm", "EVI+LandCov", "EVI+Elevm+LandCov"))
#tab <- rename(tab, NDVIcoeff = NDVI)
tab2 <- rename(tab2, AdjRsquared = V2)
tab2 <- rename(tab2, StdError = V3)
View(tab2)

library(AICcmodavg)
Cand.set <- list( )
Cand.set[[1]] <- lm(sqrt(HerbBiomass) ~ EVI, data=all.phen)
Cand.set[[2]] <- lm(sqrt(HerbBiomass) ~ EVI+Elevm, data=all.phen)
Cand.set[[3]] <- lm(sqrt(HerbBiomass) ~ EVI+LandCov, data=all.phen)
Cand.set[[4]] <- lm(sqrt(HerbBiomass) ~ EVI+Elevm+LandCov, data=all.phen)
names(Cand.set) <- c("EVI", 
                     "EVI+Elevm", 
                     "EVI+LandCov", 
                     "EVI+Elevm+LandCov")
aictable <- aictab(Cand.set, second.ord=TRUE)
aicresults <- print(aictable, digits = 2, LL = FALSE)
