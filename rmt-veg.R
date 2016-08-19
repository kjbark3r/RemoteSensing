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

bio <- read.csv("biomass-plot.csv")
nute <- read.csv("gdm-plot.csv")
rmt <- read.csv("remote-plot.csv")

## COMBINED DATA

veg <- full_join(bio, nute, by="PlotVisit") %>%
  rename(VisitDate = Date) %>%
  select(PlotVisit, PlotID, VisitDate, Biomass, ForageBiomass, GDM)

rmt.veg <- full_join(veg, rmt, by = "PlotVisit") 
# remove "outliers" (2 huge sagebrush)
rmt.veg <- rmt.veg[!rmt.veg$PlotVisit == "401.2014-07-18",]
rmt.veg <- rmt.veg[!rmt.veg$PlotVisit == "376.2014-07-17",]

# check for NAs
#rmt.veg[rmt.veg$NDVI %in% NA,] #nope
#rmt.veg[rmt.veg$EVI %in% NA,] #nope
#rmt.veg[rmt.veg$Biomass %in% NA,] #nope
#rmt.veg[rmt.veg$GDM %in% NA,] #yup
rmt.veg$GDM[is.na(rmt.veg$GDM)] <- 0 #make them 0s

## VISUALIZATIONS

# distributions (raw)
par(mfrow=c(2,2))
hist(rmt.veg$NDVI)
hist(rmt.veg$EVI)
hist(rmt.veg$Biomass)
hist(rmt.veg$GDM)

# transformations
par(mfrow=c(2,2))
hist(log(rmt.veg$Biomass))
hist(log(rmt.veg$ForageBiomass))
hist(rmt.veg$GDM)
hist(log(rmt.veg$GDM))

# transformed distributions
par(mfrow=c(2,2))
hist(rmt.veg$NDVI)
hist(rmt.veg$EVI)
hist(log(rmt.veg$Biomass))
hist(log(rmt.veg$GDM)) 

# relationships
par(mfrow=c(2,2))
plot(log(Biomass) ~ NDVI, data = rmt.veg)
plot(log(GDM) ~ NDVI, data = rmt.veg)
plot(log(Biomass) ~ EVI, data = rmt.veg)
plot(log(GDM) ~ EVI, data = rmt.veg)

## REGRESSIONS

bio.ndvi <- lm(log(Biomass) ~ NDVI, data=rmt.veg); summary(bio.ndvi)
bio.evi <- lm(log(Biomass) ~ EVI, data=rmt.veg); summary(bio.evi)
for.ndvi <- lm(log(ForageBiomass+0.05) ~ NDVI, data=rmt.veg); summary(for.ndvi)
for.evi <- lm(log(ForageBiomass+0.05) ~ EVI, data=rmt.veg); summary(for.evi)
gdm.ndvi <- lm(log(GDM+0.05) ~ NDVI, data=rmt.veg); summary(gdm.ndvi)
gdm.evi <- lm(log(GDM+0.05) ~ EVI, data=rmt.veg); summary(gdm.evi)

lm1 <- c(bio.ndvi$coefficients[1], summary(bio.ndvi)$adj.r.squared, summary(bio.ndvi)$sigma)
lm2 <- c(bio.evi$coefficients[1], summary(bio.evi)$adj.r.squared, summary(bio.evi)$sigma)
lm3 <- c(for.ndvi$coefficients[1], summary(for.ndvi)$adj.r.squared, summary(for.ndvi)$sigma)
lm4 <- c(for.evi$coefficients[1], summary(for.evi)$adj.r.squared, summary(for.evi)$sigma)
lm5 <- c(gdm.ndvi$coefficients[1], summary(gdm.ndvi)$adj.r.squared, summary(gdm.ndvi)$sigma)
lm6 <- c(gdm.evi$coefficients[1], summary(gdm.evi)$adj.r.squared, summary(gdm.evi)$sigma)

tprep <- rbind(lm1, lm2, lm3, lm4, lm5, lm6)
tab <- as.data.frame(tprep, row.names = c("Biomass-NDVI", "Biomass-EVI", "ForageBiomass-NDVI",
                                          "ForageBiomass-EVI", "GDM-NDVI", "GDM-EVI"))
#tab <- rename(tab, NDVIcoeff = NDVI)
tab <- rename(tab, AdjRsquared = V2)
tab <- rename(tab, StdError = V3)
View(tab)

## AIC 

# ndvi or evi?

library(AICcmodavg)
Cand.set <- list( )
Cand.set[[1]] <- lm(log(Biomass) ~ NDVI, data=rmt.veg)
Cand.set[[2]] <- lm(log(Biomass) ~ EVI, data=rmt.veg)
names(Cand.set) <- c("Biomass-NDVI", "Biomass-EVI")
aictable <- aictab(Cand.set, second.ord=TRUE)
aicresults <- print(aictable, digits = 2, LL = FALSE) #NDVI

Cand.set[[1]] <- lm(log(ForageBiomass+0.05) ~ NDVI, data=rmt.veg)
Cand.set[[2]] <- lm(log(ForageBiomass+0.05) ~ EVI, data=rmt.veg)
names(Cand.set) <- c("ForageBiomass-NDVI", "ForageBiomass-EVI")
aictable <- aictab(Cand.set, second.ord=TRUE)
aicresults <- print(aictable, digits = 2, LL = FALSE) #NDVI

Cand.set[[1]] <- lm(log(GDM+0.05) ~ NDVI, data=rmt.veg)
Cand.set[[2]] <- lm(log(GDM+0.05) ~ EVI, data=rmt.veg)
names(Cand.set) <- c("GDM-NDVI", "GDM-EVI")
aictable <- aictab(Cand.set, second.ord=TRUE)
aicresults <- print(aictable, digits = 2, LL = FALSE) #NDVI

# which has best reln with ndvi (not sure this is valid mathematically...)

Cand.set[[1]] <- lm(NDVI ~ log(Biomass+0.05), data=rmt.veg)
Cand.set[[2]] <- lm(NDVI ~ log(ForageBiomass+0.05), data=rmt.veg)
Cand.set[[3]] <- lm(NDVI ~ log(GDM+0.05), data=rmt.veg)
names(Cand.set) <- c("Biomass", "ForageBiomass", "GDM")
aictable <- aictab(Cand.set, second.ord=TRUE)
aicresults <- print(aictable, digits = 2, LL = FALSE) #Biomass, of course

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
