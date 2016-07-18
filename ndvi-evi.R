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

# herbaceous biomass at phenology plots
phenology <- read.csv(file = "biomass-phenology.csv", as.is = TRUE)
  phenology$VisitDate <- as.POSIXlt(phenology$Date)
  phenology$VisitDOY <- phenology$VisitDate$yday 
  phenology$VisitDate <- as.POSIXct(phenology$VisitDate)
  phenology <- select(phenology, -c(PlotID, Date))

# ndvi, long form
ndvi <- remote %>%
  filter(Type == "Phenology") %>%
  select(PlotVisit, starts_with("ndvi."), starts_with("doy.")) %>%
  gather(RemoteDate, NDVI, -PlotVisit)
ndvi$VisitDate <- as.POSIXlt(substr(ndvi$PlotVisit, 5, 14))
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
  filter(Type == "Phenology") %>%
  select(PlotVisit, starts_with("evi."), starts_with("doy.")) %>%
  gather(RemoteDate, EVI, -PlotVisit)
evi$VisitDate <- as.POSIXlt(substr(evi$PlotVisit, 5, 14))
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

latlong <- read.csv(file = "NSERP_AllPlots.csv")
latlong$Date <- as.Date(latlong$Date, format = "%m/%d/%Y")
latlong <- latlong %>%
  subset(Type == "Phenology") %>%
  mutate(PlotVisit = paste(PlotID, ".", Date, sep="")) %>%
  select(PlotVisit, Latitude, Longitude)
  
remote.phen <- full_join(ndvi, evi, by = "PlotVisit") %>%
  select(-c(VisitDate, VisitDOY)) %>%
  full_join(phenology, by = "PlotVisit") %>%
  select(PlotVisit, VisitDate, VisitDOY, RemoteDOY, doydiff, NDVI, EVI,
         ForbBiomass, ForageForbBiomass, GrassBiomass, ForageGrassBiomass, 
         HerbBiomass, ForageHerbBiomass) %>%
  full_join(latlong, by = "PlotVisit")

## RELATIONSHIPS

## visualizations

plot(HerbBiomass ~ NDVI, data = remote.phen)
# yeesh

par(mfrow=c(2,2))
hist(remote.phen$NDVI)
hist(remote.phen$EVI)
hist(remote.phen$HerbBiomass)
hist(remote.phen$ForageHerbBiomass)
  #biomass measures are right-skewed
  #transforming...
    par(mfrow=c(2,1))
    hist(sqrt(remote.phen$HerbBiomass))
    hist(sqrt(remote.phen$ForageHerbBiomass))

##NDVI
plot(sqrt(HerbBiomass) ~ NDVI, data = remote.phen)
plot(sqrt(ForageHerbBiomass) ~ NDVI, data = remote.phen)

plot(sqrt(ForbBiomass) ~ NDVI, data = remote.phen)
plot(sqrt(ForageForbBiomass) ~ NDVI, data = remote.phen)

plot(sqrt(GrassBiomass) ~ NDVI, data = remote.phen)
plot(sqrt(ForageGrassBiomass) ~ NDVI, data = remote.phen)


##EVI
plot(sqrt(HerbBiomass) ~ EVI, data = remote.phen)
plot(sqrt(ForageHerbBiomass) ~ EVI, data = remote.phen)

plot(sqrt(ForbBiomass) ~ EVI, data = remote.phen)
plot(sqrt(ForageForbBiomass) ~ EVI, data = remote.phen)

plot(sqrt(GrassBiomass) ~ EVI, data = remote.phen)
plot(sqrt(ForageGrassBiomass) ~ EVI, data = remote.phen)

## regressions

##NDVI
herb <- lm(sqrt(HerbBiomass) ~ NDVI, data=remote.phen); summary(herb)
  herb.for <- lm(sqrt(ForageHerbBiomass) ~ NDVI, data=remote.phen); summary(herb.for)
forb <- lm(sqrt(ForbBiomass) ~ NDVI, data=remote.phen); summary(forb)
  forb.for <- lm(sqrt(ForageForbBiomass) ~ NDVI, data=remote.phen); summary(forb.for)
grass <- lm(sqrt(GrassBiomass) ~ NDVI, data=remote.phen); summary(grass)
  grass.for <- lm(sqrt(ForageGrassBiomass) ~ NDVI, data=remote.phen); summary(grass.for)
  
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
