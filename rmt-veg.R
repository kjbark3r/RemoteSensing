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
library(AICcmodavg)

## DATA

bio <- read.csv("biomass-plot.csv")
nute <- read.csv("gdm-plot.csv") %>%
  select(c(PlotVisit, GDM))
rmt <- read.csv("remote-plot.csv")

#did landcov in arcmap; couldn't get it to work in r 
landcov <- read.csv(file = "plots-landcov.txt") 
landcov$Date <- as.Date(landcov$Date, format = "%m/%d/%Y")
landcov <- landcov %>%
  filter(Type == "Phenology" | Type == "Biomass") %>%
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
                          NA))))))))) %>%
  mutate(TreeCov = ifelse(CovClass == 2, 1, #riparian and forested
                          ifelse(CovClass == 3, 1, 0))) #=treecover


#fix plot that had date typo in database
landcov$PlotVisit[landcov$PlotVisit=="344.2014-05-27"] <- "344.2014-05-21"
landcov <- unique(landcov[]) #remove duplicates

#KRISTIN CHECK THESE THINGS:
  #why there are duplicates in landcover
  #whether riparian is generally forested like you assumed

## COMBINED DATA

veg <- full_join(bio, nute, by="PlotVisit") %>%
  rename(VisitDate = Date) %>%
  select(PlotVisit, PlotID, VisitDate, Biomass, ForageBiomass, GDM)

rmt.veg <- full_join(veg, rmt, by = "PlotVisit") %>%
  full_join(landcov, by = "PlotVisit")
# remove "outliers" (2 huge sagebrush; one tons of huckleberry)
rmt.veg <- rmt.veg[!rmt.veg$PlotVisit == "401.2014-07-18",]
rmt.veg <- rmt.veg[!rmt.veg$PlotVisit == "376.2014-07-17",]
rmt.veg <- rmt.veg[!rmt.veg$PlotVisit == "220.2015-08-03",]

# check for NAs
#rmt.veg[rmt.veg$NDVI %in% NA,] #nope
#rmt.veg[rmt.veg$EVI %in% NA,] #nope
#rmt.veg[rmt.veg$Biomass %in% NA,] #nope
#rmt.veg[rmt.veg$GDM %in% NA,] #yup
rmt.veg$GDM[is.na(rmt.veg$GDM)] <- 0 #make NAs 0s
##BELOW CODE IS TEMPORARY UNTIL GET UPDATED DATA FROM BRADY
rmt.veg <- subset(rmt.veg, PlotVisit != "326.2015-09-23") #wrong lat/long on this plot

## VISUALIZATIONS

# distributions (raw)
par(mfrow=c(2,2))
hist(rmt.veg$NDVI)
hist(rmt.veg$EVI)
hist(rmt.veg$Biomass)
hist(rmt.veg$GDM)

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

## REGRESSIONS - NO TREE/LAND COVER

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

## REGRESSIONS AND AICc - WITH TREE/LAND COVER

bio.ndvi <- lm(log(Biomass) ~ NDVI, data=rmt.veg); summary(bio.ndvi)
bio.land <- lm(log(Biomass) ~ NDVI+LandCov, data=rmt.veg); summary(bio.land)
bio.land.int <- lm(log(Biomass) ~ NDVI*LandCov, data=rmt.veg); summary(bio.land.int)
bio.tree <- lm(log(Biomass) ~ NDVI+TreeCov, data=rmt.veg); summary(bio.tree)
bio.tree.int <- lm(log(Biomass) ~ NDVI*TreeCov, data=rmt.veg); summary(bio.tree.int)

Cand.set <- list( )
Cand.set[[1]] <- lm(log(Biomass) ~ NDVI, data=rmt.veg)
Cand.set[[2]] <- lm(log(Biomass) ~ NDVI+LandCov, data=rmt.veg)
Cand.set[[3]] <- lm(log(Biomass) ~ NDVI*LandCov, data=rmt.veg)
Cand.set[[4]] <- lm(log(Biomass) ~ NDVI+TreeCov, data=rmt.veg)
Cand.set[[5]] <- lm(log(Biomass) ~ NDVI*TreeCov, data=rmt.veg)
names(Cand.set) <- c("NDVI", 
                     "NDVI+LandCov",
                     "NDVI*LandCov",
                     "NDVI+TreeCov",
                     "NDVI*TreeCov")
aictable <- aictab(Cand.set, second.ord=TRUE)
aicresults <- print(aictable, digits = 2, LL = FALSE)


