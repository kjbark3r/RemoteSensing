########################################################
###### COMPARING GROUND-BASED VEGETATION MEASURES ###### 
######### TO REMOTELY SENSED VEGETATION INDICES ######## 
############# NSERP KJB  Jul-Aug 2016  #################
########################################################

#### SETUP ####

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

#### DATA ####

bio <- read.csv("biomass-plot.csv") # from Vegetation/biomass.R
nute <- read.csv("gdm-plot.csv") # from Vegetation/biomass.R
rmt <- read.csv("remote-plot.csv") # from RemoteSensing/ndvi-evi.R
landcov <- read.csv(file = "plots-landcov.txt") # from arcmap 

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
#bc this is easier than redoing it in arcmap
landcov$PlotVisit[landcov$PlotVisit=="344.2014-05-27"] <- "344.2014-05-21"
#and remove phenology duplicates
landcov <- landcov[!duplicated(landcov),]

#### COMBINED DATA ####

veg <- full_join(bio, nute, by="PlotVisit") %>% # biomass & GDM per plot visit
  rename(VisitDate = Date) %>%
  dplyr::select(PlotVisit, PlotID, VisitDate, Biomass, ForageBiomass, GDM)

rmt.veg <- right_join(veg, rmt, by = "PlotVisit") # add NDVI/EVI
rmt.veg <- left_join(rmt.veg, landcov, by = "PlotVisit") # and landcover

#remove 4 incorrect NDVI values (unclear why, but values are definitely too low) 
rmt.veg <- rmt.veg[!rmt.veg$PlotVisit == "37.2015-08-20",]
rmt.veg <- rmt.veg[!rmt.veg$PlotVisit == "35.2015-08-20",]
rmt.veg <- rmt.veg[!rmt.veg$PlotVisit == "36.2015-08-20",]
rmt.veg <- rmt.veg[!rmt.veg$PlotVisit == "39.2015-08-20",]

# check for NAs
#rmt.veg[rmt.veg$NDVI %in% NA,] #nope
#rmt.veg[rmt.veg$EVI %in% NA,] #nope
#rmt.veg[rmt.veg$Biomass %in% NA,] #nope
#rmt.veg[rmt.veg$GDM %in% NA,] #yup
rmt.veg$GDM[is.na(rmt.veg$GDM)] <- 0 #make GDM NAs 0s

#### VISUALIZATIONS ####

# distributions (raw)
par(mfrow=c(3,2))
hist(rmt.veg$NDVI, main="NDVI")
hist(rmt.veg$EVI, main="EVI")
hist(rmt.veg$Biomass, main="Biomass")
hist(rmt.veg$ForageBiomass, main="Forage Biomass")
hist(rmt.veg$GDM, main="Avail Nute")
frame()

# distributions - transformed
par(mfrow=c(1,5))
hist(rmt.veg$NDVI, main="NDVI")
hist(log(rmt.veg$EVI), main="Log EVI")
hist(log(rmt.veg$Biomass), main="Log Biomass")
hist(log(rmt.veg$ForageBiomass), main="Log Forage Biomass")
hist(log(rmt.veg$GDM), main="Log Avail Nute")

# plotting relationships
par(mfrow=c(3,1))
scatter.smooth(log(rmt.veg$Biomass) ~ rmt.veg$NDVI)
scatter.smooth(log(rmt.veg$ForageBiomass+0.05) ~ rmt.veg$NDVI)
scatter.smooth(log(rmt.veg$GDM+0.05) ~ rmt.veg$NDVI)

#### NDVI or EVI? ####

Cand.set <- list( )
Cand.set[[1]] <- lm(log(Biomass) ~ NDVI, data=rmt.veg)
Cand.set[[2]] <- lm(log(Biomass) ~ log(EVI), data=rmt.veg)
names(Cand.set) <- c("Biomass-NDVI", "Biomass-EVI")
aictable <- aictab(Cand.set, second.ord=TRUE)
aicresults <- print(aictable, digits = 2, LL = FALSE) #NDVI

Cand.set[[1]] <- glm(log(ForageBiomass+0.05) ~ NDVI, data=rmt.veg)
Cand.set[[2]] <- glm(log(ForageBiomass+0.05) ~ log(EVI), data=rmt.veg)
names(Cand.set) <- c("ForageBiomass-NDVI", "ForageBiomass-EVI")
aictable <- aictab(Cand.set, second.ord=TRUE)
aicresults <- print(aictable, digits = 2, LL = FALSE) #NDVI

Cand.set[[1]] <- glm(log(GDM+0.05) ~ NDVI, data=rmt.veg)
Cand.set[[2]] <- glm(log(GDM+0.05) ~ log(EVI), data=rmt.veg)
names(Cand.set) <- c("GDM-NDVI", "GDM-EVI")
aictable <- aictab(Cand.set, second.ord=TRUE)
aicresults <- print(aictable, digits = 2, LL = FALSE) #NDVI

# NDVI FTW

# which has best reln with ndvi (prob not valid mathematically...)

Cand.set[[1]] <- lm(NDVI ~ log(Biomass+0.05), data=rmt.veg)
Cand.set[[2]] <- lm(NDVI ~ log(ForageBiomass+0.05), data=rmt.veg)
Cand.set[[3]] <- lm(NDVI ~ log(GDM+0.05), data=rmt.veg)
names(Cand.set) <- c("Biomass", "ForageBiomass", "GDM")
aictable <- aictab(Cand.set, second.ord=TRUE)
aicresults <- print(aictable, digits = 2, LL = FALSE) #Biomass, of course

#### REGRESSIONS - STRAIGHT NDVI; LINEAR OR QUADRATIC ####

# biomass

Cand.set <- list( )
Cand.set[[1]] <- lm(log(Biomass) ~ NDVI, data=rmt.veg)
Cand.set[[2]] <- lm(log(Biomass) ~ NDVI+I(NDVI^2), data=rmt.veg)
names(Cand.set) <- c("NDVI", "NDVI^2")
aictable <- aictab(Cand.set, second.ord=TRUE)
aicresults <- print(aictable, digits = 2, LL = FALSE)

# forage biomass

Cand.set <- list( )
Cand.set[[1]] <- lm(log(ForageBiomass+0.05) ~ NDVI, data=rmt.veg)
Cand.set[[2]] <- lm(log(ForageBiomass+0.05) ~ NDVI+I(NDVI^2), data=rmt.veg)
names(Cand.set) <- c("NDVI", "NDVI^2")
aictable <- aictab(Cand.set, second.ord=TRUE)
aicresults <- print(aictable, digits = 2, LL = FALSE)

# available nutrition

Cand.set <- list( )
Cand.set[[1]] <- lm(log(ForageBiomass+0.05) ~ NDVI, data=rmt.veg)
Cand.set[[2]] <- lm(log(ForageBiomass+0.05) ~ NDVI+I(NDVI^2), data=rmt.veg)
names(Cand.set) <- c("NDVI", "NDVI^2")
aictable <- aictab(Cand.set, second.ord=TRUE)
aicresults <- print(aictable, digits = 2, LL = FALSE)

# not enough additional support for quadratic to justify using it

#### REGRESSIONS - WITH TREE/LAND COVER ####Cand.set <- list( )
Cand.set[[1]] <- lm(log(GDM+0.05) ~ NDVI, data=rmt.veg)
Cand.set[[2]] <- lm(log(GDM+0.05) ~ NDVI+I(NDVI^2), data=rmt.veg)
names(Cand.set) <- c("NDVI", "NDVI^2")
aictable <- aictab(Cand.set, second.ord=TRUE)
aicresults <- print(aictable, digits = 2, LL = FALSE)

# biomass

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

# forage biomass

Cand.set <- list( )
Cand.set[[1]] <- lm(log(ForageBiomass+0.05) ~ NDVI, data=rmt.veg)
Cand.set[[2]] <- lm(log(ForageBiomass+0.05) ~ NDVI+LandCov, data=rmt.veg)
Cand.set[[3]] <- lm(log(ForageBiomass+0.05) ~ NDVI*LandCov, data=rmt.veg)
Cand.set[[4]] <- lm(log(ForageBiomass+0.05) ~ NDVI+TreeCov, data=rmt.veg)
Cand.set[[5]] <- lm(log(ForageBiomass+0.05) ~ NDVI*TreeCov, data=rmt.veg)
names(Cand.set) <- c("NDVI", 
                     "NDVI+LandCov",
                     "NDVI*LandCov",
                     "NDVI+TreeCov",
                     "NDVI*TreeCov")
aictable <- aictab(Cand.set, second.ord=TRUE)
aicresults <- print(aictable, digits = 2, LL = FALSE)

# avail nute

Cand.set <- list( )
Cand.set[[1]] <- lm(log(GDM+0.05) ~ NDVI, data=rmt.veg)
Cand.set[[2]] <- lm(log(GDM+0.05) ~ NDVI+LandCov, data=rmt.veg)
Cand.set[[3]] <- lm(log(GDM+0.05) ~ NDVI*LandCov, data=rmt.veg)
Cand.set[[4]] <- lm(log(GDM+0.05) ~ NDVI+TreeCov, data=rmt.veg)
Cand.set[[5]] <- lm(log(GDM+0.05) ~ NDVI*TreeCov, data=rmt.veg)
names(Cand.set) <- c("NDVI", 
                     "NDVI+LandCov",
                     "NDVI*LandCov",
                     "NDVI+TreeCov",
                     "NDVI*TreeCov")
aictable <- aictab(Cand.set, second.ord=TRUE)
aicresults <- print(aictable, digits = 2, LL = FALSE)

gdm.lc <- lm(log(GDM+0.05) ~ NDVI+LandCov, data=rmt.veg); summary(gdm.lc)

#### OLD/DELETED/MAYBE STILL HELPFUL CODE ####

## REGRESSIONS AND AICc - QUADRATIC RELATIONSHIP FOR GDM
# not so sure this is mathematically valid, but... totes doing it anyway
Cand.set <- list( )
Cand.set[[1]] <- lm(NDVI ~ log(GDM+0.05), data=rmt.veg)
Cand.set[[2]] <- lm(NDVI ~ log(GDM+0.05) + (log(GDM+0.05))^2, data=rmt.veg)
names(Cand.set) <- c("GDM", "GDM^2")
aictable <- aictab(Cand.set, second.ord=TRUE)
aicresults <- print(aictable, digits = 2, LL = FALSE) 

gdm <- lm(log(GDM+0.05) ~ NDVI, data=rmt.veg)
summary(gdm)
gdm.quadiplus <- lm(log(GDM+0.05) + I(log(GDM+0.05)^2) ~ NDVI, data=rmt.veg)
summary(gdm.quadiplus)

# plotting
par(mfrow=c(3,1))
scatter.smooth(log(rmt.veg$GDM+0.05) ~ rmt.veg$NDVI)
# not sure which of the below makes more sense (or if they're even that different)
scatter.smooth((log(rmt.veg$GDM+0.05))^2 ~ rmt.veg$NDVI))
scatter.smooth(log(rmt.veg$GDM+0.05)+(log(rmt.veg$GDM+0.05))^2 ~ rmt.veg$NDVI)

bio.ndvi <- lm(log(Biomass) ~ NDVI, data=rmt.veg); summary(bio.ndvi); plot(bio.ndvi)
bio.land <- lm(log(Biomass) ~ NDVI+LandCov, data=rmt.veg); summary(bio.land)
bio.land.int <- lm(log(Biomass) ~ NDVI*LandCov, data=rmt.veg); summary(bio.land.int)
bio.tree <- lm(log(Biomass) ~ NDVI+TreeCov, data=rmt.veg); summary(bio.tree)
bio.tree.int <- lm(log(Biomass) ~ NDVI*TreeCov, data=rmt.veg); summary(bio.tree.int)

# the ones below were me trying to figure out how to write the quadratic
#gdm.quad <- lm(log(GDM+0.05) + (log(GDM+0.05)^2) ~ NDVI, data=rmt.veg)
#summary(gdm.quad)
#gdm.quadi <- lm(I(log(GDM+0.05)^2) ~ NDVI, data=rmt.veg)
#summary(gdm.quadi)


### NDVI/VEG RELATIONSHIP - NO TREE/LAND COVER ###

bio.ndvi <- lm(log(Biomass) ~ NDVI, data=rmt.veg); summary(bio.ndvi)
#bio.evi <- lm(log(Biomass) ~ EVI, data=rmt.veg); summary(bio.evi)
for.ndvi <- lm(log(ForageBiomass+0.05) ~ NDVI, data=rmt.veg); summary(for.ndvi)
#for.evi <- lm(log(ForageBiomass+0.05) ~ EVI, data=rmt.veg); summary(for.evi)
gdm.ndvi <- lm(log(GDM+0.05) ~ NDVI, data=rmt.veg); summary(gdm.ndvi)
#gdm.evi <- lm(log(GDM+0.05) ~ EVI, data=rmt.veg); summary(gdm.evi)

lm1 <- c(bio.ndvi$coefficients[1], bio.ndvi$coefficients[2], summary(bio.ndvi)$adj.r.squared, summary(bio.ndvi)$sigma)
#lm2 <- c(bio.evi$coefficients[1], summary(bio.evi)$adj.r.squared, summary(bio.evi)$sigma)
lm3 <- c(for.ndvi$coefficients[1], for.ndvi$coefficients[2], summary(for.ndvi)$adj.r.squared, summary(for.ndvi)$sigma)
#lm4 <- c(for.evi$coefficients[1], summary(for.evi)$adj.r.squared, summary(for.evi)$sigma)
lm5 <- c(gdm.ndvi$coefficients[1], gdm.ndvi$coefficients[2], summary(gdm.ndvi)$adj.r.squared, summary(gdm.ndvi)$sigma)
#lm6 <- c(gdm.evi$coefficients[1], summary(gdm.evi)$adj.r.squared, summary(gdm.evi)$sigma)

tprep <- rbind(lm1, lm3, lm5)
tab <- as.data.frame(tprep, row.names = c("Biomass", "ForageBiomass", "GDM"))
tab <- rename(tab, NDVIcoeff = NDVI)
tab <- rename(tab, AdjRsquared = V3)
tab <- rename(tab, StdError = V4)
View(tab)