########################################################
###############   MISC CODE RELATED TO   ###############
###### COMPARING GROUND-BASED VEGETATION MEASURES ###### 
######### TO REMOTELY SENSED VEGETATION INDICES ######## 
############ NSERP KJB  July 2016  #####################
########################################################

########
# converting plot visit date to day of year

test <- biomass
test$Date <- as.Date(test$Date)
test$Date$yday[1,]
#nope

rm(test)
test <- biomass
test$DOY <- as.POSIXlt(test$Date)
test$DOY <- test$DOY$yday
#yep

########
# figuring out why i can't join based on plotvisit
########

#checking out the data
remote <- filter(remote, Type =="Phenology")
same <- as.data.frame(intersect(remote$PlotVisit, biomass$PlotVisit)) 
colnames(same) <- "PlotVisit"
same <- arrange(same, PlotVisit)
  #diff = in remote but not in biomass
diff <- as.data.frame(setdiff(remote$PlotVisit, biomass$PlotVisit)) 
  colnames(diff) <- "PlotVisit"
  diff <- arrange(diff, PlotVisit)
nas <- as.data.frame(rep(NA, 15))
  colnames(nas) <- "PlotVisit"
diff <- rbind(diff, nas)
wtf <- cbind(same, diff)

#just looking at what's different
remote <- filter(remote, Type =="Phenology")
remoteonly <- as.data.frame(setdiff(remote$PlotVisit, biomass$PlotVisit))
  colnames(remoteonly) <- "r"  
  remoteonly <- arrange(remoteonly, r)
biomassonly <- as.data.frame(setdiff(biomass$PlotVisit, remote$PlotVisit))
  colnames(biomassonly) <- "b"
  biomassonly <- arrange(biomassonly, b)
wtf <- cbind(remoteonly, biomassonly)
#remote values never off by more than one day
#must be date conversion issue

########
# fixing date conversion issue in remote data
rm(remoteonly, biomassonly, wtf)

#specify time zone?
remote <- read.csv(file = "NSERP_AllPlots_NDVI-EVI.csv", as.is = TRUE) 
remote$Date <- as.Date(as.POSIXct((remote$Date+0.1)/1000, origin="1970-01-01", tz = "MDT"))
#nope

#don't add +0.1? i don't remember where i got that from anyway
remote <- read.csv(file = "NSERP_AllPlots_NDVI-EVI.csv", as.is = TRUE) 
remote$Date <- as.Date(as.POSIXct((remote$Date)/1000, origin="1970-01-01"))
#nope

#maybe brady's data comes back in universal time?
remote <- read.csv(file = "NSERP_AllPlots_NDVI-EVI.csv", as.is = TRUE) 
remote$Date <- as.Date(as.POSIXct((remote$Date+0.1)/1000, origin="1970-01-01", tz = "UTC"))
#nope

#add six hours?
remote <- read.csv(file = "NSERP_AllPlots_NDVI-EVI.csv", as.is = TRUE) 
remote$Date <- as.Date(as.POSIXct((remote$Date+21600000)/1000, origin="1970-01-01", tz = "MDT"))
#omggggg down to only one issue

#add twelve?
remote <- read.csv(file = "NSERP_AllPlots_NDVI-EVI.csv", as.is = TRUE) 
remote$Date <- as.Date(as.POSIXct((remote$Date+43200000)/1000, origin="1970-01-01", tz = "MDT"))
#still one 

#sixteen?
remote <- read.csv(file = "NSERP_AllPlots_NDVI-EVI.csv", as.is = TRUE) 
remote$Date <- as.Date(as.POSIXct((remote$Date+64800000)/1000, origin="1970-01-01", tz = "MDT"))
#still one (344.2014-05-27 - it's 344.2014-05-21 in biomass. typo?)
#yep. fixed. also figured out brady's data is in UTC, so changed time zone.

remote$PlotVisit <- paste(remote$PlotID, ".", remote$Date, sep = "")
remote <- filter(remote, Type =="Phenology")
remoteonly <- as.data.frame(setdiff(remote$PlotVisit, biomass$PlotVisit))

########
#### putting data in long form

#what i did last time
remote.data <- read.csv(file = "remotedata.csv") %>%
  filter(Type == "Pellet 2014") %>%
  select(starts_with("ndvi."), PlotID) %>%
  gather(key = SDate, value = "NDVI")
#prob best to do this in advance
#for ndvi and evi separately
#THEN merge them with biomass

#playing with gather

#gather_ is programmatic
#first try it naming the key column instead of storing it
gather_cols <- c(colnames(ndvi[4:ncol(ndvi)]))
test <- gather_(ndvi, RemoteDOY, NDVI, gather_cols)
#nope gotta store

keycol <- "RemoteDOY"
valuecol <- "NDVI"
gathercols <- c(colnames(ndvi[4:ncol(ndvi)]))
test <- gather_(ndvi, key_col, value_col, gather_cols, na.rm=FALSE)
#if this doesn't work, paste column names and move on
#ok just one more quick thing
test <- gather_(ndvi, keycol, valuecol, gathercols, na.rm=FALSE)

#got gather to work (yay)
#but need to figure out how to gather multiple columns
  #bc need DOY and NDVI/EVI value
#ex from extract() help file
df <- data.frame(x = c(NA, "a-b", "a-d", "b-c", "d-e"))
df %>% extract(x, "A")
df %>% extract(x, c("A", "B"), "([[:alnum:]]+)-([[:alnum:]]+)")

#need to write a regular expression to ID DOY and NDVI cols
  #see r - Gather multiple sets of columns... answer by Hadley
#separate into doy.XXX and ndvi.XXX
ndvi <- remote %>%
  filter(Type == "Phenology") %>%
  select(PlotVisit, starts_with("ndvi."), starts_with("doy.")) %>%
  gather(RemoteDate, NDVI, -PlotVisit) %>%
  extract(RemoteDate, c("NDVIDate", "RemoteDate"), "(^doy)-(^ndvi)")
#close - created columns without giving error message
#but they're NA - no match - need to fix expression 

ndvi <- remote %>%
  filter(Type == "Phenology") %>%
  select(PlotVisit, starts_with("ndvi."), starts_with("doy.")) %>%
  gather(RemoteDate, NDVI, -PlotVisit) %>%
  extract(RemoteDate, c("NDVIDate", "RemoteDate"), "(doy)-(ndvi)")
#nope

ndvi <- remote %>%
  filter(Type == "Phenology") %>%
  select(PlotVisit, starts_with("ndvi."), starts_with("doy.")) %>%
  gather(RemoteDate, NDVI, -PlotVisit) %>%
  extract(RemoteDate, c("NDVIDate", "RemoteDate"), "(doy.)-(ndvi.)")
#nope

ndvi <- remote %>%
  filter(Type == "Phenology") %>%
  select(PlotVisit, starts_with("ndvi."), starts_with("doy.")) %>%
  gather(RemoteDate, NDVI, -PlotVisit) %>%
  extract(RemoteDate, c("NDVIDate", "RemoteDate"), "("doy.")-("ndvi.")")
#hey a new error message, oh boy
#Error: unexpected symbol in:
#"  gather(RemoteDate, NDVI, -PlotVisit) %>%
#  extract(RemoteDate, c("NDVIDate", "RemoteDate"), "("doy."

ndvi <- remote %>%
  filter(Type == "Phenology") %>%
  select(PlotVisit, starts_with("ndvi."), starts_with("doy.")) %>%
  gather(RemoteDate, NDVI, -PlotVisit) %>%
  extract(RemoteDate, c("NDVIDate", "RemoteDate"), "(^doy(.*))-(^ndvi(.*))")
#still NAs but also new NA columns now

ndvi <- remote %>%
  filter(Type == "Phenology") %>%
  select(PlotVisit, starts_with("ndvi."), starts_with("doy.")) %>%
  gather(RemoteDate, NDVI, -PlotVisit) %>%
  extract(RemoteDate, c("NDVIDate", "RemoteDate"), "(^doy(.*?))-(^ndvi(.*?))")
#ditto

ndvi <- remote %>%
  filter(Type == "Phenology") %>%
  select(PlotVisit, starts_with("ndvi."), starts_with("doy.")) %>%
  gather(RemoteDate, NDVI, -PlotVisit) %>%
  extract(RemoteDate, c("NDVIDate", "RemoteDate"), "(doy.*)-(ndvi.*)")
#ok fuck this, let's try something else
#this isn't even keeping the right NDVI values with the right plot visits

########
# finding closest remoteDOY to visitDOY
########

which.min(abs(ndvi$RemoteDOY - 249))
#um... no.
which.min(abs((1:59876) - 249))
#yes

test <- ndvi %>%
  group_by(PlotVisit) %>%
  filter(which.min(abs(RemoteDOY - VisitDOY)))
#r-splosion

test <- ndvi %>%
  group_by(PlotVisit) %>%
  filter(which.min(abs(ndvi$RemoteDOY - ndvi$VisitDOY)))
#r-splosion

test <- ndvi %>%
  mutate(doydiff = RemoteDOY-VisitDOY) %>%
  group_by(PlotVisit) %>%
  filter(min(doydiff))
#not quite... time to steal code from stack

test <- ndvi %>%
  mutate(doydiff = RemoteDOY-VisitDOY) %>%
  group_by(PlotVisit) %>%
  filter(doydiff == min(doydiff))
#STOP EXPLODING ON ME DAMNIT

test <- ndvi %>%
  mutate(doydiff = RemoteDOY-VisitDOY) %>%
  group_by(PlotVisit) %>%
  filter(doydiff == which.min(doydiff))
#SRSLY

test <- ndvi %>%
  mutate(doydiff = RemoteDOY-VisitDOY) %>%
  group_by(PlotVisit) %>%
  slice(which.min(doydiff))
#:(

test <- ndvi %>%
  mutate(doydiff = RemoteDOY-VisitDOY) %>%
  group_by(PlotVisit) %>%
  filter(row_number() ==n())
#no

test <- ndvi %>%
  mutate(doydiff = RemoteDOY-VisitDOY) %>%
  group_by(PlotVisit) %>%
  filter(which.min(doydiff))
#no
#oh dude... the problem is group_by, not filter

test <- ndvi %>%
  mutate(doydiff = RemoteDOY-VisitDOY) %>%
  group_by(VisitDate)

test <- ndvi %>%
  mutate(doydiff = abs(RemoteDOY-VisitDOY)) %>%
  group_by(PlotVisit) %>%
  filter(which.min(doydiff))
#which.min isn't logical

test <- ndvi %>%
  mutate(doydiff = abs(RemoteDOY-VisitDOY)) %>%
  group_by(PlotVisit) %>%
  filter(doydiff < 16)
#returns multiple values for some plot visits

test <- ndvi %>%
  mutate(doydiff = abs(RemoteDOY-VisitDOY)) %>%
  group_by(PlotVisit) %>%
  filter(doydiff == min(doydiff))
#returns 0 observations

test <- ndvi %>%
  mutate(doydiff = abs(RemoteDOY-VisitDOY)) %>%
  group_by(PlotVisit) %>%
  filter(doydiff == which.min(doydiff)) 
#returns 14 observations

test <- ndvi %>%
  mutate(doydiff = abs(RemoteDOY-VisitDOY)) %>%
  group_by(PlotVisit) %>%
  arrange(doydiff) %>%
  filter(doydiff == min_rank(doydiff)) 
#16 observations....
  #all the ones with doydiff = 1

test <- ndvi %>%
  mutate(doydiff = abs(RemoteDOY-VisitDOY)) %>%
  group_by(PlotVisit) %>%
  arrange(doydiff) %>%
  slice(which.min(doydiff)) 
#yayyyyyyy


########
#### EXTRACTING ELEVS FROM RASTERS
#### because screw you, arcmap
########

library(raster)

locs <- read.csv(file = "NSERP_AllPlots.csv") %>%
  subset(Type == "Phenology")
locs$Date <- as.Date(locs$Date, format = "%m/%d/%Y")
locs <- locs %>%
  mutate(PlotVisit = paste(PlotID, ".", Date, sep = "")) %>%
  subset(PlotVisit != "326.2015-09-23")

elevSE <- raster("C:/Users/kristin.barker/Documents/ArcGIS/Backgrounds/NED/NEDn46w114/grdn46w114_13")
elevSW <- raster("C:/Users/kristin.barker/Documents/ArcGIS/Backgrounds/NED/NEDn46w115/grdn46w115_13")
elevNE <- raster("C:/Users/kristin.barker/Documents/ArcGIS/Backgrounds/NED/NEDn47w114/grdn47w114_13")
elevNW <- raster("C:/Users/kristin.barker/Documents/ArcGIS/Backgrounds/NED/NEDn47w115/grdn47w115_13")
landcov <- raster("C:/Users/kristin.barker/Documents/NSERP/GIS/HabitatTypes/MSDI_Reclass_MTtiff/MSDI_RC_MT.tif")
  

xy <- data.frame("x"=locs$Longitude, "y"=locs$Latitude)
locs$elevNE <- extract(elevNE, xy); locs$elevNE[is.na(locs$elevNE)] <- 0
locs$elevNW <- extract(elevNW, xy); locs$elevNW[is.na(locs$elevNW)] <- 0
locs <- locs %>%
  mutate(Elevm = elevNE+elevNW) %>%
  dplyr::select(-c(elevNW, elevNE))


locs$LandCov <- raster::extract(landcov, xy)
  #nope
brick <- brick(landcov)
locs$LandCov <- extract(brick, xy)
  #nope
locs$LandCov <- raster::extract(landcov@data@attributes, xy)
raster::extract(brick, xy)
  #effit

########
#### Dealing with the log(0) issue
###  to make regressions work
########

# make sure adding a small number to everything doesn't
# somehow screw everything up like the self-righteous
# statisticians on stackexchange say it will
# also checking a few number options

par(mfrow=c(2,2))
plot(log(Biomass) ~ NDVI, data = no.out)
plot(log(Biomass + 0.1) ~ NDVI, data = no.out)
plot(log(Biomass + 0.0001) ~ NDVI, data = no.out)
plot(log(Biomass + 0.0000000001) ~ NDVI, data = no.out)
# whooooooa, why are there so many 0s? 
# must have something wrong in my biomass code
# fixed. now just have to deal with a couple 0s in foragebiomass
length(remote.veg[remote.veg$ForageBiomass == 0]) #5, to be precise

par(mfrow=c(2,2))
plot(log(ForageBiomass) ~ NDVI, data = remote.veg)
plot(log(ForageBiomass + 0.1) ~ NDVI, data = remote.veg)
plot(log(ForageBiomass + 0.0001) ~ NDVI, data = remote.veg)
plot(log(ForageBiomass + 0.0000000001) ~ NDVI, data = remote.veg)
# 0.1 looks best (messes up data least) but seems so big
# smallest non-0 foragebiomass value in the data is 0.24

par(mfrow=c(2,2))
plot(log(ForageBiomass) ~ NDVI, data = remote.veg)
plot(log(ForageBiomass + 0.1) ~ NDVI, data = remote.veg)
plot(log(ForageBiomass + 0.05) ~ NDVI, data = remote.veg)
plot(log(ForageBiomass + 0.005) ~ NDVI, data = remote.veg)
# i'm going with 0.05 for now, not sure how to know what the best number is
# picked this bc didn't make the data look very different from original
# but wasn't such a high number as 0.1 which just seems super big

########
#### DELETED CODE
########

 %>% full_join(biomass, by = "PlotVisit") #apparently buggy; crashes r




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