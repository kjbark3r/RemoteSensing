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


########
#### DELETED CODE

 %>% full_join(biomass, by = "PlotVisit") #apparently buggy; crashes r