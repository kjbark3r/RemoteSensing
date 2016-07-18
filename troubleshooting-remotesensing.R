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
#### DELETED CODE

 %>% full_join(biomass, by = "PlotVisit") #apparently buggy; crashes r