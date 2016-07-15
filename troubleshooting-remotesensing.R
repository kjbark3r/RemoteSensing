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
#damn i'm good