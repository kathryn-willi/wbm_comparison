####### Water Balance code ##########
source("WaterBalance/R/ET_functions.R")
source("WaterBalance/R/WB_functions.R")
source("katie_adds/getParkBoundary.R")
library(tidyverse)
library(plotly)
library(terra)
library(sf)
library(mapview)

############################################################# USER INPUTS ##################################################################### 

#KW comment: Historical was originally pulled from data we can't access. Therefore, I pulled this from AWS.
# (https://parkfutures.s3.us-west-2.amazonaws.com/maca-tprh-data/index.html)
Historical <- read.csv("katie_adds/centroid_data/PETE_historical.csv") 
#Site characteristics 
wb_sites <- read.csv("PETE_site_parameters.csv") 

n<-nrow(wb_sites)
#Threshold temperature (deg C) for growing degree-days calculation
T.Base = 0 

#Method for PET calculation 
Method = "Oudin"  #Oudin is default method for daily PRISM and MACA data (containing only Tmax, Tmin, and Date). 

#Date format
DateFormat = "%m/%d/%Y"

############################################################ END USER INPUTS ###################################################################

############################################################ CREATE CLIMATE INPUTS #############################################################
#### Historical
# Convert pr.In to mm and F to C
Historical$ppt_mm <- (Historical$Precip..in.*25.4)
Historical$tmax_C <- 5/9*(Historical$Tmax..F. - 32)
Historical$tmin_C <- 5/9*(Historical$Tmin..F. - 32)
Historical$tmean_C <- 5/9*(Historical$Tavg..F. - 32)

######################################################### END CLIMATE INPUTS ####################################################################


######################################################### CALCULATE WB VARIABLES ################################################################
AllDailyWB<-list()
DailyWB<-Historical

for(i in 1:nrow(wb_sites)){
  ID = wb_sites$SiteID[i] # KW: was previously "Site"
  Lat = wb_sites$Lat[i]
  Lon = wb_sites$Lon[i]
  Elev = wb_sites$Elev[i] # KW: was previously "Elevation"
  Aspect = wb_sites$Aspect[i]
  Slope = wb_sites$Slope[i]
  SWC.Max = wb_sites$SWC.Max[i]
  Wind = wb_sites$Wind[i]
  Snowpack.Init = wb_sites$Snowpack.Init[i]
  Soil.Init = wb_sites$Soil.Init[i]
  Shade.Coeff = wb_sites$Shade.Coeff[i]
  
  #Calculate daily water balance variables 
  
  DailyWB$ID = ID
  DailyWB$doy <- yday(DailyWB$Date)
  DailyWB$daylength = get_daylength(DailyWB$Date, Lat)
  DailyWB$jtemp = as.numeric(get_jtemp(Lon, Lat))
  DailyWB$F = get_freeze(DailyWB$jtemp, DailyWB$tmean_C)
  DailyWB$RAIN = get_rain(DailyWB$ppt_mm, DailyWB$F)
  DailyWB$SNOW = get_snow(DailyWB$ppt_mm, DailyWB$F)
  DailyWB$MELT = get_melt(DailyWB$tmean_C, DailyWB$jtemp, hock=4, DailyWB$SNOW, Snowpack.Init)
  DailyWB$PACK = get_snowpack(DailyWB$jtemp, DailyWB$SNOW, DailyWB$MELT)
  DailyWB$W = DailyWB$MELT + DailyWB$RAIN
  if(Method == "Hamon"){
    DailyWB$PET = ET_Hamon_daily(DailyWB)
  } else {
    if(Method == "Penman-Monteith"){
      DailyWB$PET = ET_PenmanMonteith_daily(DailyWB)
    } else {
      if(Method == "Oudin"){
        DailyWB$PET = get_OudinPET(DailyWB$doy, Lat, DailyWB$PACK, DailyWB$tmean_C, Slope, Aspect, Shade.Coeff)
      } else {
        print("Error - PET method not found")
      }
    }
  }
  DailyWB$PET = modify_PET(DailyWB$PET, Slope, Aspect, Lat, Shade.Coeff)
  DailyWB$W_PET = DailyWB$W - DailyWB$PET
  DailyWB$SOIL = get_soil(DailyWB$W, Soil.Init, DailyWB$PET, DailyWB$W_PET, SWC.Max)
  DailyWB$DSOIL = diff(c(Soil.Init, DailyWB$SOIL))
  DailyWB$AET = get_AET(DailyWB$W, DailyWB$PET, DailyWB$SOIL, Soil.Init)
  DailyWB$W_ET_DSOIL = DailyWB$W - DailyWB$AET - DailyWB$DSOIL
  DailyWB$D = DailyWB$PET - DailyWB$AET
  DailyWB$GDD = get_GDD(DailyWB$tmean_C, T.Base)
  AllDailyWB[[i]] = DailyWB
}
WBData<-do.call(rbind,AllDailyWB)