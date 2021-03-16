library(ncdf4)
library(reshape2)

#set study area
lonmax <- -81 #top northern most coordinate
lonmin <- -98 #bottom southern coordinate
latmax <- 30.5 #left eastern coordinate
latmin <- 18.5 #right western coordinate

#set working directory to you Lab9 data folder
#setwd(dir = "[path]")

#list netCDF files in your directory
c <- list.files(path ="chlor_a/" , pattern=".nc", full.names=T, recursive = T)

file <- c[1]

chl <- plyr::adply(c, 1, function(file){
  
  # open netCDF file
  data<-nc_open(file) #replace all "c" objects with "file" if using the plyr function
  
  fname <- basename(file)
  
  # extract data
  lon<-ncvar_get(data,"lon")
  lat<-ncvar_get(data,"lat")
  tmp.array <- ncvar_get(data, data$var$chlor_a)
  dunits <- ncatt_get(nc = data, varid = "chlor_a", attname = "units")$value
  fillvalue <- ncatt_get(data, varid = "chlor_a", attname = "_FillValue")$value
  
  #get dimensions
  dim(tmp.array)
  
  # replace the pixes=ls with missing data by finding those with the "_FillValue" and replacing that value with a "NA"
  tmp.array[tmp.array == fillvalue] <- NA
  
  #  matrix to data.frame
  dimnames(tmp.array)<-list(lon=lon,lat=lat)
  dat.var<-melt(tmp.array,id="lon")
  
  # select data from the study area using the values you set in the first part of the script
  dat.varSAtmp<-subset(dat.var, lon<=lonmax & lon>=lonmin & lat<=latmax & lat>=latmin)
  
  # extract date information
  dateini<-ncatt_get(data,0,"time_coverage_start")$value
  dateend<-ncatt_get(data,0,"time_coverage_end")$value
  datemean<-mean(c(as.Date(dateend,"%Y-%m-%dT%H:%M:%OSZ"),as.Date(dateini,"%Y-%m-%dT%H:%M:%OSZ")))
  year<-substring(datemean,0,4)
  month<-substring(datemean,6,7)
  day<-substring(datemean,9,10)
  
  # prepare final data set by adding meta data to your data frame
  dat.varSA<- data.frame(fname, rep(x = as.integer(year), nrow(dat.varSAtmp)),
                         rep(as.integer(month),nrow(dat.varSAtmp)),
                         rep(as.integer(day),nrow(dat.varSAtmp)),
                         dat.varSAtmp,
                         rep("chla", nrow(dat.varSAtmp)),
                         rep(dunits,nrow(dat.varSAtmp)))
  
  names(dat.varSA)<-c("file.name","year","month","day","lon","lat","value","unit","var")
  
  return(dat.varSA)
  
  # close connection
  nc_close(data)
  
}, .progress = "text", .inform = T)

library(tidyverse)

chl_sum <- chl %>% group_by(lon,lat) %>% summarise(
  mean_chl = mean(value, na.rm = T),
  std_chl = sd(value, na.rm = T),
  var_chl = var(value, na.rm=T),
  n_chl = n())

# #testing to see if summarise function works as intended
#chl_sum <- dat.varSA %>% summarise(mean_chl = mean(value, na.rm = T),
#                                                   std_chl = sd(value, na.rm = T),
#                                                   var_chl = var(value, na.rm=T),
#                                                  n_chl = n())

# save csv file
write.csv(chl_sum,"netCDF_statistic.csv",row.names=FALSE)
