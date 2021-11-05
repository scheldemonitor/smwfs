###############################################################################
#Function_ImportData.R: Develop general functions for importing abiotic and biotic data

#Written by Wout Van Echelpoel (UGent)
#Wout.VanEchelpoel@UGent.be - Sep 2021
###############################################################################

###############################################################################
# 0 - Libraries
###############################################################################
library(sf) # For using webservices
library(readr)
###############################################################################

###############################################################################
# 1 - Static part
###############################################################################
# Define work directory
# s.wd<-'~/T2021/00_Algemeen/Scripts' # s.wd<-choose.dir() # Alternative (not in Scheldemonitor environment)
# setwd(s.wd)

###############################################################################

###############################################################################
# 2 - Script
###############################################################################
# Define function for abiotic data
f.importAbioticData<-function(param,start = 2015,end = 2021){
  # Define time period
  v.years<-c(start:end)

  # Define fixed elements of URL
  v.url<-c('http://geo.vliz.be/geoserver/wfs/ows?service=WFS&version=1.1.0&request=GetFeature&typeName=Dataportal%3Aabiotic_observations&resultType=results&viewParams=where%3Aobs.context+%26%26+ARRAY%5B1%5D+AND+standardparameterid+IN+%28','%29+AND+%28%28datetime_search+BETWEEN+%27','-01-01%27+AND+%27','-12-32%27+%29%29%3Bcontext%3A0001%3Bloggedin%3A1&propertyName=stationname%2Clongitude%2Clatitude%2Cdatetime%2Cdepth%2Cparametername%2Cvaluesign%2Cvalue%2Cdataprovider%2Cdatasettitle&outputFormat=csv')

  # Define parameter-specific section of URL
  n.par<-paste(param,collapse = '%5C%2C')

  # Define dataframe for output
  df.dat<-data.frame()

  # Import data for each year
  for (i in 1:length(v.years)) {
    message(paste('Import for year ',v.years[i],sep = ''))
    # Compose year-specific URL and import data
    s.url<-paste(v.url[1],n.par,v.url[2],v.years[i],v.url[3],v.years[i],v.url[4],sep = '')
    df.dat.yr<-read_csv(s.url)

    # If no data, go to next iteration, otherwise add to output dataframe
    if (nrow(df.dat.yr) == 0) { next() }
    df.dat<-rbind(df.dat.yr,df.dat)
  }

  # Generate output
  return(df.dat)
}

# Define function for biotic data
f.importBioticData<-function(param,start = 2015,end = 2021){
  # Define time period
  v.years<-c(start:end)

  # Define fixed elements of URL
  v.url<-c('http://geo.vliz.be/geoserver/wfs/ows?service=WFS&version=1.1.0&request=GetFeature&typeName=Dataportal%3Abiotic_observations&resultType=results&viewParams=where%3Aobs.context+%26%26+ARRAY%5B1%5D+AND+imisdatasetid+IN+%28','%29+AND+%28%28datetime_search+BETWEEN+%27','-01-01%27+AND+%27','-12-32%27+%29%29%3Bcontext%3A0001%3Bloggedin%3A1&propertyName=stationname%2Caphiaid%2Cscientificname%2Cobservationdate%2Clongitude%2Clatitude%2Cvalue%2Cparametername%2Cdataprovider%2Cimisdatasetid%2Cdatasettitle%2Cdatafichetitle&outputFormat=csv')

  # Define parameter-specific section of URL
  n.par<-paste(param,collapse = '%5C%2C')

  # Define dataframe for output
  df.dat<-data.frame()

  # Import data for each year
  for (i in 1:length(v.years)) {
    message(paste('Import for year ',v.years[i],sep = ''))
    # Compose year-specific URL and import data
    s.url<-paste(v.url[1],n.par,v.url[2],v.years[i],v.url[3],v.years[i],v.url[4],sep = '')
    df.dat.yr<-data.frame(st_read(s.url,stringsAsFactors = F))

    # If no data, go to next iteration, otherwise add to output dataframe
    if (nrow(df.dat.yr) == 0) { next() }
    df.dat<-rbind(df.dat.yr,df.dat)
  }

  # Generate output
  return(df.dat)
}

###############################################################################

###############################################################################
# 3 - Remove excess variables
###############################################################################
# rm(s.wd)
###############################################################################
