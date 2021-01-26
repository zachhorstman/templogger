library(tidyverse)
library(foreach)
library(lubridate)
library(scales)

options(stringsAsFactors = FALSE)

# create function to read csv data ----------------------------------------

#this function gets last charachter in a string...used to choose proper data formats when reading data
getLastCharOfString<-function(x,n){
  substr(x, nchar(x)-n+1, nchar(x))
}

#this function will be called to read csv data files
readData<-function(myFile, mySite, myStream, myYear){
  
  #read file and cut out first row and only keep date and temp columns
  tmp<-read.csv(myFile, skip=1, header=FALSE) %>%
    filter(row_number()!=1) %>%
    select(2,3)
  
  #rename columns
  names(tmp)<-c("dt", "temp")
  
  #add location and year info
  tmp<-tmp %>%
    mutate(site=mySite, stream=myStream, yr=myYear) %>%
    select(yr, stream, site, dt, temp) %>%
    filter(complete.cases(.)) %>%
    filter(temp!="")
      
  #convert dt to date time format
  if (getLastCharOfString(tmp$dt[1],1)=="M") {
    tmp$dt<-mdy_hms(tmp$dt)
  } else {
    tmp$dt<-mdy_hm(tmp$dt)
  }
  
  #convert temp to number
  tmp$temp<-as.numeric(tmp$temp)

  return(tmp)
}




# read data from csv files ------------------------------------------------
#get list of year folders/
myFolders_years<-list.dirs(path="H:/Temp Data/", ##"W:/Fisheries/Fisheries HQ/Cool Water Streams/Monitoring Data/Base Files/",
                     full.names = TRUE,
                     recursive=FALSE)

#loop through each year folder
streamData<- foreach(y = myFolders_years, .combine="rbind") %do% {
  
  #save name of folder as year 
  currentYear=basename(y)

  #get list of stream folders
  myFolders_streams<-list.dirs(path=y,
                               full.names=TRUE,
                               recursive=FALSE)

  #loop through each stream folder
  foreach(s=myFolders_streams, .combine="rbind") %do% {
    
    #save name of folder as stream name
    currentStream=basename(s)

    #get list of sites
    myFiles_sites<-list.files(path=s,
                             pattern="*.csv",
                             full.names=TRUE, 
                             recursive=FALSE, 
                             ignore.case=TRUE)
    
    #read data from each csv file
    foreach(f=myFiles_sites, .combine="rbind") %do% {
      
      #save name of folder as site
      currentSite=basename(f)
      print(paste(currentYear, "   |   ", currentStream, "   |   ", currentSite))
      
      #read data
      fileData<-readData(myFile=f, mySite=currentSite, myStream=currentStream, myYear=currentYear)
    }
  }
}



# Create and save graph ---------------------------------------------------
myStreams<-streamData %>%
  select(stream) %>%
  unique() %>%
  pull(stream)

#create folder if it doesn't exist
dir.create("charts")

foreach(s = myStreams) %do% {
  
  #get data
  op<-streamData %>%
    filter(stream==s) %>%
    mutate(dt=as.Date(dt, origin = "1960-10-01")) %>%
    mutate(year = format(dt, "%Y")) %>% ##+ group_by(month, year) %>%
    mutate(jul = format(dt,  "%j"))# new_date = julian2date(julian_date)) 

  #create temp range
  #this function assigns color values to the temps
  op$tempRange="cool"
  op$tempRange[op$temp<55.94]<-"cold"
  op$tempRange[op$temp>=20]<-"warm"
  op$tempRange[op$temp>=77]<-"hot"
  op$tempRange<-factor(op$tempRange, levels=c("cold", "cool", "warm", "hot"))
  
  #create plot
  p<-ggplot(data=op) +
    #geom_line(aes(x=dt, y=temp), size=.5) +
    geom_point(aes(x=(as.Date(jul, origin=as.date("2015-01-01"))), y=temp, color=tempRange), size=.3) +
    #geom_smooth(aes(x=jul, y=temp), color="green", size=1) +
    facet_grid(~site) +
    labs(x="Julian Day", y="Water Temperature (degrees F)", title="Water Temps", subtitle=s) +
    scale_y_continuous(limits=c(0,85)) +
    #scale_x_date(date_breaks = "50 days", date_minor_breaks = "50 days", date_labels = "%b")+
    scale_x_discrete( date_labels="50 days")+

    scale_color_manual(values=c("blue", "dodgerblue", "gold2", "firebrick1")) +
    theme_bw() +
    facet_wrap(site~year)+
    theme(axis.text.x=element_text(angle=45, hjust=1),
          panel.grid.major=element_line(color="gray70"),
          panel.background = element_rect(fill="gray90"),
          title = element_text(hjust=0.5,
                               color="blue"),
          #legend.position = "none",
          strip.background = element_rect(fill="white"),
          strip.text = element_text(color="gray20"),
          axis.text=element_text(color="black"))
  
  ggsave(paste("./charts/",s, ".jpg", sep=""), plot=p)

}




