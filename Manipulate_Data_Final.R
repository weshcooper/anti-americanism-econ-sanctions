#install packages
install.packages("dplyr")

#load packages
library(readxl)
library(dplyr)

#read data sets

#perception_of_us <- read_excel("Raw Data/perception_of_us.xlsx", 
#                               na = "-")
View(perception_of_us)


#TIESv4 <- read_excel("Raw Data/TIESv4.xls")
View(TIESv4)

#Remove observations with years prior to 1992 from TIESv4

After1991TIESv4 <- select(filter(TIESv4, startyear >= 1992, primarysender == 2), c(caseid, imposition, finaloutcome, threat, startmonth, startday, startyear, endmonth, endday, endyear, targetstate))



#Find Anti-Americanism Score in perception_of_us data set
#by taking average of favorability score across dates and subtracting it from 100


perception_of_us_AVG <- mutate(perception_of_us, AntiAmericanismScore = 100-rowMeans(perception_of_us[3:18], na.rm = TRUE))

#rename column in perception_of_us_AVG so AntiAmericanismScore column can be merged to After1991TIESv4
perception_of_us_AVG <- perception_of_us_AVG %>% 
  rename(
     targetstate = `COW Code`
  )

#Merge average column from perception_of_us_AVG data set to After1991TIESv4 data set


MergedTable <- merge(After1991TIESv4, perception_of_us_AVG[, c("targetstate", "COUNTRY", "AntiAmericanismScore")], by="targetstate")

#Create new column containing success variable based off final outcome variable
#Success is coded as 1 = successful and 0 = unsuccessful 
#Of the 10 final outcomes the following will be coded successful
###-1-partial acquiescence by target state to threat
###-2-complete acquiescence by target to threat
###-5-negotiated settlement
###-6-partial acquiescence by the target state following sanctions imposition
###-7-total acquiescence by target state following sanctions imposition
###-10-negotiated settlement following sanctions imposition

#Loop through data frame
loopedframe <- MergedTable
for (i in 1:nrow(loopedframe)){
  
  if (is.na(loopedframe$finaloutcome[i])){
    #Create success variable and assign NA
    loopedframe$success[i] <- loopedframe$finaloutcome[i]
  }
  else if (loopedframe$finaloutcome[i] == 1 || loopedframe$finaloutcome[i] == 2 || loopedframe$finaloutcome[i] == 5 || loopedframe$finaloutcome[i] == 6 || loopedframe$finaloutcome[i] == 7 || loopedframe$finaloutcome[i] == 10){
    #Create success variable and assign 1
    loopedframe$success[i] <- 1
  } 
  else if (loopedframe$finaloutcome[i] == 3 || loopedframe$finaloutcome[i] == 4 || loopedframe$finaloutcome[i] == 8 || loopedframe$finaloutcome[i] == 9)
  {
    #Create success variable and assign 0
    loopedframe$success[i] <- 0
  }
  
}


#Calculate distance between start date and end date and assign to new variable (lengthofcase)

loopedframedates <- loopedframe
for (i in 1:nrow(loopedframe)){
  startdate <- paste(loopedframedates$startyear[i],"-",loopedframedates$startmonth[i],"-",loopedframedates$startday[i],sep="")
  enddate <- paste(loopedframedates$endyear[i],"-",loopedframedates$endmonth[i],"-",loopedframedates$endday[i],sep="")

  date1 <- as.Date(startdate,  format="%Y-%m-%d", tz="UTC")
  date2 <- strptime(enddate, format="%Y-%m-%d", tz="UTC")
  loopedframedates$lengthofcase[i] <- as.numeric(difftime(as.POSIXct(date2), as.POSIXct(date1, tz="UTC"), units="days"))
}

#Assign AntiAmericanismEconSanctions to manipulated data frame
AntiAmericanismEconSanctions <- loopedframedates

#Sort columns to make the data easier to read
AAES <- AntiAmericanismEconSanctions[,c("caseid", "targetstate", "COUNTRY", "imposition", "threat", "finaloutcome", "success", "startmonth", "startday", "startyear", "endmonth", "endday", "endyear", "lengthofcase", "AntiAmericanismScore")]


