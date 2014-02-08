##Libaries needed##
library(ggplot2)
library(gridExtra)
library(scales)

#If working off harddisk
setwd("~/Dartmouth/Manuscripts/MME/Data/")
#data<-read.delim("Data110113.txt")# Don't use text files!
data<-read.csv("Data111813.csv",dec=".")

##If working off dropbox, using direct xls read
#setwd("Dropbox/Mass Kills Paper/QC Database/")
#library(xlsx)
#data<-read.xlsx("Data110113.xls","working")


#Check out structure of data
colnames(data)
str(data)

#When do MME start? Working with start data, code as date in .csv file
#remove NAs for start date and focus on complete dates
s1data<-data[data$Start.dat.cat=="1" & !is.na(data$Start.dat.cat),]
#convert to date format based on above dataframe
s1data$Final.start.date
s1data$Final.start.date<-as.Date(s1data$Final.start.date,format="%m/%d/%Y")
s1data$Final.start.date #this column can be used for starting dates for any analysis

length(s1data$Final.start.date)


# #THIS NEXT PART IS A BIT OF A DIGRESSION, BUT MAKES A SEASON COLUMN, WHICH TURNS OUT NOT TO BE NEEDED
# #Add a "Hemisphere specific date" column converting start dates and hemisphere
# #First make a column with dates for south hemisphere by adding six months to these dates
# s1data$HemiData<-rep("",length(s1data$Final.start.date))
# for (i in 1:length(s1data$Final.start.date)){
# if(s1data[i,"Hemisphere"]=="South") {
#   (s1data[i,"HemiDate"]<-(s1data[i,"Final.start.date"]+182))} else {
#   (s1data[i,"HemiDate"]<-s1data[i,"Final.start.date"]) }
#   }
# (s1data$HemiDate)
# #Apparently this origin date works for excel conversions, pay attention to this
# s1data$HemiDate<-as.Date(s1data$HemiDate,format="%m/%d/%Y",origin="1904-01-01")
# 
# #check new column
# s1data[,c("Hemisphere","Final.start.date","HemiDate")]
# 
# #create look up table to convert months and days to seasons
# d = function(month_day) which(lut$month_day == month_day)
# lut = data.frame(all_dates = as.POSIXct("2012-1-1") + ((0:365) * 3600 * 24),
#                  season = NA)
# lut = within(lut, { month_day = strftime(all_dates, "%b-%d") })
# lut[c(d("Jan-01"):d("Mar-20"), d("Dec-21"):d("Dec-31")), "season"] = "winter"
# lut[c(d("Mar-21"):d("Jun-20")), "season"] = "spring"
# lut[c(d("Jun-21"):d("Sep-20")), "season"] = "summer"
# lut[c(d("Sep-21"):d("Dec-20")), "season"] = "autumn"
# rownames(lut) = lut$month_day
# 
# #Convert Hemidate to seasons
# Seasondata<-data.frame(dates=s1data$HemiDate)
# Seasondata<- within(Seasondata, { 
#   season<-  lut[strftime(dates, "%b-%d"), "season"] 
# })
# Seasondata
# Seasondata$season
# #integrate column into s1data dataframe
# s1data$Season<-Seasondata$season
# colnames(s1data)
# s1data$Season
# s1data[,c("Final.start.date","HemiDate","Season")]
# #####END DIGRESSION OF MAKING NEW SEASON COLUMN


##STARTING TO VISUALIZE DATA
##USE FOR ALL ANALYSES WHERE START DATES ARE NEEDED###
##split by# $Diagnoses.course   #Taxa.v.course         #Habitat.course

TimePlot<-ggplot(s1data,aes(x=s1data$Final.start.date,fill=s1data$Habitat.course))
TimePlot<-TimePlot+geom_bar(binwidth=1000,position="stack")
#+scale_x_date(labels=date_format("%m/%d/%Y"), limits=as.Date(c("1945-01-01","2010-01-01")))
TimePlot

#Include all start dates with year, not just those with complete dates

fulldata<-data[data$Start.year!="" & data$Start.year!="1997-2002" & data$Start.year!="1976-1982" & 
data$Start.year!="2003-2005" & data$Start.year!="1997 & 1998" & data$Start.year!="~1977" &
data$Start.year!="~151,000 YA" & data$Start.year!="~1 MYA" & data$Start.year!="~ 1 MYA",]

fulldata$Start.year<-as.Date(fulldata$Start.year,format="%Y")
##Truncate X axis to show data better, here we loose one old observation
fulldata<-fulldata[fulldata$Start.year>"1875-01-01",]


TimePlotFull1<-ggplot(fulldata,aes(x=fulldata$Start.year,fill=fulldata$Diagnoses.course))
TimePlotFull1<-TimePlotFull1+geom_bar(binwidth=1500,position="stack")
TimePlotFull1
TimePlotFull2<-ggplot(fulldata,aes(x=fulldata$Start.year,fill=fulldata$Taxa.v.course))
TimePlotFull2<-TimePlotFull2+geom_bar(binwidth=4000,position="stack")
TimePlotFull2
TimePlotFull3<-ggplot(fulldata,aes(x=fulldata$Start.year,fill=fulldata$Habitat.course))
TimePlotFull3<-TimePlotFull3+geom_bar(binwidth=1000,position="stack")
TimePlotFull3
TimePlotFull4<-ggplot(fulldata,aes(x=fulldata$Start.year,fill=fulldata$Start.season))
TimePlotFull4<-TimePlotFull4+geom_bar(binwidth=1000,position="stack")
TimePlotFull4
plot(data$Year,data$Start.year)

##Tried density plots, probablye too confusion
#TimePlot<-ggplot(s1data,aes(x=Final.start.date,fill=Diagnoses.collapsed))
#TimePlot<-TimePlot+geom_density(position="stack",alpha=.2)
#TimePlot
#Above, are reports of MME increasing through time ()


##Start month on x axis, these data are ONLY the dates with complete dd/mm/YY starting dates
IntrayearPlot<-ggplot(s1data,aes(x=Start.month,fill=Hemisphere))
IntrayearPlot<-IntrayearPlot+geom_bar()
IntrayearPlot
##Start season on x axis********************************
IntrayearPlot1<-ggplot(s1data,aes(x=Start.season,fill=Diagnoses.course))
IntrayearPlot1<-IntrayearPlot1+geom_bar(position="stack")
IntrayearPlot1
IntrayearPlot2<-ggplot(s1data,aes(x=Start.season,fill=Taxa.v.course))
IntrayearPlot2<-IntrayearPlot2+geom_bar(position="stack")
IntrayearPlot2
IntrayearPlot3<-ggplot(s1data,aes(x=Start.season,fill=Habitat.course))
IntrayearPlot3<-IntrayearPlot3+geom_bar(position="stack")
IntrayearPlot3
IntrayearPlot4<-ggplot(s1data,aes(x=Start.season,fill=Continent))
IntrayearPlot4<-IntrayearPlot4+geom_bar(position="stack")
IntrayearPlot4

#Total mortality numbers ***************************************
#Converting mortality data to numeric data, # convert csv cells to all numbers
mdata<-data[!is.na(data$Mortality.cons.value),]
logmdata<-log(mdata$Mortality.cons.value)
(mdata$Mortality.cons.value)

MPlot1<-ggplot(mdata,aes(x=log(Mortality.cons.value,10),fill=mdata$Diagnoses.course))
MPlot1<-MPlot1+geom_bar(position="stack",binwidth=.25)
MPlot1
MPlot2<-ggplot(mdata,aes(x=log(Mortality.cons.value,10),fill=mdata$Taxa.v.course))
MPlot2<-MPlot2+geom_bar(position="stack",binwidth=.25)
MPlot2
MPlot3<-ggplot(mdata,aes(x=log(Mortality.cons.value,10),fill=mdata$Habitat.course))
MPlot3<-MPlot3+geom_bar(position="stack",binwidth=.25)
MPlot3
MPlot4<-ggplot(mdata,aes(x=log(Mortality.cons.value,10),fill=mdata$Start.season))
MPlot4<-MPlot4+geom_bar(position="stack",binwidth=.25)
MPlot4
##this could be split out a number of different ways

#Old fassion way
hist(log(mdata$Mortality.cons.value,10))
hist(logmdata)
levels(data$Mortality.cons.value)
which(data$Mortality.cons.value=="NS")
data[1186,]

###Weight mortality numbers
wdata<-data[data$Weight.ton.value>0 & !is.na(data$Weight.ton.value),]
wdata$Weight.ton.value
hist(wdata$Weight.ton.value)
logwdata<-log(wdata$Weight.ton.value)
hist(logwdata)

WPlot<-ggplot(wdata,aes(x=log(Weight.ton.value,10),fill=wdata$Diagnoses.course))
WPlot<-WPlot+geom_bar(position="stack",binwidth=.5)
WPlot

summary(wdata$Weight.ton.value)

wdata[,c("Weight.ton.value","Taxa")]

###Rate data#######*********************************************
rdata<-data[data$Mortality.Org.Rate>0 & !is.na(data$Mortality.Org.Rate),]
rdata$Mortality.Org.Rate
hist(rdata$Mortality.Org.Rate)
logrdata<-log(rdata$Mortality.Org.Rate)
hist(logrdata)

RPlot1<-ggplot(rdata,aes(x=log(Mortality.Org.Rate,10),fill=rdata$Diagnoses.course))
RPlot1<-RPlot1+geom_bar(position="stack",binwidth=.25)
RPlot1
RPlot2<-ggplot(rdata,aes(x=log(Mortality.Org.Rate,10),fill=rdata$Taxa.v.course))
RPlot2<-RPlot2+geom_bar(position="stack",binwidth=.25)
RPlot2
RPlot3<-ggplot(rdata,aes(x=log(Mortality.Org.Rate,10),fill=rdata$Habitat.course))
RPlot3<-RPlot3+geom_bar(position="stack",binwidth=.25)
RPlot3
RPlot4<-ggplot(rdata,aes(x=log(Mortality.Org.Rate,10),fill=rdata$Start.season))
RPlot4<-RPlot4+geom_bar(position="stack",binwidth=.25)
RPlot4

###Population lost data### ***************************************
pdata<-data[!is.na(data$Percent.pop.lost),]
nrow(pdata)
(pdata$Percent.pop.lost)


hist(pdata$Percent.pop.lost)

PPlot1<-ggplot(pdata,aes(x=Percent.pop.lost,fill=pdata$Diagnoses.course))
PPlot1<-PPlot1+geom_bar(position="stack",binwidth=7)
PPlot1
PPlot2<-ggplot(pdata,aes(x=Percent.pop.lost,fill=pdata$Taxa.v.course))
PPlot2<-PPlot2+geom_bar(position="stack",binwidth=7)
PPlot2
PPlot3<-ggplot(pdata,aes(x=Percent.pop.lost,fill=pdata$Habitat.course))
PPlot3<-PPlot3+geom_bar(position="stack",binwidth=7)
PPlot3
PPlot4<-ggplot(pdata,aes(x=Percent.pop.lost,fill=pdata$Start.season))
PPlot4<-PPlot4+geom_bar(position="stack",binwidth=7)
PPlot4

pdata[,c("Percent.pop.lost","Habitat")]



#Length of MME data####
ldata<-data[data$Duration.est>0 & !is.na(data$Duration.est),]
ldata$Duration.est
hist(ldata$Duration.est)
logldata<-log(ldata$Duration.est)
hist(logldata)

LPlot<-ggplot(ldata,aes(x=log(Duration.est,10),fill=ldata$Diagnoses.course))
LPlot<-LPlot+geom_bar(position="stack",binwidth=.25)
LPlot

####################################
#Simple hist data for main categories
causedata<-data[!is.na(data$Diagnoses.course),]
causePlot<-ggplot(causedata,aes(x=Diagnoses.course,fill=Start.season))
causePlot<-causePlot+geom_bar(position="stack",binwidth=.25)
causePlot

humancauses<-causedata[causedata$Diagnoses.course=="Human perturbation",]
humancauses<-humancauses[,c("Diagnosis.raw","Diagnoses.course")]
humancauses
humancausePlot2<-ggplot(causes,aes(x=Diagnoses.course,fill=Diagnosis.raw))
humancausePlot2<-humancausePlot2+geom_bar(position="stack",binwidth=.25)
humuancausePlot2

#Figure out most abundant causes
discauses<-causedata[causedata$Diagnoses.course=="Disease",]
discauses<-discauses[,c("Diagnoses.collapsed","Diagnoses.course")]
sort(summary(discauses$Diagnoses.collapsed),decreasing=TRUE)
length(discauses$Diagnoses.collapsed)


habitatdata<-data[!is.na(data$Habitat.course),]
habitatPlot<-ggplot(habitatdata,aes(x=Habitat.course,fill=Habitat.course))
habitatPlot<-habitatPlot+geom_bar(position="stack",binwidth=.25)
habitatPlot


taxadata<-data[!is.na(data$Taxa.v.course),]
taxaPlot<-ggplot(taxadata,aes(x=Taxa.v.course,fill=Taxa.v.course))
taxaPlot<-taxaPlot+geom_bar(position="stack",binwidth=.25)
taxaPlot


seasondata<-data[!is.na(data$Start.season) ,]
seasonPlot<-ggplot(seasondata,aes(x=Start.season,fill=Start.season))
seasonPlot<-seasonPlot+geom_bar(position="stack",binwidth=.25)
seasonPlot

continentdata<-data[!is.na(data$Continent) ,]
continentPlot<-ggplot(continentdata,aes(x=Continent,fill=Continent))
continentPlot<-continentPlot+geom_bar(position="stack",binwidth=.25)
continentPlot



###Visualizing multiple graphs

#Simple patterns
grid.arrange(taxaPlot, habitatPlot, continentPlot,ncol=1)
#grid.arrange(taxaPlot, causePlot, habitatPlot, seasonPlot)

###Intraanual patterns
grid.arrange(IntrayearPlot2,IntrayearPlot3,IntrayearPlot4,IntrayearPlot1)

##Interannual patterns
grid.arrange(TimePlotFull2,TimePlotFull3,TimePlotFull4,TimePlotFull1)

#Mortality data, by habitats, causes, and taxa
grid.arrange(MPlot2,MPlot3,MPlot4,MPlot1,ncol=2)
#Starvation=8, dessication=23, other=10

#Percent Population lost Patterns###
grid.arrange(PPlot2,PPlot3,PPlot4,PPlot1)

#Rate of mortality patterns####
grid.arrange(RPlot2,RPlot3,RPlot4,RPlot1)

grid.arrange(RPlot1,MPlot1)




#correlation between numbers dead and pop lost
# cdata<-data[!is.na(data$Percent.pop.lost) & !is.na(data$Mortality.cons.value) & data$Mortality.cons.value>0,]
# 
# cdata$Percent.pop.lost
# cdata$Mortality.cons.value<-as.numeric(cdata$Mortality.cons.value)
# plot(log(cdata$Mortality.cons.value),cdata$percent.pop.lost)
# cmod<-lm(cdata$ercent.pop.lost~cdata$Mortality.cons.value)
# summary(cmod)

#Find problem cells
#which(data$Year>data$Start.year)

##Check to see if start years are lower than pub years
#pubyear<-as.numeric(substr(data$Year,1,4))
#styear<-as.numeric(substr(data$Start.year,1,4))
#new<-cbind(pubyear,styear)
#which(new[,1]<new[,2])

#This is me adding new text on 2-08