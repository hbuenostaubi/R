install.packages("writexl")
install.packages("readxl")
install.packages("dplyr") 
install.packages("xlsx")
install.packages("lubridate")
library(lubridate)  ###lubridate
library(writexl)
library(readxl)##read xls
library(xlsx)
library(dplyr) ###functions below dplyr:: "%>%" pipe symbol for R to chain functions
options(java.parameters = "-Xmx2048m")  ## memory set to 2 GB
library(XLConnect)

########## Loading Functions  ##############################################################

# overloading operator
'%=%' = function(l, r, ...) UseMethod('%=%')

# Binary Operator
'%=%.lbunch' = function(l, r, ...) {
  Envir = as.environment(-1)
  
  if (length(r) > length(l))
    warning("RHS has more vars than LHS. Only first", length(l), "used.")
  
  if (length(l) > length(r))  {
    warning("LHS has more vars than RHS. RHS will be repeated.")
    r <- extendToMatch(r, l)
  }
  
  for (II in 1:length(l)) {
    do.call('<-', list(l[[II]], r[[II]]), envir=Envir)
  }
}

# Used if LHS is larger than RHS
extendToMatch <- function(source, destin) {
  s <- length(source)
  d <- length(destin)
  
  # Assume that destin is a length when it is a single number and source is not
  if(d==1 && s>1 && !is.null(as.numeric(destin)))
    d <- destin
  
  dif <- d - s
  if (dif > 0) {
    source <- rep(source, ceiling(d/s))[1:d]
  }
  return (source)
}

# Grouping the left hand side
g = function(...) {
  List = as.list(substitute(list(...)))[-1L]
  class(List) = 'lbunch'
  return(List)
}

     ###########################################################################################
################################################################################################

getwd()

setwd("s:/ProdDev/123data/PLANNING/HBueno/Messing")
RG=read_xlsx("AV Sched - 20180911-Try.xlsx")

########## Removing B Matches, Basic Economy Flights and Non-matching flights  #############

for(i in 1:length(RG$`FUNJETPKG SearchLevel`)){
  if(RG$`FUNJETPKG IsFlightMatch`[i]=="No"| RG$`FUNJETPKG Basic Economy Fare`[i]=="Yes" | RG$`FUNJETPKG SearchLevel`[i]=="B") RG$`Variance With FUNJETPKG`[i]="-"
}#Funjet
for(i in 1:length(RG$`FUNJETPKG SearchLevel`)){
  if(RG$`DELTAVACATIONSPKG IsFlightMatch`[i]=="No"| RG$`DELTAVACATIONSPKG Basic Economy Fare`[i]=="Yes" | RG$`DELTAVACATIONSPKG SearchLevel`[i]=="B") RG$`Variance With DELTAVACATIONSPKG`[i]="-"
}#Delta
for(i in 1:length(RG$`FUNJETPKG SearchLevel`)){#OTA
  if(RG$`BOOKITDOTCOMPKG IsFlightMatch`[i]=="No"| RG$`BOOKITDOTCOMPKG Basic Economy Fare`[i]=="Yes" | RG$`BOOKITDOTCOMPKG SearchLevel`[i]=="B") RG$`Variance With BOOKITDOTCOMPKG`[i]="-"
}#Bookit
for(i in 1:length(RG$`FUNJETPKG SearchLevel`)){
  if(RG$`AAVACATIONSPKG IsFlightMatch`[i]=="No"| RG$`AAVACATIONSPKG Basic Economy Fare`[i]=="Yes"|RG$`AAVACATIONSPKG SearchLevel`[i]=="B") RG$`Variance With AAVACATIONSPKG`[i]="-"
}#AAVacations
for(i in 1:length(RG$`FUNJETPKG SearchLevel`)){
  if(RG$`VACATIONEXPRESSPKG IsFlightMatch`[i]=="No"| RG$`VACATIONEXPRESSPKG Basic Economy Fare`[i]=="Yes" | RG$`VACATIONEXPRESSPKG SearchLevel`[i]=="B") RG$`Variance With VACATIONEXPRESSPKG`[i]="-"
}#VE
for(i in 1:length(RG$`FUNJETPKG SearchLevel`)){
  if(RG$`JETBLUEPKG IsFlightMatch`[i]=="No"| RG$`JETBLUEPKG Basic Economy Fare`[i]=="Yes" | RG$`JETBLUEPKG SearchLevel`[i]=="B") RG$`Variance With JETBLUEPKG`[i]="-"
}#B6
for(i in 1:length(RG$`FUNJETPKG SearchLevel`)){#OTA
  if(RG$`EXPEDIAPKG IsFlightMatch`[i]=="No"| RG$`EXPEDIAPKG Basic Economy Fare`[i]=="Yes" | RG$`EXPEDIAPKG SearchLevel`[i]=="B") RG$`Variance With EXPEDIAPKG`[i]="-"
}#Expedia
for(i in 1:length(RG$`FUNJETPKG SearchLevel`)){
  if(RG$`VACATIONSUNITEDPKG IsFlightMatch`[i]=="No"| RG$`VACATIONSUNITEDPKG Basic Economy Fare`[i]=="Yes" | RG$`VACATIONSUNITEDPKG SearchLevel`[i]=="B") RG$`Variance With VACATIONEXPRESSPKG`[i]="-"
}#VacationsUA

RG$IS<-if_else(RG$Variance.With.JETBLUEPKG=="-"&RG$Variance.With.VACATIONEXPRESSPKG=="-"&RG$Variance.With.VACATIONSUNITEDPKG=="-"&RG$Variance.With.DELTAVACATIONSPKG=="-"&RG$Variance.With.TRAVIMPPKG=="-"&RG$Variance.With.FUNJETPKG=="-"&RG$Variance.With.EXPEDIAPKG=="-"&RG$Variance.With.BOOKITDOTCOMPKG=="-"&RG$Variance.With.AAVACATIONSPKG=="-",0,1)

RG$Month=month(as.POSIXlt(RG$Departure.Date, format = "%m/%d/%Y"));RG$Year=year(as.POSIXlt(RG$Departure.Date, format = "%m/%d/%Y"));RG$bind=paste(RG$Vendor,RG$Month,RG$Year, sep='')


#################################################################################


RG$datediff=as.numeric(as_date(RG$`Departure Date`)-as_date(RG$ShopDate))

for(i in 1:length(RG$datediff)){
  if(RG$datediff[i]<=25) {RG$N1[i]=4} 
  else if(RG$datediff[i]>25 && RG$datediff[i]<=90){RG$N1[i]=3} 
  else if(RG$datediff[i]>90 && RG$datediff[i]<=190){RG$N1[i]=2} 
  else if(RG$datediff[i]>190 && RG$datediff[i]<=280){RG$N1[i]=1} 
  else if(RG$datediff[i]>280){RG$N1[i]=0}}

RG$Hotel.Sales=as.double(sub("%","",RG$Hotel.Sales))/100   #######only do once!  ###  Dependant on how we continue this format

for(i in 1:length(RG$Hotel.Sales)){
  if(RG$Hotel.Sales[i]<=-.11){RG$N2[i]=3}
  else if(RG$Hotel.Sales[i]>-.10 && RG$Hotel.Sales[i]<=0){RG$N2[i]=2}
  else if(RG$Hotel.Sales[i]>0.0 && RG$Hotel.Sales[i]<=.19){RG$N2[i]=1}
  else {RG$N2[i]=0}}


#######################################################################################
RG$Lowest.Tour.Operator=as.numeric(gsub('[$,]','',RG$Lowest.Tour.Operator))
RG$Lowest.OTA=as.numeric(gsub('[$,]','',RG$Lowest.OTA))

LoopupAvgPrice=RG%>%
  group_by(Vendor,Month,Year)%>%
  filter(`Variance With AAVACATIONSPKG`!="-")%>%filter(`Variance With FUNJETPKG`!="-")%>%filter(`Variance With TRAVIMPPKG`!="-")%>%
  filter(`Variance With TRAVIMPPKG`!="-")%>%filter(`Variance With BOOKITDOTCOMPKG`!="-")%>%filter(`Variance With DELTAVACATIONSPKG`!="-")%>%
  filter(`Variance With EXPEDIAPKG`!="-")%>%
  summarise(mean(`Variance With AAVACATIONSPKG`,`Variance With EXPEDIAPKG`,`Variance With DELTAVACATIONSPKG`,`Variance With BOOKITDOTCOMPKG`,`Variance With TRAVIMPPKG`,`Variance With TRAVIMPPKG`,`Variance With FUNJETPKG`))

RG$bind=paste(RG$Vendor,RG$Month, RG$Year, sep='')

LookupOTA=RG%>%group_by(Vendor,Month,Year)%>%filter(Lowest.OTA!=0)%>%summarise(mean(OTA.Margin))
LookupOTA$bind=paste(LookupOTA$Vendor, LookupOTA$Month, LookupOTA$Year, sep='')

LookupTO=RG%>%group_by(Vendor,Month,Year)%>%filter(Lowest.Tour.Operator!=0)%>%summarise(mean(TO.Margin))
LookupTO$bind=paste(LookupTO$Vendor, LookupTO$Month, LookupTO$Year, sep='')

LookupRGSuggested=aggregate(RG.Suggested.Margin~Vendor+Month+Year, RG, mean)
LookupRGSuggested$bind=paste(LookupRGSuggested$Vendor,LookupRGSuggested$Month, LookupRGSuggested$Year, sep='')
#adding avg for OTA and TO to RG sheet
RG$OTA.avg=LookupOTA$`mean(OTA.Margin)`[match(RG$bind, LookupOTA$bind)]
RG$TO.avg=LookupTO$`mean(TO.Margin)`[match(RG$bind, LookupTO$bind)]
RG$RG.sug.avg=LookupRGSuggested$RG.Suggested.Margin[match(RG$bind, LookupRGSuggested$bind)]

################################################### Margin Of Error ###########

LookupOTA_count=RG%>%group_by(Vendor,Month,Year)%>%filter(Lowest.OTA!=0)%>%count(Month)
LookupOTA_count$bind=paste(LookupOTA_count$Vendor, LookupOTA_count$Month, LookupOTA_count$Year, sep='')
RG$OTA.count=LookupOTA_count$n[match(RG$bind, LookupOTA_count$bind)]

LookupTO_count=RG%>%group_by(Vendor,Month,Year)%>%filter(Lowest.Tour.Operator!=0)%>%count(Month)
LookupTO_count$bind=paste(LookupTO_count$Vendor, LookupTO_count$Month, LookupTO_count$Year, sep='')
RG$TO.Count=LookupTO_count$n[match(RG$bind, LookupTO_count$bind)]

LookupRGSuggested=aggregate(RG.Suggested.Margin~Vendor+Month+Year, RG, mean)
LookupRGSuggested$bind=paste(LookupRGSuggested$Vendor,LookupRGSuggested$Month, LookupRGSuggested$Year, sep='')

LookupOTA_var=RG%>%group_by(Vendor,Month,Year)%>%filter(Lowest.OTA!=0)%>%summarise(var(OTA.Margin))
LookupOTA_var$bind=paste(LookupOTA_var$Vendor, LookupOTA_var$Month, LookupOTA_var$Year, sep='')
LookupTO_var=RG%>%group_by(Vendor,Month,Year)%>%filter(Lowest.Tour.Operator!=0)%>%summarise(var(TO.Margin))
LookupTO_var$bind=paste(LookupTO_var$Vendor, LookupTO_var$Month, LookupTO_var$Year, sep='')

LookupOTA_var$OTA.count=LookupOTA_count$n[match(LookupOTA_var$bind, LookupOTA_count$bind)]
LookupTO_var$TO.Count=LookupTO_count$n[match(LookupTO_var$bind, LookupTO_count$bind)]
LookupOTA_var$Error=qt(.8, df = LookupOTA_var$OTA.count)*sqrt(LookupOTA_var$`var(OTA.Margin)`/(LookupOTA_var$OTA.count-1))#60% confidence(Two Tail)
LookupTO_var$Error=qt(.8, df = LookupTO_var$TO.Count)*sqrt(LookupTO_var$`var(TO.Margin)`/(LookupTO_var$TO.Count-1))#60% confidence(Two Tail)
RG$TO.Error=LookupTO_var$Error[match(RG$bind,LookupTO_var$bind)]
RG$TO.var=LookupTO_var$`var(TO.Margin)`[match(RG$bind,LookupTO_var$bind)]
RG$OTA.Error=LookupOTA_var$Error[match(RG$bind,LookupOTA_var$bind)]
RG$OTA.var=LookupOTA_var$`var(OTA.Margin)`[match(RG$bind,LookupOTA_var$bind)]  

##############################################################################################################

write_xlsx(RG, "testingEX.xlsx")