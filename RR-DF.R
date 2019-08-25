
library(dplyr);library(lubridate);library(stringr);

setwd("G:/CAP-Team/Domestic Team/Harrison/Data_Downloads2/RR_Jobs")
RR=read.delim("JOB003147.txt", header = F)


RR<-distinct(RR);RR<-RR[6:dim(RR)[1],]

RR<-str_split(RR, "\\s{2,}");

for(i in 1:length(RR)){
  RR[[i]]=RR[[i]][c(-1)]}

RR_df<-data.frame(matrix(ncol=8, nrow = 0));x<-c("DEPART_YEAR", "PROMOTION_DESC", "RES_ID", "DEPT_DATE", "BK_DATE", "MILES", "PAX_COUNT", "MILES_PER_RES")
colnames(RR_df)<-x;x2<-RR[[1]][2];x3<-RR[[1]][1]

Depart_yr<-NULL;Promotion_desc<-NULL

for(i in 1:length(RR)){
  RR_df[nrow(RR_df)+i,];RR_df[i,]<-"None"
  
  if(length(RR[[i]])==8){
    Depart_yr=RR[[i]][1];Promotion_desc=RR[[i]][2]
    RR_df$DEPART_YEAR[i]=RR[[i]][1]
    RR_df$PROMOTION_DESC[i]=RR[[i]][2]
    RR_df$RES_ID[i]=RR[[i]][3]
    RR_df$DEPT_DATE[i]=RR[[i]][4]
    RR_df$BK_DATE[i]=RR[[i]][5]
    RR_df$MILES[i]=RR[[i]][6]
    RR_df$PAX_COUNT[i]=RR[[i]][7]
    RR_df$MILES_PER_RES[i]=RR[[i]][8]}
  else if(length(RR[[i]])==6){
    RR_df$DEPART_YEAR[i]=Depart_yr
    RR_df$PROMOTION_DESC[i]=Promotion_desc
    RR_df$RES_ID[i]=RR[[i]][1]
    RR_df$DEPT_DATE[i]=RR[[i]][2]
    RR_df$BK_DATE[i]=RR[[i]][3]
    RR_df$MILES[i]=RR[[i]][4]
    RR_df$PAX_COUNT[i]=RR[[i]][5]
    RR_df$MILES_PER_RES[i]=RR[[i]][6]
  }
  else if(length(RR[[i]])==7){
    Depart_yr="None";Promotion_desc=RR[[i]][1]
    RR_df$DEPART_YEAR[i]="None"
    RR_df$PROMOTION_DESC[i]=RR[[i]][1]
    RR_df$RES_ID[i]=RR[[i]][2]
    RR_df$DEPT_DATE[i]=RR[[i]][3]
    RR_df$BK_DATE[i]=RR[[i]][4]
    RR_df$MILES[i]=RR[[i]][5]
    RR_df$PAX_COUNT[i]=RR[[i]][6]
    RR_df$MILES_PER_RES[i]=RR[[i]][7]
  }else{next}
}


write.csv(RR_df, "JOB003147.csv")

