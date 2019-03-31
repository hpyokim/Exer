# loading packages
library(tidyverse)
library(knitr)

# import dataset 
ABIA = read.csv("../data/ABIA.csv")
summary(ABIA)

# transform data
ABIA$DOW = cut(ABIA$DayOfWeek, breaks=7, labels=c("MON", "TUE", "WED", "THU", "FRI", "SAT", "SUN"))
ABIA$MN = cut(ABIA$Month, breaks=12, labels=c("JAN", "FEB", "MAR", "APR", "MAY", "JUN", "JUL", "AUG", "SEP", "OCT", "NOV", "DEC"))
ABIA[ABIA$CancellationCode=='A', 'CC']="carrier"
ABIA[ABIA$CancellationCode=='B', 'CC']="weather"
ABIA[ABIA$CancellationCode=='C', 'CC']="NAS"
ABIA[ABIA$Origin=='AUS', 'FromAUS']=1
ABIA[ABIA$Dest=='AUS', 'FromAUS']=0


## Cancellation 
#Cancel_Month_rate = round(prop.table(xtabs(~Cancelled + Month, data=ABIA), margin=2)*100, digits=2)
#kable(Cancel_Month_rate, caption="Cancellation Rate by Month")
#Cancel_DOW_rate = round(prop.table(xtabs(~Cancelled + DOW, data=ABIA), margin=2)*100, digits=2)
#kable(Cancel_DOW_rate, caption="Cancellation Rate by Day")
#Cancel_rate = round(prop.table(xtabs(~Cancelled + DOW+ Month, data=ABIA), margin=2)*100, digits=2)
#kable(Cancel_rate, caption="Cancellation Rate by Day")
# mosaicplot(~ Month + DOW + Cancelled, data=ABIA)

### cancellation rate
ABIA_Cc_D = ABIA %>%
  group_by(DOW) %>%
  summarize(Cancel_Rate=round(sum(Cancelled==1)/n()*100,digits=2))
ggplot() + geom_col(data=ABIA_Cc_D, aes(x=DOW, y=Cancel_Rate))
kable(ABIA_Cc_D, caption="Cancellation Rate by Day")

ABIA_Cc_M = ABIA %>%
  group_by(MN) %>%
  summarize(Cancel_Rate=round(sum(Cancelled==1)/n()*100,digits=2))
ggplot() + geom_col(data=ABIA_Cc_M, aes(x=MN, y=Cancel_Rate))
ABIA_Cc_M=table(ABIA_Cc_M$Cancel_Rate~ABIA_Cc_M$MN)
kable(ABIA_Cc_M, caption="Cancellation Rate by Month")

ABIA_Cc = ABIA %>%
  group_by(DOW, MN) %>%
  summarize(Cancel_Rate=round(sum(Cancelled==1)/n()*100,digits=2))
ggplot() + geom_col(data=ABIA_Cc, aes(x=DOW, y=Cancel_Rate)) + facet_wrap(~MN, nrow=6)
ABIA_Cc=xtabs(Cancel_Rate ~ DOW + MN, data=ABIA_Cc)
kable(ABIA_Cc, caption="Cancellation Rate by Month and Day")

ABIA_Cc_C = ABIA %>%
  group_by(UniqueCarrier) %>%
  summarize(Cancel_Rate=round(sum(Cancelled==1)/n()*100,digits=2))
ggplot() + geom_col(data=ABIA_Cc_C, aes(x=UniqueCarrier, y=Cancel_Rate))
kable(ABIA_Cc_C, caption="Cancellation Rate by Carrier")

ABIA_Cc_A = ABIA %>%
  group_by(FromAUS) %>%
  summarize(Cancel_Rate=round(sum(Cancelled==1)/n()*100,digits=2))
ggplot() + geom_col(data=ABIA_Cc_A, aes(x=FromAUS, y=Cancel_Rate))
kable(ABIA_Cc_A, caption="Cancellation Rate for departure and arrival")


### cancellation reason
#(Cancel_reason = round(prop.table(xtabs(~Cancelled + CancellationCode, data=ABIA), margin=1)*100, digits=2))

ABIA_CC = ABIA %>%
  filter(Cancelled==1) %>%
  group_by(CC) %>%
  summarize(Number=n(),Rate=round(n()/length(which(ABIA$Cancelled==1))*100, digits=2))
ggplot() + 
  geom_bar(data=ABIA_CC, aes(x="", y=Rate, fill=CC), width=1, stat="identity") +
  coord_polar("y", start=0)
kable(ABIA_CC, caption="Cancellation Reason")

## month/day + cancel_code

ABIA_C = subset(ABIA, Cancelled==1)
ggplot() + geom_bar(data=ABIA_C, aes(x=MN, fill=CC))
ggplot() + geom_bar(data=ABIA_C, aes(x=DOW, fill=CC))
ggplot() + geom_bar(data=ABIA_C, aes(x=UniqueCarrier, fill=CC))


## Delay. Begin by cleaning the canceled and diveted data
#ABIA_DL=ABIA[(ABIA$Cancelled==0&ABIA$Diverted==0),]



