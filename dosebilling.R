library(tidyverse)
library(readr)
library(lubridate)
library(readxl)


source('C:/Users/vance/Documents/myR/functions/getSQL.r')

dose<-"SELECT [StudID]
      ,[Dest] AS 'Farm Name'
,RIGHT([Dest],4) AS 'Type'
,[Date_Shipped] as 'Shipping Date'
,[Breed]
,[Doses]
,[BatchID] AS 'Batch_ID'
,[Accounting] AS 'Dept #'
FROM [Intranet].[dbo].[Boar_Distrib]
WHERE [Date_Shipped] > '01-01-22 00:00:00'
AND [StudID] in ('High Desert','MB 7081','MB 7082','MB 7092','MB 7093','MB 7094',
'MBW Cimarron','MBW Cyclone','SPG62','MBW Yuma','Princeton','Skyline Boar Stud','SPGNC','SPGVA','SPGTX','SPG9644')
AND [Dest]!='* TRASH *'"
doseraw<-getSQL('Intranet', query=dose)

dose0<-read_csv('key.csv')

dose1<-doseraw %>% 
  mutate('Dose Size'=ifelse(`Type`=='TRAD','3 Bill',
                            ifelse(`Type`=='Trad','3 Bill',
                                   ifelse(`Type`=='trad','3 Bill',
                                          ifelse(`Type`=='PCAI','2 Bill',
                                                 ifelse(`Type`=='PCIA','2 Bill',
                                                        ifelse(`Type`=='pcai','2 Bill',
                                                               ifelse(`Type`=='Trad.','3 Bill',
                                                                      ifelse(`Type`=='PCA','2 Bill','Unknown')))))))))

dose1a<-left_join(x = dose1,y = dose0,by=c("StudID"="StudID"))

dose1a$`Dept #`<-trimws(dose1a$`Dept #`,"r")

dose1b<-read_csv("sowfarms.csv", col_types = cols(cc = col_character()))

dose1c<-left_join(x = dose1a,y = dose1b, by=c("Dept #"="cc"))

order.dd<-order(dose1c$Site,dose1c$`Dose Size`,dose1c$`Farm Name`)

# doseff<-dose1c %>%
#   filter(StudID%in%c('MB 7083','MB 7084'))
# 
# doseff$`Shipping Date`<-as.Date(doseff$`Shipping Date`)
# 
# write_csv(x = doseff, file = 'joe.csv', append = FALSE)

dose2<-dose1c[order.dd,]

dose2<-dose2[c(10,8,2,7,5,4,9,6,11,12)]

dose2$`Shipping Date`<-as.Date(dose2$`Shipping Date`)


dose3<-dose2 %>% 
  filter(`Shipping Date`>=floor_date(today(),unit = "week",week_start = 1)-7) %>% 
  filter(`Shipping Date`<floor_date(today(),unit = "week",week_start = 1)) %>% 
  filter(!is.na(`Dept #`),
         `Dose Size`!='Unknown',
         !is.na(`Destination #`))

dose4<-dose2 %>%
  filter(`Shipping Date`>=floor_date(today(),unit = "week",week_start = 1)-7) %>% 
  filter(`Shipping Date`<floor_date(today(),unit = "week",week_start = 1)) %>% 
  filter(is.na(`Dept #`) | `Dose Size`=='Unknown' | is.na(`Destination #`))

write_csv(x = dose4,path = 'fixme.csv',append = FALSE)

########## STOP AND FIX ERRORS ######################

dose5 <- read_csv("fixme.csv", col_types = cols(`Shipping Date` = col_date(format = "%m/%d/%Y")))

dose6<-rbind(dose3,dose5)

dose7<-read_csv('dosebillingweek.csv')

# dose7a<-dose7 %>% 
#   ungroup() %>% 
#   group_by(`Dept #`) %>% 
#   filter(`Shipping Date`==max(`Shipping Date`))

write_csv(x = dose6,'dosebillingweek.csv')

####################### ADD MANUAL DOSES TO WEEKLY FILE ##################

dose8<-read_csv('dosebillingweek.csv')

dose8$`Dept #`<-as.character(dose8$`Dept #`)

dose9<-dose7 %>% 
  group_by(`Dept #`) %>% 
  summarise('Previous'=sum(Doses))

dose10<-dose8 %>% 
  group_by(`Dept #`) %>% 
  summarise('Current'=sum(Doses))

dose11<-full_join(x = dose9,y = dose10,by=c("Dept #"="Dept #"))

dose11$Variance<-dose11$Current-dose11$Previous

dose12<-dose11 %>% 
  filter(is.na(Variance) | abs(Variance)>50)

dose13<-left_join(x = dose12,y = dose8,by=c("Dept #"="Dept #"))
# 
# dose11<-dose9 %>% 
#   filter(`Shipping Date`>=floor_date(today(),unit = "week",week_start = 1)-7) %>% 
#   filter(`Shipping Date`<floor_date(today(),unit = "week",week_start = 1)) %>% 
#   group_by(`Dept #`) %>% 
#   summarise('Billing Week'=sum(Doses))
#   
# dose12<-dose9 %>% 
#   filter(`Shipping Date`>=floor_date(today(),unit = "week",week_start = 1)-14) %>% 
#   filter(`Shipping Date`<floor_date(today(),unit = "week",week_start = 1)-7) %>% 
#   group_by(`Dept #`) %>% 
#   summarise('Previous Week'=sum(Doses))
# 
# dose13<-full_join(x = dose11,y = dose12,by=c("Dept #"="Dept #"))
# dose13$`Variance`<-((dose13$`Billing Week`-dose13$`Previous Week`)/dose13$`Billing Week`)*100
# 
# dose14<-left_join(x = dose13,y = dose7,by=c("Dept #"="Accounting Number"))
# dose14$`SPG Order`<-dose14$Total
# dose14$`Order Variance`<-dose14$`Billing Week`-dose14$`SPG Order`
# 
# dose15<-dose14 %>% 
#   filter(is.na(Variance) | abs(Variance)>20 | abs(`Order Variance`)>10)
# 
# write_csv(x = dose14,path = 'flagfarms.csv',append = FALSE)
