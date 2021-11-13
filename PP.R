##To load packages
library(tidyverse)
library(lubridate)
library(ggplot2)
library(readxl)
library(writexl)
library(dplyr)
library(stringr)
library(plyr)
library(formattable)
library(data.table)



##To import the datasets
PP1 <- read_xlsx("Datasets/Payment Practices/PP1.xlsx")
PP2 <- read_xlsx("Datasets/Payment Practices/PP2.xlsx")

##To check the 2 datasets - PP1 & PP2
head(PP1)
head(PP2)
colnames(PP1)
colnames(PP2)
str(PP1)
str(PP2)

##To change the column names to shorter names of PP1 dataset
names(PP1) <- c("Id", "SDate", "EDate", "FDate", "Company", "CNum", "INRP", "ATP", "ORP", "PTC", "SNC", "EIO", "SCFO")

##To count all the elements in all columns of PP1 dataset
nrow(PP1)
length(PP1$Id)
length(PP1$INRP)
length(PP1$SDate)
length(PP1$EDate)
length(PP1$FDate)
length(PP1$Company)
length(PP1$CNum)
length(PP1$INRP)
length(PP1$ATP)
length(PP1$ORP)
length(PP1$PTC)
length(PP1$SNC)
length(PP1$EIO)
length(PP1$SCFO)



##1A. To calculate the total UK businesses that pay within and outside the reporting period
summary(PP1$INRP)
trp <- nrow(PP1)
na1 <- (nrow(subset(PP1, INRP == "NULL"))/trp)
i1 <- (nrow(subset(PP1, INRP == "Yes"))/trp)
o1 <- (nrow(subset(PP1, INRP == "No"))/trp)

na2 <- formattable::percent(na1)
i2 <- formattable::percent(i1)
o2 <- formattable::percent(o1)
na2
i2
o2


##To create the 1st table - % of UK businesses that pay within and outside reporting period in 2017 to 2019.

t1 <- matrix(c(i2, o2, na2))
colnames(t1) <- ("PaidInReportingPeriodPercentage")
rownames(t1) <- c("YES", "NO", "NULL")
t1 <- as.table(t1)
t1

 ##To create the 2nd table - totals of UK businesses that pay within and outside reporting period.
t2 <- table(PP1$INRP)

colnames(t2) <- ("PaidInReportingPeriod")
rownames(t2) <- c("YES", "NO", "NULL")
t2 <- as.table(t2)
t2

##1B. To calculate the total average number of days UK businesses pay
mean(PP1$ATP, na.rm=TRUE)

##1C. To calculate % of invoices outside agreed terms where payment terms did not change and payment terms did changed but suppliers notified the changes.
 sum(PP1$ORP, na.rm=TRUE)
  
 ##Total Average Pay Time 
    mean(PP1$ORP, na.rm=TRUE)  

 ##Total Average Pay Time with no payment term changes + Total Average Pay Time with payment term changes where suppliers notified the changes.
  TATP <- mean(PP1$ORP[which(PP1$PTC =="No", 9)]) + mean(PP1$ORP[which(PP1$PTC =="Yes" & PP1$SNC =="Yes", 9)])
  TATP

##2. To get the Top 20 UK businesses that pays on time.
  
PP1 %>%
  select(FDate, Company, INRP, ATP, PTC) %>%
  filter(INRP == "Yes" & PTC == "No") %>%
  head(ATP, n=20) %>%
  arrange(ATP) %>%
  group_by(FDate)

##3. To get the 20 UK businesses that pays the longest time.

PP1 %>%
  select(FDate, Company, INRP, ATP, PTC) %>%
  filter(INRP != "NULL") %>%
  arrange(desc(ATP)) %>%
  head(ATP, n=20) %>%
  group_by(FDate)

##To change the column names to shorter names of PP2 dataset
names(PP2) <- c("Id", "SDate", "EDate", "FDate", "Company", "CNum", "D30", "D31to60", "D61andUp")

##To count the number of rows in PP2 dataset
nrow(PP2)

##To count the total average percentage of UK businesses that pay within 30 days.
mean(PP2$D30)

##To count the total average percentage of UK businesses that pay between 31-60 days.
mean(PP2$D31to60)

##To count the total average percentage of UK businesses that pay between 31-60 days.
mean(PP2$D61andUp)






