set.seed(2017556072)
install.packages("tidyverse")
library(tidyverse)
x<-read.csv("C:/ProgramData/Microsoft/Windows/Start Menu/Programs/RStudio/covid-data-2020.csv",sep = "\t",header = TRUE)
sample1<-x[sample(nrow(x),1000),]
sample1
#1
sample4<-filter(sample1,new_cases !="N/A")
sample4 %>%
  group_by(location,month) %>%
  summarise(min=min(new_cases),Q1=quantile(new_cases,0.25,na.rm=TRUE),Q2=quantile(new_cases,0.5,na.rm=TRUE),Q3=quantile(new_cases,0.75,na.rm=TRUE),max=max(new_cases)
  )
#2
sample2<-filter(sample1,new_deaths != "N/A" & new_cases != "N/A")
  sample3<-sample2%>%
    group_by(location)%>%
    summarise(max_cases=max(new_cases),max_deaths=max(new_deaths))
    arrange(sample3,desc(max_cases))

#3
xy<-sample2 %>%
  group_by(location,month) %>%
  summarise(mean=mean(new_cases))

 xx<- xy[order(xy$location,-xy$mean),]
 xx <- xx[!duplicated(xx$location),]
 xx
