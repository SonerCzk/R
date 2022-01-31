set.seed(2017556072)
library(tidyverse)

sample1<-sample(sentences,size=100)
sample1
y<-str_c(sample1,collapse = " ")
y<-str_replace_all(y,"\\.","")
y<-str_replace_all(y,"\\?","") #because (') on (don't) being detected as (?????)
y<-y%>%
  str_split(" ")%>%
  .[[1]]
y<-unique(y)
#1
str_view(y,"^a.*e$",match = TRUE)
#2
x<-str_detect(y, "^(.*[aeuio].*){3,}$")
sum(x, na.rm = TRUE)
#3
y[order(-nchar(y),y)] %>%
  head(5)
#4
str_view(y,"(age|any|day|exp|her|pro|the)",match = TRUE)

