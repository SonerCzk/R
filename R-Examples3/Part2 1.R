set.seed(2017556072)
library(tidyverse)
sorter<-function(sentences){
x<-str_c(sentences,collapse = " ")
x<-str_replace_all(x,"\\.","")
x<-x%>%
  str_split(" ")%>%
  .[[1]]
x<-x[order(nchar(x), x)]
x<-str_c(x,collapse = " ")
x}
sentences<-sample(sentences,size=5)
sorter(sentences)
