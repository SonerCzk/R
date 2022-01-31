set.seed(2017556072)
library(tidyverse)
primeNumbers =c()
unPrimeNumbers =c()
findpn<-function(number){
  for(i in 1:length(number)){
  if(sum(number[i]/1:number[i]==number[i]%/%1:number[i])==2){
    primeNumbers<-append(primeNumbers,number[i])
    }
  else{
    unPrimeNumbers<-append(unPrimeNumbers,number[i])
  }
  }
  print("Prime Numbers:")
  print(primeNumbers)
  print("UnPrime Numbers:")
  print(unPrimeNumbers)
  
  for(j in 1:length(unPrimeNumbers)){
  prime_factors <- function(unPrimeNumbers, i=2, factors = NULL){
    if(unPrimeNumbers<i) factors
    else if(! unPrimeNumbers %% i) prime_factors(unPrimeNumbers/i, i, c(factors, i))
    else  prime_factors(unPrimeNumbers, i+1, factors)
  }}
  for(k in 1:length(unPrimeNumbers)){
    cat("Prime factors of ",unPrimeNumbers[k]," are ")
    #i used cat funtion as print because print show number of vector and switches to a new line 
  z<-prime_factors(unPrimeNumbers[k])
  cat(z)
  cat("\n")
}}
randomNumbers<-sample(1:1000,5,replace = F)
randomNumbers
findpn(randomNumbers)



