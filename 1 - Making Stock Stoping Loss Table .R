
#The problem of charater encoding between UTF-8 and big-5 is so bothering
#Thus, using English to comment is a better way

if (!require(tidyquant)){install.packages("tidyquant")};library(tidyquant)

if (!require(tidyverse)){install.packages("tidyverse")};library(tidyverse)

if (!require(quantmod)){install.packages("quantmod")};library(quantmod)

if (!require(data.table)){install.packages("data.table")};library(data.table)

if (!require(lubridate)){install.packages("lubridate")};library(lubridate)

if (!require(magrittr)){install.packages("magrittr")};library(magrittr)

if (!require(caTools)){install.packages("caTools")};library(caTools)

if (!require(DiagrammeR)){install.packages("DiagrammeR")};library(DiagrammeR)

if (!require(drat)){install.packages("drat", repos="https://cran.rstudio.com")};library(drat)

drat:::addRepo("dmlc")

if (!require(xgboost))install.packages("xgboost", repos="http://dmlc.ml/drat/", type="source");library(xgboost)

if (!require(ROCR)){install.packages("ROCR")};library(ROCR)


#-----------Set Work Directory And Parameters--------------------
setwd("/Users/alex/desktop/R/data")

accel=c(.02,.2)

closing_days<-60  
#Set a fixed number that determine closing date in 

holding_day_at_least=3  
#The number is that the least days we want to hold positions

#-----------Read Daily Data And Arrange---------------------------

stock_price_origin_table<-fread("stock_price_daily_TW.txt",na.strings = "-",colClasses =c(rep("character",1),rep("numeric",7)))

stock_price_origin_table<-stock_price_origin_table %>% .[,-2]  %>% as.tibble() 

colnames(stock_price_origin_table)<-c("Corp","DailyDate","Ot","Ht","Lt","Ct","Volt")

stock_price_origin_table<-arrange(stock_price_origin_table,Corp,DailyDate)


#-----------Creats The Table---------------------------------------

stock_price_stop_table<-stock_price_origin_table %>% 

group_by(.,Corp) %>% 

mutate(sampleN=n()) %>% 

filter(sampleN>300) %>%  # Make sure that every companys has enough samples
  
#-----------Loss-Stoping / Profit-Realizing Strategies------------

mutate(.,initGap=sd(drop(coredata(Ht - Lt)), na.rm=TRUE),
        sar=.Call("sar", Ht, Lt, accel, initGap, PACKAGE = "TTR"),
       Sig=Ct<sar,
       OutSig=as.numeric(Sig==1))   #If it sends stoping Signal on this day, mark it as "1" 

#-----------Make a function that creates mutiple columns------------

multiMAdown <- function(df, n) 
{
  varname <- paste("OutSig", n , sep="")
  mutate(df, !!varname := ifelse(lead(OutSig,n+1)==0,0,lead(Ot,n+2)))

#If it sends stoping signal at the day , set the selling price at the opening price after that day
#If OutSig1 is 1, it means, if it sends buying signal at t, you should open positions at t+1, and it sends stoping signal at t+2 as well, so you should sell it at t+3 at opening price on that day
#In other words, OutSig1 means you have held that postions for 1 day long.

}



for(i in 1:closing_days)
{
  stock_price_stop_table<-multiMAdown(stock_price_stop_table,i)
  cat(i)
}

#-----------Caculate Final Stop Prices And Holding Days------------

DT <- data.table(stock_price_stop_table)

findValue <- function(X)
{
  tmp <- which(X != 0, arr.ind = TRUE)
  
  minColLoc <- tapply(tmp[ ,2], tmp[ ,1], min)
  
  locMat <- cbind(row = as.integer(names(minColLoc)) , col = minColLoc)
  
  zeroLocIdx2 <- setdiff(1:nrow(X), locMat[ , 1])
  
  if (length(zeroLocIdx2) > 0)
  
  locMat <- rbind(locMat, cbind(zeroLocIdx2, 1))
  
  locMat <- locMat[order(locMat[ , 1]), ]
  
  X[locMat]
}


findDay <- function(X)
{
  
  tmp <- which(X != 0, arr.ind = TRUE)
  
  minColLoc <- tapply(tmp[ ,2], tmp[ ,1], min)
  
  locMat <- cbind(row = as.integer(names(minColLoc)) , col = minColLoc)
  
  zeroLocIdx2 <- setdiff(1:nrow(X), locMat[ , 1])
  
  if (length(zeroLocIdx2) > 0)
  
  locMat <- rbind(locMat, cbind(zeroLocIdx2, 1))
  
  locMat <- locMat[order(locMat[ , 1]), ]
  
  locMat[,2]+holding_day_at_least-1  
}


start_column<-which(colnames(DT)==paste("OutSig",holding_day_at_least,sep=""))

end_column<-which(colnames(DT)==paste("OutSig",closing_days,sep=""))


DT[ , OutPrice := findValue(do.call(cbind, .SD)), .SDcols=start_column:end_column]

DT[ , OutDay := findDay(do.call(cbind, .SD)), .SDcols=start_column:end_column]

#-----------Clean The Data That Has No Stop Price--------------------

OutSig_column<-which(colnames(DT)==paste("OutSig",holding_day_at_least,sep=""))


stock_price_stop_table<-as.data.frame(DT) %>% 

.[which(is.na(.[,OutSig_column])==F),] %>% 

filter(.,OutPrice!=0) %>% 

select(.,Corp,DailyDate,OutPrice,OutDay)


rm(DT)

