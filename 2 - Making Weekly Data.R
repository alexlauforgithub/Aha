#-----------Set Work Directory And Parameters--------------------

setwd("/Users/alex/desktop/R/data")

MACD_DIF_short_periods<-12
MACD_DIF_long_periods<-26
MACD_DEM_periods<-9

BBand_middle_periods<-20
BBnad_SD_mutiple<-2

Long_EMA_periods<-22
Middle_EMA_periods<-10
Short_EMA_periods<-7

RSI_periods<-14


#-----------Load Weekly Data And Arrange-------------------------

stock_price_weekly<-fread("stock_price_weekly_TW.txt",na.strings = "-",colClasses =c(rep("character",1),rep("numeric",7)))

stock_price_weekly<-stock_price_weekly %>% .[,-2]  %>% as.tibble() 

colnames(stock_price_weekly)<-c("Corp","WeeklyDate","OtWk","HtWk","LtWk","CtWk","VoltWk")

#-----------Add Technical Analysis Columns-----------------------


stock_price_weekly<-stock_price_weekly %>%

group_by(Corp) %>% 

arrange(Corp,WeeklyDate) %>% 

mutate(sampleN=n()) %>% 

filter(sampleN>120) %>% 
  

#-----------Weekly MACD------------------------------------------

mutate(DIF_Wk=EMA(CtWk,MACD_DIF_short_periods)-EMA(CtWk,MACD_DIF_long_periods),
       
       DEM_Wk=SMA(DIF_Wk,MACD_DEM_periods),
       
       MACDosc_Wk= DIF_Wk - DEM_Wk) %>% 
  
#-----------Weekly Bollinger Bands-------------------------------

mutate(.,movingSDtWk=runSD(CtWk,BBand_middle_periods)) %>% 
  
mutate(.,bbMiddletWk=SMA(CtWk,BBand_middle_periods),   

       bbUptWk=bbMiddletWk + BBnad_SD_mutiple*movingSDtWk,

       bbDntWk=bbMiddletWk - BBnad_SD_mutiple*movingSDtWk,  

       partBtWk=(CtWk-bbDntWk)/(bbUptWk-bbDntWk), 

       bbWidthtWk=(bbUptWk-bbDntWk)/bbMiddletWk,   

       bbWidthMA10tWk=SMA(bbWidthtWk,10)) %>% 

#-----------Weekly EMA--------------------------------------------

mutate(.,Long_EMA_Wk=EMA(CtWk,Long_EMA_periods),
       
       Middle_EMA_Wk=EMA(CtWk,Middle_EMA_periods),
       
       Short_EMA_Wk=EMA(CtWk,Short_EMA_periods)) %>% 
#-----------Logical Signal----------------------------------------
         
mutate(.,EMA_WkUp=as.numeric(Long_EMA_Wk > lag(Long_EMA_Wk,1)),
       
       MACD_Wk_Sig=as.numeric(DIF_Wk>0 & DEM_Wk>0),
       
       MACDoscWk_Up=as.numeric(MACDosc_Wk > lag(MACDosc_Wk,1)))


