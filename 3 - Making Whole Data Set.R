#-----------Set Parameters-------------------------------------------


Significant_KBars_Percentage<-0.75
Significant_KBars_Periods<-250



choosen_indicators_weekly<-c("Corp",
                             "WeeklyDate",
                             "partBtWk",
                             "bbWidthtWk",
                             "EMA_WkUp",
                             "MACD_Wk_Sig",
                             "MACDoscWk_Up")
#Choose the indicators that you want to put in daily sheet


MACD_DIF_short_periods<-12
MACD_DIF_long_periods<-26
MACD_DEM_periods<-9

BBand_middle_periods<-12
BBnad_SD_mutiple<-2

RSI_periods<-14

nfastK <- 60 
nfstD <-  5  
nslowD <- 3  
lag_para<-10  
target_ret<-0.06 
lead_days_para<-0 


#-----------Create Daily Data-----------------------------------------


stock_price_daily<-stock_price_origin_table %>% 
  
group_by(Corp) %>%   
  
mutate(sampleN_daily=n()) %>% 
  
filter(sampleN_daily>600)

  
colnames(stock_price_weekly)


#-----------Create Daily Date And Weekly Date Index---------------------


daily_date<-data.frame(DailyDate=unique(stock_price_daily$DailyDate)) 

weekly_date<-data.frame(WeeklyDate=unique(stock_price_weekly$WeeklyDate))


findweekday<-function(x)
{
  locate<-which(weekly_date$WeeklyDate<daily_date$DailyDate[x])
  
  ifelse(length(locate)>=1,max(weekly_date$WeeklyDate[locate]),NA)
}

daily_date<-daily_date %>%
  
mutate(WeeklyDate=sapply(c(1:nrow(daily_date)),findweekday))


#-----------Bind Date Index to Daily Data and Bind Weekly Data------------


stock_price_daily<-left_join(stock_price_daily,daily_date) %>% 
  
left_join(.,stock_price_weekly[,which(colnames(stock_price_weekly) %in% choosen_indicators_weekly)]) %>% 

  
#-----------Add Technical Analysis Columns--------------------------------

#-----------Significant K Bars--------------------------------------------


mutate(KBarDelta=abs((Ct-Ot)/lag(Ct,1))) %>%  
  
mutate(KBarDeltaCond=runquantile(KBarDelta,Significant_KBars_Periods,Significant_KBars_Percentage,align = "right",endrule="NA")) %>%  

  
#-----------Price SMA-----------------------------------------------------


mutate(.,MA5t=SMA(Ct,n=5),
       
       MA10t=SMA(Ct,n=10), 
       
       MA20t=SMA(Ct,n=20), 
       
       MA60t=SMA(Ct,n=60)) %>%  
  
  
#-----------Trade Volumes SMA---------------------------------------------


mutate(.,VolMA5t=SMA(Volt,n=5),
  
       VolMA10t=SMA(Volt,n=10),
  
       VolMA5t1=lag(VolMA5t,1)) %>% 
  
  
#-----------Buying Price--------------------------------------------------


mutate(.,CtLead=lead(Ct,lead_days_para)) %>%
#Close positions in fixd days
  
mutate(.,Ot1day=lead(Ot,1)) %>%
#If it sends buying signal today, open positions at opening price tomorrow   
  
  
#----------Bollinger Bands------------------------------------------------


mutate(.,movingSDt=runSD(Ct,BBand_middle_periods),

bbMiddlet=SMA(Ct,BBand_middle_periods),

bbUpt=bbMiddlet + BBnad_SD_mutiple * movingSDt,

bbDnt=bbMiddlet - BBnad_SD_mutiple * movingSDt,  

partBt=(Ct-bbDnt)/(bbUpt-bbDnt), 

bbWidtht=(bbUpt-bbDnt)/bbMiddlet,   

bbWidthMA10t=SMA(bbWidtht,10), 

bbWidthBrkout=as.numeric(bbWidtht > bbWidthMA10t)) %>%  
  
#----------RSI------------------------------------------------------------

mutate(.,RSIt=RSI(Ct,n=RSI_periods)) %>% 

#----------MACD-----------------------------------------------------------

mutate(.,DIF=EMA(Ct,MACD_DIF_short_periods)-EMA(Ct,MACD_DIF_long_periods),
       DEM=EMA(DIF,MACD_DEM_periods),
       MACDosc=DIF-DEM) %>% 
  
#----------Logical Strategies---------------------------------------------

mutate(.,higherMA5t=as.numeric(Ct>MA5t),
       higherMA5t1=lag(higherMA5t,1),
       MA5Brkout=as.numeric(higherMA5t==1&higherMA5t1==0)) %>%     
#Break through MA5  

    
mutate(.,higherMA20t=as.numeric(Ct>MA20t),
       higherMA20t1=lag(higherMA20t,1),
       MA20Brkout=as.numeric(higherMA20t==1&higherMA20t1==0)) %>%  
#Break through MA20  
  
mutate(., MA5throughMA20t=as.numeric(MA5t>MA20t),
       MA5throughMA20t1=lag(MA5throughMA20t,1),
       MA5MA20Brkout=as.numeric(MA5throughMA20t==1&MA5throughMA20t1==0)) %>% 
#MA5 breaks through MA20  
  

#----------CCI------------------------------------------------------------


mutate(tPrice=(Ht+Ct+Lt)/3,
       absDiff=abs(tPrice-MA20t),
       absDiffMA=SMA(absDiff,20),
       CCI=(1/0.015)*(tPrice-MA20t)/absDiffMA) %>% 
  
  
#-----------KD-------------------------------------------------------------

  mutate(maxHigh=rollmax(Ht, nfastK, fill=NA, align="right"),
         minLow=-rollmax(-Lt, nfastK, fill=NA, align="right"),
         fastKt=(Ct-minLow)/(maxHigh-minLow),
         fastKtdis=(fastKt>lag(fastKt,1))*1+(fastKt<lag(fastKt,1))*-1) %>%
  
  na.omit() %>%   
  mutate(fastDt=rollmean(fastKt,nfstD,align="right",fill=NA),
         fastDtdis=(fastDt>lag(fastDt,1))*1+(fastDt<lag(fastDt,1))*-1) %>%
  
  na.omit() %>% 
  mutate(slowDt=rollmean(fastDt,nslowD,align="right",fill=NA)) %>% 
  na.omit() %>% 
  
#-----------Price divides by MA-------------------------------------------

mutate(.,MA5level=Ct/MA5t,
         
       MA10level=Ct/MA10t,
         
       MA20level=Ct/MA20t) %>% 
  
#-----------Arrange and Combine with Stop Price Table---------------------

select(.,-maxHigh,-minLow) %>%   
  
left_join(.,stock_price_stop_table) %>% 

mutate(.,LeadRet=OutPrice/Ot1day,
       LeadRetPercentage=OutPrice/Ot1day-1) %>% 
  
mutate(.,LeadRetSTD=LeadRet^(20/OutDay)-1) %>% 
  
mutate(.,Labels = ((LeadRetSTD>target_ret)*2 + (LeadRetSTD<=target_ret)*1)) %>% 

na.omit()

