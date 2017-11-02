#-----------Choose Indicators For Machine Learning--------------------------
choosen_indicators_ML<-c("Corp",
                         "OutDay",
                         "LeadRetPercentage",
                         "DailyDate",
                         "partBtWk",
                         "bbWidthtWk",
                         "EMA_WkUp",
                         "MACD_Wk_Sig",
                         "MACDoscWk_Up",
                         "bbWidtht",
                         "partBt",
                         "bbWidthBrkout",
                         "fastKt",
                         "fastDt",
                         "slowDt",
                         "RSIt",
                         "MA5level",
                         "Labels")


#-----------Arrange The Machine Learning Table------------------------------

ML_table<-filter(stock_price_daily,
             KBarDelta>KBarDeltaCond,
             VolMA5t>1000)  

ML_table<-ungroup(ML_table) %>% 

.[,which(colnames(.) %in% choosen_indicators_ML)] %>% 
  
na.omit()

#for(i in lag_para:1) {KBar_matrix<-cbind(KBar_matrix,lag(KBar_matrix$fastKt,i))
#colnames(KBar_matrix)[ncol(KBar_matrix)]<-paste("fastKt",i,sep = "")}


#for(i in lag_para:1) {KBar_matrix<-cbind(KBar_matrix,lag(KBar_matrix$fastDt,i))
#colnames(KBar_matrix)[ncol(KBar_matrix)]<-paste("fastDt",i,sep = "")}


#for(i in lag_para:1) {KBar_matrix<-cbind(KBar_matrix,lag(KBar_matrix$bbWidtht,i))
#colnames(KBar_matrix)[ncol(KBar_matrix)]<-paste("bbWidtht",i,sep = "")}

#-------------Arrange Training Data For ML-----------------------------------


table<-filter(ML_table,DailyDate<20140000) 
table$Labels<-table$Labels-1 
smp.size <- floor(0.8*nrow(table)) 

train.ind <- sample(seq_len(nrow(table)), smp.size)
train <- table[train.ind, ]
test <- table[-train.ind, ]

train_vari<-select(train,-OutDay,-LeadRetPercentage,-Corp,-DailyDate,-Labels)  
test_vari<-select(test,-OutDay,-LeadRetPercentage,-Corp,-DailyDate,-Labels)   

Y=train$Labels


train_m = as.matrix(train_vari)
test_m = as.matrix(test_vari)



m = length(unique(table$Labels))

#-----------Set ML Parameters And Verificate---------------------------

param = list("objective" = "multi:softprob","eval_metric" = "mlogloss","num_class"=m)

result = xgboost(param=param, data=train_m, label=Y, nrounds=20)

Ypred = predict(result,test_m)
Ypred = t(matrix(Ypred,m,length(Ypred)/m))
Ypred = levels(as.factor(test$Labels))[max.col(Ypred)]

test<-cbind(Ypred,test) 

#-----------Caculate Perfomance of Verification-----------------------

avg_return<-filter(test,Ypred=="1") %>% summarise(.,avg=mean(LeadRetPercentage)) %>% as.numeric(.)

t0 = table(test$Labels,Ypred) 

accuracy<-sum(diag(t0))/sum(t0)

loss_rate<-t0[2,1]/(t0[2,2]+t0[2,1])

win_rate<-t0[2,2]/(t0[1,2]+t0[2,2])

ret_win_rate<-filter(test,Ypred==1) %>% select(.,LeadRetPercentage) %>% 
  summarise(.,avg_return=mean(LeadRetPercentage>0)) %>% as.numeric(.)



#-----------Implementation-----------------------------------------------

imp_matrix<-filter(ML_table,DailyDate>20140000)

imp_matrix$Labels<-imp_matrix$Labels-1

imp_vari<-select(imp_matrix,-OutDay,-LeadRetPercentage,-Corp,-DailyDate,-Labels)

imp_m<-as.matrix(imp_vari)

Ypred_imp = predict(result,imp_m)
Ypred_imp = t(matrix(Ypred_imp,m,length(Ypred_imp)/m))
pred_p<-Ypred_imp[,2]
Y_imp<-imp_matrix$Labels
Ypred_imp = levels(as.factor(imp_matrix$Labels))[max.col(Ypred_imp)]
imp_matrix<-cbind(Ypred_imp,imp_matrix)

#----------Caculate Perfomance of Implementation------------------


avg_return_imp<-filter(imp_matrix,Ypred_imp=="1") %>% summarise(.,avg=mean(LeadRetPercentage)) %>% as.numeric(.)

t0_imp = table(imp_matrix$Labels,Ypred_imp)  #?V?ïx?}

accuracy_imp<-sum(diag(t0_imp))/sum(t0_imp)

loss_rate_imp<-t0_imp[2,1]/(t0_imp[2,2]+t0_imp[2,1])

win_rate_imp<-t0_imp[2,2]/(t0_imp[1,2]+t0_imp[2,2])

ret_win_rate_imp<-filter(imp_matrix,Ypred_imp==1) %>% select(.,LeadRetPercentage) %>% 
  summarise(.,avg_return_imp=mean(LeadRetPercentage>0)) %>% as.numeric(.)


#----------Plot Important Variables---------------------------------------
model <- xgb.dump(result, with_stats = T)
names <- colnames(data.matrix(imp_vari))
importance_matrix <- xgb.importance(names, model = result)
xgb.plot.importance(importance_matrix[1:15])

#---------Plot ROC Curve------------------------------------------------

pred = prediction(pred_p, Y_imp)
perf = performance(pred,"tpr","fpr")
plot(perf,col="blue",lty=3,lwd=3,cex.lab=1.5,cex.main=1.5,main="ROC_curve")
auc=performance(pred,"auc")
auc=unlist(slot(auc,"y.values"))


#----------Plot And Export Decision Trees-----------------------------

treee<- xgb.plot.tree(model=result, trees=0:1, render=F,plot_width = 2000,plot_height = 2000)
export_graph(treee, 'treee.png', width=1000, height=10000)


#-----------Caculate Trade Frequency For Years-------------------------

tradedays1<-nrow(filter(imp_matrix,Ypred_imp=="1"&DailyDate>20140000&DailyDate<200150000))
tradedays2<-nrow(filter(imp_matrix,Ypred_imp=="1"&DailyDate>20150000&DailyDate<200160000))
tradedays3<-nrow(filter(imp_matrix,Ypred_imp=="1"&DailyDate>20160000&DailyDate<200170000))
tradedays4<-nrow(filter(imp_matrix,Ypred_imp=="1"&DailyDate>20170000&DailyDate<200180000))




#-----------Print Overall Performace---------------------------------

cat("Implementation","\n","Overall Accuracy",accuracy_imp,"\n",
    "Signal Loss Rate",loss_rate_imp,"\n",
    "Signal Accuracy",win_rate_imp,"\n",
    "Win rate of Returns",ret_win_rate_imp,"\n",
    "Avg. Return of Periods",avg_return_imp,"\n",
    "Avg. Holding Days",mean(filter(imp_matrix,Ypred_imp==1)$OutDay),"\n","\n",
    
    "Verification","\n","Overall Accuracy",accuracy,"\n",
    "Signal Loss Rate",loss_rate,"\n",
    "Signal Accuracy",win_rate,"\n",
    "Win rate of Returns",ret_win_rate,"\n",
    "Avg. Return of Periods",avg_return,"\n",
    "AUC",auc,"\n","\n",
    "2014 Trade Frequency",tradedays1,"\n",
    "2015 Trade Frequency",tradedays2,"\n",
    "2016 Trade Frequency",tradedays3,"\n",
    "2017 Trade Frequency",tradedays4,"\n")

data<-filter(imp_matrix,Ypred_imp=="1")

a<-filter(imp_matrix,Ypred_imp==1 & DailyDate>20160000) %>% arrange(.,desc(LeadRetPercentage))
