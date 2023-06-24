setwd("~/Desktop/4-1/DataMining/project")
source("./code/make_result_train_val_mse_0511.R.r")
library(glmnet)
library(xgboost)
library(randomForest)
library(dplyr)
library(leaps)
library(randomForest)
filter.fun = function(data){
  data = data %>% select(-c(timestamp, spot_number))
  data = data %>% rename("y"=names(data)[1])
  return(data)
}

param.lst = c('return_elnet_best_result.csv',
              'return_rf_best_result.csv',
              'return_xg_best_result.csv')
data.lst = c("건국대학교 (입학정보관)_590_return_data.csv",
  "건국대학교 (행정관)_591_return_data.csv",
  '건국대학교 과학관(이과대) 앞_3523_return_data.csv',
  '건국대학교 정문 앞_3860_return_data.csv',
  '건국대학교 학생회관_592_return_data.csv',
  '건대병원후문_3569_return_data.csv',
  '광진 캠퍼스시티_3579_return_data.csv',
  '어린이대공원역 3번출구 앞_500_return_data.csv',
  '화양 APT(횡단보도 옆)_3571_return_data.csv',
  '화양동 우체국_3582_return_data.csv',
  '화양사거리_3508_return_data.csv',
  '광진구_sample_return_data.csv',
  '화양동_sample_return_data.csv')
num.lst = list(3508,3523,3569,3571,3579,3582,3860,500,590,591,592,'gu','dong')

for (param in param.lst){
  param.df = read.csv(paste("./result/파생변수o/return/test/",param,sep=''))
  model = strsplit(param,'_')[[1]][2]
  if(model == 'elnet'){
    for (data in data.lst){
      df = read.csv(paste("./data/파생변수o/return/analysis_data/",data,sep=''))
      df = filter.fun(df)
      col = colnames(df)[!colnames(df) %in% 'y']
      org_org_elnet_station = param.df['station']
      lm_model = lm(df$y ~., data = df)
      step_model = step(object = lm_model,  # 기본 모델
                         direction = "both")
      df_sel = df[,names(step_model$coefficients)[-1]]
      df_sel= cbind(df$y,df_sel)
      colnames(df_sel)[1]='y'
      cut = length(colnames(df_sel)) - 1
      write.csv(df_sel, paste("./data/변수선택o_파생변수o/return/",model,'_',data,sep=''), row.names=F)
    }
  }
  if(model == 'rf'){
    id = 1
    for (data in data.lst){
      df = read.csv(paste("./data/파생변수o/return/analysis_data/",data,sep=''))
      df = filter.fun(df)
      col = colnames(df)[!colnames(df) %in% 'y']
      ntree = param.df$ntree[id]; mtry = param.df$mtry[id]
      rf_model = randomForest(df$y ~., data=df, ntree=100, mtry=20)
      importance = rf_model$importance 
      importance = data.frame(importance)
      importance$var = row.names(importance)
      importance[order(-importance$IncNodePurity), ]
      
      choose_var = importance[order(-importance$IncNodePurity), ]$var[1:cut]
      df_sel = df[,choose_var]
      df_sel= cbind(df$y,df_sel)
      colnames(df_sel)[1]='y'
      res = colnames(df_sel)
      # write.csv(df_sel, paste("./data/변수선택o_파생변수o/return/",model,'_',data,sep=''), row.names=F)
      id = id + 1
    }
    
  }
  
  if(model == 'xg'){
    id = 1
    for (data in data.lst){
      df = read.csv(paste("./data/파생변수o/return/analysis_data/",data,sep=''))
      df = filter.fun(df)
      col = colnames(df)[!colnames(df) %in% 'y']
      nrounds = param.df$nrounds[id]; max_depth = param.df$max_depth[id]
      eta = param.df$eta[id]
      
      xgb = xgboost(data=as.matrix(df[, -which(names(df) == "y")]), label=df$y, verbose=0, nrounds=nrounds, max_depth=max_depth, eta=eta)
      
      xgb.imp = data.frame(xgb.importance(model=xgb))
      xgb.imp[order(-xgb.imp$Gain), ]
      
      choose_var = importance[order(-importance$IncNodePurity), ]$var[1:cut]
      df_sel = df[,choose_var]
      df_sel= cbind(df$y,df_sel)
      colnames(df_sel)[1]='y'
      
      write.csv(df_sel, paste("./data/변수선택o_파생변수o/return/",model,'_',data,sep=''), row.names=F)
      id = id + 1
    }
  }
  
}


library(randomForest)
library(xgboost)

importance <- rf_model$importance
var_importance <- importance[, "IncNodePurity"]

# 변수 중요도의 합 계산 및 정규화
total_importance <- sum(var_importance)
normalized_importance <- var_importance / total_importance

# 변수 중요도를 백분율로 변환
percentage_importance <- normalized_importance * 100

importance = data.frame(percentage_importance)
importance$var = row.names(importance)
final_var = importance[importance$percentage_importance>=0.1,]$var
df_sel = df[,final_var]
df_sel= cbind(df$y,df_sel)
colnames(df_sel)[1]='y'
df_sel


importance[order(-importance$percentage_importance), ]


nrounds = param.df$nrounds[id]; max_depth = param.df$max_depth[id]
eta = param.df$eta[id]

xgb = xgboost(data=as.matrix(df[, -which(names(df) == "y")]), label=df$y, verbose=0, nrounds=nrounds, max_depth=max_depth, eta=eta)

xgb.imp = data.frame(xgb.importance(model=xgb))
final_var = xgb.imp[xgb.imp$Gain>=0.001,]$Feature
df_sel = df[,final_var]
df_sel= cbind(df$y,df_sel)
colnames(df_sel)[1]='y'

final_var = importance[importance$percentage_importance>=0.1,]$var
df_sel = df[,final_var]
df_sel= cbind(df$y,df_sel)
colnames(df_sel)[1]='y'
df_sel


# 결과 출력
print(percentage_importance)
plot(percentage_importance)
varImpPlot(rf_model)
df = data.frame(percentage_importance)
df[df$percentage_importance<0.1,]
