setwd("C:/Users/s_sjw0513/Desktop/데이터마이닝/project")
rm(list=ls())

library(dplyr)
library(randomForest)
library(xgboost)
library(ggplot2)
library(cowplot)
library(gridExtra)
source("./code/make_reult_train_val_mse_0412.R")
source("./code/tune_hyperparameter.R")
source("./code/graph_function.R")

set.seed(0504)
# Setting
rf_params = expand.grid(ntree = c(100, 200),
                        mtry = c(40, 43, 46, 50))

xg_params = expand.grid(nrounds = c(300, 400, 500),
                        max_depth = c(10, 20, 30),
                        eta = c(0.1, 0.2, 0.3))

n_cv_tt = 1  # number of cv for train / test
n_cv_tv = 10  # number of cv for train / validation


file_list = list.files("./data/final_data", pattern="*.csv")
file_list = file_list[1:4]  # 웅
# file_list = file_list[5:9]  # 준희
# file_list = file_list[10:13]  # 다은

for (file in file_list){
  print(paste("-------------", file, "-------------"))
  data = read.csv(paste0("./data/final_data/", file))
  rf_result = tune_hyperparameter(model='rf', params=rf_params,
                                  data=data, n_cv_tt, n_cv_tv)
  xg_result = tune_hyperparameter(model='xg', params=xg_params,
                                  data=data, n_cv_tt, n_cv_tv)
  
  rf_train_result = rf_result$train_mse
  rf_val_result = rf_result$val_mse
  xg_train_result = xg_result$train_mse
  xg_val_result = xg_result$val_mse
  
  # 기존 result
  pre_rf_train = read.csv(paste0("./result/tuning/rf_train_result_", file))
  pre_rf_val = read.csv(paste0("./result/tuning/rf_val_result_", file))
  pre_xg_train = read.csv(paste0("./result/tuning/xg_train_result_", file),)
  pre_xg_val = read.csv(paste0("./result/tuning/xg_val_result_", file))
  
  # 기존 + 새로운 결과 합치기
  rf_train_result = rbind(pre_rf_train, rf_train_result)
  rf_val_result = rbind(pre_rf_val, rf_val_result)
  xg_train_result = rbind(pre_xg_train, xg_train_result)
  xg_val_result = rbind(pre_xg_val, xg_val_result)
  
  # 새로운 결과 저장
  write.csv(rf_train_result, paste0("./result/tuning/rf_train_result_", file), row.names=F)
  write.csv(rf_val_result, paste0("./result/tuning/rf_val_result_", file), row.names=F)
  write.csv(xg_train_result, paste0("./result/tuning/xg_train_result_", file), row.names=F)
  write.csv(xg_val_result, paste0("./result/tuning/xg_val_result_", file), row.names=F)
  
  # 새로운 그래프 뽑기
  graph.fun(mod='RF', file_list=file_list)
  graph.fun(mod='XG', file_list=file_list)  

}
