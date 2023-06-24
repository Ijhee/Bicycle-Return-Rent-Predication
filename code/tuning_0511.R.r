setwd("~/Desktop/4-1/DataMining/5.4 HP4")
rm(list=ls())

library(dplyr)
library(randomForest)
library(xgboost)
library(ggplot2)
library(cowplot)
library(gridExtra)
source("./make_result_train_val_mse_0511.R")
source("./tune_hyperparameter.R")
source("./graph_function.R")

set.seed(0504)
# Setting
rf_params = expand.grid(ntree = c(600,1000),
                        mtry = c(50,100,200))
# rf_params = expand.grid(ntree = c(10),
#                         mtry = c(30, 33))

# elnet_params = expand.grid(alpha = seq(0, 1, by=0.1),
#                            lambda = 10^seq(-3, 3, by=0.1))

xg_params = expand.grid(nrounds = c(800, 1300, 1800),
                        max_depth = c(10, 40, 70),
                        eta = c(0.08, 0.1, 0.12))
# 
# xg_params = expand.grid(nrounds = c(10, 20),
#                         max_depth = c(9, 15),
#                         eta = c(0.1, 0.2))

n_cv_tt = 1  # number of cv for train / test
n_cv_tv = 10  # number of cv for train / validation


file_list = list.files(pattern="*.csv")
#file_list = file_list[1:4]  # 웅
file_list = file_list[5:9]  # 준희
# file_list = file_list[10:13]  # 다은

for (file in file_list){
  print(paste("-------------", file, "-------------"))
  data = read.csv(file)
  rf_result = tune_hyperparameter(model='rf', params=rf_params,
                                  data=data, n_cv_tt, n_cv_tv)
  # elnet_result = tune_hyperparameter(model='elnet', params=elnet_params,
  # data=data, n_cv_tt, n_cv_tv)
  xg_result = tune_hyperparameter(model='xg', params=xg_params,
                                  data=data, n_cv_tt, n_cv_tv)
  
  rf_train_result = rf_result$train_mse
  rf_val_result = rf_result$val_mse
  # elnet_train_result = elnet_result$train_mse
  # elnet_val_result = elnet_result$val_mse
  xg_train_result = xg_result$train_mse
  xg_val_result = xg_result$val_mse
  
  # 기존 graph
  rf_train_df = read.csv(paste0("rbind_rf_train_result_", file))
  rf_val_df = read.csv(paste0("rbind_rf_val_result_", file))

  xg_train_df = read.csv(paste0("rbind_xg_train_result_", file),)
  xg_val_df = read.csv(paste0("rbind_xg_val_result_", file))

  rf_train_result = rbind(rf_train_df,rf_train_result)
  rf_val_result = rbind(rf_val_df,rf_val_result)

  xg_train_result = rbind(xg_train_df,xg_train_result)
  xg_val_result = rbind(xg_val_df,xg_val_result)
  
  write.csv(rf_train_result, paste0("rbind_rf_train_result_", file), row.names=F)
  write.csv(rf_val_result, paste0("rbind_rf_val_result_", file), row.names=F)
  
  write.csv(xg_train_result, paste0("rbind_xg_train_result_", file), row.names=F)
  write.csv(xg_val_result, paste0("rbind_xg_val_result_", file), row.names=F)
  
  graph.fun(mod='RF',file_list=file_list)
  graph.fun(mod='XG',file_list=file_list)  
  

  # write.csv(elnet_train_result, paste0("elnet_train_result_", file), row.names=F)
  # write.csv(elnet_val_result, paste0("elnet_val_result_", file), row.names=F)
  
}
