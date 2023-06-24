setwd("C:/Users/s_sjw0513/Desktop/데이터마이닝/project")
rm(list=ls())

library(xgboost)
library(randomForest)
library(dplyr)
set.seed(0420)

ttuk_data = read.csv("./data/ttukseom_result.csv")
ttuk_data = ttuk_data %>% select(-c('timestamp', 'rent_num'))

dong_data = read.csv("./data/dong_result.csv", fileEncoding='euc-kr')
dong_data = dong_data %>% 
    select(-c('timestamp', 'spot_number', 'spot_name', 'rent_num'))
dong_data = dong_data[sample(nrow(dong_data), nrow(ttuk_data)), ]

gu_data = read.csv("./data/gwangjin_result.csv", fileEncoding='euc-kr')
gu_data = gu_data %>% 
    select(-c('timestamp', 'spot_number', 'spot_name', 'rent_num'))
gu_data = gu_data[sample(nrow(gu_data), nrow(ttuk_data)), ]


ttuk_rf = randomForest(return_num ~., data=ttuk_data)
dong_rf = randomForest(return_num ~., data=dong_data)
gu_rf = randomForest(return_num ~., data=gu_data)

plot(ttuk_rf, main="뚝섬유원지 대여소 random forest")
plot(dong_rf, main="자양3동 대여소 random forest")
plot(gu_rf, main="광진구 대여소 random forest")

ttuk_xg = xgboost(data=as.matrix(ttuk_data[, -1]), label=ttuk_data[, 1], 
                  verbose=0, nrounds=2000)
dong_xg = xgboost(data=as.matrix(dong_data[, -1]), label=dong_data[, 1], 
                  verbose=0, nrounds=2000)
gu_xg = xgboost(data=as.matrix(gu_data[, -1]), label=gu_data[, 1], 
                  verbose=0, nrounds=2000)

plot(ttuk_xg$evaluation_log$iter, ttuk_xg$evaluation_log$train_rmse,
     main="뚝섬유원지 대여소 xgboost", ylab="train rmse", xlab="iteration")
plot(dong_xg$evaluation_log$iter, dong_xg$evaluation_log$train_rmse,
     main="자양3동 대여소 xgboost", ylab="train rmse", xlab="iteration")
plot(gu_xg$evaluation_log$iter, gu_xg$evaluation_log$train_rmse,
     main="광진구 대여소 xgboost", ylab="train rmse", xlab="iteration")
