setwd("C:/Users/s_sjw0513/Desktop/데이터마이닝/project")
rm(list=ls())

# --------------------------------------------------- #
#
# 0. Load libraries and functions
#
# --------------------------------------------------- #

library(dplyr)
source("./code/make_reult_train_val_mse_0412.R")

make_result_form = function(data_name, n_cv_tt, params){
    result = data.frame(matrix(NA, n_cv_tt, ncol=ncol(params)+1) )
    colnames(result) = c(names(params), paste0(data_name, '_mse'))
    
    return(result)
}

find_best_combination = function(val_result, params){
    # Compute the median val_mse for each combination
    val_medians = aggregate(val_mse ~ ., val_result, median)
    # Find the combination with the lowest median val_mse value
    best_com = val_medians[which.min(val_medians$val_mse), ]
    
    return(best_com)
}

# --------------------------------------------------- #
#
# 1. Read data
#
# --------------------------------------------------- #

data_3508 = read.csv("./data/final_data/data_3508.csv")
data_3523 = read.csv("./data/final_data/data_3523.csv")
data_3569 = read.csv("./data/final_data/data_3569.csv")
data_3571 = read.csv("./data/final_data/data_3571.csv")
data_3579 = read.csv("./data/final_data/data_3579.csv")
data_3582 = read.csv("./data/final_data/data_3582.csv")
data_3860 = read.csv("./data/final_data/data_3860.csv")
data_500 = read.csv("./data/final_data/data_500.csv")
data_590 = read.csv("./data/final_data/data_590.csv")
data_591 = read.csv("./data/final_data/data_591.csv")
data_592 = read.csv("./data/final_data/data_592.csv")
gu = read.csv("./data/final_data/gwangjin_data.csv")
dong = read.csv("./data/final_data/hwayang_data.csv")

# --------------------------------------------------- #
#
# 2. Setting for analysis
#
# --------------------------------------------------- #

# -------- Setting hyper parameters -------- #

# each station hyper parameters
########################## 3508 ##########################
rf_params_3508 = expand.grid(ntree = c(50, 100),
                             mtry = c(17, 22, 27))

elnet_params_3508 = expand.grid(alpha = seq(0, 1, by=0.1),
                                lambda = 10^seq(-3, 3, by=0.1))

xg_params_3508 = expand.grid(nrounds = c(50, 100),
                             max_depth = c(3, 6, 9),
                             eta = c(0.1, 0.01, 0.001))

########################## 3523 ##########################
rf_params_3523 = expand.grid(ntree = c(50, 100),
                             mtry = c(17, 22, 27))

elnet_params_3523 = expand.grid(alpha = seq(0, 1, by=0.1),
                                lambda = 10^seq(-3, 3, by=0.1))

xg_params_3523 = expand.grid(nrounds = c(50, 100),
                             max_depth = c(3, 6, 9),eta = c(0.1, 0.01, 0.001))

########################## 3569 ##########################
rf_params_3569 = expand.grid(ntree = c(50, 100),
                             mtry = c(17, 22, 27))

elnet_params_3569 = expand.grid(alpha = seq(0, 1, by=0.1),
                                lambda = 10^seq(-3, 3, by=0.1))

xg_params_3569 = expand.grid(nrounds = c(50, 100),
                             max_depth = c(3, 6, 9),
                             eta = c(0.1, 0.01, 0.001))

########################## 3571 ##########################
rf_params_3571 = expand.grid(ntree = c(50, 100),
                             mtry = c(17, 22, 27))

elnet_params_3571 = expand.grid(alpha = seq(0, 1, by=0.1),
                                lambda = 10^seq(-3, 3, by=0.1))

xg_params_3571 = expand.grid(nrounds = c(50, 100),
                             max_depth = c(3, 6, 9),
                             eta = c(0.1, 0.01, 0.001))

########################## 3579 ##########################
rf_params_3579 = expand.grid(ntree = c(50, 100),
                             mtry = c(17, 22, 27))

elnet_params_3579 = expand.grid(alpha = seq(0, 1, by=0.1),
                                lambda = 10^seq(-3, 3, by=0.1))

xg_params_3579 = expand.grid(nrounds = c(50, 100),
                             max_depth = c(3, 6, 9),
                             eta = c(0.1, 0.01, 0.001))

########################## 3582 ##########################
rf_params_3582 = expand.grid(ntree = c(50, 100),
                             mtry = c(17, 22, 27))

elnet_params_3582 = expand.grid(alpha = seq(0, 1, by=0.1),
                                lambda = 10^seq(-3, 3, by=0.1))

xg_params_3582 = expand.grid(nrounds = c(50, 100),
                             max_depth = c(3, 6, 9),
                             eta = c(0.1, 0.01, 0.001))

########################## 3860 ##########################
rf_params_3860 = expand.grid(ntree = c(50, 100),
                             mtry = c(17, 22, 27))

elnet_params_3860 = expand.grid(alpha = seq(0, 1, by=0.1),
                                lambda = 10^seq(-3, 3, by=0.1))

xg_params_3860 = expand.grid(nrounds = c(50, 100),
                             max_depth = c(3, 6, 9),
                             eta = c(0.1, 0.01, 0.001))

########################## 500 ##########################
rf_params_500 = expand.grid(ntree = c(50, 100),
                            mtry = c(17, 22, 27))

elnet_params_500 = expand.grid(alpha = seq(0, 1, by=0.1),
                               lambda = 10^seq(-3, 3, by=0.1))

xg_params_500 = expand.grid(nrounds = c(50, 100),
                            max_depth = c(3, 6, 9),
                            eta = c(0.1, 0.01, 0.001))

########################## 590 ##########################
rf_params_590 = expand.grid(ntree = c(50, 100),
                            mtry = c(17, 22, 27))

elnet_params_590 = expand.grid(alpha = seq(0, 1, by=0.1),
                               lambda = 10^seq(-3, 3, by=0.1))

xg_params_590 = expand.grid(nrounds = c(50, 100),
                            max_depth = c(3, 6, 9),
                            eta = c(0.1, 0.01, 0.001))

########################## 591 ##########################
rf_params_591 = expand.grid(ntree = c(50, 100),
                            mtry = c(17, 22, 27))

elnet_params_591 = expand.grid(alpha = seq(0, 1, by=0.1),
                               lambda = 10^seq(-3, 3, by=0.1))

xg_params_591 = expand.grid(nrounds = c(50, 100),
                            max_depth = c(3, 6, 9),
                            eta = c(0.1, 0.01, 0.001))

########################## 592 ##########################
rf_params_592 = expand.grid(ntree = c(50, 100),
                            mtry = c(17, 22, 27))

elnet_params_592 = expand.grid(alpha = seq(0, 1, by=0.1),
                               lambda = 10^seq(-3, 3, by=0.1))

xg_params_592 = expand.grid(nrounds = c(50, 100),
                            max_depth = c(3, 6, 9),
                            eta = c(0.1, 0.01, 0.001))

########################## gu ##########################
rf_params_gu = expand.grid(ntree = c(50, 100),
                           mtry = c(17, 22, 27))

elnet_params_gu = expand.grid(alpha = seq(0, 1, by=0.1),
                              lambda = 10^seq(-3, 3, by=0.1))

xg_params_gu = expand.grid(nrounds = c(50, 100),
                           max_depth = c(3, 6, 9),
                           eta = c(0.1, 0.01, 0.001))

########################## dong ##########################
rf_params_dong = expand.grid(ntree = c(50, 100),
                             mtry = c(17, 22, 27))

elnet_params_dong = expand.grid(alpha = seq(0, 1, by=0.1),
                                lambda = 10^seq(-3, 3, by=0.1))

xg_params_dong = expand.grid(nrounds = c(50, 100),
                             max_depth = c(3, 6, 9),
                             eta = c(0.1, 0.01, 0.001))

# ---------------- Setting result table ------------------ #
n_cv_tt = 2  # number of cv for train / test
n_cv_tv = 3  # number of cv for train / validation

######## results in each station ###########

########################## 3508 ##########################
# Results in train / validation loop 
rf_train_result_3508 = rf_val_result_3508 = NULL
elnet_train_result_3508 = elnet_val_result_3508 = NULL
xg_train_result_3508 = xg_val_result_3508 = NULL

# Results in train(best parameters) / test loop
rf_best_result_3508 = make_result_form(data_name="best", n_cv_tt=n_cv_tt, params=rf_params_3508)
rf_test_result_3508 = make_result_form(data_name="test", n_cv_tt=n_cv_tt, params=rf_params_3508)

elnet_best_result_3508 = make_result_form(data_name="best", n_cv_tt=n_cv_tt, params=elnet_params_3508)
elnet_test_result_3508 = make_result_form(data_name="test", n_cv_tt=n_cv_tt, params=elnet_params_3508)

xg_best_result_3508 = make_result_form(data_name="best", n_cv_tt=n_cv_tt, params=xg_params_3508)
xg_test_result_3508 = make_result_form(data_name="test", n_cv_tt=n_cv_tt, params=xg_params_3508)

########################## 3523 ##########################
# Results in train / validation loop 
rf_train_result_3523 = rf_val_result_3523 = NULL
elnet_train_result_3523 = elnet_val_result_3523 = NULL
xg_train_result_3523 = xg_val_result_3523 = NULL

# Results in train(best parameters) / test loop
rf_best_result_3523 = make_result_form(data_name="best", n_cv_tt=n_cv_tt, params=rf_params_3523)
rf_test_result_3523 = make_result_form(data_name="test", n_cv_tt=n_cv_tt, params=rf_params_3523)

elnet_best_result_3523 = make_result_form(data_name="best", n_cv_tt=n_cv_tt, params=elnet_params_3523)
elnet_test_result_3523 = make_result_form(data_name="test", n_cv_tt=n_cv_tt, params=elnet_params_3523)

xg_best_result_3523 = make_result_form(data_name="best", n_cv_tt=n_cv_tt, params=xg_params_3523)
xg_test_result_3523 = make_result_form(data_name="test", n_cv_tt=n_cv_tt, params=xg_params_3523)

########################## 3569 ##########################
# Results in train / validation loop 
rf_train_result_3569 = rf_val_result_3569 = NULL
elnet_train_result_3569 = elnet_val_result_3569 = NULL
xg_train_result_3569 = xg_val_result_3569 = NULL

# Results in train(best parameters) / test loop
rf_best_result_3569 = make_result_form(data_name="best", n_cv_tt=n_cv_tt, params=rf_params_3569)
rf_test_result_3569 = make_result_form(data_name="test", n_cv_tt=n_cv_tt, params=rf_params_3569)

elnet_best_result_3569 = make_result_form(data_name="best", n_cv_tt=n_cv_tt, params=elnet_params_3569)
elnet_test_result_3569 = make_result_form(data_name="test", n_cv_tt=n_cv_tt, params=elnet_params_3569)

xg_best_result_3569 = make_result_form(data_name="best", n_cv_tt=n_cv_tt, params=xg_params_3569)
xg_test_result_3569 = make_result_form(data_name="test", n_cv_tt=n_cv_tt, params=xg_params_3569)

########################## 3571 ##########################
# Results in train / validation loop 
rf_train_result_3571 = rf_val_result_3571 = NULL
elnet_train_result_3571 = elnet_val_result_3571 = NULL
xg_train_result_3571 = xg_val_result_3571 = NULL

# Results in train(best parameters) / test loop
rf_best_result_3571 = make_result_form(data_name="best", n_cv_tt=n_cv_tt, params=rf_params_3571)
rf_test_result_3571 = make_result_form(data_name="test", n_cv_tt=n_cv_tt, params=rf_params_3571)

elnet_best_result_3571 = make_result_form(data_name="best", n_cv_tt=n_cv_tt, params=elnet_params_3571)
elnet_test_result_3571 = make_result_form(data_name="test", n_cv_tt=n_cv_tt, params=elnet_params_3571)

xg_best_result_3571 = make_result_form(data_name="best", n_cv_tt=n_cv_tt, params=xg_params_3571)
xg_test_result_3571 = make_result_form(data_name="test", n_cv_tt=n_cv_tt, params=xg_params_3571)

########################## 3579 ##########################
# Results in train / validation loop 
rf_train_result_3579 = rf_val_result_3579 = NULL
elnet_train_result_3579 = elnet_val_result_3579 = NULL
xg_train_result_3579 = xg_val_result_3579 = NULL

# Results in train(best parameters) / test loop
rf_best_result_3579 = make_result_form(data_name="best", n_cv_tt=n_cv_tt, params=rf_params_3579)
rf_test_result_3579 = make_result_form(data_name="test", n_cv_tt=n_cv_tt, params=rf_params_3579)

elnet_best_result_3579 = make_result_form(data_name="best", n_cv_tt=n_cv_tt, params=elnet_params_3579)
elnet_test_result_3579 = make_result_form(data_name="test", n_cv_tt=n_cv_tt, params=elnet_params_3579)

xg_best_result_3579 = make_result_form(data_name="best", n_cv_tt=n_cv_tt, params=xg_params_3579)
xg_test_result_3579 = make_result_form(data_name="test", n_cv_tt=n_cv_tt, params=xg_params_3579)

########################## 3582 ##########################
# Results in train / validation loop 
rf_train_result_3582 = rf_val_result_3582 = NULL
elnet_train_result_3582 = elnet_val_result_3582 = NULL
xg_train_result_3582 = xg_val_result_3582 = NULL

# Results in train(best parameters) / test loop
rf_best_result_3582 = make_result_form(data_name="best", n_cv_tt=n_cv_tt, params=rf_params_3582)
rf_test_result_3582 = make_result_form(data_name="test", n_cv_tt=n_cv_tt, params=rf_params_3582)

elnet_best_result_3582 = make_result_form(data_name="best", n_cv_tt=n_cv_tt, params=elnet_params_3582)
elnet_test_result_3582 = make_result_form(data_name="test", n_cv_tt=n_cv_tt, params=elnet_params_3582)

xg_best_result_3582 = make_result_form(data_name="best", n_cv_tt=n_cv_tt, params=xg_params_3582)
xg_test_result_3582 = make_result_form(data_name="test", n_cv_tt=n_cv_tt, params=xg_params_3582)

########################## 3860 ##########################
# Results in train / validation loop 
rf_train_result_3860 = rf_val_result_3860 = NULL
elnet_train_result_3860 = elnet_val_result_3860 = NULL
xg_train_result_3860 = xg_val_result_3860 = NULL

# Results in train(best parameters) / test loop
rf_best_result_3860 = make_result_form(data_name="best", n_cv_tt=n_cv_tt, params=rf_params_3860)
rf_test_result_3860 = make_result_form(data_name="test", n_cv_tt=n_cv_tt, params=rf_params_3860)

elnet_best_result_3860 = make_result_form(data_name="best", n_cv_tt=n_cv_tt, params=elnet_params_3860)
elnet_test_result_3860 = make_result_form(data_name="test", n_cv_tt=n_cv_tt, params=elnet_params_3860)

xg_best_result_3860 = make_result_form(data_name="best", n_cv_tt=n_cv_tt, params=xg_params_3860)
xg_test_result_3860 = make_result_form(data_name="test", n_cv_tt=n_cv_tt, params=xg_params_3860)

########################## 500 ##########################
# Results in train / validation loop 
rf_train_result_500 = rf_val_result_500 = NULL
elnet_train_result_500 = elnet_val_result_500 = NULL
xg_train_result_500 = xg_val_result_500 = NULL

# Results in train(best parameters) / test loop
rf_best_result_500 = make_result_form(data_name="best", n_cv_tt=n_cv_tt, params=rf_params_500)
rf_test_result_500 = make_result_form(data_name="test", n_cv_tt=n_cv_tt, params=rf_params_500)

elnet_best_result_500 = make_result_form(data_name="best", n_cv_tt=n_cv_tt, params=elnet_params_500)
elnet_test_result_500 = make_result_form(data_name="test", n_cv_tt=n_cv_tt, params=elnet_params_500)

xg_best_result_500 = make_result_form(data_name="best", n_cv_tt=n_cv_tt, params=xg_params_500)
xg_test_result_500 = make_result_form(data_name="test", n_cv_tt=n_cv_tt, params=xg_params_500)

########################## 590 ##########################
# Results in train / validation loop 
rf_train_result_590 = rf_val_result_590 = NULL
elnet_train_result_590 = elnet_val_result_590 = NULL
xg_train_result_590 = xg_val_result_590 = NULL

# Results in train(best parameters) / test loop
rf_best_result_590 = make_result_form(data_name="best", n_cv_tt=n_cv_tt, params=rf_params_590)
rf_test_result_590 = make_result_form(data_name="test", n_cv_tt=n_cv_tt, params=rf_params_590)

elnet_best_result_590 = make_result_form(data_name="best", n_cv_tt=n_cv_tt, params=elnet_params_590)
elnet_test_result_590 = make_result_form(data_name="test", n_cv_tt=n_cv_tt, params=elnet_params_590)

xg_best_result_590 = make_result_form(data_name="best", n_cv_tt=n_cv_tt, params=xg_params_590)
xg_test_result_590 = make_result_form(data_name="test", n_cv_tt=n_cv_tt, params=xg_params_590)

########################## 591 ##########################
# Results in train / validation loop 
rf_train_result_591 = rf_val_result_591 = NULL
elnet_train_result_591 = elnet_val_result_591 = NULL
xg_train_result_591 = xg_val_result_591 = NULL

# Results in train(best parameters) / test loop
rf_best_result_591 = make_result_form(data_name="best", n_cv_tt=n_cv_tt, params=rf_params_591)
rf_test_result_591 = make_result_form(data_name="test", n_cv_tt=n_cv_tt, params=rf_params_591)

elnet_best_result_591 = make_result_form(data_name="best", n_cv_tt=n_cv_tt, params=elnet_params_591)
elnet_test_result_591 = make_result_form(data_name="test", n_cv_tt=n_cv_tt, params=elnet_params_591)

xg_best_result_591 = make_result_form(data_name="best", n_cv_tt=n_cv_tt, params=xg_params_591)
xg_test_result_591 = make_result_form(data_name="test", n_cv_tt=n_cv_tt, params=xg_params_591)

########################## 592 ##########################
# Results in train / validation loop 
rf_train_result_592 = rf_val_result_592 = NULL
elnet_train_result_592 = elnet_val_result_592 = NULL
xg_train_result_592 = xg_val_result_592 = NULL

# Results in train(best parameters) / test loop
rf_best_result_592 = make_result_form(data_name="best", n_cv_tt=n_cv_tt, params=rf_params_592)
rf_test_result_592 = make_result_form(data_name="test", n_cv_tt=n_cv_tt, params=rf_params_592)

elnet_best_result_592 = make_result_form(data_name="best", n_cv_tt=n_cv_tt, params=elnet_params_592)
elnet_test_result_592 = make_result_form(data_name="test", n_cv_tt=n_cv_tt, params=elnet_params_592)

xg_best_result_592 = make_result_form(data_name="best", n_cv_tt=n_cv_tt, params=xg_params_592)
xg_test_result_592 = make_result_form(data_name="test", n_cv_tt=n_cv_tt, params=xg_params_592)

########################## gu ##########################
# Results in train / validation loop 
rf_train_result_gu = rf_val_result_gu = NULL
elnet_train_result_gu = elnet_val_result_gu = NULL
xg_train_result_gu = xg_val_result_gu = NULL

# Results in train(best parameters) / test loop
rf_best_result_gu = make_result_form(data_name="best", n_cv_tt=n_cv_tt, params=rf_params_gu)
rf_test_result_gu = make_result_form(data_name="test", n_cv_tt=n_cv_tt, params=rf_params_gu)
# (구, 개별 11개 대여소 결과 저장)
rf_test_result_gu <- do.call(rbind, replicate(12, rf_test_result_gu, simplify = FALSE))

elnet_best_result_gu = make_result_form(data_name="best", n_cv_tt=n_cv_tt, params=elnet_params_gu)
elnet_test_result_gu = make_result_form(data_name="test", n_cv_tt=n_cv_tt, params=elnet_params_gu)
# (구, 개별 11개 대여소 결과 저장)
elnet_test_result_gu <- do.call(rbind, replicate(12, elnet_test_result_gu, simplify = FALSE))

xg_best_result_gu = make_result_form(data_name="best", n_cv_tt=n_cv_tt, params=xg_params_gu)
xg_test_result_gu = make_result_form(data_name="test", n_cv_tt=n_cv_tt, params=xg_params_gu)
# (구, 개별 11개 대여소 결과 저장)
xg_test_result_gu <- do.call(rbind, replicate(12, xg_test_result_gu, simplify = FALSE))


########################## dong ##########################
# Results in train / validation loop 
rf_train_result_dong = rf_val_result_dong = NULL
elnet_train_result_dong = elnet_val_result_dong = NULL
xg_train_result_dong = xg_val_result_dong = NULL

# Results in train(best parameters) / test loop
rf_best_result_dong = make_result_form(data_name="best", n_cv_tt=n_cv_tt, params=rf_params_dong)
rf_test_result_dong = make_result_form(data_name="test", n_cv_tt=n_cv_tt, params=rf_params_dong)
# (동, 개별 11개 대여소 결과 저장)
rf_test_result_dong <- do.call(rbind, replicate(12, rf_test_result_dong, simplify = FALSE))

elnet_best_result_dong = make_result_form(data_name="best", n_cv_tt=n_cv_tt, params=elnet_params_dong)
elnet_test_result_dong = make_result_form(data_name="test", n_cv_tt=n_cv_tt, params=elnet_params_dong)
# (동, 개별 11개 대여소 결과 저장)
elnet_test_result_dong <- do.call(rbind, replicate(12, elnet_test_result_dong, simplify = FALSE))

xg_best_result_dong = make_result_form(data_name="best", n_cv_tt=n_cv_tt, params=xg_params_dong)
xg_test_result_dong = make_result_form(data_name="test", n_cv_tt=n_cv_tt, params=xg_params_dong)
# (동, 개별 11개 대여소 결과 저장)
xg_test_result_dong <- do.call(rbind, replicate(12, xg_test_result_dong, simplify = FALSE))

########################## all individual ##########################
# combine all individual station test result
rf_test_result_all = make_result_form(data_name="test", n_cv_tt=n_cv_tt, params=rf_params_3508)
rf_test_result_all <- do.call(rbind, replicate(11, rf_test_result_all, simplify = FALSE))

elnet_test_result_all = make_result_form(data_name="test", n_cv_tt=n_cv_tt, params=elnet_params_3508)
elnet_test_result_all <- do.call(rbind, replicate(11, elnet_test_result_all, simplify = FALSE))

xg_test_result_all = make_result_form(data_name="test", n_cv_tt=n_cv_tt, params=xg_params_3508)
xg_test_result_all <- do.call(rbind, replicate(11, xg_test_result_all, simplify = FALSE))

# --------------------------------------------------- #
#
# 3. Run model
#
# --------------------------------------------------- #

# to split data
n = 365*24
idx = 1:n

for (iter in 1:n_cv_tt){
    print(paste("#", iter, "cv train/test"))
    # split data --> train / test with 80:20
    train_val_idx = sample(idx, n*0.8)
    test_idx = setdiff(idx, train_val_idx)
    
    test_3508 = data_3508[test_idx, ]
    test_3523 = data_3523[test_idx, ]
    test_3569 = data_3569[test_idx, ]
    test_3571 = data_3571[test_idx, ]
    test_3579 = data_3579[test_idx, ]
    test_3582 = data_3582[test_idx, ]
    test_3860 = data_3860[test_idx, ]
    test_500 = data_500[test_idx, ]
    test_590 = data_590[test_idx, ]
    test_591 = data_591[test_idx, ]
    test_592 = data_592[test_idx, ]
    test_gu = gu[test_idx, ]
    test_dong = dong[test_idx, ]
    
    for (iiter in 1:n_cv_tv){
        print(paste("#", iiter, "cv train/val"))
        # split train --> train / validation with 40:40
        train_idx = sample(train_val_idx, size=n*0.4)
        val_idx = setdiff(train_val_idx, train_idx)
        
        train_3508 = data_3508[train_idx, ]
        train_3523 = data_3523[train_idx, ]
        train_3569 = data_3569[train_idx, ]
        train_3571 = data_3571[train_idx, ]
        train_3579 = data_3579[train_idx, ]
        train_3582 = data_3582[train_idx, ]
        train_3860 = data_3860[train_idx, ]
        train_500 = data_500[train_idx, ]
        train_590 = data_590[train_idx, ]
        train_591 = data_591[train_idx, ]
        train_592 = data_592[train_idx, ]
        train_gu = gu[train_idx, ]
        train_dong = dong[train_idx, ]
        
        val_3508 = data_3508[val_idx, ]
        val_3523 = data_3523[val_idx, ]
        val_3569 = data_3569[val_idx, ]
        val_3571 = data_3571[val_idx, ]
        val_3579 = data_3579[val_idx, ]
        val_3582 = data_3582[val_idx, ]
        val_3860 = data_3860[val_idx, ]
        val_500 = data_500[val_idx, ]
        val_590 = data_590[val_idx, ]
        val_591 = data_591[val_idx, ]
        val_592 = data_592[val_idx, ]
        val_gu = gu[val_idx, ]
        val_dong = dong[val_idx, ]
        
        ########################## 3508 ##########################
        print(paste("#", iiter, "cv train/val", 3508))
        # mse of random forest
        rf_tmp_3508 = make_reult_train_val_mse(model="rf", params=rf_params_3508, 
                                               train=train_3508, val=val_3508)
        rf_train_result_3508 = rbind(rf_train_result_3508, rf_tmp_3508$train_mse_mat)
        rf_val_result_3508 = rbind(rf_val_result_3508, rf_tmp_3508$val_mse_mat)
        
        # mse of elastic net
        elnet_tmp_3508 = make_reult_train_val_mse(model="elnet", params=elnet_params_3508, 
                                                  train=train_3508, val=val_3508)
        elnet_train_result_3508 = rbind(elnet_train_result_3508, elnet_tmp_3508$train_mse_mat)
        elnet_val_result_3508 = rbind(elnet_val_result_3508, elnet_tmp_3508$val_mse_mat)
        
        # mse of xgboost
        xg_tmp_3508 = make_reult_train_val_mse(model="xg", params=xg_params_3508, 
                                               train=train_3508, val=val_3508)
        xg_train_result_3508 = rbind(xg_train_result_3508, xg_tmp_3508$train_mse_mat)
        xg_val_result_3508 = rbind(xg_val_result_3508, xg_tmp_3508$val_mse_mat)
        
        ########################## 3523 ##########################
        print(paste("#", iiter, "cv train/val", 3523))
        # mse of random forest
        rf_tmp_3523 = make_reult_train_val_mse(model="rf", params=rf_params_3523, 
                                               train=train_3523, val=val_3523)
        rf_train_result_3523 = rbind(rf_train_result_3523, rf_tmp_3523$train_mse_mat)
        rf_val_result_3523 = rbind(rf_val_result_3523, rf_tmp_3523$val_mse_mat)
        
        # mse of elastic net
        elnet_tmp_3523 = make_reult_train_val_mse(model="elnet", params=elnet_params_3523, 
                                                  train=train_3523, val=val_3523)
        elnet_train_result_3523 = rbind(elnet_train_result_3523, elnet_tmp_3523$train_mse_mat)
        elnet_val_result_3523 = rbind(elnet_val_result_3523, elnet_tmp_3523$val_mse_mat)
        
        # mse of xgboost
        xg_tmp_3523 = make_reult_train_val_mse(model="xg", params=xg_params_3523, 
                                               train=train_3523, val=val_3523)
        xg_train_result_3523 = rbind(xg_train_result_3523, xg_tmp_3523$train_mse_mat)
        xg_val_result_3523 = rbind(xg_val_result_3523, xg_tmp_3523$val_mse_mat)
        
        ########################## 3569 ##########################
        print(paste("#", iiter, "cv train/val", 3569))
        # mse of random forest
        rf_tmp_3569 = make_reult_train_val_mse(model="rf", params=rf_params_3569, 
                                               train=train_3569, val=val_3569)
        rf_train_result_3569 = rbind(rf_train_result_3569, rf_tmp_3569$train_mse_mat)
        rf_val_result_3569 = rbind(rf_val_result_3569, rf_tmp_3569$val_mse_mat)
        
        # mse of elastic net
        elnet_tmp_3569 = make_reult_train_val_mse(model="elnet", params=elnet_params_3569, 
                                                  train=train_3569, val=val_3569)
        elnet_train_result_3569 = rbind(elnet_train_result_3569, elnet_tmp_3569$train_mse_mat)
        elnet_val_result_3569 = rbind(elnet_val_result_3569, elnet_tmp_3569$val_mse_mat)
        
        # mse of xgboost
        xg_tmp_3569 = make_reult_train_val_mse(model="xg", params=xg_params_3569, 
                                               train=train_3569, val=val_3569)
        xg_train_result_3569 = rbind(xg_train_result_3569, xg_tmp_3569$train_mse_mat)
        xg_val_result_3569 = rbind(xg_val_result_3569, xg_tmp_3569$val_mse_mat)
        
        ########################## 3571 ##########################
        print(paste("#", iiter, "cv train/val", 3571))
        # mse of random forest
        rf_tmp_3571 = make_reult_train_val_mse(model="rf", params=rf_params_3571, 
                                               train=train_3571, val=val_3571)
        rf_train_result_3571 = rbind(rf_train_result_3571, rf_tmp_3571$train_mse_mat)
        rf_val_result_3571 = rbind(rf_val_result_3571, rf_tmp_3571$val_mse_mat)
        
        # mse of elastic net
        elnet_tmp_3571 = make_reult_train_val_mse(model="elnet", params=elnet_params_3571, 
                                                  train=train_3571, val=val_3571)
        elnet_train_result_3571 = rbind(elnet_train_result_3571, elnet_tmp_3571$train_mse_mat)
        elnet_val_result_3571 = rbind(elnet_val_result_3571, elnet_tmp_3571$val_mse_mat)
        
        # mse of xgboost
        xg_tmp_3571 = make_reult_train_val_mse(model="xg", params=xg_params_3571, 
                                               train=train_3571, val=val_3571)
        xg_train_result_3571 = rbind(xg_train_result_3571, xg_tmp_3571$train_mse_mat)
        xg_val_result_3571 = rbind(xg_val_result_3571, xg_tmp_3571$val_mse_mat)
        
        ########################## 3579 ##########################
        print(paste("#", iiter, "cv train/val", 3579))
        # mse of random forest
        rf_tmp_3579 = make_reult_train_val_mse(model="rf", params=rf_params_3579, 
                                               train=train_3579, val=val_3579)
        rf_train_result_3579 = rbind(rf_train_result_3579, rf_tmp_3579$train_mse_mat)
        rf_val_result_3579 = rbind(rf_val_result_3579, rf_tmp_3579$val_mse_mat)
        
        # mse of elastic net
        elnet_tmp_3579 = make_reult_train_val_mse(model="elnet", params=elnet_params_3579, 
                                                  train=train_3579, val=val_3579)
        elnet_train_result_3579 = rbind(elnet_train_result_3579, elnet_tmp_3579$train_mse_mat)
        elnet_val_result_3579 = rbind(elnet_val_result_3579, elnet_tmp_3579$val_mse_mat)
        
        # mse of xgboost
        xg_tmp_3579 = make_reult_train_val_mse(model="xg", params=xg_params_3579, 
                                               train=train_3579, val=val_3579)
        xg_train_result_3579 = rbind(xg_train_result_3579, xg_tmp_3579$train_mse_mat)
        xg_val_result_3579 = rbind(xg_val_result_3579, xg_tmp_3579$val_mse_mat)
        
        ########################## 3582 ##########################
        print(paste("#", iiter, "cv train/val", 3582))
        # mse of random forest
        rf_tmp_3582 = make_reult_train_val_mse(model="rf", params=rf_params_3582, 
                                               train=train_3582, val=val_3582)
        rf_train_result_3582 = rbind(rf_train_result_3582, rf_tmp_3582$train_mse_mat)
        rf_val_result_3582 = rbind(rf_val_result_3582, rf_tmp_3582$val_mse_mat)
        
        # mse of elastic net
        elnet_tmp_3582 = make_reult_train_val_mse(model="elnet", params=elnet_params_3582, 
                                                  train=train_3582, val=val_3582)
        elnet_train_result_3582 = rbind(elnet_train_result_3582, elnet_tmp_3582$train_mse_mat)
        elnet_val_result_3582 = rbind(elnet_val_result_3582, elnet_tmp_3582$val_mse_mat)
        
        # mse of xgboost
        xg_tmp_3582 = make_reult_train_val_mse(model="xg", params=xg_params_3582, 
                                               train=train_3582, val=val_3582)
        xg_train_result_3582 = rbind(xg_train_result_3582, xg_tmp_3582$train_mse_mat)
        xg_val_result_3582 = rbind(xg_val_result_3582, xg_tmp_3582$val_mse_mat)
        
        ########################## 3860 ##########################
        print(paste("#", iiter, "cv train/val", 3860))
        # mse of random forest
        rf_tmp_3860 = make_reult_train_val_mse(model="rf", params=rf_params_3860, 
                                               train=train_3860, val=val_3860)
        rf_train_result_3860 = rbind(rf_train_result_3860, rf_tmp_3860$train_mse_mat)
        rf_val_result_3860 = rbind(rf_val_result_3860, rf_tmp_3860$val_mse_mat)
        
        # mse of elastic net
        elnet_tmp_3860 = make_reult_train_val_mse(model="elnet", params=elnet_params_3860, 
                                                  train=train_3860, val=val_3860)
        elnet_train_result_3860 = rbind(elnet_train_result_3860, elnet_tmp_3860$train_mse_mat)
        elnet_val_result_3860 = rbind(elnet_val_result_3860, elnet_tmp_3860$val_mse_mat)
        
        # mse of xgboost
        xg_tmp_3860 = make_reult_train_val_mse(model="xg", params=xg_params_3860, 
                                               train=train_3860, val=val_3860)
        xg_train_result_3860 = rbind(xg_train_result_3860, xg_tmp_3860$train_mse_mat)
        xg_val_result_3860 = rbind(xg_val_result_3860, xg_tmp_3860$val_mse_mat)
        
        ########################## 500 ##########################
        print(paste("#", iiter, "cv train/val", 500))
        # mse of random forest
        rf_tmp_500 = make_reult_train_val_mse(model="rf", params=rf_params_500, 
                                              train=train_500, val=val_500)
        rf_train_result_500 = rbind(rf_train_result_500, rf_tmp_500$train_mse_mat)
        rf_val_result_500 = rbind(rf_val_result_500, rf_tmp_500$val_mse_mat)
        
        # mse of elastic net
        elnet_tmp_500 = make_reult_train_val_mse(model="elnet", params=elnet_params_500, 
                                                 train=train_500, val=val_500)
        elnet_train_result_500 = rbind(elnet_train_result_500, elnet_tmp_500$train_mse_mat)
        elnet_val_result_500 = rbind(elnet_val_result_500, elnet_tmp_500$val_mse_mat)
        
        # mse of xgboost
        xg_tmp_500 = make_reult_train_val_mse(model="xg", params=xg_params_500, 
                                              train=train_500, val=val_500)
        xg_train_result_500 = rbind(xg_train_result_500, xg_tmp_500$train_mse_mat)
        xg_val_result_500 = rbind(xg_val_result_500, xg_tmp_500$val_mse_mat)
        
        ########################## 590 ##########################
        print(paste("#", iiter, "cv train/val", 590))
        # mse of random forest
        rf_tmp_590 = make_reult_train_val_mse(model="rf", params=rf_params_590, 
                                              train=train_590, val=val_590)
        rf_train_result_590 = rbind(rf_train_result_590, rf_tmp_590$train_mse_mat)
        rf_val_result_590 = rbind(rf_val_result_590, rf_tmp_590$val_mse_mat)
        
        # mse of elastic net
        elnet_tmp_590 = make_reult_train_val_mse(model="elnet", params=elnet_params_590, 
                                                 train=train_590, val=val_590)
        elnet_train_result_590 = rbind(elnet_train_result_590, elnet_tmp_590$train_mse_mat)
        elnet_val_result_590 = rbind(elnet_val_result_590, elnet_tmp_590$val_mse_mat)
        
        # mse of xgboost
        xg_tmp_590 = make_reult_train_val_mse(model="xg", params=xg_params_590, 
                                              train=train_590, val=val_590)
        xg_train_result_590 = rbind(xg_train_result_590, xg_tmp_590$train_mse_mat)
        xg_val_result_590 = rbind(xg_val_result_590, xg_tmp_590$val_mse_mat)
        
        ########################## 591 ##########################
        print(paste("#", iiter, "cv train/val", 591))
        # mse of random forest
        rf_tmp_591 = make_reult_train_val_mse(model="rf", params=rf_params_591, 
                                              train=train_591, val=val_591)
        rf_train_result_591 = rbind(rf_train_result_591, rf_tmp_591$train_mse_mat)
        rf_val_result_591 = rbind(rf_val_result_591, rf_tmp_591$val_mse_mat)
        
        # mse of elastic net
        elnet_tmp_591 = make_reult_train_val_mse(model="elnet", params=elnet_params_591, 
                                                 train=train_591, val=val_591)
        elnet_train_result_591 = rbind(elnet_train_result_591, elnet_tmp_591$train_mse_mat)
        elnet_val_result_591 = rbind(elnet_val_result_591, elnet_tmp_591$val_mse_mat)
        
        # mse of xgboost
        xg_tmp_591 = make_reult_train_val_mse(model="xg", params=xg_params_591, 
                                              train=train_591, val=val_591)
        xg_train_result_591 = rbind(xg_train_result_591, xg_tmp_591$train_mse_mat)
        xg_val_result_591 = rbind(xg_val_result_591, xg_tmp_591$val_mse_mat)
        
        ########################## 592 ##########################
        print(paste("#", iiter, "cv train/val", 592))
        # mse of random forest
        rf_tmp_592 = make_reult_train_val_mse(model="rf", params=rf_params_592, 
                                              train=train_592, val=val_592)
        rf_train_result_592 = rbind(rf_train_result_592, rf_tmp_592$train_mse_mat)
        rf_val_result_592 = rbind(rf_val_result_592, rf_tmp_592$val_mse_mat)
        
        # mse of elastic net
        elnet_tmp_592 = make_reult_train_val_mse(model="elnet", params=elnet_params_592, 
                                                 train=train_592, val=val_592)
        elnet_train_result_592 = rbind(elnet_train_result_592, elnet_tmp_592$train_mse_mat)
        elnet_val_result_592 = rbind(elnet_val_result_592, elnet_tmp_592$val_mse_mat)
        
        # mse of xgboost
        xg_tmp_592 = make_reult_train_val_mse(model="xg", params=xg_params_592, 
                                              train=train_592, val=val_592)
        xg_train_result_592 = rbind(xg_train_result_592, xg_tmp_592$train_mse_mat)
        xg_val_result_592 = rbind(xg_val_result_592, xg_tmp_592$val_mse_mat)
        
        ########################## gu ##########################
        print(paste("#", iiter, "cv train/val", "gu"))
        # mse of random forest
        rf_tmp_gu = make_reult_train_val_mse(model="rf", params=rf_params_gu, 
                                             train=train_gu, val=val_gu)
        rf_train_result_gu = rbind(rf_train_result_gu, rf_tmp_gu$train_mse_mat)
        rf_val_result_gu = rbind(rf_val_result_gu, rf_tmp_gu$val_mse_mat)
        
        # mse of elastic net
        elnet_tmp_gu = make_reult_train_val_mse(model="elnet", params=elnet_params_gu, 
                                                train=train_gu, val=val_gu)
        elnet_train_result_gu = rbind(elnet_train_result_gu, elnet_tmp_gu$train_mse_mat)
        elnet_val_result_gu = rbind(elnet_val_result_gu, elnet_tmp_gu$val_mse_mat)
        
        # mse of xgboost
        xg_tmp_gu = make_reult_train_val_mse(model="xg", params=xg_params_gu, 
                                             train=train_gu, val=val_gu)
        xg_train_result_gu = rbind(xg_train_result_gu, xg_tmp_gu$train_mse_mat)
        xg_val_result_gu = rbind(xg_val_result_gu, xg_tmp_gu$val_mse_mat)
        
        ########################## dong ##########################
        print(paste("#", iiter, "cv train/val", "dong"))
        # mse of random forest
        rf_tmp_dong = make_reult_train_val_mse(model="rf", params=rf_params_dong, 
                                               train=train_dong, val=val_dong)
        rf_train_result_dong = rbind(rf_train_result_dong, rf_tmp_dong$train_mse_mat)
        rf_val_result_dong = rbind(rf_val_result_dong, rf_tmp_dong$val_mse_mat)
        
        # mse of elastic net
        elnet_tmp_dong = make_reult_train_val_mse(model="elnet", params=elnet_params_dong, 
                                                  train=train_dong, val=val_dong)
        elnet_train_result_dong = rbind(elnet_train_result_dong, elnet_tmp_dong$train_mse_mat)
        elnet_val_result_dong = rbind(elnet_val_result_dong, elnet_tmp_dong$val_mse_mat)
        
        # mse of xgboost
        xg_tmp_dong = make_reult_train_val_mse(model="xg", params=xg_params_dong, 
                                               train=train_dong, val=val_dong)
        xg_train_result_dong = rbind(xg_train_result_dong, xg_tmp_dong$train_mse_mat)
        xg_val_result_dong = rbind(xg_val_result_dong, xg_tmp_dong$val_mse_mat)
        
    }
    
    # ------------------- find the best hyper parameters ------------------ #
    
    ########################## 3508 ##########################
    rf_best_com_3508 = find_best_combination(rf_val_result_3508, rf_params_3508)
    elnet_best_com_3508 = find_best_combination(elnet_val_result_3508, elnet_params_3508)
    xg_best_com_3508 = find_best_combination(xg_val_result_3508, xg_params_3508)
    
    ########################## 3523 ##########################
    rf_best_com_3523 = find_best_combination(rf_val_result_3523, rf_params_3523)
    elnet_best_com_3523 = find_best_combination(elnet_val_result_3523, elnet_params_3523)
    xg_best_com_3523 = find_best_combination(xg_val_result_3523, xg_params_3523)
    
    ########################## 3569 ##########################
    rf_best_com_3569 = find_best_combination(rf_val_result_3569, rf_params_3569)
    elnet_best_com_3569 = find_best_combination(elnet_val_result_3569, elnet_params_3569)
    xg_best_com_3569 = find_best_combination(xg_val_result_3569, xg_params_3569)
    
    ########################## 3571 ##########################
    rf_best_com_3571 = find_best_combination(rf_val_result_3571, rf_params_3571)
    elnet_best_com_3571 = find_best_combination(elnet_val_result_3571, elnet_params_3571)
    xg_best_com_3571 = find_best_combination(xg_val_result_3571, xg_params_3571)
    
    ########################## 3579 ##########################
    rf_best_com_3579 = find_best_combination(rf_val_result_3579, rf_params_3579)
    elnet_best_com_3579 = find_best_combination(elnet_val_result_3579, elnet_params_3579)
    xg_best_com_3579 = find_best_combination(xg_val_result_3579, xg_params_3579)
    
    ########################## 3582 ##########################
    rf_best_com_3582 = find_best_combination(rf_val_result_3582, rf_params_3582)
    elnet_best_com_3582 = find_best_combination(elnet_val_result_3582, elnet_params_3582)
    xg_best_com_3582 = find_best_combination(xg_val_result_3582, xg_params_3582)
    
    ########################## 3860 ##########################
    rf_best_com_3860 = find_best_combination(rf_val_result_3860, rf_params_3860)
    elnet_best_com_3860 = find_best_combination(elnet_val_result_3860, elnet_params_3860)
    xg_best_com_3860 = find_best_combination(xg_val_result_3860, xg_params_3860)
    
    ########################## 500 ##########################
    rf_best_com_500 = find_best_combination(rf_val_result_500, rf_params_500)
    elnet_best_com_500 = find_best_combination(elnet_val_result_500, elnet_params_500)
    xg_best_com_500 = find_best_combination(xg_val_result_500, xg_params_500)
    
    ########################## 590 ##########################
    rf_best_com_590 = find_best_combination(rf_val_result_590, rf_params_590)
    elnet_best_com_590 = find_best_combination(elnet_val_result_590, elnet_params_590)
    xg_best_com_590 = find_best_combination(xg_val_result_590, xg_params_590)
    
    ########################## 591 ##########################
    rf_best_com_591 = find_best_combination(rf_val_result_591, rf_params_591)
    elnet_best_com_591 = find_best_combination(elnet_val_result_591, elnet_params_591)
    xg_best_com_591 = find_best_combination(xg_val_result_591, xg_params_591)
    
    ########################## 592 ##########################
    rf_best_com_592 = find_best_combination(rf_val_result_592, rf_params_592)
    elnet_best_com_592 = find_best_combination(elnet_val_result_592, elnet_params_592)
    xg_best_com_592 = find_best_combination(xg_val_result_592, xg_params_592)
    
    ########################## gu ##########################
    rf_best_com_gu = find_best_combination(rf_val_result_gu, rf_params_gu)
    elnet_best_com_gu = find_best_combination(elnet_val_result_gu, elnet_params_gu)
    xg_best_com_gu = find_best_combination(xg_val_result_gu, xg_params_gu)
    
    ########################## dong ##########################
    rf_best_com_dong = find_best_combination(rf_val_result_dong, rf_params_dong)
    elnet_best_com_dong = find_best_combination(elnet_val_result_dong, elnet_params_dong)
    xg_best_com_dong = find_best_combination(xg_val_result_dong, xg_params_dong)
    
    # --------------- Make model with the best hyper parameters --------------- #
    # And save the best model's mse and test mse
    
    # Make best model with (train + val) data
    train_val_3508 = data_3508[-test_idx, ]
    train_val_3523 = data_3523[-test_idx, ]
    train_val_3569 = data_3569[-test_idx, ]
    train_val_3571 = data_3571[-test_idx, ]
    train_val_3579 = data_3579[-test_idx, ]
    train_val_3582 = data_3582[-test_idx, ]
    train_val_3860 = data_3860[-test_idx, ]
    train_val_500 = data_500[-test_idx, ]
    train_val_590 = data_590[-test_idx, ]
    train_val_591 = data_591[-test_idx, ]
    train_val_592 = data_592[-test_idx, ]
    train_val_gu = gu[-test_idx, ]
    train_val_dong = dong[-test_idx, ]
    
    ########################## 3508 ##########################
    print(paste("#", iter, "cv train/test", 3508))
    # random forest
    rf_best_fit_3508 = randomForest(train_val_3508[, 1] ~., data=train_val_3508, 
                                    ntree=rf_best_com_3508$ntree, mtry=rf_best_com_3508$mtry)
    
    rf_best_mse_3508 = calculate_mse(model='rf', fit=rf_best_fit_3508, train_val_3508)
    rf_test_mse_3508 = calculate_mse(model='rf', fit=rf_best_fit_3508, test_3508)
    
    rf_best_result_3508[iter, ] = c(rf_best_com_3508$ntree, rf_best_com_3508$mtry, rf_best_mse_3508)
    rf_test_result_3508[iter, ] = c(rf_best_com_3508$ntree, rf_best_com_3508$mtry, rf_test_mse_3508)
    
    # elastic net
    elnet_best_fit_3508 = glmnet(x=train_val_3508[, -1], y=train_val_3508[, 1], 
                                 alpha=elnet_best_com_3508$alpha, lambda=elnet_best_com_3508$alpha)
    
    elnet_best_mse_3508 = calculate_mse(model='elnet', fit=elnet_best_fit_3508, train_val_3508)
    elnet_test_mse_3508 = calculate_mse(model='elnet', fit=elnet_best_fit_3508, test_3508)
    
    elnet_best_result_3508[iter, ] = c(elnet_best_com_3508$alpha, elnet_best_com_3508$lambda, elnet_best_mse_3508)
    elnet_test_result_3508[iter, ] = c(elnet_best_com_3508$alpha, elnet_best_com_3508$lambda, elnet_test_mse_3508)
    
    # xgboost
    xg_best_fit_3508 = xgboost(data=as.matrix(train_val_3508[, -1]), label=train_val_3508[, 1], verbose=0,
                               nrounds=xg_best_com_3508$nrounds, 
                               max_depth=xg_best_com_3508$max_depth, 
                               eta=xg_best_com_3508$eta)
    
    xg_best_mse_3508 = calculate_mse(model='xg', fit=xg_best_fit_3508, train_val_3508)
    xg_test_mse_3508 = calculate_mse(model='xg', fit=xg_best_fit_3508, test_3508)
    
    xg_best_result_3508[iter, ] = c(xg_best_com_3508$nrounds, 
                                    xg_best_com_3508$max_depth, 
                                    xg_best_com_3508$eta, 
                                    xg_best_mse_3508)
    xg_test_result_3508[iter, ] = c(xg_best_com_3508$nrounds, 
                                    xg_best_com_3508$max_depth, 
                                    xg_best_com_3508$eta, 
                                    xg_test_mse_3508)
    
    ########################## 3523 ##########################
    print(paste("#", iter, "cv train/test", 3523))
    # random forest
    rf_best_fit_3523 = randomForest(train_val_3523[, 1] ~., data=train_val_3523, 
                                    ntree=rf_best_com_3523$ntree, mtry=rf_best_com_3523$mtry)
    
    rf_best_mse_3523 = calculate_mse(model='rf', fit=rf_best_fit_3523, train_val_3523)
    rf_test_mse_3523 = calculate_mse(model='rf', fit=rf_best_fit_3523, test_3523)
    
    rf_best_result_3523[iter, ] = c(rf_best_com_3523$ntree, rf_best_com_3523$mtry, rf_best_mse_3523)
    rf_test_result_3523[iter, ] = c(rf_best_com_3523$ntree, rf_best_com_3523$mtry, rf_test_mse_3523)
    
    # elastic net
    elnet_best_fit_3523 = glmnet(x=train_val_3523[, -1], y=train_val_3523[, 1], 
                                 alpha=elnet_best_com_3523$alpha, lambda=elnet_best_com_3523$alpha)
    
    elnet_best_mse_3523 = calculate_mse(model='elnet', fit=elnet_best_fit_3523, train_val_3523)
    elnet_test_mse_3523 = calculate_mse(model='elnet', fit=elnet_best_fit_3523, test_3523)
    
    elnet_best_result_3523[iter, ] = c(elnet_best_com_3523$alpha, elnet_best_com_3523$lambda, elnet_best_mse_3523)
    elnet_test_result_3523[iter, ] = c(elnet_best_com_3523$alpha, elnet_best_com_3523$lambda, elnet_test_mse_3523)
    
    # xgboost
    xg_best_fit_3523 = xgboost(data=as.matrix(train_val_3523[, -1]), label=train_val_3523[, 1], verbose=0,
                               nrounds=xg_best_com_3523$nrounds, 
                               max_depth=xg_best_com_3523$max_depth, 
                               eta=xg_best_com_3523$eta)
    
    xg_best_mse_3523 = calculate_mse(model='xg', fit=xg_best_fit_3523, train_val_3523)
    xg_test_mse_3523 = calculate_mse(model='xg', fit=xg_best_fit_3523, test_3523)
    
    xg_best_result_3523[iter, ] = c(xg_best_com_3523$nrounds, 
                                    xg_best_com_3523$max_depth, 
                                    xg_best_com_3523$eta, 
                                    xg_best_mse_3523)
    xg_test_result_3523[iter, ] = c(xg_best_com_3523$nrounds, 
                                    xg_best_com_3523$max_depth, 
                                    xg_best_com_3523$eta, 
                                    xg_test_mse_3523)
    
    ########################## 3569 ##########################
    print(paste("#", iter, "cv train/test", 3569))
    # random forest
    rf_best_fit_3569 = randomForest(train_val_3569[, 1] ~., data=train_val_3569, 
                                    ntree=rf_best_com_3569$ntree, mtry=rf_best_com_3569$mtry)
    
    rf_best_mse_3569 = calculate_mse(model='rf', fit=rf_best_fit_3569, train_val_3569)
    rf_test_mse_3569 = calculate_mse(model='rf', fit=rf_best_fit_3569, test_3569)
    
    rf_best_result_3569[iter, ] = c(rf_best_com_3569$ntree, rf_best_com_3569$mtry, rf_best_mse_3569)
    rf_test_result_3569[iter, ] = c(rf_best_com_3569$ntree, rf_best_com_3569$mtry, rf_test_mse_3569)
    
    # elastic net
    elnet_best_fit_3569 = glmnet(x=train_val_3569[, -1], y=train_val_3569[, 1], 
                                 alpha=elnet_best_com_3569$alpha, lambda=elnet_best_com_3569$alpha)
    
    elnet_best_mse_3569 = calculate_mse(model='elnet', fit=elnet_best_fit_3569, train_val_3569)
    elnet_test_mse_3569 = calculate_mse(model='elnet', fit=elnet_best_fit_3569, test_3569)
    
    elnet_best_result_3569[iter, ] = c(elnet_best_com_3569$alpha, elnet_best_com_3569$lambda, elnet_best_mse_3569)
    elnet_test_result_3569[iter, ] = c(elnet_best_com_3569$alpha, elnet_best_com_3569$lambda, elnet_test_mse_3569)
    
    # xgboost
    xg_best_fit_3569 = xgboost(data=as.matrix(train_val_3569[, -1]), label=train_val_3569[, 1], verbose=0,
                               nrounds=xg_best_com_3569$nrounds, 
                               max_depth=xg_best_com_3569$max_depth, 
                               eta=xg_best_com_3569$eta)
    
    xg_best_mse_3569 = calculate_mse(model='xg', fit=xg_best_fit_3569, train_val_3569)
    xg_test_mse_3569 = calculate_mse(model='xg', fit=xg_best_fit_3569, test_3569)
    
    xg_best_result_3569[iter, ] = c(xg_best_com_3569$nrounds, 
                                    xg_best_com_3569$max_depth, 
                                    xg_best_com_3569$eta, 
                                    xg_best_mse_3569)
    xg_test_result_3569[iter, ] = c(xg_best_com_3569$nrounds, 
                                    xg_best_com_3569$max_depth, 
                                    xg_best_com_3569$eta, 
                                    xg_test_mse_3569)
    
    ########################## 3571 ##########################
    print(paste("#", iter, "cv train/test", 3571))
    # random forest
    rf_best_fit_3571 = randomForest(train_val_3571[, 1] ~., data=train_val_3571, 
                                    ntree=rf_best_com_3571$ntree, mtry=rf_best_com_3571$mtry)
    
    rf_best_mse_3571 = calculate_mse(model='rf', fit=rf_best_fit_3571, train_val_3571)
    rf_test_mse_3571 = calculate_mse(model='rf', fit=rf_best_fit_3571, test_3571)
    
    rf_best_result_3571[iter, ] = c(rf_best_com_3571$ntree, rf_best_com_3571$mtry, rf_best_mse_3571)
    rf_test_result_3571[iter, ] = c(rf_best_com_3571$ntree, rf_best_com_3571$mtry, rf_test_mse_3571)
    
    # elastic net
    elnet_best_fit_3571 = glmnet(x=train_val_3571[, -1], y=train_val_3571[, 1], 
                                 alpha=elnet_best_com_3571$alpha, lambda=elnet_best_com_3571$alpha)
    
    elnet_best_mse_3571 = calculate_mse(model='elnet', fit=elnet_best_fit_3571, train_val_3571)
    elnet_test_mse_3571 = calculate_mse(model='elnet', fit=elnet_best_fit_3571, test_3571)
    
    elnet_best_result_3571[iter, ] = c(elnet_best_com_3571$alpha, elnet_best_com_3571$lambda, elnet_best_mse_3571)
    elnet_test_result_3571[iter, ] = c(elnet_best_com_3571$alpha, elnet_best_com_3571$lambda, elnet_test_mse_3571)
    
    # xgboost
    xg_best_fit_3571 = xgboost(data=as.matrix(train_val_3571[, -1]), label=train_val_3571[, 1], verbose=0,
                               nrounds=xg_best_com_3571$nrounds, 
                               max_depth=xg_best_com_3571$max_depth, 
                               eta=xg_best_com_3571$eta)
    
    xg_best_mse_3571 = calculate_mse(model='xg', fit=xg_best_fit_3571, train_val_3571)
    xg_test_mse_3571 = calculate_mse(model='xg', fit=xg_best_fit_3571, test_3571)
    
    xg_best_result_3571[iter, ] = c(xg_best_com_3571$nrounds, 
                                    xg_best_com_3571$max_depth, 
                                    xg_best_com_3571$eta, 
                                    xg_best_mse_3571)
    xg_test_result_3571[iter, ] = c(xg_best_com_3571$nrounds, 
                                    xg_best_com_3571$max_depth, 
                                    xg_best_com_3571$eta, 
                                    xg_test_mse_3571)
    
    ########################## 3579 ##########################
    print(paste("#", iter, "cv train/test", 3579))
    # random forest
    rf_best_fit_3579 = randomForest(train_val_3579[, 1] ~., data=train_val_3579, 
                                    ntree=rf_best_com_3579$ntree, mtry=rf_best_com_3579$mtry)
    
    rf_best_mse_3579 = calculate_mse(model='rf', fit=rf_best_fit_3579, train_val_3579)
    rf_test_mse_3579 = calculate_mse(model='rf', fit=rf_best_fit_3579, test_3579)
    
    rf_best_result_3579[iter, ] = c(rf_best_com_3579$ntree, rf_best_com_3579$mtry, rf_best_mse_3579)
    rf_test_result_3579[iter, ] = c(rf_best_com_3579$ntree, rf_best_com_3579$mtry, rf_test_mse_3579)
    
    # elastic net
    elnet_best_fit_3579 = glmnet(x=train_val_3579[, -1], y=train_val_3579[, 1], 
                                 alpha=elnet_best_com_3579$alpha, lambda=elnet_best_com_3579$alpha)
    
    elnet_best_mse_3579 = calculate_mse(model='elnet', fit=elnet_best_fit_3579, train_val_3579)
    elnet_test_mse_3579 = calculate_mse(model='elnet', fit=elnet_best_fit_3579, test_3579)
    
    elnet_best_result_3579[iter, ] = c(elnet_best_com_3579$alpha, elnet_best_com_3579$lambda, elnet_best_mse_3579)
    elnet_test_result_3579[iter, ] = c(elnet_best_com_3579$alpha, elnet_best_com_3579$lambda, elnet_test_mse_3579)
    
    # xgboost
    xg_best_fit_3579 = xgboost(data=as.matrix(train_val_3579[, -1]), label=train_val_3579[, 1], verbose=0,
                               nrounds=xg_best_com_3579$nrounds, 
                               max_depth=xg_best_com_3579$max_depth, 
                               eta=xg_best_com_3579$eta)
    
    xg_best_mse_3579 = calculate_mse(model='xg', fit=xg_best_fit_3579, train_val_3579)
    xg_test_mse_3579 = calculate_mse(model='xg', fit=xg_best_fit_3579, test_3579)
    
    xg_best_result_3579[iter, ] = c(xg_best_com_3579$nrounds, 
                                    xg_best_com_3579$max_depth, 
                                    xg_best_com_3579$eta, 
                                    xg_best_mse_3579)
    xg_test_result_3579[iter, ] = c(xg_best_com_3579$nrounds, 
                                    xg_best_com_3579$max_depth, 
                                    xg_best_com_3579$eta, 
                                    xg_test_mse_3579)
    
    ########################## 3582 ##########################
    print(paste("#", iter, "cv train/test", 3582))
    # random forest
    rf_best_fit_3582 = randomForest(train_val_3582[, 1] ~., data=train_val_3582, 
                                    ntree=rf_best_com_3582$ntree, mtry=rf_best_com_3582$mtry)
    
    rf_best_mse_3582 = calculate_mse(model='rf', fit=rf_best_fit_3582, train_val_3582)
    rf_test_mse_3582 = calculate_mse(model='rf', fit=rf_best_fit_3582, test_3582)
    
    rf_best_result_3582[iter, ] = c(rf_best_com_3582$ntree, rf_best_com_3582$mtry, rf_best_mse_3582)
    rf_test_result_3582[iter, ] = c(rf_best_com_3582$ntree, rf_best_com_3582$mtry, rf_test_mse_3582)
    
    # elastic net
    elnet_best_fit_3582 = glmnet(x=train_val_3582[, -1], y=train_val_3582[, 1], 
                                 alpha=elnet_best_com_3582$alpha, lambda=elnet_best_com_3582$alpha)
    
    elnet_best_mse_3582 = calculate_mse(model='elnet', fit=elnet_best_fit_3582, train_val_3582)
    elnet_test_mse_3582 = calculate_mse(model='elnet', fit=elnet_best_fit_3582, test_3582)
    
    elnet_best_result_3582[iter, ] = c(elnet_best_com_3582$alpha, elnet_best_com_3582$lambda, elnet_best_mse_3582)
    elnet_test_result_3582[iter, ] = c(elnet_best_com_3582$alpha, elnet_best_com_3582$lambda, elnet_test_mse_3582)
    
    # xgboost
    xg_best_fit_3582 = xgboost(data=as.matrix(train_val_3582[, -1]), label=train_val_3582[, 1], verbose=0,
                               nrounds=xg_best_com_3582$nrounds, 
                               max_depth=xg_best_com_3582$max_depth, 
                               eta=xg_best_com_3582$eta)
    
    xg_best_mse_3582 = calculate_mse(model='xg', fit=xg_best_fit_3582, train_val_3582)
    xg_test_mse_3582 = calculate_mse(model='xg', fit=xg_best_fit_3582, test_3582)
    
    xg_best_result_3582[iter, ] = c(xg_best_com_3582$nrounds, 
                                    xg_best_com_3582$max_depth, 
                                    xg_best_com_3582$eta, 
                                    xg_best_mse_3582)
    xg_test_result_3582[iter, ] = c(xg_best_com_3582$nrounds, 
                                    xg_best_com_3582$max_depth, 
                                    xg_best_com_3582$eta, 
                                    xg_test_mse_3582)
    
    ########################## 3860 ##########################
    print(paste("#", iter, "cv train/test", 3860))
    # random forest
    rf_best_fit_3860 = randomForest(train_val_3860[, 1] ~., data=train_val_3860, 
                                    ntree=rf_best_com_3860$ntree, mtry=rf_best_com_3860$mtry)
    
    rf_best_mse_3860 = calculate_mse(model='rf', fit=rf_best_fit_3860, train_val_3860)
    rf_test_mse_3860 = calculate_mse(model='rf', fit=rf_best_fit_3860, test_3860)
    
    rf_best_result_3860[iter, ] = c(rf_best_com_3860$ntree, rf_best_com_3860$mtry, rf_best_mse_3860)
    rf_test_result_3860[iter, ] = c(rf_best_com_3860$ntree, rf_best_com_3860$mtry, rf_test_mse_3860)
    
    # elastic net
    elnet_best_fit_3860 = glmnet(x=train_val_3860[, -1], y=train_val_3860[, 1], 
                                 alpha=elnet_best_com_3860$alpha, lambda=elnet_best_com_3860$alpha)
    
    elnet_best_mse_3860 = calculate_mse(model='elnet', fit=elnet_best_fit_3860, train_val_3860)
    elnet_test_mse_3860 = calculate_mse(model='elnet', fit=elnet_best_fit_3860, test_3860)
    
    elnet_best_result_3860[iter, ] = c(elnet_best_com_3860$alpha, elnet_best_com_3860$lambda, elnet_best_mse_3860)
    elnet_test_result_3860[iter, ] = c(elnet_best_com_3860$alpha, elnet_best_com_3860$lambda, elnet_test_mse_3860)
    
    # xgboost
    xg_best_fit_3860 = xgboost(data=as.matrix(train_val_3860[, -1]), label=train_val_3860[, 1], verbose=0,
                               nrounds=xg_best_com_3860$nrounds, 
                               max_depth=xg_best_com_3860$max_depth, 
                               eta=xg_best_com_3860$eta)
    
    xg_best_mse_3860 = calculate_mse(model='xg', fit=xg_best_fit_3860, train_val_3860)
    xg_test_mse_3860 = calculate_mse(model='xg', fit=xg_best_fit_3860, test_3860)
    
    xg_best_result_3860[iter, ] = c(xg_best_com_3860$nrounds, 
                                    xg_best_com_3860$max_depth, 
                                    xg_best_com_3860$eta, 
                                    xg_best_mse_3860)
    xg_test_result_3860[iter, ] = c(xg_best_com_3860$nrounds, 
                                    xg_best_com_3860$max_depth, 
                                    xg_best_com_3860$eta, 
                                    xg_test_mse_3860)
    
    ########################## 500 ##########################
    print(paste("#", iter, "cv train/test", 500))
    # random forest
    rf_best_fit_500 = randomForest(train_val_500[, 1] ~., data=train_val_500, 
                                   ntree=rf_best_com_500$ntree, mtry=rf_best_com_500$mtry)
    
    rf_best_mse_500 = calculate_mse(model='rf', fit=rf_best_fit_500, train_val_500)
    rf_test_mse_500 = calculate_mse(model='rf', fit=rf_best_fit_500, test_500)
    
    rf_best_result_500[iter, ] = c(rf_best_com_500$ntree, rf_best_com_500$mtry, rf_best_mse_500)
    rf_test_result_500[iter, ] = c(rf_best_com_500$ntree, rf_best_com_500$mtry, rf_test_mse_500)
    
    # elastic net
    elnet_best_fit_500 = glmnet(x=train_val_500[, -1], y=train_val_500[, 1], 
                                alpha=elnet_best_com_500$alpha, lambda=elnet_best_com_500$alpha)
    
    elnet_best_mse_500 = calculate_mse(model='elnet', fit=elnet_best_fit_500, train_val_500)
    elnet_test_mse_500 = calculate_mse(model='elnet', fit=elnet_best_fit_500, test_500)
    
    elnet_best_result_500[iter, ] = c(elnet_best_com_500$alpha, elnet_best_com_500$lambda, elnet_best_mse_500)
    elnet_test_result_500[iter, ] = c(elnet_best_com_500$alpha, elnet_best_com_500$lambda, elnet_test_mse_500)
    
    # xgboost
    xg_best_fit_500 = xgboost(data=as.matrix(train_val_500[, -1]), label=train_val_500[, 1], verbose=0,
                              nrounds=xg_best_com_500$nrounds, 
                              max_depth=xg_best_com_500$max_depth, 
                              eta=xg_best_com_500$eta)
    
    xg_best_mse_500 = calculate_mse(model='xg', fit=xg_best_fit_500, train_val_500)
    xg_test_mse_500 = calculate_mse(model='xg', fit=xg_best_fit_500, test_500)
    
    xg_best_result_500[iter, ] = c(xg_best_com_500$nrounds, 
                                   xg_best_com_500$max_depth, 
                                   xg_best_com_500$eta, 
                                   xg_best_mse_500)
    xg_test_result_500[iter, ] = c(xg_best_com_500$nrounds, 
                                   xg_best_com_500$max_depth, 
                                   xg_best_com_500$eta, 
                                   xg_test_mse_500)
    
    ########################## 590 ##########################
    print(paste("#", iter, "cv train/test", 590))
    # random forest
    rf_best_fit_590 = randomForest(train_val_590[, 1] ~., data=train_val_590, 
                                   ntree=rf_best_com_590$ntree, mtry=rf_best_com_590$mtry)
    
    rf_best_mse_590 = calculate_mse(model='rf', fit=rf_best_fit_590, train_val_590)
    rf_test_mse_590 = calculate_mse(model='rf', fit=rf_best_fit_590, test_590)
    
    rf_best_result_590[iter, ] = c(rf_best_com_590$ntree, rf_best_com_590$mtry, rf_best_mse_590)
    rf_test_result_590[iter, ] = c(rf_best_com_590$ntree, rf_best_com_590$mtry, rf_test_mse_590)
    
    # elastic net
    elnet_best_fit_590 = glmnet(x=train_val_590[, -1], y=train_val_590[, 1], 
                                alpha=elnet_best_com_590$alpha, lambda=elnet_best_com_590$alpha)
    
    elnet_best_mse_590 = calculate_mse(model='elnet', fit=elnet_best_fit_590, train_val_590)
    elnet_test_mse_590 = calculate_mse(model='elnet', fit=elnet_best_fit_590, test_590)
    
    elnet_best_result_590[iter, ] = c(elnet_best_com_590$alpha, elnet_best_com_590$lambda, elnet_best_mse_590)
    elnet_test_result_590[iter, ] = c(elnet_best_com_590$alpha, elnet_best_com_590$lambda, elnet_test_mse_590)
    
    # xgboost
    xg_best_fit_590 = xgboost(data=as.matrix(train_val_590[, -1]), label=train_val_590[, 1], verbose=0,
                              nrounds=xg_best_com_590$nrounds, 
                              max_depth=xg_best_com_590$max_depth, 
                              eta=xg_best_com_590$eta)
    
    xg_best_mse_590 = calculate_mse(model='xg', fit=xg_best_fit_590, train_val_590)
    xg_test_mse_590 = calculate_mse(model='xg', fit=xg_best_fit_590, test_590)
    
    xg_best_result_590[iter, ] = c(xg_best_com_590$nrounds, 
                                   xg_best_com_590$max_depth, 
                                   xg_best_com_590$eta, 
                                   xg_best_mse_590)
    xg_test_result_590[iter, ] = c(xg_best_com_590$nrounds, 
                                   xg_best_com_590$max_depth, 
                                   xg_best_com_590$eta, 
                                   xg_test_mse_590)
    
    ########################## 591 ##########################
    print(paste("#", iter, "cv train/test", 591))
    # random forest
    rf_best_fit_591 = randomForest(train_val_591[, 1] ~., data=train_val_591, 
                                   ntree=rf_best_com_591$ntree, mtry=rf_best_com_591$mtry)
    
    rf_best_mse_591 = calculate_mse(model='rf', fit=rf_best_fit_591, train_val_591)
    rf_test_mse_591 = calculate_mse(model='rf', fit=rf_best_fit_591, test_591)
    
    rf_best_result_591[iter, ] = c(rf_best_com_591$ntree, rf_best_com_591$mtry, rf_best_mse_591)
    rf_test_result_591[iter, ] = c(rf_best_com_591$ntree, rf_best_com_591$mtry, rf_test_mse_591)
    
    # elastic net
    elnet_best_fit_591 = glmnet(x=train_val_591[, -1], y=train_val_591[, 1], 
                                alpha=elnet_best_com_591$alpha, lambda=elnet_best_com_591$alpha)
    
    elnet_best_mse_591 = calculate_mse(model='elnet', fit=elnet_best_fit_591, train_val_591)
    elnet_test_mse_591 = calculate_mse(model='elnet', fit=elnet_best_fit_591, test_591)
    
    elnet_best_result_591[iter, ] = c(elnet_best_com_591$alpha, elnet_best_com_591$lambda, elnet_best_mse_591)
    elnet_test_result_591[iter, ] = c(elnet_best_com_591$alpha, elnet_best_com_591$lambda, elnet_test_mse_591)
    
    # xgboost
    xg_best_fit_591 = xgboost(data=as.matrix(train_val_591[, -1]), label=train_val_591[, 1], verbose=0,
                              nrounds=xg_best_com_591$nrounds, 
                              max_depth=xg_best_com_591$max_depth, 
                              eta=xg_best_com_591$eta)
    
    xg_best_mse_591 = calculate_mse(model='xg', fit=xg_best_fit_591, train_val_591)
    xg_test_mse_591 = calculate_mse(model='xg', fit=xg_best_fit_591, test_591)
    
    xg_best_result_591[iter, ] = c(xg_best_com_591$nrounds, 
                                   xg_best_com_591$max_depth, 
                                   xg_best_com_591$eta, 
                                   xg_best_mse_591)
    xg_test_result_591[iter, ] = c(xg_best_com_591$nrounds, 
                                   xg_best_com_591$max_depth, 
                                   xg_best_com_591$eta, 
                                   xg_test_mse_591)
    
    ########################## 592 ##########################
    print(paste("#", iter, "cv train/test", 592))
    # random forest
    rf_best_fit_592 = randomForest(train_val_592[, 1] ~., data=train_val_592, 
                                   ntree=rf_best_com_592$ntree, mtry=rf_best_com_592$mtry)
    
    rf_best_mse_592 = calculate_mse(model='rf', fit=rf_best_fit_592, train_val_592)
    rf_test_mse_592 = calculate_mse(model='rf', fit=rf_best_fit_592, test_592)
    
    rf_best_result_592[iter, ] = c(rf_best_com_592$ntree, rf_best_com_592$mtry, rf_best_mse_592)
    rf_test_result_592[iter, ] = c(rf_best_com_592$ntree, rf_best_com_592$mtry, rf_test_mse_592)
    
    # elastic net
    elnet_best_fit_592 = glmnet(x=train_val_592[, -1], y=train_val_592[, 1], 
                                alpha=elnet_best_com_592$alpha, lambda=elnet_best_com_592$alpha)
    
    elnet_best_mse_592 = calculate_mse(model='elnet', fit=elnet_best_fit_592, train_val_592)
    elnet_test_mse_592 = calculate_mse(model='elnet', fit=elnet_best_fit_592, test_592)
    
    elnet_best_result_592[iter, ] = c(elnet_best_com_592$alpha, elnet_best_com_592$lambda, elnet_best_mse_592)
    elnet_test_result_592[iter, ] = c(elnet_best_com_592$alpha, elnet_best_com_592$lambda, elnet_test_mse_592)
    
    # xgboost
    xg_best_fit_592 = xgboost(data=as.matrix(train_val_592[, -1]), label=train_val_592[, 1], verbose=0,
                              nrounds=xg_best_com_592$nrounds, 
                              max_depth=xg_best_com_592$max_depth, 
                              eta=xg_best_com_592$eta)
    
    xg_best_mse_592 = calculate_mse(model='xg', fit=xg_best_fit_592, train_val_592)
    xg_test_mse_592 = calculate_mse(model='xg', fit=xg_best_fit_592, test_592)
    
    xg_best_result_592[iter, ] = c(xg_best_com_592$nrounds, 
                                   xg_best_com_592$max_depth, 
                                   xg_best_com_592$eta, 
                                   xg_best_mse_592)
    xg_test_result_592[iter, ] = c(xg_best_com_592$nrounds, 
                                   xg_best_com_592$max_depth, 
                                   xg_best_com_592$eta, 
                                   xg_test_mse_592)
    
    
    
    ########################## gu ##########################
    print(paste("#", iter, "cv train/test", "gu"))
    # random forest
    rf_best_fit_gu = randomForest(train_val_gu[, 1] ~., data=train_val_gu, 
                                  ntree=rf_best_com_gu$ntree, mtry=rf_best_com_gu$mtry)
    
    rf_best_mse_gu = calculate_mse(model='rf', fit=rf_best_fit_gu, train_val_gu)
    
    rf_test_mse_gu = calculate_mse(model='rf', fit=rf_best_fit_gu, test_gu)
    rf_test_mse_gu_3508 = calculate_mse(model='rf', fit=rf_best_fit_gu, test_3508)
    rf_test_mse_gu_3523 = calculate_mse(model='rf', fit=rf_best_fit_gu, test_3523)
    rf_test_mse_gu_3569 = calculate_mse(model='rf', fit=rf_best_fit_gu, test_3569)
    rf_test_mse_gu_3571 = calculate_mse(model='rf', fit=rf_best_fit_gu, test_3571)
    rf_test_mse_gu_3579 = calculate_mse(model='rf', fit=rf_best_fit_gu, test_3579)
    rf_test_mse_gu_3582 = calculate_mse(model='rf', fit=rf_best_fit_gu, test_3582)
    rf_test_mse_gu_3860 = calculate_mse(model='rf', fit=rf_best_fit_gu, test_3860)
    rf_test_mse_gu_500 = calculate_mse(model='rf', fit=rf_best_fit_gu, test_500)
    rf_test_mse_gu_590 = calculate_mse(model='rf', fit=rf_best_fit_gu, test_590)
    rf_test_mse_gu_591 = calculate_mse(model='rf', fit=rf_best_fit_gu, test_591)
    rf_test_mse_gu_592 = calculate_mse(model='rf', fit=rf_best_fit_gu, test_592)
    
    rf_best_result_gu[iter, ] = c(rf_best_com_gu$ntree, rf_best_com_gu$mtry, rf_best_mse_gu)
    
    rf_test_result_gu[12*(iter-1)+1, ] = c(rf_best_com_gu$ntree, rf_best_com_gu$mtry, rf_test_mse_gu)
    rf_test_result_gu[12*(iter-1)+2, ] = c(rf_best_com_gu$ntree, rf_best_com_gu$mtry, rf_test_mse_gu_3508)
    rf_test_result_gu[12*(iter-1)+3, ] = c(rf_best_com_gu$ntree, rf_best_com_gu$mtry, rf_test_mse_gu_3523)
    rf_test_result_gu[12*(iter-1)+4, ] = c(rf_best_com_gu$ntree, rf_best_com_gu$mtry, rf_test_mse_gu_3569)
    rf_test_result_gu[12*(iter-1)+5, ] = c(rf_best_com_gu$ntree, rf_best_com_gu$mtry, rf_test_mse_gu_3571)
    rf_test_result_gu[12*(iter-1)+6, ] = c(rf_best_com_gu$ntree, rf_best_com_gu$mtry, rf_test_mse_gu_3579)
    rf_test_result_gu[12*(iter-1)+7, ] = c(rf_best_com_gu$ntree, rf_best_com_gu$mtry, rf_test_mse_gu_3582)
    rf_test_result_gu[12*(iter-1)+8, ] = c(rf_best_com_gu$ntree, rf_best_com_gu$mtry, rf_test_mse_gu_3860)
    rf_test_result_gu[12*(iter-1)+9, ] = c(rf_best_com_gu$ntree, rf_best_com_gu$mtry, rf_test_mse_gu_500)
    rf_test_result_gu[12*(iter-1)+10, ] = c(rf_best_com_gu$ntree, rf_best_com_gu$mtry, rf_test_mse_gu_590)
    rf_test_result_gu[12*(iter-1)+11, ] = c(rf_best_com_gu$ntree, rf_best_com_gu$mtry, rf_test_mse_gu_591)
    rf_test_result_gu[12*(iter-1)+12, ] = c(rf_best_com_gu$ntree, rf_best_com_gu$mtry, rf_test_mse_gu_592)
    
    # elastic net
    elnet_best_fit_gu = glmnet(x=train_val_gu[, -1], y=train_val_gu[, 1], 
                               alpha=elnet_best_com_gu$alpha, lambda=elnet_best_com_gu$alpha)
    
    elnet_best_mse_gu = calculate_mse(model='elnet', fit=elnet_best_fit_gu, train_val_gu)
    
    elnet_test_mse_gu = calculate_mse(model='elnet', fit=elnet_best_fit_gu, test_gu)
    elnet_test_mse_gu_3508 = calculate_mse(model='elnet', fit=elnet_best_fit_gu, test_3508)
    elnet_test_mse_gu_3523 = calculate_mse(model='elnet', fit=elnet_best_fit_gu, test_3523)
    elnet_test_mse_gu_3569 = calculate_mse(model='elnet', fit=elnet_best_fit_gu, test_3569)
    elnet_test_mse_gu_3571 = calculate_mse(model='elnet', fit=elnet_best_fit_gu, test_3571)
    elnet_test_mse_gu_3579 = calculate_mse(model='elnet', fit=elnet_best_fit_gu, test_3579)
    elnet_test_mse_gu_3582 = calculate_mse(model='elnet', fit=elnet_best_fit_gu, test_3582)
    elnet_test_mse_gu_3860 = calculate_mse(model='elnet', fit=elnet_best_fit_gu, test_3860)
    elnet_test_mse_gu_500 = calculate_mse(model='elnet', fit=elnet_best_fit_gu, test_500)
    elnet_test_mse_gu_590 = calculate_mse(model='elnet', fit=elnet_best_fit_gu, test_590)
    elnet_test_mse_gu_591 = calculate_mse(model='elnet', fit=elnet_best_fit_gu, test_591)
    elnet_test_mse_gu_592 = calculate_mse(model='elnet', fit=elnet_best_fit_gu, test_592)
    
    elnet_best_result_gu[iter, ] = c(elnet_best_com_gu$alpha, elnet_best_com_gu$lambda, elnet_best_mse_gu)
    
    elnet_test_result_gu[12*(iter-1)+1, ] = c(elnet_best_com_gu$alpha, elnet_best_com_gu$lambda, elnet_test_mse_gu)
    elnet_test_result_gu[12*(iter-1)+2, ] = c(elnet_best_com_gu$alpha, elnet_best_com_gu$lambda, elnet_test_mse_gu_3508)
    elnet_test_result_gu[12*(iter-1)+3, ] = c(elnet_best_com_gu$alpha, elnet_best_com_gu$lambda, elnet_test_mse_gu_3523)
    elnet_test_result_gu[12*(iter-1)+4, ] = c(elnet_best_com_gu$alpha, elnet_best_com_gu$lambda, elnet_test_mse_gu_3569)
    elnet_test_result_gu[12*(iter-1)+5, ] = c(elnet_best_com_gu$alpha, elnet_best_com_gu$lambda, elnet_test_mse_gu_3571)
    elnet_test_result_gu[12*(iter-1)+6, ] = c(elnet_best_com_gu$alpha, elnet_best_com_gu$lambda, elnet_test_mse_gu_3579)
    elnet_test_result_gu[12*(iter-1)+7, ] = c(elnet_best_com_gu$alpha, elnet_best_com_gu$lambda, elnet_test_mse_gu_3582)
    elnet_test_result_gu[12*(iter-1)+8, ] = c(elnet_best_com_gu$alpha, elnet_best_com_gu$lambda, elnet_test_mse_gu_3860)
    elnet_test_result_gu[12*(iter-1)+9, ] = c(elnet_best_com_gu$alpha, elnet_best_com_gu$lambda, elnet_test_mse_gu_500)
    elnet_test_result_gu[12*(iter-1)+10, ] = c(elnet_best_com_gu$alpha, elnet_best_com_gu$lambda, elnet_test_mse_gu_590)
    elnet_test_result_gu[12*(iter-1)+11, ] = c(elnet_best_com_gu$alpha, elnet_best_com_gu$lambda, elnet_test_mse_gu_591)
    elnet_test_result_gu[12*(iter-1)+12, ] = c(elnet_best_com_gu$alpha, elnet_best_com_gu$lambda, elnet_test_mse_gu_592)
    
    # xgboost
    xg_best_fit_gu = xgboost(data=as.matrix(train_val_gu[, -1]), label=train_val_gu[, 1], verbose=0,
                             nrounds=xg_best_com_gu$nrounds, 
                             max_depth=xg_best_com_gu$max_depth, 
                             eta=xg_best_com_gu$eta)
    
    xg_best_mse_gu = calculate_mse(model='xg', fit=xg_best_fit_gu, train_val_gu)
    
    xg_test_mse_gu = calculate_mse(model='xg', fit=xg_best_fit_gu, test_gu)
    xg_test_mse_gu_3508 = calculate_mse(model='xg', fit=xg_best_fit_gu, test_3508)
    xg_test_mse_gu_3523 = calculate_mse(model='xg', fit=xg_best_fit_gu, test_3523)
    xg_test_mse_gu_3569 = calculate_mse(model='xg', fit=xg_best_fit_gu, test_3569)
    xg_test_mse_gu_3571 = calculate_mse(model='xg', fit=xg_best_fit_gu, test_3571)
    xg_test_mse_gu_3579 = calculate_mse(model='xg', fit=xg_best_fit_gu, test_3579)
    xg_test_mse_gu_3582 = calculate_mse(model='xg', fit=xg_best_fit_gu, test_3582)
    xg_test_mse_gu_3860 = calculate_mse(model='xg', fit=xg_best_fit_gu, test_3860)
    xg_test_mse_gu_500 = calculate_mse(model='xg', fit=xg_best_fit_gu, test_500)
    xg_test_mse_gu_590 = calculate_mse(model='xg', fit=xg_best_fit_gu, test_590)
    xg_test_mse_gu_591 = calculate_mse(model='xg', fit=xg_best_fit_gu, test_591)
    xg_test_mse_gu_592 = calculate_mse(model='xg', fit=xg_best_fit_gu, test_592)
    
    
    xg_best_result_gu[iter, ] = c(xg_best_com_gu$nrounds, 
                                  xg_best_com_gu$max_depth, 
                                  xg_best_com_gu$eta, 
                                  xg_best_mse_gu)
    
    xg_test_result_gu[12*(iter-1)+1, ] = c(xg_best_com_gu$nrounds, 
                                           xg_best_com_gu$max_depth, 
                                           xg_best_com_gu$eta, 
                                           xg_test_mse_gu)
    xg_test_result_gu[12*(iter-1)+2, ] = c(xg_best_com_gu$nrounds, 
                                           xg_best_com_gu$max_depth, 
                                           xg_best_com_gu$eta, 
                                           xg_test_mse_gu_3508)
    xg_test_result_gu[12*(iter-1)+3, ] = c(xg_best_com_gu$nrounds, 
                                           xg_best_com_gu$max_depth, 
                                           xg_best_com_gu$eta, 
                                           xg_test_mse_gu_3523)
    xg_test_result_gu[12*(iter-1)+4, ] = c(xg_best_com_gu$nrounds, 
                                           xg_best_com_gu$max_depth, 
                                           xg_best_com_gu$eta, 
                                           xg_test_mse_gu_3569)
    xg_test_result_gu[12*(iter-1)+5, ] = c(xg_best_com_gu$nrounds, 
                                           xg_best_com_gu$max_depth, 
                                           xg_best_com_gu$eta, 
                                           xg_test_mse_gu_3571)
    xg_test_result_gu[12*(iter-1)+6, ] = c(xg_best_com_gu$nrounds, 
                                           xg_best_com_gu$max_depth, 
                                           xg_best_com_gu$eta, 
                                           xg_test_mse_gu_3579)
    xg_test_result_gu[12*(iter-1)+7, ] = c(xg_best_com_gu$nrounds, 
                                           xg_best_com_gu$max_depth, 
                                           xg_best_com_gu$eta, 
                                           xg_test_mse_gu_3582)
    xg_test_result_gu[12*(iter-1)+8, ] = c(xg_best_com_gu$nrounds, 
                                           xg_best_com_gu$max_depth, 
                                           xg_best_com_gu$eta, 
                                           xg_test_mse_gu_3860)
    xg_test_result_gu[12*(iter-1)+9, ] = c(xg_best_com_gu$nrounds, 
                                           xg_best_com_gu$max_depth, 
                                           xg_best_com_gu$eta, 
                                           xg_test_mse_gu_500)
    xg_test_result_gu[12*(iter-1)+10, ] = c(xg_best_com_gu$nrounds, 
                                            xg_best_com_gu$max_depth, 
                                            xg_best_com_gu$eta, 
                                            xg_test_mse_gu_590)
    xg_test_result_gu[12*(iter-1)+11, ] = c(xg_best_com_gu$nrounds, 
                                            xg_best_com_gu$max_depth, 
                                            xg_best_com_gu$eta, 
                                            xg_test_mse_gu_591)
    xg_test_result_gu[12*(iter-1)+12, ] = c(xg_best_com_gu$nrounds, 
                                            xg_best_com_gu$max_depth, 
                                            xg_best_com_gu$eta, 
                                            xg_test_mse_gu_592)
    
    ########################## dong ##########################
    print(paste("#", iter, "cv train/test", "dong"))
    # random forest
    rf_best_fit_dong = randomForest(train_val_dong[, 1] ~., data=train_val_dong, 
                                    ntree=rf_best_com_dong$ntree, mtry=rf_best_com_dong$mtry)
    
    rf_best_mse_dong = calculate_mse(model='rf', fit=rf_best_fit_dong, train_val_dong)
    
    rf_test_mse_dong = calculate_mse(model='rf', fit=rf_best_fit_dong, test_dong)
    rf_test_mse_dong_3508 = calculate_mse(model='rf', fit=rf_best_fit_dong, test_3508)
    rf_test_mse_dong_3523 = calculate_mse(model='rf', fit=rf_best_fit_dong, test_3523)
    rf_test_mse_dong_3569 = calculate_mse(model='rf', fit=rf_best_fit_dong, test_3569)
    rf_test_mse_dong_3571 = calculate_mse(model='rf', fit=rf_best_fit_dong, test_3571)
    rf_test_mse_dong_3579 = calculate_mse(model='rf', fit=rf_best_fit_dong, test_3579)
    rf_test_mse_dong_3582 = calculate_mse(model='rf', fit=rf_best_fit_dong, test_3582)
    rf_test_mse_dong_3860 = calculate_mse(model='rf', fit=rf_best_fit_dong, test_3860)
    rf_test_mse_dong_500 = calculate_mse(model='rf', fit=rf_best_fit_dong, test_500)
    rf_test_mse_dong_590 = calculate_mse(model='rf', fit=rf_best_fit_dong, test_590)
    rf_test_mse_dong_591 = calculate_mse(model='rf', fit=rf_best_fit_dong, test_591)
    rf_test_mse_dong_592 = calculate_mse(model='rf', fit=rf_best_fit_dong, test_592)
    
    rf_best_result_dong[iter, ] = c(rf_best_com_dong$ntree, rf_best_com_dong$mtry, rf_best_mse_dong)
    
    rf_test_result_dong[12*(iter-1)+1, ] = c(rf_best_com_dong$ntree, rf_best_com_dong$mtry, rf_test_mse_dong)
    rf_test_result_dong[12*(iter-1)+2, ] = c(rf_best_com_dong$ntree, rf_best_com_dong$mtry, rf_test_mse_dong_3508)
    rf_test_result_dong[12*(iter-1)+3, ] = c(rf_best_com_dong$ntree, rf_best_com_dong$mtry, rf_test_mse_dong_3523)
    rf_test_result_dong[12*(iter-1)+4, ] = c(rf_best_com_dong$ntree, rf_best_com_dong$mtry, rf_test_mse_dong_3569)
    rf_test_result_dong[12*(iter-1)+5, ] = c(rf_best_com_dong$ntree, rf_best_com_dong$mtry, rf_test_mse_dong_3571)
    rf_test_result_dong[12*(iter-1)+6, ] = c(rf_best_com_dong$ntree, rf_best_com_dong$mtry, rf_test_mse_dong_3579)
    rf_test_result_dong[12*(iter-1)+7, ] = c(rf_best_com_dong$ntree, rf_best_com_dong$mtry, rf_test_mse_dong_3582)
    rf_test_result_dong[12*(iter-1)+8, ] = c(rf_best_com_dong$ntree, rf_best_com_dong$mtry, rf_test_mse_dong_3860)
    rf_test_result_dong[12*(iter-1)+9, ] = c(rf_best_com_dong$ntree, rf_best_com_dong$mtry, rf_test_mse_dong_500)
    rf_test_result_dong[12*(iter-1)+10, ] = c(rf_best_com_dong$ntree, rf_best_com_dong$mtry, rf_test_mse_dong_590)
    rf_test_result_dong[12*(iter-1)+11, ] = c(rf_best_com_dong$ntree, rf_best_com_dong$mtry, rf_test_mse_dong_591)
    rf_test_result_dong[12*(iter-1)+12, ] = c(rf_best_com_dong$ntree, rf_best_com_dong$mtry, rf_test_mse_dong_592)
    
    # elastic net
    elnet_best_fit_dong = glmnet(x=train_val_dong[, -1], y=train_val_dong[, 1], 
                                 alpha=elnet_best_com_dong$alpha, lambda=elnet_best_com_dong$alpha)
    
    elnet_best_mse_dong = calculate_mse(model='elnet', fit=elnet_best_fit_dong, train_val_dong)
    
    elnet_test_mse_dong = calculate_mse(model='elnet', fit=elnet_best_fit_dong, test_dong)
    elnet_test_mse_dong_3508 = calculate_mse(model='elnet', fit=elnet_best_fit_dong, test_3508)
    elnet_test_mse_dong_3523 = calculate_mse(model='elnet', fit=elnet_best_fit_dong, test_3523)
    elnet_test_mse_dong_3569 = calculate_mse(model='elnet', fit=elnet_best_fit_dong, test_3569)
    elnet_test_mse_dong_3571 = calculate_mse(model='elnet', fit=elnet_best_fit_dong, test_3571)
    elnet_test_mse_dong_3579 = calculate_mse(model='elnet', fit=elnet_best_fit_dong, test_3579)
    elnet_test_mse_dong_3582 = calculate_mse(model='elnet', fit=elnet_best_fit_dong, test_3582)
    elnet_test_mse_dong_3860 = calculate_mse(model='elnet', fit=elnet_best_fit_dong, test_3860)
    elnet_test_mse_dong_500 = calculate_mse(model='elnet', fit=elnet_best_fit_dong, test_500)
    elnet_test_mse_dong_590 = calculate_mse(model='elnet', fit=elnet_best_fit_dong, test_590)
    elnet_test_mse_dong_591 = calculate_mse(model='elnet', fit=elnet_best_fit_dong, test_591)
    elnet_test_mse_dong_592 = calculate_mse(model='elnet', fit=elnet_best_fit_dong, test_592)
    
    elnet_best_result_dong[iter, ] = c(elnet_best_com_dong$alpha, elnet_best_com_dong$lambda, elnet_best_mse_dong)
    
    elnet_test_result_dong[12*(iter-1)+1, ] = c(elnet_best_com_dong$alpha, elnet_best_com_dong$lambda, elnet_test_mse_dong)
    elnet_test_result_dong[12*(iter-1)+2, ] = c(elnet_best_com_dong$alpha, elnet_best_com_dong$lambda, elnet_test_mse_dong_3508)
    elnet_test_result_dong[12*(iter-1)+3, ] = c(elnet_best_com_dong$alpha, elnet_best_com_dong$lambda, elnet_test_mse_dong_3523)
    elnet_test_result_dong[12*(iter-1)+4, ] = c(elnet_best_com_dong$alpha, elnet_best_com_dong$lambda, elnet_test_mse_dong_3569)
    elnet_test_result_dong[12*(iter-1)+5, ] = c(elnet_best_com_dong$alpha, elnet_best_com_dong$lambda, elnet_test_mse_dong_3571)
    elnet_test_result_dong[12*(iter-1)+6, ] = c(elnet_best_com_dong$alpha, elnet_best_com_dong$lambda, elnet_test_mse_dong_3579)
    elnet_test_result_dong[12*(iter-1)+7, ] = c(elnet_best_com_dong$alpha, elnet_best_com_dong$lambda, elnet_test_mse_dong_3582)
    elnet_test_result_dong[12*(iter-1)+8, ] = c(elnet_best_com_dong$alpha, elnet_best_com_dong$lambda, elnet_test_mse_dong_3860)
    elnet_test_result_dong[12*(iter-1)+9, ] = c(elnet_best_com_dong$alpha, elnet_best_com_dong$lambda, elnet_test_mse_dong_500)
    elnet_test_result_dong[12*(iter-1)+10, ] = c(elnet_best_com_dong$alpha, elnet_best_com_dong$lambda, elnet_test_mse_dong_590)
    elnet_test_result_dong[12*(iter-1)+11, ] = c(elnet_best_com_dong$alpha, elnet_best_com_dong$lambda, elnet_test_mse_dong_591)
    elnet_test_result_dong[12*(iter-1)+12, ] = c(elnet_best_com_dong$alpha, elnet_best_com_dong$lambda, elnet_test_mse_dong_592)
    
    # xgboost
    xg_best_fit_dong = xgboost(data=as.matrix(train_val_dong[, -1]), label=train_val_dong[, 1], verbose=0,
                               nrounds=xg_best_com_dong$nrounds, 
                               max_depth=xg_best_com_dong$max_depth, 
                               eta=xg_best_com_dong$eta)
    
    xg_best_mse_dong = calculate_mse(model='xg', fit=xg_best_fit_dong, train_val_dong)
    
    xg_test_mse_dong = calculate_mse(model='xg', fit=xg_best_fit_dong, test_dong)
    xg_test_mse_dong_3508 = calculate_mse(model='xg', fit=xg_best_fit_dong, test_3508)
    xg_test_mse_dong_3523 = calculate_mse(model='xg', fit=xg_best_fit_dong, test_3523)
    xg_test_mse_dong_3569 = calculate_mse(model='xg', fit=xg_best_fit_dong, test_3569)
    xg_test_mse_dong_3571 = calculate_mse(model='xg', fit=xg_best_fit_dong, test_3571)
    xg_test_mse_dong_3579 = calculate_mse(model='xg', fit=xg_best_fit_dong, test_3579)
    xg_test_mse_dong_3582 = calculate_mse(model='xg', fit=xg_best_fit_dong, test_3582)
    xg_test_mse_dong_3860 = calculate_mse(model='xg', fit=xg_best_fit_dong, test_3860)
    xg_test_mse_dong_500 = calculate_mse(model='xg', fit=xg_best_fit_dong, test_500)
    xg_test_mse_dong_590 = calculate_mse(model='xg', fit=xg_best_fit_dong, test_590)
    xg_test_mse_dong_591 = calculate_mse(model='xg', fit=xg_best_fit_dong, test_591)
    xg_test_mse_dong_592 = calculate_mse(model='xg', fit=xg_best_fit_dong, test_592)
    
    
    xg_best_result_dong[iter, ] = c(xg_best_com_dong$nrounds, 
                                    xg_best_com_dong$max_depth, 
                                    xg_best_com_dong$eta, 
                                    xg_best_mse_dong)
    
    xg_test_result_dong[12*(iter-1)+1, ] = c(xg_best_com_dong$nrounds, 
                                             xg_best_com_dong$max_depth, 
                                             xg_best_com_dong$eta, 
                                             xg_test_mse_dong)
    xg_test_result_dong[12*(iter-1)+2, ] = c(xg_best_com_dong$nrounds, 
                                             xg_best_com_dong$max_depth, 
                                             xg_best_com_dong$eta, 
                                             xg_test_mse_dong_3508)
    xg_test_result_dong[12*(iter-1)+3, ] = c(xg_best_com_dong$nrounds, 
                                             xg_best_com_dong$max_depth, 
                                             xg_best_com_dong$eta, 
                                             xg_test_mse_dong_3523)
    xg_test_result_dong[12*(iter-1)+4, ] = c(xg_best_com_dong$nrounds, 
                                             xg_best_com_dong$max_depth, 
                                             xg_best_com_dong$eta, 
                                             xg_test_mse_dong_3569)
    xg_test_result_dong[12*(iter-1)+5, ] = c(xg_best_com_dong$nrounds, 
                                             xg_best_com_dong$max_depth, 
                                             xg_best_com_dong$eta, 
                                             xg_test_mse_dong_3571)
    xg_test_result_dong[12*(iter-1)+6, ] = c(xg_best_com_dong$nrounds, 
                                             xg_best_com_dong$max_depth, 
                                             xg_best_com_dong$eta, 
                                             xg_test_mse_dong_3579)
    xg_test_result_dong[12*(iter-1)+7, ] = c(xg_best_com_dong$nrounds, 
                                             xg_best_com_dong$max_depth, 
                                             xg_best_com_dong$eta, 
                                             xg_test_mse_dong_3582)
    xg_test_result_dong[12*(iter-1)+8, ] = c(xg_best_com_dong$nrounds, 
                                             xg_best_com_dong$max_depth, 
                                             xg_best_com_dong$eta, 
                                             xg_test_mse_dong_3860)
    xg_test_result_dong[12*(iter-1)+9, ] = c(xg_best_com_dong$nrounds, 
                                             xg_best_com_dong$max_depth, 
                                             xg_best_com_dong$eta, 
                                             xg_test_mse_dong_500)
    xg_test_result_dong[12*(iter-1)+10, ] = c(xg_best_com_dong$nrounds, 
                                              xg_best_com_dong$max_depth, 
                                              xg_best_com_dong$eta, 
                                              xg_test_mse_dong_590)
    xg_test_result_dong[12*(iter-1)+11, ] = c(xg_best_com_dong$nrounds, 
                                              xg_best_com_dong$max_depth, 
                                              xg_best_com_dong$eta, 
                                              xg_test_mse_dong_591)
    xg_test_result_dong[12*(iter-1)+12, ] = c(xg_best_com_dong$nrounds, 
                                              xg_best_com_dong$max_depth, 
                                              xg_best_com_dong$eta, 
                                              xg_test_mse_dong_592)
    
}


station_list_gu = c('gu', 3508, 3523, 3569, 3571, 3579, 
                    3582, 3860, 500, 590, 591, 592)
rf_test_result_gu$station = rep(station_list_gu, n_cv_tt)
elnet_test_result_gu$station = rep(station_list_gu, n_cv_tt)
xg_test_result_gu$station = rep(station_list_gu, n_cv_tt)

station_list_dong = c('dong', 3508, 3523, 3569, 3571, 3579, 
                      3582, 3860, 500, 590, 591, 592)
rf_test_result_dong$station = rep(station_list_dong, n_cv_tt)
elnet_test_result_dong$station = rep(station_list_dong, n_cv_tt)
xg_test_result_dong$station = rep(station_list_dong, n_cv_tt)



########################## 3508 ##########################
write.csv(rf_train_result_3508, "./result/test/rf_train_result_3508.csv", row.names=F)
write.csv(rf_val_result_3508, "./result/test/rf_val_result_3508.csv", row.names=F)
write.csv(rf_best_result_3508, "./result/test/rf_best_result_3508.csv", row.names=F)
write.csv(rf_test_result_3508, "./result/test/rf_test_result_3508.csv", row.names=F)

########################## 3523 ##########################
write.csv(rf_train_result_3523, "./result/test/rf_train_result_3523.csv", row.names=F)
write.csv(rf_val_result_3523, "./result/test/rf_val_result_3523.csv", row.names=F)
write.csv(rf_best_result_3523, "./result/test/rf_best_result_3523.csv", row.names=F)
write.csv(rf_test_result_3523, "./result/test/rf_test_result_3523.csv", row.names=F)

########################## 3569 ##########################
write.csv(rf_train_result_3569, "./result/test/rf_train_result_3569.csv", row.names=F)
write.csv(rf_val_result_3569, "./result/test/rf_val_result_3569.csv", row.names=F)
write.csv(rf_best_result_3569, "./result/test/rf_best_result_3569.csv", row.names=F)
write.csv(rf_test_result_3569, "./result/test/rf_test_result_3569.csv", row.names=F)

########################## 3571 ##########################
write.csv(rf_train_result_3571, "./result/test/rf_train_result_3571.csv", row.names=F)
write.csv(rf_val_result_3571, "./result/test/rf_val_result_3571.csv", row.names=F)
write.csv(rf_best_result_3571, "./result/test/rf_best_result_3571.csv", row.names=F)
write.csv(rf_test_result_3571, "./result/test/rf_test_result_3571.csv", row.names=F)

########################## 3579 ##########################
write.csv(rf_train_result_3579, "./result/test/rf_train_result_3579.csv", row.names=F)
write.csv(rf_val_result_3579, "./result/test/rf_val_result_3579.csv", row.names=F)
write.csv(rf_best_result_3579, "./result/test/rf_best_result_3579.csv", row.names=F)
write.csv(rf_test_result_3579, "./result/test/rf_test_result_3579.csv", row.names=F)

########################## 3582 ##########################
write.csv(rf_train_result_3582, "./result/test/rf_train_result_3582.csv", row.names=F)
write.csv(rf_val_result_3582, "./result/test/rf_val_result_3582.csv", row.names=F)
write.csv(rf_best_result_3582, "./result/test/rf_best_result_3582.csv", row.names=F)
write.csv(rf_test_result_3582, "./result/test/rf_test_result_3582.csv", row.names=F)

########################## 3860 ##########################
write.csv(rf_train_result_3860, "./result/test/rf_train_result_3860.csv", row.names=F)
write.csv(rf_val_result_3860, "./result/test/rf_val_result_3860.csv", row.names=F)
write.csv(rf_best_result_3860, "./result/test/rf_best_result_3860.csv", row.names=F)
write.csv(rf_test_result_3860, "./result/test/rf_test_result_3860.csv", row.names=F)

########################## 500 ##########################
write.csv(rf_train_result_500, "./result/test/rf_train_result_500.csv", row.names=F)
write.csv(rf_val_result_500, "./result/test/rf_val_result_500.csv", row.names=F)
write.csv(rf_best_result_500, "./result/test/rf_best_result_500.csv", row.names=F)
write.csv(rf_test_result_500, "./result/test/rf_test_result_500.csv", row.names=F)

########################## 590 ##########################
write.csv(rf_train_result_590, "./result/test/rf_train_result_590.csv", row.names=F)
write.csv(rf_val_result_590, "./result/test/rf_val_result_590.csv", row.names=F)
write.csv(rf_best_result_590, "./result/test/rf_best_result_590.csv", row.names=F)
write.csv(rf_test_result_590, "./result/test/rf_test_result_590.csv", row.names=F)

########################## 591 ##########################
write.csv(rf_train_result_591, "./result/test/rf_train_result_591.csv", row.names=F)
write.csv(rf_val_result_591, "./result/test/rf_val_result_591.csv", row.names=F)
write.csv(rf_best_result_591, "./result/test/rf_best_result_591.csv", row.names=F)
write.csv(rf_test_result_591, "./result/test/rf_test_result_591.csv", row.names=F)

########################## 592 ##########################
write.csv(rf_train_result_592, "./result/test/rf_train_result_592.csv", row.names=F)
write.csv(rf_val_result_592, "./result/test/rf_val_result_592.csv", row.names=F)
write.csv(rf_best_result_592, "./result/test/rf_best_result_592.csv", row.names=F)
write.csv(rf_test_result_592, "./result/test/rf_test_result_592.csv", row.names=F)

########################## gu ##########################
write.csv(rf_train_result_gu, "./result/test/rf_train_result_gu.csv", row.names=F)
write.csv(rf_val_result_gu, "./result/test/rf_val_result_gu.csv", row.names=F)
write.csv(rf_best_result_gu, "./result/test/rf_best_result_gu.csv", row.names=F)
write.csv(rf_test_result_gu, "./result/test/rf_test_result_gu.csv", row.names=F)

########################## dong ##########################
write.csv(rf_train_result_dong, "./result/test/rf_train_result_dong.csv", row.names=F)
write.csv(rf_val_result_dong, "./result/test/rf_val_result_dong.csv", row.names=F)
write.csv(rf_best_result_dong, "./result/test/rf_best_result_dong.csv", row.names=F)
write.csv(rf_test_result_dong, "./result/test/rf_test_result_dong.csv", row.names=F)



### 개별 대여소 결과합쳐서 한 데이터 프레임으로 만들 예정 ###
