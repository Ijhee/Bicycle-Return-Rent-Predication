setwd("C:/Users/s_sjw0513/Desktop/데이터마이닝/project")
rm(list=ls())

y_name = "rent"
version = "변수추가o"
start_time = Sys.time()

# --------------------------------------------------- #
#
# 0. Load libraries and functions
#
# --------------------------------------------------- #

library(dplyr)
source("./code/make_result_train_val_mse_0511.R")
source("./code/variable_selection_function.R")

find_best_combination = function(val_result){
    # This function is to find the best combination of parameters minimizing validation error
    val_medians = aggregate(val_mse ~ ., val_result, median)
    best_com = val_medians[which.min(val_medians$val_mse), ]
    
    return(best_com)
}

filter.fun = function(data){
    data = data %>% select(-c(timestamp, spot_number))
    data = data %>% rename("y"=names(data)[1])
    return(data)
}

# --------------------------------------------------- #
#
# 1. Read data
#
# --------------------------------------------------- #

data_590 = read.csv(paste0("./data/", version, "/", y_name, "/analysis_data/건국대학교 (입학정보관)_590_",
                           y_name, "_data.csv"))
data_591 = read.csv(paste0("./data/", version, "/", y_name, "/analysis_data/건국대학교 (행정관)_591_",
                           y_name, "_data.csv"))
data_3523 = read.csv(paste0("./data/", version, "/", y_name, "/analysis_data/건국대학교 과학관(이과대) 앞_3523_",
                            y_name, "_data.csv"))
data_3860 = read.csv(paste0("./data/", version, "/", y_name, "/analysis_data/건국대학교 정문 앞_3860_",
                            y_name, "_data.csv"))
data_592 = read.csv(paste0("./data/", version, "/", y_name, "/analysis_data/건국대학교 학생회관_592_",
                           y_name, "_data.csv"))
data_3569 = read.csv(paste0("./data/", version, "/", y_name, "/analysis_data/건대병원후문_3569_",
                            y_name, "_data.csv"))
data_3579 = read.csv(paste0("./data/", version, "/", y_name, "/analysis_data/광진 캠퍼스시티_3579_",
                            y_name, "_data.csv"))
data_500 = read.csv(paste0("./data/", version, "/", y_name, "/analysis_data/어린이대공원역 3번출구 앞_500_",
                           y_name, "_data.csv"))
data_3571 = read.csv(paste0("./data/", version, "/", y_name, "/analysis_data/화양 APT(횡단보도 옆)_3571_",
                            y_name, "_data.csv"))
data_3582 = read.csv(paste0("./data/", version, "/", y_name, "/analysis_data/화양동 우체국_3582_",
                            y_name, "_data.csv"))
data_3508 = read.csv(paste0("./data/", version, "/", y_name, "/analysis_data/화양사거리_3508_",
                            y_name, "_data.csv"))
gu = read.csv(paste0("./data/", version, "/", y_name, "/analysis_data/광진구_sample_",
                     y_name, "_data.csv"))
dong = read.csv(paste0("./data/", version, "/", y_name, "/analysis_data/화양동_sample_",
                       y_name, "_data.csv"))

# Delete timestamp, spot_number
data_590 = filter.fun(data_590)
data_591 = filter.fun(data_591)
data_3523 = filter.fun(data_3523)
data_3860 = filter.fun(data_3860)
data_592 = filter.fun(data_592)
data_3569 = filter.fun(data_3569)
data_3579 = filter.fun(data_3579)
data_500 = filter.fun(data_500)
data_3571 = filter.fun(data_3571)
data_3582 = filter.fun(data_3582)
data_3508 = filter.fun(data_3508)
gu = filter.fun(gu)
dong = filter.fun(dong)

##### variable selection #####

### individual station ###
# random forest
rf_data_590 = data_590[, vs.fun(data_590, 'rf', '590', y_name)]
rf_data_591 = data_591[, vs.fun(data_591, 'rf', '591', y_name)]
rf_data_3523 = data_3523[, vs.fun(data_3523, 'rf', '3523', y_name)]
rf_data_3860 = data_3860[, vs.fun(data_3860, 'rf', '3860', y_name)]
rf_data_592 = data_592[, vs.fun(data_592, 'rf', '592', y_name)]
rf_data_3569 = data_3569[, vs.fun(data_3569, 'rf', '3569', y_name)]
rf_data_3579 = data_3579[, vs.fun(data_3579, 'rf', '3579', y_name)]
rf_data_500 = data_500[, vs.fun(data_500, 'rf', '500', y_name)]
rf_data_3571 = data_3571[, vs.fun(data_3571, 'rf', '3571', y_name)]
rf_data_3582 = data_3582[, vs.fun(data_3582, 'rf', '3582', y_name)]
rf_data_3508 = data_3508[, vs.fun(data_3508, 'rf', '3508', y_name)]

# elastic net
elnet_data_590 = data_590[, vs.fun(data_590, 'elnet', '590', y_name)]
elnet_data_591 = data_591[, vs.fun(data_591, 'elnet', '591', y_name)]
elnet_data_3523 = data_3523[, vs.fun(data_3523, 'elnet', '3523', y_name)]
elnet_data_3860 = data_3860[, vs.fun(data_3860, 'elnet', '3860', y_name)]
elnet_data_592 = data_592[, vs.fun(data_592, 'elnet', '592', y_name)]
elnet_data_3569 = data_3569[, vs.fun(data_3569, 'elnet', '3569', y_name)]
elnet_data_3579 = data_3579[, vs.fun(data_3579, 'elnet', '3579', y_name)]
elnet_data_500 = data_500[, vs.fun(data_500, 'elnet', '500', y_name)]
elnet_data_3571 = data_3571[, vs.fun(data_3571, 'elnet', '3571', y_name)]
elnet_data_3582 = data_3582[, vs.fun(data_3582, 'elnet', '3582', y_name)]
elnet_data_3508 = data_3508[, vs.fun(data_3508, 'elnet', '3508', y_name)]

# random forest
xg_data_590 = data_590[, vs.fun(data_590, 'xg', '590', y_name)]
xg_data_591 = data_591[, vs.fun(data_591, 'xg', '591', y_name)]
xg_data_3523 = data_3523[, vs.fun(data_3523, 'xg', '3523', y_name)]
xg_data_3860 = data_3860[, vs.fun(data_3860, 'xg', '3860', y_name)]
xg_data_592 = data_592[, vs.fun(data_592, 'xg', '592', y_name)]
xg_data_3569 = data_3569[, vs.fun(data_3569, 'xg', '3569', y_name)]
xg_data_3579 = data_3579[, vs.fun(data_3579, 'xg', '3579', y_name)]
xg_data_500 = data_500[, vs.fun(data_500, 'xg', '500', y_name)]
xg_data_3571 = data_3571[, vs.fun(data_3571, 'xg', '3571', y_name)]
xg_data_3582 = data_3582[, vs.fun(data_3582, 'xg', '3582', y_name)]
xg_data_3508 = data_3508[, vs.fun(data_3508, 'xg', '3508', y_name)]

### gu ###
# random forest
rf_gu_var = vs.fun(gu, 'rf', 'gu', y_name)
rf_gu = gu[, rf_gu_var]
rf_data_590_gu = data_590[, rf_gu_var]
rf_data_591_gu = data_591[, rf_gu_var]
rf_data_3523_gu = data_3523[, rf_gu_var]
rf_data_3860_gu = data_3860[, rf_gu_var]
rf_data_592_gu = data_592[, rf_gu_var]
rf_data_3569_gu = data_3569[, rf_gu_var]
rf_data_3579_gu = data_3579[, rf_gu_var]
rf_data_500_gu = data_500[, rf_gu_var]
rf_data_3571_gu = data_3571[, rf_gu_var]
rf_data_3582_gu = data_3582[, rf_gu_var]
rf_data_3508_gu = data_3508[, rf_gu_var]

# elastic net
elnet_gu_var = vs.fun(gu, 'elnet', 'gu', y_name)
elnet_gu = gu[, elnet_gu_var]
elnet_data_590_gu = data_590[, elnet_gu_var]
elnet_data_591_gu = data_591[, elnet_gu_var]
elnet_data_3523_gu = data_3523[, elnet_gu_var]
elnet_data_3860_gu = data_3860[, elnet_gu_var]
elnet_data_592_gu = data_592[, elnet_gu_var]
elnet_data_3569_gu = data_3569[, elnet_gu_var]
elnet_data_3579_gu = data_3579[, elnet_gu_var]
elnet_data_500_gu = data_500[, elnet_gu_var]
elnet_data_3571_gu = data_3571[, elnet_gu_var]
elnet_data_3582_gu = data_3582[, elnet_gu_var]
elnet_data_3508_gu = data_3508[, elnet_gu_var]

# xgboost
xg_gu_var = vs.fun(gu, 'xg', 'gu', y_name)
xg_gu = gu[, xg_gu_var]
xg_data_590_gu = data_590[, xg_gu_var]
xg_data_591_gu = data_591[, xg_gu_var]
xg_data_3523_gu = data_3523[, xg_gu_var]
xg_data_3860_gu = data_3860[, xg_gu_var]
xg_data_592_gu = data_592[, xg_gu_var]
xg_data_3569_gu = data_3569[, xg_gu_var]
xg_data_3579_gu = data_3579[, xg_gu_var]
xg_data_500_gu = data_500[, xg_gu_var]
xg_data_3571_gu = data_3571[, xg_gu_var]
xg_data_3582_gu = data_3582[, xg_gu_var]
xg_data_3508_gu = data_3508[, xg_gu_var]

### dong ###
# random forest
rf_dong_var = vs.fun(dong, 'rf', 'dong', y_name)
rf_dong = dong[, rf_dong_var]
rf_data_590_dong = data_590[, rf_dong_var]
rf_data_591_dong = data_591[, rf_dong_var]
rf_data_3523_dong = data_3523[, rf_dong_var]
rf_data_3860_dong = data_3860[, rf_dong_var]
rf_data_592_dong = data_592[, rf_dong_var]
rf_data_3569_dong = data_3569[, rf_dong_var]
rf_data_3579_dong = data_3579[, rf_dong_var]
rf_data_500_dong = data_500[, rf_dong_var]
rf_data_3571_dong = data_3571[, rf_dong_var]
rf_data_3582_dong = data_3582[, rf_dong_var]
rf_data_3508_dong = data_3508[, rf_dong_var]

# elastic net
elnet_dong_var = vs.fun(dong, 'elnet', 'dong', y_name)
elnet_dong = dong[, elnet_dong_var]
elnet_data_590_dong = data_590[, elnet_dong_var]
elnet_data_591_dong = data_591[, elnet_dong_var]
elnet_data_3523_dong = data_3523[, elnet_dong_var]
elnet_data_3860_dong = data_3860[, elnet_dong_var]
elnet_data_592_dong = data_592[, elnet_dong_var]
elnet_data_3569_dong = data_3569[, elnet_dong_var]
elnet_data_3579_dong = data_3579[, elnet_dong_var]
elnet_data_500_dong = data_500[, elnet_dong_var]
elnet_data_3571_dong = data_3571[, elnet_dong_var]
elnet_data_3582_dong = data_3582[, elnet_dong_var]
elnet_data_3508_dong = data_3508[, elnet_dong_var]

# xgboost
xg_dong_var = vs.fun(dong, 'xg', 'dong', y_name)
xg_dong = dong[, xg_dong_var]
xg_data_590_dong = data_590[, xg_dong_var]
xg_data_591_dong = data_591[, xg_dong_var]
xg_data_3523_dong = data_3523[, xg_dong_var]
xg_data_3860_dong = data_3860[, xg_dong_var]
xg_data_592_dong = data_592[, xg_dong_var]
xg_data_3569_dong = data_3569[, xg_dong_var]
xg_data_3579_dong = data_3579[, xg_dong_var]
xg_data_500_dong = data_500[, xg_dong_var]
xg_data_3571_dong = data_3571[, xg_dong_var]
xg_data_3582_dong = data_3582[, xg_dong_var]
xg_data_3508_dong = data_3508[, xg_dong_var]

# --------------------------------------------------- #
#
# 2. Setting for analysis
#
# --------------------------------------------------- #

# -------- Setting hyper parameters -------- #

########################## gu ##########################
rf_params_gu = expand.grid(ntree = c(100, 150, 200),
                           mtry = c(10, 20, 30))

elnet_params_gu = expand.grid(alpha = seq(0, 1, by=0.1),
                              lambda = 10^seq(-3, 3, by=0.1))

xg_params_gu = expand.grid(nrounds = c(800, 1000, 1200),
                           max_depth = c(6, 9, 12),
                           eta = c(0.08, 0.1, 0.12))

########################## dong ##########################
rf_params_dong = expand.grid(ntree = c(100, 150, 200),
                             mtry = c(10, 20, 30))

elnet_params_dong = expand.grid(alpha = seq(0, 1, by=0.1),
                                lambda = 10^seq(-3, 3, by=0.1))

xg_params_dong = expand.grid(nrounds = c(800, 1000, 1200),
                             max_depth = c(6, 9, 12),
                             eta = c(0.08, 0.1, 0.12))

# ---------------- Setting to make result  ------------------ #
n_cv_tt = 1  # number of cv for train / test
n_cv_tv = 5  # number of cv for train / validation

# train, validation result (2 X n_cv_tv)
# best result (2 X n_cv_tt)
rf_train_result = rf_val_result = rf_best_result = NULL
elnet_train_result = elnet_val_result = elnet_best_result = NULL
xg_train_result = xg_val_result = xg_best_result = NULL

# test result (11 X n_cv_tt) (gu, dong)
rf_test_result_gu = elnet_test_result_gu = xg_test_result_gu = NULL
rf_test_result_dong = elnet_test_result_dong = xg_test_result_dong = NULL

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
    
    ##### make test set #####
    
    ### gu ###
    # random forest
    rf_test_3508_gu = rf_data_3508_gu[test_idx, ]
    rf_test_3523_gu = rf_data_3523_gu[test_idx, ]
    rf_test_3569_gu = rf_data_3569_gu[test_idx, ]
    rf_test_3571_gu = rf_data_3571_gu[test_idx, ]
    rf_test_3579_gu = rf_data_3579_gu[test_idx, ]
    rf_test_3582_gu = rf_data_3582_gu[test_idx, ]
    rf_test_3860_gu = rf_data_3860_gu[test_idx, ]
    rf_test_500_gu = rf_data_500_gu[test_idx, ]
    rf_test_590_gu = rf_data_590_gu[test_idx, ]
    rf_test_591_gu = rf_data_591_gu[test_idx, ]
    rf_test_592_gu = rf_data_592_gu[test_idx, ]
    
    # elastic net
    elnet_test_3508_gu = elnet_data_3508_gu[test_idx, ]
    elnet_test_3523_gu = elnet_data_3523_gu[test_idx, ]
    elnet_test_3569_gu = elnet_data_3569_gu[test_idx, ]
    elnet_test_3571_gu = elnet_data_3571_gu[test_idx, ]
    elnet_test_3579_gu = elnet_data_3579_gu[test_idx, ]
    elnet_test_3582_gu = elnet_data_3582_gu[test_idx, ]
    elnet_test_3860_gu = elnet_data_3860_gu[test_idx, ]
    elnet_test_500_gu = elnet_data_500_gu[test_idx, ]
    elnet_test_590_gu = elnet_data_590_gu[test_idx, ]
    elnet_test_591_gu = elnet_data_591_gu[test_idx, ]
    elnet_test_592_gu = elnet_data_592_gu[test_idx, ]
    
    # xgboost
    xg_test_3508_gu = xg_data_3508_gu[test_idx, ]
    xg_test_3523_gu = xg_data_3523_gu[test_idx, ]
    xg_test_3569_gu = xg_data_3569_gu[test_idx, ]
    xg_test_3571_gu = xg_data_3571_gu[test_idx, ]
    xg_test_3579_gu = xg_data_3579_gu[test_idx, ]
    xg_test_3582_gu = xg_data_3582_gu[test_idx, ]
    xg_test_3860_gu = xg_data_3860_gu[test_idx, ]
    xg_test_500_gu = xg_data_500_gu[test_idx, ]
    xg_test_590_gu = xg_data_590_gu[test_idx, ]
    xg_test_591_gu = xg_data_591_gu[test_idx, ]
    xg_test_592_gu = xg_data_592_gu[test_idx, ]
    
    ### dong ###
    # random forest
    rf_test_3508_dong = rf_data_3508_dong[test_idx, ]
    rf_test_3523_dong = rf_data_3523_dong[test_idx, ]
    rf_test_3569_dong = rf_data_3569_dong[test_idx, ]
    rf_test_3571_dong = rf_data_3571_dong[test_idx, ]
    rf_test_3579_dong = rf_data_3579_dong[test_idx, ]
    rf_test_3582_dong = rf_data_3582_dong[test_idx, ]
    rf_test_3860_dong = rf_data_3860_dong[test_idx, ]
    rf_test_500_dong = rf_data_500_dong[test_idx, ]
    rf_test_590_dong = rf_data_590_dong[test_idx, ]
    rf_test_591_dong = rf_data_591_dong[test_idx, ]
    rf_test_592_dong = rf_data_592_dong[test_idx, ]
    
    # elastic net
    elnet_test_3508_dong = elnet_data_3508_dong[test_idx, ]
    elnet_test_3523_dong = elnet_data_3523_dong[test_idx, ]
    elnet_test_3569_dong = elnet_data_3569_dong[test_idx, ]
    elnet_test_3571_dong = elnet_data_3571_dong[test_idx, ]
    elnet_test_3579_dong = elnet_data_3579_dong[test_idx, ]
    elnet_test_3582_dong = elnet_data_3582_dong[test_idx, ]
    elnet_test_3860_dong = elnet_data_3860_dong[test_idx, ]
    elnet_test_500_dong = elnet_data_500_dong[test_idx, ]
    elnet_test_590_dong = elnet_data_590_dong[test_idx, ]
    elnet_test_591_dong = elnet_data_591_dong[test_idx, ]
    elnet_test_592_dong = elnet_data_592_dong[test_idx, ]
    
    # xgboost
    xg_test_3508_dong = xg_data_3508_dong[test_idx, ]
    xg_test_3523_dong = xg_data_3523_dong[test_idx, ]
    xg_test_3569_dong = xg_data_3569_dong[test_idx, ]
    xg_test_3571_dong = xg_data_3571_dong[test_idx, ]
    xg_test_3579_dong = xg_data_3579_dong[test_idx, ]
    xg_test_3582_dong = xg_data_3582_dong[test_idx, ]
    xg_test_3860_dong = xg_data_3860_dong[test_idx, ]
    xg_test_500_dong = xg_data_500_dong[test_idx, ]
    xg_test_590_dong = xg_data_590_dong[test_idx, ]
    xg_test_591_dong = xg_data_591_dong[test_idx, ]
    xg_test_592_dong = xg_data_592_dong[test_idx, ]
    
    for (iiter in 1:n_cv_tv){
        print(paste("#", iiter, "cv train/val"))
        # split train --> train / validation with 40:40
        train_idx = sample(train_val_idx, size=n*0.4)
        val_idx = setdiff(train_val_idx, train_idx)
        
        ##### make train set #####
        rf_train_gu = rf_gu[train_idx, ]
        rf_train_dong = rf_dong[train_idx, ]
        elnet_train_gu = elnet_gu[train_idx, ]
        elnet_train_dong = elnet_dong[train_idx, ]
        xg_train_gu = xg_gu[train_idx, ]
        xg_train_dong = xg_dong[train_idx, ]
        
        ### gu ###
        # random forest
        rf_train_3508_gu = rf_data_3508_gu[train_idx, ]
        rf_train_3523_gu = rf_data_3523_gu[train_idx, ]
        rf_train_3569_gu = rf_data_3569_gu[train_idx, ]
        rf_train_3571_gu = rf_data_3571_gu[train_idx, ]
        rf_train_3579_gu = rf_data_3579_gu[train_idx, ]
        rf_train_3582_gu = rf_data_3582_gu[train_idx, ]
        rf_train_3860_gu = rf_data_3860_gu[train_idx, ]
        rf_train_500_gu = rf_data_500_gu[train_idx, ]
        rf_train_590_gu = rf_data_590_gu[train_idx, ]
        rf_train_591_gu = rf_data_591_gu[train_idx, ]
        rf_train_592_gu = rf_data_592_gu[train_idx, ]
        
        # elastic net
        elnet_train_3508_gu = elnet_data_3508_gu[train_idx, ]
        elnet_train_3523_gu = elnet_data_3523_gu[train_idx, ]
        elnet_train_3569_gu = elnet_data_3569_gu[train_idx, ]
        elnet_train_3571_gu = elnet_data_3571_gu[train_idx, ]
        elnet_train_3579_gu = elnet_data_3579_gu[train_idx, ]
        elnet_train_3582_gu = elnet_data_3582_gu[train_idx, ]
        elnet_train_3860_gu = elnet_data_3860_gu[train_idx, ]
        elnet_train_500_gu = elnet_data_500_gu[train_idx, ]
        elnet_train_590_gu = elnet_data_590_gu[train_idx, ]
        elnet_train_591_gu = elnet_data_591_gu[train_idx, ]
        elnet_train_592_gu = elnet_data_592_gu[train_idx, ]
        
        # xgboost
        xg_train_3508_gu = xg_data_3508_gu[train_idx, ]
        xg_train_3523_gu = xg_data_3523_gu[train_idx, ]
        xg_train_3569_gu = xg_data_3569_gu[train_idx, ]
        xg_train_3571_gu = xg_data_3571_gu[train_idx, ]
        xg_train_3579_gu = xg_data_3579_gu[train_idx, ]
        xg_train_3582_gu = xg_data_3582_gu[train_idx, ]
        xg_train_3860_gu = xg_data_3860_gu[train_idx, ]
        xg_train_500_gu = xg_data_500_gu[train_idx, ]
        xg_train_590_gu = xg_data_590_gu[train_idx, ]
        xg_train_591_gu = xg_data_591_gu[train_idx, ]
        xg_train_592_gu = xg_data_592_gu[train_idx, ]
        
        ### dong ###
        # random forest
        rf_train_3508_dong = rf_data_3508_dong[train_idx, ]
        rf_train_3523_dong = rf_data_3523_dong[train_idx, ]
        rf_train_3569_dong = rf_data_3569_dong[train_idx, ]
        rf_train_3571_dong = rf_data_3571_dong[train_idx, ]
        rf_train_3579_dong = rf_data_3579_dong[train_idx, ]
        rf_train_3582_dong = rf_data_3582_dong[train_idx, ]
        rf_train_3860_dong = rf_data_3860_dong[train_idx, ]
        rf_train_500_dong = rf_data_500_dong[train_idx, ]
        rf_train_590_dong = rf_data_590_dong[train_idx, ]
        rf_train_591_dong = rf_data_591_dong[train_idx, ]
        rf_train_592_dong = rf_data_592_dong[train_idx, ]
        
        # elastic net
        elnet_train_3508_dong = elnet_data_3508_dong[train_idx, ]
        elnet_train_3523_dong = elnet_data_3523_dong[train_idx, ]
        elnet_train_3569_dong = elnet_data_3569_dong[train_idx, ]
        elnet_train_3571_dong = elnet_data_3571_dong[train_idx, ]
        elnet_train_3579_dong = elnet_data_3579_dong[train_idx, ]
        elnet_train_3582_dong = elnet_data_3582_dong[train_idx, ]
        elnet_train_3860_dong = elnet_data_3860_dong[train_idx, ]
        elnet_train_500_dong = elnet_data_500_dong[train_idx, ]
        elnet_train_590_dong = elnet_data_590_dong[train_idx, ]
        elnet_train_591_dong = elnet_data_591_dong[train_idx, ]
        elnet_train_592_dong = elnet_data_592_dong[train_idx, ]
        
        # xgboost
        xg_train_3508_dong = xg_data_3508_dong[train_idx, ]
        xg_train_3523_dong = xg_data_3523_dong[train_idx, ]
        xg_train_3569_dong = xg_data_3569_dong[train_idx, ]
        xg_train_3571_dong = xg_data_3571_dong[train_idx, ]
        xg_train_3579_dong = xg_data_3579_dong[train_idx, ]
        xg_train_3582_dong = xg_data_3582_dong[train_idx, ]
        xg_train_3860_dong = xg_data_3860_dong[train_idx, ]
        xg_train_500_dong = xg_data_500_dong[train_idx, ]
        xg_train_590_dong = xg_data_590_dong[train_idx, ]
        xg_train_591_dong = xg_data_591_dong[train_idx, ]
        xg_train_592_dong = xg_data_592_dong[train_idx, ]
        
        ##### make validation set #####
        
        rf_val_gu = rf_gu[val_idx, ]
        rf_val_dong = rf_dong[val_idx, ]
        elnet_val_gu = elnet_gu[val_idx, ]
        elnet_val_dong = elnet_dong[val_idx, ]
        xg_val_gu = xg_gu[val_idx, ]
        xg_val_dong = xg_dong[val_idx, ]
        
        ### gu ###
        # random forest
        rf_val_3508_gu = rf_data_3508_gu[val_idx, ]
        rf_val_3523_gu = rf_data_3523_gu[val_idx, ]
        rf_val_3569_gu = rf_data_3569_gu[val_idx, ]
        rf_val_3571_gu = rf_data_3571_gu[val_idx, ]
        rf_val_3579_gu = rf_data_3579_gu[val_idx, ]
        rf_val_3582_gu = rf_data_3582_gu[val_idx, ]
        rf_val_3860_gu = rf_data_3860_gu[val_idx, ]
        rf_val_500_gu = rf_data_500_gu[val_idx, ]
        rf_val_590_gu = rf_data_590_gu[val_idx, ]
        rf_val_591_gu = rf_data_591_gu[val_idx, ]
        rf_val_592_gu = rf_data_592_gu[val_idx, ]
        
        # elastic net
        elnet_val_3508_gu = elnet_data_3508_gu[val_idx, ]
        elnet_val_3523_gu = elnet_data_3523_gu[val_idx, ]
        elnet_val_3569_gu = elnet_data_3569_gu[val_idx, ]
        elnet_val_3571_gu = elnet_data_3571_gu[val_idx, ]
        elnet_val_3579_gu = elnet_data_3579_gu[val_idx, ]
        elnet_val_3582_gu = elnet_data_3582_gu[val_idx, ]
        elnet_val_3860_gu = elnet_data_3860_gu[val_idx, ]
        elnet_val_500_gu = elnet_data_500_gu[val_idx, ]
        elnet_val_590_gu = elnet_data_590_gu[val_idx, ]
        elnet_val_591_gu = elnet_data_591_gu[val_idx, ]
        elnet_val_592_gu = elnet_data_592_gu[val_idx, ]
        
        # xgboost
        xg_val_3508_gu = xg_data_3508_gu[val_idx, ]
        xg_val_3523_gu = xg_data_3523_gu[val_idx, ]
        xg_val_3569_gu = xg_data_3569_gu[val_idx, ]
        xg_val_3571_gu = xg_data_3571_gu[val_idx, ]
        xg_val_3579_gu = xg_data_3579_gu[val_idx, ]
        xg_val_3582_gu = xg_data_3582_gu[val_idx, ]
        xg_val_3860_gu = xg_data_3860_gu[val_idx, ]
        xg_val_500_gu = xg_data_500_gu[val_idx, ]
        xg_val_590_gu = xg_data_590_gu[val_idx, ]
        xg_val_591_gu = xg_data_591_gu[val_idx, ]
        xg_val_592_gu = xg_data_592_gu[val_idx, ]
        
        ### dong ###
        # random forest
        rf_val_3508_dong = rf_data_3508_dong[val_idx, ]
        rf_val_3523_dong = rf_data_3523_dong[val_idx, ]
        rf_val_3569_dong = rf_data_3569_dong[val_idx, ]
        rf_val_3571_dong = rf_data_3571_dong[val_idx, ]
        rf_val_3579_dong = rf_data_3579_dong[val_idx, ]
        rf_val_3582_dong = rf_data_3582_dong[val_idx, ]
        rf_val_3860_dong = rf_data_3860_dong[val_idx, ]
        rf_val_500_dong = rf_data_500_dong[val_idx, ]
        rf_val_590_dong = rf_data_590_dong[val_idx, ]
        rf_val_591_dong = rf_data_591_dong[val_idx, ]
        rf_val_592_dong = rf_data_592_dong[val_idx, ]
        
        # elastic net
        elnet_val_3508_dong = elnet_data_3508_dong[val_idx, ]
        elnet_val_3523_dong = elnet_data_3523_dong[val_idx, ]
        elnet_val_3569_dong = elnet_data_3569_dong[val_idx, ]
        elnet_val_3571_dong = elnet_data_3571_dong[val_idx, ]
        elnet_val_3579_dong = elnet_data_3579_dong[val_idx, ]
        elnet_val_3582_dong = elnet_data_3582_dong[val_idx, ]
        elnet_val_3860_dong = elnet_data_3860_dong[val_idx, ]
        elnet_val_500_dong = elnet_data_500_dong[val_idx, ]
        elnet_val_590_dong = elnet_data_590_dong[val_idx, ]
        elnet_val_591_dong = elnet_data_591_dong[val_idx, ]
        elnet_val_592_dong = elnet_data_592_dong[val_idx, ]
        
        # xgboost
        xg_val_3508_dong = xg_data_3508_dong[val_idx, ]
        xg_val_3523_dong = xg_data_3523_dong[val_idx, ]
        xg_val_3569_dong = xg_data_3569_dong[val_idx, ]
        xg_val_3571_dong = xg_data_3571_dong[val_idx, ]
        xg_val_3579_dong = xg_data_3579_dong[val_idx, ]
        xg_val_3582_dong = xg_data_3582_dong[val_idx, ]
        xg_val_3860_dong = xg_data_3860_dong[val_idx, ]
        xg_val_500_dong = xg_data_500_dong[val_idx, ]
        xg_val_590_dong = xg_data_590_dong[val_idx, ]
        xg_val_591_dong = xg_data_591_dong[val_idx, ]
        xg_val_592_dong = xg_data_592_dong[val_idx, ]
        
        # ----------- Calculate train and validation error  ------------- #
        # to find the best hyper parameters
        
        ########################## random forest ##########################
        print(paste("#", iiter, "cv train/val", "random forest"))
        # train validation mse of random forest
        rf_tv_mse_gu = make_result_train_val_mse(model="rf", params=rf_params_gu, 
                                                 train=rf_train_gu, val=rf_val_gu)
        rf_tv_mse_dong = make_result_train_val_mse(model="rf", params=rf_params_dong, 
                                                   train=rf_train_dong, val=rf_val_dong)
        
        # combine train result
        rf_train_result = rbind(rf_train_result, rf_tv_mse_gu$train_mse_mat)
        rf_train_result = rbind(rf_train_result, rf_tv_mse_dong$train_mse_mat)
        
        # combine validation result
        rf_val_result = rbind(rf_val_result, rf_tv_mse_gu$val_mse_mat)
        rf_val_result = rbind(rf_val_result, rf_tv_mse_dong$val_mse_mat)
        
        ########################## elastic net ##########################
        print(paste("#", iiter, "cv train/val", "elastic net"))
        # train validation mse of elastic net
        elnet_tv_mse_gu = make_result_train_val_mse(model="elnet", params=elnet_params_gu, 
                                                    train=elnet_train_gu, val=elnet_val_gu)
        elnet_tv_mse_dong = make_result_train_val_mse(model="elnet", params=elnet_params_dong, 
                                                      train=elnet_train_dong, val=elnet_val_dong)
        
        # combine train result
        elnet_train_result = rbind(elnet_train_result, elnet_tv_mse_gu$train_mse_mat)
        elnet_train_result = rbind(elnet_train_result, elnet_tv_mse_dong$train_mse_mat)
        
        # combine validation result
        elnet_val_result = rbind(elnet_val_result, elnet_tv_mse_gu$val_mse_mat)
        elnet_val_result = rbind(elnet_val_result, elnet_tv_mse_dong$val_mse_mat)
        
        ########################## xgboost ##########################
        print(paste("#", iiter, "cv train/val", "xgboost"))
        # train validation mse of xgboost
        xg_tv_mse_gu = make_result_train_val_mse(model="xg", params=xg_params_gu, 
                                                 train=xg_train_gu, val=xg_val_gu)
        xg_tv_mse_dong = make_result_train_val_mse(model="xg", params=xg_params_dong, 
                                                   train=xg_train_dong, val=xg_val_dong)
        
        # combine train result
        xg_train_result = rbind(xg_train_result, xg_tv_mse_gu$train_mse_mat)
        xg_train_result = rbind(xg_train_result, xg_tv_mse_dong$train_mse_mat)
        
        # combine validation result
        xg_val_result = rbind(xg_val_result, xg_tv_mse_gu$val_mse_mat)
        xg_val_result = rbind(xg_val_result, xg_tv_mse_dong$val_mse_mat)
        
    }
    
    # ------------------- Find the best hyper parameters ------------------ #
    
    ########################## random forest ##########################
    rf_best_com_gu = find_best_combination(rf_tv_mse_gu$val_mse_mat)
    rf_best_com_dong = find_best_combination(rf_tv_mse_dong$val_mse_mat)
    
    ########################## elastic net ##########################
    elnet_best_com_gu = find_best_combination(elnet_tv_mse_gu$val_mse_mat)
    elnet_best_com_dong = find_best_combination(elnet_tv_mse_dong$val_mse_mat)
    
    ########################## xgboost ##########################
    xg_best_com_gu = find_best_combination(xg_tv_mse_gu$val_mse_mat)
    xg_best_com_dong = find_best_combination(xg_tv_mse_dong$val_mse_mat)
    
    # --------------- Make model with the best hyper parameters --------------- #
    
    # Make the best model with (train + val) data
    
    # random forest
    rf_train_val_gu = rf_gu[-test_idx, ]
    rf_train_val_dong = rf_dong[-test_idx, ]
    
    # elastic net
    elnet_train_val_gu = elnet_gu[-test_idx, ]
    elnet_train_val_dong = elnet_dong[-test_idx, ]
    
    # xgboost
    xg_train_val_gu = xg_gu[-test_idx, ]
    xg_train_val_dong = xg_dong[-test_idx, ]
    
    ### gu ###
    # random forest
    rf_train_val_3508_gu = rf_data_3508_gu[-test_idx, ]
    rf_train_val_3523_gu = rf_data_3523_gu[-test_idx, ]
    rf_train_val_3569_gu = rf_data_3569_gu[-test_idx, ]
    rf_train_val_3571_gu = rf_data_3571_gu[-test_idx, ]
    rf_train_val_3579_gu = rf_data_3579_gu[-test_idx, ]
    rf_train_val_3582_gu = rf_data_3582_gu[-test_idx, ]
    rf_train_val_3860_gu = rf_data_3860_gu[-test_idx, ]
    rf_train_val_500_gu = rf_data_500_gu[-test_idx, ]
    rf_train_val_590_gu = rf_data_590_gu[-test_idx, ]
    rf_train_val_591_gu = rf_data_591_gu[-test_idx, ]
    rf_train_val_592_gu = rf_data_592_gu[-test_idx, ]
    
    # elastic net
    elnet_train_val_3508_gu = elnet_data_3508_gu[-test_idx, ]
    elnet_train_val_3523_gu = elnet_data_3523_gu[-test_idx, ]
    elnet_train_val_3569_gu = elnet_data_3569_gu[-test_idx, ]
    elnet_train_val_3571_gu = elnet_data_3571_gu[-test_idx, ]
    elnet_train_val_3579_gu = elnet_data_3579_gu[-test_idx, ]
    elnet_train_val_3582_gu = elnet_data_3582_gu[-test_idx, ]
    elnet_train_val_3860_gu = elnet_data_3860_gu[-test_idx, ]
    elnet_train_val_500_gu = elnet_data_500_gu[-test_idx, ]
    elnet_train_val_590_gu = elnet_data_590_gu[-test_idx, ]
    elnet_train_val_591_gu = elnet_data_591_gu[-test_idx, ]
    elnet_train_val_592_gu = elnet_data_592_gu[-test_idx, ]
    
    # xgboost
    xg_train_val_3508_gu = xg_data_3508_gu[-test_idx, ]
    xg_train_val_3523_gu = xg_data_3523_gu[-test_idx, ]
    xg_train_val_3569_gu = xg_data_3569_gu[-test_idx, ]
    xg_train_val_3571_gu = xg_data_3571_gu[-test_idx, ]
    xg_train_val_3579_gu = xg_data_3579_gu[-test_idx, ]
    xg_train_val_3582_gu = xg_data_3582_gu[-test_idx, ]
    xg_train_val_3860_gu = xg_data_3860_gu[-test_idx, ]
    xg_train_val_500_gu = xg_data_500_gu[-test_idx, ]
    xg_train_val_590_gu = xg_data_590_gu[-test_idx, ]
    xg_train_val_591_gu = xg_data_591_gu[-test_idx, ]
    xg_train_val_592_gu = xg_data_592_gu[-test_idx, ]
    
    ### dong ###
    # random forest
    rf_train_val_3508_dong = rf_data_3508_dong[-test_idx, ]
    rf_train_val_3523_dong = rf_data_3523_dong[-test_idx, ]
    rf_train_val_3569_dong = rf_data_3569_dong[-test_idx, ]
    rf_train_val_3571_dong = rf_data_3571_dong[-test_idx, ]
    rf_train_val_3579_dong = rf_data_3579_dong[-test_idx, ]
    rf_train_val_3582_dong = rf_data_3582_dong[-test_idx, ]
    rf_train_val_3860_dong = rf_data_3860_dong[-test_idx, ]
    rf_train_val_500_dong = rf_data_500_dong[-test_idx, ]
    rf_train_val_590_dong = rf_data_590_dong[-test_idx, ]
    rf_train_val_591_dong = rf_data_591_dong[-test_idx, ]
    rf_train_val_592_dong = rf_data_592_dong[-test_idx, ]
    
    # elastic net
    elnet_train_val_3508_dong = elnet_data_3508_dong[-test_idx, ]
    elnet_train_val_3523_dong = elnet_data_3523_dong[-test_idx, ]
    elnet_train_val_3569_dong = elnet_data_3569_dong[-test_idx, ]
    elnet_train_val_3571_dong = elnet_data_3571_dong[-test_idx, ]
    elnet_train_val_3579_dong = elnet_data_3579_dong[-test_idx, ]
    elnet_train_val_3582_dong = elnet_data_3582_dong[-test_idx, ]
    elnet_train_val_3860_dong = elnet_data_3860_dong[-test_idx, ]
    elnet_train_val_500_dong = elnet_data_500_dong[-test_idx, ]
    elnet_train_val_590_dong = elnet_data_590_dong[-test_idx, ]
    elnet_train_val_591_dong = elnet_data_591_dong[-test_idx, ]
    elnet_train_val_592_dong = elnet_data_592_dong[-test_idx, ]
    
    # xgboost
    xg_train_val_3508_dong = xg_data_3508_dong[-test_idx, ]
    xg_train_val_3523_dong = xg_data_3523_dong[-test_idx, ]
    xg_train_val_3569_dong = xg_data_3569_dong[-test_idx, ]
    xg_train_val_3571_dong = xg_data_3571_dong[-test_idx, ]
    xg_train_val_3579_dong = xg_data_3579_dong[-test_idx, ]
    xg_train_val_3582_dong = xg_data_3582_dong[-test_idx, ]
    xg_train_val_3860_dong = xg_data_3860_dong[-test_idx, ]
    xg_train_val_500_dong = xg_data_500_dong[-test_idx, ]
    xg_train_val_590_dong = xg_data_590_dong[-test_idx, ]
    xg_train_val_591_dong = xg_data_591_dong[-test_idx, ]
    xg_train_val_592_dong = xg_data_592_dong[-test_idx, ]
    
    # Fitting with the best hyper parameters
    
    ########################## random forest ##########################
    print(paste("#", iter, "cv train/test", "random forest"))
    # Fitting
    rf_best_fit_gu = randomForest(y ~., data=rf_train_val_gu, 
                                  ntree=rf_best_com_gu$ntree, mtry=rf_best_com_gu$mtry)
    rf_best_fit_dong = randomForest(y ~., data=rf_train_val_dong, 
                                    ntree=rf_best_com_dong$ntree, mtry=rf_best_com_dong$mtry)
    
    # calculate mse with the best model
    rf_best_mse_gu = calculate_mse(model='rf', fit=rf_best_fit_gu, rf_train_val_gu)
    rf_best_mse_dong = calculate_mse(model='rf', fit=rf_best_fit_dong, rf_train_val_dong)
    
    # combine best mse
    rf_best_result = rbind(rf_best_result, c(rf_best_com_gu$ntree, rf_best_com_gu$mtry, rf_best_mse_gu))
    rf_best_result = rbind(rf_best_result, c(rf_best_com_dong$ntree, rf_best_com_dong$mtry, rf_best_mse_dong))
    
    # Calculate the test mse for gu
    rf_test_mse_gu_3508 = calculate_mse(model='rf', fit=rf_best_fit_gu, rf_test_3508_gu)
    rf_test_mse_gu_3523 = calculate_mse(model='rf', fit=rf_best_fit_gu, rf_test_3523_gu)
    rf_test_mse_gu_3569 = calculate_mse(model='rf', fit=rf_best_fit_gu, rf_test_3569_gu)
    rf_test_mse_gu_3571 = calculate_mse(model='rf', fit=rf_best_fit_gu, rf_test_3571_gu)
    rf_test_mse_gu_3579 = calculate_mse(model='rf', fit=rf_best_fit_gu, rf_test_3579_gu)
    rf_test_mse_gu_3582 = calculate_mse(model='rf', fit=rf_best_fit_gu, rf_test_3582_gu)
    rf_test_mse_gu_3860 = calculate_mse(model='rf', fit=rf_best_fit_gu, rf_test_3860_gu)
    rf_test_mse_gu_500 = calculate_mse(model='rf', fit=rf_best_fit_gu, rf_test_500_gu)
    rf_test_mse_gu_590 = calculate_mse(model='rf', fit=rf_best_fit_gu, rf_test_590_gu)
    rf_test_mse_gu_591 = calculate_mse(model='rf', fit=rf_best_fit_gu, rf_test_591_gu)
    rf_test_mse_gu_592 = calculate_mse(model='rf', fit=rf_best_fit_gu, rf_test_592_gu)
    
    # Calculate the test mse for dong
    rf_test_mse_dong_3508 = calculate_mse(model='rf', fit=rf_best_fit_dong, rf_test_3508_dong)
    rf_test_mse_dong_3523 = calculate_mse(model='rf', fit=rf_best_fit_dong, rf_test_3523_dong)
    rf_test_mse_dong_3569 = calculate_mse(model='rf', fit=rf_best_fit_dong, rf_test_3569_dong)
    rf_test_mse_dong_3571 = calculate_mse(model='rf', fit=rf_best_fit_dong, rf_test_3571_dong)
    rf_test_mse_dong_3579 = calculate_mse(model='rf', fit=rf_best_fit_dong, rf_test_3579_dong)
    rf_test_mse_dong_3582 = calculate_mse(model='rf', fit=rf_best_fit_dong, rf_test_3582_dong)
    rf_test_mse_dong_3860 = calculate_mse(model='rf', fit=rf_best_fit_dong, rf_test_3860_dong)
    rf_test_mse_dong_500 = calculate_mse(model='rf', fit=rf_best_fit_dong, rf_test_500_dong)
    rf_test_mse_dong_590 = calculate_mse(model='rf', fit=rf_best_fit_dong, rf_test_590_dong)
    rf_test_mse_dong_591 = calculate_mse(model='rf', fit=rf_best_fit_dong, rf_test_591_dong)
    rf_test_mse_dong_592 = calculate_mse(model='rf', fit=rf_best_fit_dong, rf_test_592_dong)
    
    # combine the test mse result for gu
    rf_test_result_gu = rbind(rf_test_result_gu, c(rf_best_com_gu$ntree, rf_best_com_gu$mtry, rf_test_mse_gu_3508))
    rf_test_result_gu = rbind(rf_test_result_gu, c(rf_best_com_gu$ntree, rf_best_com_gu$mtry, rf_test_mse_gu_3523))
    rf_test_result_gu = rbind(rf_test_result_gu, c(rf_best_com_gu$ntree, rf_best_com_gu$mtry, rf_test_mse_gu_3569))
    rf_test_result_gu = rbind(rf_test_result_gu, c(rf_best_com_gu$ntree, rf_best_com_gu$mtry, rf_test_mse_gu_3571))
    rf_test_result_gu = rbind(rf_test_result_gu, c(rf_best_com_gu$ntree, rf_best_com_gu$mtry, rf_test_mse_gu_3579))
    rf_test_result_gu = rbind(rf_test_result_gu, c(rf_best_com_gu$ntree, rf_best_com_gu$mtry, rf_test_mse_gu_3582))
    rf_test_result_gu = rbind(rf_test_result_gu, c(rf_best_com_gu$ntree, rf_best_com_gu$mtry, rf_test_mse_gu_3860))
    rf_test_result_gu = rbind(rf_test_result_gu, c(rf_best_com_gu$ntree, rf_best_com_gu$mtry, rf_test_mse_gu_500))
    rf_test_result_gu = rbind(rf_test_result_gu, c(rf_best_com_gu$ntree, rf_best_com_gu$mtry, rf_test_mse_gu_590))
    rf_test_result_gu = rbind(rf_test_result_gu, c(rf_best_com_gu$ntree, rf_best_com_gu$mtry, rf_test_mse_gu_591))
    rf_test_result_gu = rbind(rf_test_result_gu, c(rf_best_com_gu$ntree, rf_best_com_gu$mtry, rf_test_mse_gu_592))
    
    # combine the test mse result for dong
    rf_test_result_dong = rbind(rf_test_result_dong, c(rf_best_com_dong$ntree, rf_best_com_dong$mtry, rf_test_mse_dong_3508))
    rf_test_result_dong = rbind(rf_test_result_dong, c(rf_best_com_dong$ntree, rf_best_com_dong$mtry, rf_test_mse_dong_3523))
    rf_test_result_dong = rbind(rf_test_result_dong, c(rf_best_com_dong$ntree, rf_best_com_dong$mtry, rf_test_mse_dong_3569))
    rf_test_result_dong = rbind(rf_test_result_dong, c(rf_best_com_dong$ntree, rf_best_com_dong$mtry, rf_test_mse_dong_3571))
    rf_test_result_dong = rbind(rf_test_result_dong, c(rf_best_com_dong$ntree, rf_best_com_dong$mtry, rf_test_mse_dong_3579))
    rf_test_result_dong = rbind(rf_test_result_dong, c(rf_best_com_dong$ntree, rf_best_com_dong$mtry, rf_test_mse_dong_3582))
    rf_test_result_dong = rbind(rf_test_result_dong, c(rf_best_com_dong$ntree, rf_best_com_dong$mtry, rf_test_mse_dong_3860))
    rf_test_result_dong = rbind(rf_test_result_dong, c(rf_best_com_dong$ntree, rf_best_com_dong$mtry, rf_test_mse_dong_500))
    rf_test_result_dong = rbind(rf_test_result_dong, c(rf_best_com_dong$ntree, rf_best_com_dong$mtry, rf_test_mse_dong_590))
    rf_test_result_dong = rbind(rf_test_result_dong, c(rf_best_com_dong$ntree, rf_best_com_dong$mtry, rf_test_mse_dong_591))
    rf_test_result_dong = rbind(rf_test_result_dong, c(rf_best_com_dong$ntree, rf_best_com_dong$mtry, rf_test_mse_dong_592))
    
    
    ########################## elastic net ##########################
    print(paste("#", iter, "cv train/test", "elastic net"))
    elnet_best_fit_gu = glmnet(x=elnet_train_val_gu[, -1], y=elnet_train_val_gu[, 1], 
                               alpha=elnet_best_com_gu$alpha, lambda=elnet_best_com_gu$alpha)
    elnet_best_fit_dong = glmnet(x=elnet_train_val_dong[, -1], y=elnet_train_val_dong[, 1], 
                                 alpha=elnet_best_com_dong$alpha, lambda=elnet_best_com_dong$alpha)
    
    # calculate mse with the best model
    elnet_best_mse_gu = calculate_mse(model='elnet', fit=elnet_best_fit_gu, elnet_train_val_gu)
    elnet_best_mse_dong = calculate_mse(model='elnet', fit=elnet_best_fit_dong, elnet_train_val_dong)
    
    # combine best mse result
    elnet_best_result = rbind(elnet_best_result, c(elnet_best_com_gu$alpha, elnet_best_com_gu$lambda, elnet_best_mse_gu))
    elnet_best_result = rbind(elnet_best_result, c(elnet_best_com_dong$alpha, elnet_best_com_dong$lambda, elnet_best_mse_dong))
    
    # Calculate the test mse for gu
    elnet_test_mse_gu_3508 = calculate_mse(model='elnet', fit=elnet_best_fit_gu, elnet_test_3508_gu)
    elnet_test_mse_gu_3523 = calculate_mse(model='elnet', fit=elnet_best_fit_gu, elnet_test_3523_gu)
    elnet_test_mse_gu_3569 = calculate_mse(model='elnet', fit=elnet_best_fit_gu, elnet_test_3569_gu)
    elnet_test_mse_gu_3571 = calculate_mse(model='elnet', fit=elnet_best_fit_gu, elnet_test_3571_gu)
    elnet_test_mse_gu_3579 = calculate_mse(model='elnet', fit=elnet_best_fit_gu, elnet_test_3579_gu)
    elnet_test_mse_gu_3582 = calculate_mse(model='elnet', fit=elnet_best_fit_gu, elnet_test_3582_gu)
    elnet_test_mse_gu_3860 = calculate_mse(model='elnet', fit=elnet_best_fit_gu, elnet_test_3860_gu)
    elnet_test_mse_gu_500 = calculate_mse(model='elnet', fit=elnet_best_fit_gu, elnet_test_500_gu)
    elnet_test_mse_gu_590 = calculate_mse(model='elnet', fit=elnet_best_fit_gu, elnet_test_590_gu)
    elnet_test_mse_gu_591 = calculate_mse(model='elnet', fit=elnet_best_fit_gu, elnet_test_591_gu)
    elnet_test_mse_gu_592 = calculate_mse(model='elnet', fit=elnet_best_fit_gu, elnet_test_592_gu)
    
    # Calculate the test mse for dong
    elnet_test_mse_dong_3508 = calculate_mse(model='elnet', fit=elnet_best_fit_dong, elnet_test_3508_dong)
    elnet_test_mse_dong_3523 = calculate_mse(model='elnet', fit=elnet_best_fit_dong, elnet_test_3523_dong)
    elnet_test_mse_dong_3569 = calculate_mse(model='elnet', fit=elnet_best_fit_dong, elnet_test_3569_dong)
    elnet_test_mse_dong_3571 = calculate_mse(model='elnet', fit=elnet_best_fit_dong, elnet_test_3571_dong)
    elnet_test_mse_dong_3579 = calculate_mse(model='elnet', fit=elnet_best_fit_dong, elnet_test_3579_dong)
    elnet_test_mse_dong_3582 = calculate_mse(model='elnet', fit=elnet_best_fit_dong, elnet_test_3582_dong)
    elnet_test_mse_dong_3860 = calculate_mse(model='elnet', fit=elnet_best_fit_dong, elnet_test_3860_dong)
    elnet_test_mse_dong_500 = calculate_mse(model='elnet', fit=elnet_best_fit_dong, elnet_test_500_dong)
    elnet_test_mse_dong_590 = calculate_mse(model='elnet', fit=elnet_best_fit_dong, elnet_test_590_dong)
    elnet_test_mse_dong_591 = calculate_mse(model='elnet', fit=elnet_best_fit_dong, elnet_test_591_dong)
    elnet_test_mse_dong_592 = calculate_mse(model='elnet', fit=elnet_best_fit_dong, elnet_test_592_dong)
    
    # combine the test mse result for gu
    elnet_test_result_gu = rbind(elnet_test_result_gu, c(elnet_best_com_gu$alpha, elnet_best_com_gu$lambda, elnet_test_mse_gu_3508))
    elnet_test_result_gu = rbind(elnet_test_result_gu, c(elnet_best_com_gu$alpha, elnet_best_com_gu$lambda, elnet_test_mse_gu_3523))
    elnet_test_result_gu = rbind(elnet_test_result_gu, c(elnet_best_com_gu$alpha, elnet_best_com_gu$lambda, elnet_test_mse_gu_3569))
    elnet_test_result_gu = rbind(elnet_test_result_gu, c(elnet_best_com_gu$alpha, elnet_best_com_gu$lambda, elnet_test_mse_gu_3571))
    elnet_test_result_gu = rbind(elnet_test_result_gu, c(elnet_best_com_gu$alpha, elnet_best_com_gu$lambda, elnet_test_mse_gu_3579))
    elnet_test_result_gu = rbind(elnet_test_result_gu, c(elnet_best_com_gu$alpha, elnet_best_com_gu$lambda, elnet_test_mse_gu_3582))
    elnet_test_result_gu = rbind(elnet_test_result_gu, c(elnet_best_com_gu$alpha, elnet_best_com_gu$lambda, elnet_test_mse_gu_3860))
    elnet_test_result_gu = rbind(elnet_test_result_gu, c(elnet_best_com_gu$alpha, elnet_best_com_gu$lambda, elnet_test_mse_gu_500))
    elnet_test_result_gu = rbind(elnet_test_result_gu, c(elnet_best_com_gu$alpha, elnet_best_com_gu$lambda, elnet_test_mse_gu_590))
    elnet_test_result_gu = rbind(elnet_test_result_gu, c(elnet_best_com_gu$alpha, elnet_best_com_gu$lambda, elnet_test_mse_gu_591))
    elnet_test_result_gu = rbind(elnet_test_result_gu, c(elnet_best_com_gu$alpha, elnet_best_com_gu$lambda, elnet_test_mse_gu_592))
    
    # combine the test mse result for dong
    elnet_test_result_dong = rbind(elnet_test_result_dong, c(elnet_best_com_dong$alpha, elnet_best_com_dong$lambda, elnet_test_mse_dong_3508))
    elnet_test_result_dong = rbind(elnet_test_result_dong, c(elnet_best_com_dong$alpha, elnet_best_com_dong$lambda, elnet_test_mse_dong_3523))
    elnet_test_result_dong = rbind(elnet_test_result_dong, c(elnet_best_com_dong$alpha, elnet_best_com_dong$lambda, elnet_test_mse_dong_3569))
    elnet_test_result_dong = rbind(elnet_test_result_dong, c(elnet_best_com_dong$alpha, elnet_best_com_dong$lambda, elnet_test_mse_dong_3571))
    elnet_test_result_dong = rbind(elnet_test_result_dong, c(elnet_best_com_dong$alpha, elnet_best_com_dong$lambda, elnet_test_mse_dong_3579))
    elnet_test_result_dong = rbind(elnet_test_result_dong, c(elnet_best_com_dong$alpha, elnet_best_com_dong$lambda, elnet_test_mse_dong_3582))
    elnet_test_result_dong = rbind(elnet_test_result_dong, c(elnet_best_com_dong$alpha, elnet_best_com_dong$lambda, elnet_test_mse_dong_3860))
    elnet_test_result_dong = rbind(elnet_test_result_dong, c(elnet_best_com_dong$alpha, elnet_best_com_dong$lambda, elnet_test_mse_dong_500))
    elnet_test_result_dong = rbind(elnet_test_result_dong, c(elnet_best_com_dong$alpha, elnet_best_com_dong$lambda, elnet_test_mse_dong_590))
    elnet_test_result_dong = rbind(elnet_test_result_dong, c(elnet_best_com_dong$alpha, elnet_best_com_dong$lambda, elnet_test_mse_dong_591))
    elnet_test_result_dong = rbind(elnet_test_result_dong, c(elnet_best_com_dong$alpha, elnet_best_com_dong$lambda, elnet_test_mse_dong_592))
    
    ########################## xgboost ##########################
    print(paste("#", iter, "cv train/test", "xgboost"))
    # Fitting
    xg_best_fit_gu = xgboost(data=as.matrix(xg_train_val_gu[, -1]), label=xg_train_val_gu[, 1], verbose=0,
                             nrounds=xg_best_com_gu$nrounds, 
                             max_depth=xg_best_com_gu$max_depth, 
                             eta=xg_best_com_gu$eta)
    xg_best_fit_dong = xgboost(data=as.matrix(xg_train_val_dong[, -1]), label=xg_train_val_dong[, 1], verbose=0,
                               nrounds=xg_best_com_dong$nrounds, 
                               max_depth=xg_best_com_dong$max_depth, 
                               eta=xg_best_com_dong$eta)
    
    # calculate mse with the best model
    xg_best_mse_gu = calculate_mse(model='xg', fit=xg_best_fit_gu, xg_train_val_gu)
    xg_best_mse_dong = calculate_mse(model='xg', fit=xg_best_fit_dong, xg_train_val_dong)
    
    # combine best mse result
    xg_best_result = rbind(xg_best_result, c(xg_best_com_gu$nrounds, xg_best_com_gu$max_depth, 
                                             xg_best_com_gu$eta, xg_best_mse_gu))
    xg_best_result = rbind(xg_best_result, c(xg_best_com_dong$nrounds, xg_best_com_dong$max_depth, 
                                             xg_best_com_dong$eta, xg_best_mse_dong))
    
    # Calculate the test mse for gu
    xg_test_mse_gu_3508 = calculate_mse(model='xg', fit=xg_best_fit_gu, xg_test_3508_gu)
    xg_test_mse_gu_3523 = calculate_mse(model='xg', fit=xg_best_fit_gu, xg_test_3523_gu)
    xg_test_mse_gu_3569 = calculate_mse(model='xg', fit=xg_best_fit_gu, xg_test_3569_gu)
    xg_test_mse_gu_3571 = calculate_mse(model='xg', fit=xg_best_fit_gu, xg_test_3571_gu)
    xg_test_mse_gu_3579 = calculate_mse(model='xg', fit=xg_best_fit_gu, xg_test_3579_gu)
    xg_test_mse_gu_3582 = calculate_mse(model='xg', fit=xg_best_fit_gu, xg_test_3582_gu)
    xg_test_mse_gu_3860 = calculate_mse(model='xg', fit=xg_best_fit_gu, xg_test_3860_gu)
    xg_test_mse_gu_500 = calculate_mse(model='xg', fit=xg_best_fit_gu, xg_test_500_gu)
    xg_test_mse_gu_590 = calculate_mse(model='xg', fit=xg_best_fit_gu, xg_test_590_gu)
    xg_test_mse_gu_591 = calculate_mse(model='xg', fit=xg_best_fit_gu, xg_test_591_gu)
    xg_test_mse_gu_592 = calculate_mse(model='xg', fit=xg_best_fit_gu, xg_test_592_gu)
    
    # Calculate the test mse for dong
    xg_test_mse_dong_3508 = calculate_mse(model='xg', fit=xg_best_fit_dong, xg_test_3508_dong)
    xg_test_mse_dong_3523 = calculate_mse(model='xg', fit=xg_best_fit_dong, xg_test_3523_dong)
    xg_test_mse_dong_3569 = calculate_mse(model='xg', fit=xg_best_fit_dong, xg_test_3569_dong)
    xg_test_mse_dong_3571 = calculate_mse(model='xg', fit=xg_best_fit_dong, xg_test_3571_dong)
    xg_test_mse_dong_3579 = calculate_mse(model='xg', fit=xg_best_fit_dong, xg_test_3579_dong)
    xg_test_mse_dong_3582 = calculate_mse(model='xg', fit=xg_best_fit_dong, xg_test_3582_dong)
    xg_test_mse_dong_3860 = calculate_mse(model='xg', fit=xg_best_fit_dong, xg_test_3860_dong)
    xg_test_mse_dong_500 = calculate_mse(model='xg', fit=xg_best_fit_dong, xg_test_500_dong)
    xg_test_mse_dong_590 = calculate_mse(model='xg', fit=xg_best_fit_dong, xg_test_590_dong)
    xg_test_mse_dong_591 = calculate_mse(model='xg', fit=xg_best_fit_dong, xg_test_591_dong)
    xg_test_mse_dong_592 = calculate_mse(model='xg', fit=xg_best_fit_dong, xg_test_592_dong)
    
    # combine the test mse result for gu
    xg_test_result_gu = rbind(xg_test_result_gu, c(xg_best_com_gu$nrounds, xg_best_com_gu$max_depth, 
                                                   xg_best_com_gu$eta, xg_test_mse_gu_3508))
    xg_test_result_gu = rbind(xg_test_result_gu, c(xg_best_com_gu$nrounds, xg_best_com_gu$max_depth, 
                                                   xg_best_com_gu$eta, xg_test_mse_gu_3523))
    xg_test_result_gu = rbind(xg_test_result_gu, c(xg_best_com_gu$nrounds, xg_best_com_gu$max_depth, 
                                                   xg_best_com_gu$eta, xg_test_mse_gu_3569))
    xg_test_result_gu = rbind(xg_test_result_gu, c(xg_best_com_gu$nrounds, xg_best_com_gu$max_depth, 
                                                   xg_best_com_gu$eta, xg_test_mse_gu_3571))
    xg_test_result_gu = rbind(xg_test_result_gu, c(xg_best_com_gu$nrounds, xg_best_com_gu$max_depth, 
                                                   xg_best_com_gu$eta, xg_test_mse_gu_3579))
    xg_test_result_gu = rbind(xg_test_result_gu, c(xg_best_com_gu$nrounds, xg_best_com_gu$max_depth, 
                                                   xg_best_com_gu$eta, xg_test_mse_gu_3582))
    xg_test_result_gu = rbind(xg_test_result_gu, c(xg_best_com_gu$nrounds, xg_best_com_gu$max_depth, 
                                                   xg_best_com_gu$eta, xg_test_mse_gu_3860))
    xg_test_result_gu = rbind(xg_test_result_gu, c(xg_best_com_gu$nrounds, xg_best_com_gu$max_depth, 
                                                   xg_best_com_gu$eta, xg_test_mse_gu_500))
    xg_test_result_gu = rbind(xg_test_result_gu, c(xg_best_com_gu$nrounds, xg_best_com_gu$max_depth, 
                                                   xg_best_com_gu$eta, xg_test_mse_gu_590))
    xg_test_result_gu = rbind(xg_test_result_gu, c(xg_best_com_gu$nrounds, xg_best_com_gu$max_depth, 
                                                   xg_best_com_gu$eta, xg_test_mse_gu_591))
    xg_test_result_gu = rbind(xg_test_result_gu, c(xg_best_com_gu$nrounds, xg_best_com_gu$max_depth, 
                                                   xg_best_com_gu$eta, xg_test_mse_gu_592))
    
    # combine the test mse result for dong
    xg_test_result_dong = rbind(xg_test_result_dong, c(xg_best_com_dong$nrounds, xg_best_com_dong$max_depth, 
                                                       xg_best_com_dong$eta, xg_test_mse_dong_3508))
    xg_test_result_dong = rbind(xg_test_result_dong, c(xg_best_com_dong$nrounds, xg_best_com_dong$max_depth, 
                                                       xg_best_com_dong$eta, xg_test_mse_dong_3523))
    xg_test_result_dong = rbind(xg_test_result_dong, c(xg_best_com_dong$nrounds, xg_best_com_dong$max_depth, 
                                                       xg_best_com_dong$eta, xg_test_mse_dong_3569))
    xg_test_result_dong = rbind(xg_test_result_dong, c(xg_best_com_dong$nrounds, xg_best_com_dong$max_depth, 
                                                       xg_best_com_dong$eta, xg_test_mse_dong_3571))
    xg_test_result_dong = rbind(xg_test_result_dong, c(xg_best_com_dong$nrounds, xg_best_com_dong$max_depth, 
                                                       xg_best_com_dong$eta, xg_test_mse_dong_3579))
    xg_test_result_dong = rbind(xg_test_result_dong, c(xg_best_com_dong$nrounds, xg_best_com_dong$max_depth, 
                                                       xg_best_com_dong$eta, xg_test_mse_dong_3582))
    xg_test_result_dong = rbind(xg_test_result_dong, c(xg_best_com_dong$nrounds, xg_best_com_dong$max_depth, 
                                                       xg_best_com_dong$eta, xg_test_mse_dong_3860))
    xg_test_result_dong = rbind(xg_test_result_dong, c(xg_best_com_dong$nrounds, xg_best_com_dong$max_depth, 
                                                       xg_best_com_dong$eta, xg_test_mse_dong_500))
    xg_test_result_dong = rbind(xg_test_result_dong, c(xg_best_com_dong$nrounds, xg_best_com_dong$max_depth, 
                                                       xg_best_com_dong$eta, xg_test_mse_dong_590))
    xg_test_result_dong = rbind(xg_test_result_dong, c(xg_best_com_dong$nrounds, xg_best_com_dong$max_depth, 
                                                       xg_best_com_dong$eta, xg_test_mse_dong_591))
    xg_test_result_dong = rbind(xg_test_result_dong, c(xg_best_com_dong$nrounds, xg_best_com_dong$max_depth, 
                                                       xg_best_com_dong$eta, xg_test_mse_dong_592))
    
}


# --------------------------------------------------- #
#
# 4. Save results
#
# --------------------------------------------------- #

# convert matrix to data frame 
rf_train_result = as.data.frame(rf_train_result)
rf_val_result = as.data.frame(rf_val_result)
rf_best_result = as.data.frame(rf_best_result)
rf_test_result_gu = as.data.frame(rf_test_result_gu)
rf_test_result_dong = as.data.frame(rf_test_result_dong)

elnet_train_result = as.data.frame(elnet_train_result)
elnet_val_result = as.data.frame(elnet_val_result)
elnet_best_result = as.data.frame(elnet_best_result)
elnet_test_result_gu = as.data.frame(elnet_test_result_gu)
elnet_test_result_dong = as.data.frame(elnet_test_result_dong)

xg_train_result = as.data.frame(xg_train_result)
xg_val_result = as.data.frame(xg_val_result)
xg_best_result = as.data.frame(xg_best_result)
xg_test_result_gu = as.data.frame(xg_test_result_gu)
xg_test_result_dong = as.data.frame(xg_test_result_dong)

# naming columns
rf_colnames = c('ntree', 'mtry', 'mse')
elnet_colnames = c('alpha', 'lambda', 'mse')
xg_colnames = c('nrounds', 'max_depth', 'eta', 'mse')

colnames(rf_best_result) = rf_colnames
colnames(rf_test_result_gu) = rf_colnames
colnames(rf_test_result_dong) = rf_colnames

colnames(elnet_best_result) = elnet_colnames
colnames(elnet_test_result_gu) = elnet_colnames
colnames(elnet_test_result_dong) = elnet_colnames

colnames(xg_best_result) = xg_colnames
colnames(xg_test_result_gu) = xg_colnames
colnames(xg_test_result_dong) = xg_colnames

# make station list for separating each station
test_station_list = c(3508, 3523, 3569, 3571, 3579, 3582,
                      3860, 500, 590, 591, 592)

rf_test_result_gu$station = rep(test_station_list, n_cv_tt)
elnet_test_result_gu$station = rep(test_station_list, n_cv_tt)
xg_test_result_gu$station = rep(test_station_list, n_cv_tt)

rf_test_result_dong$station = rep(test_station_list, n_cv_tt)
elnet_test_result_dong$station = rep(test_station_list, n_cv_tt)
xg_test_result_dong$station = rep(test_station_list, n_cv_tt)

# make station list for separating each station
station_list = c('gu', 'dong')

rf_best_result$station = rep(station_list, n_cv_tt)
elnet_best_result$station = rep(station_list, n_cv_tt)
xg_best_result$station = rep(station_list, n_cv_tt)

rf_train_result$station = rep(station_list, each=nrow(rf_tv_mse_gu$train_mse_mat))
rf_val_result$station = rep(station_list, each=nrow(rf_tv_mse_gu$val_mse_mat))

elnet_train_result$station = rep(station_list, each=nrow(elnet_tv_mse_gu$train_mse_mat))
elnet_val_result$station = rep(station_list, each=nrow(elnet_tv_mse_gu$val_mse_mat))

xg_train_result$station = rep(station_list, each=nrow(xg_tv_mse_gu$train_mse_mat))
xg_val_result$station = rep(station_list, each=nrow(xg_tv_mse_gu$val_mse_mat))

# Integrate the test results
final_result = cbind(rf_test_result_gu$mse, rf_test_result_dong$mse, 
                     elnet_test_result_gu$mse, elnet_test_result_dong$mse, 
                     xg_test_result_gu$mse, xg_test_result_dong$mse, 
                     xg_test_result_gu$station)

colnames(final_result) = c("RF_GU", "RF_DONG",
                           "ELNET_GU", "ELNET_DONG", 
                           "XG_GU", "XG_DONG", "station")

write.csv(final_result, paste0("./result/", version, "/", y_name, "/test/", y_name, "_final_result.csv"), row.names=F)

# save results
# write.csv(rf_train_result, "./result/변수추가o/rent/test/rent_rf_train_result.csv", row.names=F)
# write.csv(rf_val_result, "./result/변수추가o/rent/test/rent_rf_val_result.csv", row.names=F)
# write.csv(rf_best_result, "./result/변수추가o/rent/test/rent_rf_best_result.csv", row.names=F)
# write.csv(rf_test_result_ind, "./result/변수추가o/rent/test/rent_rf_test_result_ind.csv", row.names=F)
# write.csv(rf_test_result_gu, "./result/변수추가o/rent/test/rent_rf_test_result_gu.csv", row.names=F)
# write.csv(rf_test_result_dong, "./result/변수추가o/rent/test/rent_rf_test_result_dong.csv", row.names=F)
# 
# write.csv(elnet_train_result, "./result/변수추가o/rent/test/rent_elnet_train_result.csv", row.names=F)
# write.csv(elnet_val_result, "./result/변수추가o/rent/test/rent_elnet_val_result.csv", row.names=F)
# write.csv(elnet_best_result, "./result/변수추가o/rent/test/rent_elnet_best_result.csv", row.names=F)
# write.csv(elnet_test_result_ind, "./result/변수추가o/rent/test/rent_elnet_test_result_ind.csv", row.names=F)
# write.csv(elnet_test_result_gu, "./result/변수추가o/rent/test/rent_elnet_test_result_gu.csv", row.names=F)
# write.csv(elnet_test_result_dong, "./result/변수추가o/rent/test/rent_elnet_test_result_dong.csv", row.names=F)
# 
# write.csv(xg_train_result, "./result/변수추가o/rent/test/rent_xg_train_result.csv", row.names=F)
# write.csv(xg_val_result, "./result/변수추가o/rent/test/rent_xg_val_result.csv", row.names=F)
# write.csv(xg_best_result, "./result/변수추가o/rent/test/rent_xg_best_result.csv", row.names=F)
# write.csv(xg_test_result_ind, "./result/변수추가o/rent/test/rent_xg_test_result_ind.csv", row.names=F)
# write.csv(xg_test_result_gu, "./result/변수추가o/rent/test/rent_xg_test_result_gu.csv", row.names=F)
# write.csv(xg_test_result_dong, "./result/변수추가o/rent/test/rent_xg_test_result_dong.csv", row.names=F)

end_time = Sys.time()
running_time = end_time - start_time
print(running_time)
