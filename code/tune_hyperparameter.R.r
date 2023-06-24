source("./code/make_result_train_val_mse_0511.R")

tune_hyperparameter = function(model, params, data, n_cv_tt, n_cv_tv){
    # Results in train / validation loop 
    train_result = val_result = NULL    
    # for data split
    n = nrow(data)
    idx = 1:n
    
    for (iter in 1:n_cv_tt){
        print(paste("#", iter, "cv train/test"))
        # split data --> train / test with 80:20
        train_val_idx = sample(idx, n*0.8)
        test_idx = setdiff(idx, train_val_idx)
        test = data[test_idx, ]
        
        for (iiter in 1:n_cv_tv){
            print(paste("#", iiter, "cv train/val"))
            # split train --> train / validation with 40:40
            train_idx = sample(train_val_idx, size=n*0.4)
            val_idx = setdiff(train_val_idx, train_idx)
            train = data[train_idx, ]
            val = data[val_idx, ]
            
            # result of mse
            tmp = make_result_train_val_mse(model, params, train, val)
            train_result = rbind(train_result, tmp$train_mse_mat)
            val_result = rbind(val_result, tmp$val_mse_mat)
            
        }
        
    }
    
    all_mse = list(train_result, val_result)
    names(all_mse) = c('train_mse', 'val_mse')
    
    return(all_mse)
}