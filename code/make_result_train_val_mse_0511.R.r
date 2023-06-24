calculate_mse = function(model, fit, data){
    if (model == "rf"){
        pred = predict(fit, newdata=data)
        real = data[, 1]
        mse = mean((real-pred)^2)
        return(mse) 
    }
    
    if (model == "elnet"){
        pred = predict(fit, newx=as.matrix(data[, -1]))
        real = data[, 1]
        mse = mean((real-pred)^2)    
        return(mse)    
    }
    
    
    if (model == "xg"){
        pred = predict(fit, as.matrix(data[, -1]))
        real = data[, 1]
        mse = mean((real-pred)^2)
        return(mse)
    }
}

make_result_train_val_mse = function(model, params, train, val){
    # Save the train mse and validation mse.
    # 
    # Args:
    #   model: model name
    #   params: list of parameters
    #   train: train set of data
    #   val: validation set of data
    #
    # Returns:
    # train mse and validation mse corresponding to each combination of parameters
    
    # --------------------------------------------------------------- #
    n = nrow(params)  # number of combination of parameters
    train_mse_mat = data.frame(matrix(NA, nrow=n, ncol=length(params)+1))
    val_mse_mat = train_mse_mat
    colnames(train_mse_mat) = c(names(params), 'train_mse')
    colnames(val_mse_mat) = c(names(params), 'val_mse')
    
    # -------------- random forest -------------- # 
    if (model=="rf"){
        library(randomForest)
        
        for (i in 1:n){
            ntree = params[i, 'ntree']
            mtry = params[i, 'mtry']
            
            # print(paste0('[ntree:', ntree, ', mtry:', mtry, ']'))
            # make model each combination of parameters
            fit = randomForest(train$y ~., data=train, ntree=ntree, mtry=mtry)
            train_mse = calculate_mse(model="rf", fit=fit, data=train)
            val_mse = calculate_mse(model="rf", fit=fit, data=val)
            
            train_mse_mat[i, ] = c(ntree, mtry, train_mse)
            val_mse_mat[i, ] = c(ntree, mtry, val_mse)
            
            }
        }
    
    # -------------- elastic net -------------- # 
    if (model=='elnet'){
        library(glmnet)
        
        for (i in 1:n){
            alpha = params[i, 'alpha']
            lambda = params[i, 'lambda']
            
            # print(paste0('[alpha:', alpha, ', lambda:', lambda, ']'))
            # make model each combination of parameters
            fit = glmnet(x=train[, -1], y=train[, 1], alpha=alpha, lambda=lambda)
            train_mse = calculate_mse(model="elnet", fit=fit, data=train)
            val_mse = calculate_mse(model="elnet", fit=fit, data=val)
            
            train_mse_mat[i, ] = c(alpha, lambda, train_mse)
            val_mse_mat[i, ] = c(alpha, lambda, val_mse)
            
        }
    }
    
    # -------------- xgboost -------------- # 
    if (model=='xg'){
        library(xgboost)
        
        for (i in 1:n){
            nrounds = params[i, 'nrounds']
            max_depth = params[i, 'max_depth']
            eta = params[i, 'eta']
            
            # print(paste0('[nrounds:', nrounds, ', ', 
            #              'max_depth:', max_depth, ', ', 
            #              'eta:', eta, ']'))
            
            # make model each combination of parameters
            fit = xgboost(data=as.matrix(train[, -1]), label=train[, 1], verbose=0,
                          nrounds=nrounds, 
                          max_depth=max_depth, 
                          eta=eta)
            
            train_mse = calculate_mse(model="xg", fit=fit, data=train)
            val_mse = calculate_mse(model="xg", fit=fit, data=val)
            
            train_mse_mat[i, ] = c(nrounds, max_depth, eta, train_mse)
            val_mse_mat[i, ] = c(nrounds, max_depth, eta, val_mse)
            
        }
    }
            
    # Save the index corresponding to each combination of parameters for comparison
    train_mse_mat$param_idx = 1:n
    val_mse_mat$param_idx = 1:n
    
    all_mse_mat = list(train_mse_mat, val_mse_mat)
    names(all_mse_mat) = c('train_mse_mat', 'val_mse_mat')
    
    return(all_mse_mat)
}