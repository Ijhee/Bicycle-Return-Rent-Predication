library(glmnet)
library(xgboost)
library(randomForest)
library(dplyr)
library(leaps)

vs.fun = function(df, model, st_num, y_name){
  # This function is to select variables and return selected variables
  # Args:
  #
  # df : 변수 선택하고 싶은 데이터
  # model : 기준이 될 모델
  # st_num : 대여소 번호 (튜닝 파라미터 정하기 위해 필요)
  # y_name : return or rent
  #
  # return:
  # names(selected_variables)
  # 선택된 변수명 반환
  
  
  if (model == 'elnet'){
    lm_model = lm(df$y ~., data=df)
    step_model = step(object=lm_model, direction="both", trace=0)  # 기본 모델
    df_sel = df[, names(step_model$coefficients)[-1]]
    df_sel= cbind(df$y,df_sel)
    colnames(df_sel)[1]='y'
    
    return(colnames(df_sel))
  }
  
  if (model == 'rf'){
    param.df = read.csv(paste0("./result/변수추가o/", y_name ,"/test/",
                              y_name, '_', model, '_best_result.csv'))
    ntree = subset(param.df, param.df$station==st_num)$ntree
    mtry = subset(param.df, param.df$station==st_num)$mtry
    rf_model = randomForest(df$y ~., data=df, ntree=ntree, mtry=mtry)
    importance = rf_model$importance
    var_importance = importance[, "IncNodePurity"]
    
    # 변수 중요도의 합 계산 및 정규화
    total_importance = sum(var_importance)
    normalized_importance = var_importance / total_importance
    
    # 변수 중요도를 백분율로 변환
    percentage_importance = normalized_importance * 100
    
    importance = data.frame(percentage_importance)
    importance$var = row.names(importance)
    final_var = importance[importance$percentage_importance>=0.1,]$var
    df_sel = df[,final_var]
    df_sel= cbind(df$y,df_sel)
    colnames(df_sel)[1]='y'
    
    return(colnames(df_sel))
  }
  
  if (model == 'xg'){
    param.df = read.csv(paste0("./result/변수추가o/", y_name, "/test/",
                              y_name, '_', model, '_best_result.csv'))
    
    nrounds =  subset(param.df,param.df$station==st_num)$nrounds
    max_depth =  subset(param.df,param.df$station==st_num)$max_depth
    eta =  subset(param.df,param.df$station==st_num)$eta
    
    xgb = xgboost(data=as.matrix(df[, -which(names(df) == "y")]), label=df$y, verbose=0, 
                  nrounds=nrounds, max_depth=max_depth, eta=eta)
    
    xgb.imp = data.frame(xgb.importance(model=xgb))
    final_var = xgb.imp[xgb.imp$Gain>=0.001, ]$Feature
    df_sel = df[, final_var]
    df_sel= cbind(df$y,df_sel)
    colnames(df_sel)[1]='y'
    
    return(colnames(df_sel))
  }
  
}
