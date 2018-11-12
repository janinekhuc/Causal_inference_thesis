# methods & evaluation functions
# split data into train and test set
# run regressions
# run propensity scores
# inverse propensity weigthings
# BART
# random forest
# synthetic forest - not sure how to access predictions
# extract relevant information/ PEHE & rpehe

# split data-------------------------------------------
# creates a spliited data set into test and train
# possible extensions
# TODO: rethink the hidden confounder part & include w instead of Xt
# TODO: rethink what kind of dataframes required to evaluate the different methods

split_data <- function(t= t, X= X, Y_obs= outcomes$Y[,'Y_obs'], hidden = FALSE, Xt= NULL, y_true= outcomes$y_true){
  # t- vector of observed treatments
  # X- matrix of covariates
  # Y_obs- observed outcomes
  # hidden- default is FALSE, no hidden confounders/ if changed to TRUE randomly selects a number of  covariates that impacted tretament assignment
  # simulating that ony a few were measured
  # Xt- matrix of covariates that impacted tretament assignments/ only required if hidden = TRUE
  
  # check which covariates should be used
  if(hidden == FALSE){
    X_obs= X
  }else{
    X_hidden= sample(Xt, 80, replace= FALSE) # remove one confounder to be hidden
    X_conf_obs= Xt # observed confounders
    indx= which(Xt %in% X_hidden)
    X_conf_obs = X[,-indx]
    X_obs = X[,-indx]
  }
  
  # merge data with observed treatments and outcomes
  df= cbind(X, t) # merge with t
  colnames(df)[length(colnames(df))] <- 't'
  df2 = cbind(df, Y_obs, y_true) # merge with observed outcome
  colnames(df2)[length(colnames(df2))-2] <- 'Y'
  
  # create data_train, data_test
  train_ind <- sample(seq_len(nrow(df2)), size = nrow(df2)*(2/3))
  df_train <- data.frame(df2[train_ind, ])
  tmp_test <- data.frame(df2[-train_ind, ])
  
  # HIll's approach include each observation twice in order to get prediction for t1 and t0/ allows to get an outersample 
  t1<- tmp_test[tmp_test$t==1,]
  t0<- tmp_test[tmp_test$t==0,]
  
  # add counterfactual t for each observation
  t1_counter <- t1
  t1_counter[,'t']<- 0
  t0_counter<- t0
  t0_counter[,'t']<- 1
  n_test = nrow(tmp_test)
  df_test<- rbind(t1, t0_counter, t1_counter, t0)
  ytrue_test <- select(df_test, contains('true'))
  ytrue_test['delta'] <- (ytrue_test$y1_true - ytrue_test$y0_true)[1:n_test] 
  ytrue_train <- select(df_train, contains('true'))
  ytrue_train['delta'] <- ytrue_train$y1_true - ytrue_train$y0_true 
  yobs_test <- df_test %>% select(matches('y', ignore.case= TRUE))
  yobs_train <- df_train %>% select(matches('y', ignore.case= TRUE))
  df_train <- df_train %>% select(-contains('y', ignore.case= TRUE))
  df_test <- df_test %>% select(-contains('y', ignore.case= TRUE))
  list_of_objects <- list(df_train= df_train, df_test= df_test, yobs_train= yobs_train,
                          yobs_test=yobs_test, ytrue_test= ytrue_test, ytrue_train= ytrue_train,
                          n_test = n_test)
  return(list_of_objects)
}


create_test_is <- function(df_train = df_train){
  t1<- df_train[df_train$t==1,]
  t0<- df_train[df_train$t==0,]
  n_test3 <- nrow(df_train)
  # add counterfactual t for each observation
  t1_counter <- t1; t1_counter[,'t']<- 0
  t0_counter<- t0; t0_counter[,'t']<- 1
  
  df <- rbind(t1, t0_counter, t1_counter, t0)
  return(df)
}


# estimate ite vector counterfactual-------------------
estimate_ite_cf <- function(preds, y_train= y_train){
  est_ite<-rep(NA,nrow(df_test2))
  est_ite[which(df_train[,'t']==1)] <- y_train[which(df_train[,'t']==1)]-preds[which(df_train[,'t']==1)] # observed t=1 - counterfactual t=0 estimated for treated
  est_ite[which(df_train[,'t']==0)]<- preds[which(df_train[,'t']==0)]-y_train[which(df_train[,'t']==0)] # ounterfactual estimated- observed for untreated
  mean = mean(est_ite)
  return(list(mean= mean, est_ite=est_ite))
}




rf_ite <- function(model_pred, method= 'rf'){
  # model_pred should be be probabilities that were returned as a function of a random forest of any kind
  # that had training/ test data were each observation was entered twice with the only difference in t
  # and test data had the structure of t1, t0_counter, t1_counter and t0
  # returns ite estimates, 
  if(method == 'rf'){
    tmp_t1 = model_pred[1:(nrow(model_pred)/2),'X1'] # first cases from 1 to t == 1
    tmp_t0 = model_pred[((nrow(model_pred)/2)+1):nrow(model_pred),'X1'] # from counter t1 up until t0
    ite = tmp_t1 - tmp_t0
  }else{
    tmp_t1= model_pred[1:(nrow(model_pred)/2),'1'] # first cases from 1 to t == 1
    tmp_t0=model_pred[((nrow(model_pred)/2)+1):nrow(model_pred),'1'] # from counter t1 up until t0
    ite = tmp_t1- tmp_t0 
  }
  return(ite)
} 



rpehe_fct <- function(y_est, y_true){ # sqrt of pehe
  sqrt(mean((y_est-y_true)^2))
}

pehe_fct <- function(y_est, y_true){
  mean((y_est-y_true)^2)
}

ate_bias_fct <- function(ate_est, ate_true = true_values$ate){
  (ate_est - ate_true)^2
}




create_results_tbl <- function(iter = iter){
  # creates the empty dataframes that are to be filled with the results
  # 
  # **input**
  # iter- number of terations
  # 
  # **output**
  # returns 8 dataframes
  # - result_ate: true ates
  # - result_ite: true ites
  # - rpehe_bias: 
  # - pehe_bias: error root of pehe
  # - ate_bias:
  # - pehe_model: 
  # - rpehe_model: error root of pehe for model performance (trained with perfect overlap)
  # - ate_bias_model: erorr ate absolute difference
  
  result_ate = matrix(0,iter,11)
  dimnames(result_ate)=list(NULL,c('true_ate', 'reg_te', 'ipw_te', 'rf_te_is', 'rf_te_os', 'bartp_te_is', 'bartp_te_os', 'bartl_te_is', 'bartl_te_os', 'srf_te_is', 'srf_te_os'))
  
  result_ite = matrix(0,iter,9)
  dimnames(result_ite)=list(NULL,c('true_ite', 'rf_te_is', 'rf_te_os', 'bartp_te_is', 'bartp_te_os', 'bartl_te_is', 'bartl_te_os', 'srf_te_is', 'srf_te_os'))
  
  rpehe_bias = matrix(0,iter,10)
  dimnames(rpehe_bias)=list(NULL,c('reg_adj.',  'ipw_te', 'rf_is', 'rf_os', 'bartp_is','bartp_os', 'bartl_is','bartl_os', 'srf_is', 'srf_os'))
  
  pehe_bias = matrix(0,iter,8)
  dimnames(pehe_bias)=list(NULL,c('rf_is', 'rf_os', 'bartp_is','bartp_os', 'bartl_is','bartl_os', 'srf_is', 'srf_os'))
  
  ate_bias = matrix(0,iter,10)
  dimnames(ate_bias)=list(NULL,c('reg_adj.',  'ipw_te', 'rf_is', 'rf_os', 'bartp_is','bartp_os', 'bartl_is','bartl_os', 'srf_is', 'srf_os'))
  
  pehe_model = matrix(0,iter,4)
  dimnames(pehe_model)=list(NULL,c('rf_pehe', 'bartl_pehe', 'bartp_pehe', 'srf_pehe'))
  
  rpehe_model = matrix(0,iter,4)
  dimnames(rpehe_model)=list(NULL,c('rf_rpehe', 'bartl_rpehe', 'bartp_rpehe', 'srf_rpehe'))
  
  ate_bias_model = matrix(0,iter,6)
  dimnames(ate_bias_model)=list(NULL,c('reg_adj.', 'ipw', 'rf_ate_bias', 'bartl_ate_bias', 'bartp_ate_bias', 'srf_ate_bia'))
  
  assign('result_ate',result_ate, pos=1)
  assign('result_ite',result_ite, pos=1)
  assign('rpehe_bias', rpehe_bias, pos= 1)
  assign('pehe_bias',pehe_bias, pos=1)
  assign('ate_bias',ate_bias, pos=1)
  assign('pehe_model', rpehe_model, pos= 1)
  assign('rpehe_model', rpehe_model, pos=1)
  assign('ate_bias_model', ate_bias_model, pos= 1)
}


create_train_data <- function(data = data, outcomes = outcomes, model_train = is_test){
  # creates the dataframes returned to the environment required to train the models
  # 
  # **input**
  # data- includes the datasets from split data
  # outcomes- includes the outcomes- y
  # model_train- corresponds to the insample test set
  # 
  # **output**
  # returns train sets and outcome y vectors
  # - y_train- vector of outcomes
  # - df_train- dataframe of features
  # - df_train_y - one df for outcome and features
  # - y_train_model- vector of model specific outcomes
  # - df_train_model- dataframe of features to train model specific
  
  
  #seperate outcome and variables
  y_train <- data$yobs_train[,'Y']
  df_train <- data$df_train
  
  # one df
  df_train_y = cbind.data.frame(df_train, y=y_train)
  
  # train model
  if(class(outcomes) == 'list'){
    y_train_model = c(outcomes$Y[,'y1_obs'], outcomes$Y[,'y0_obs']) 
  }else{
    y_train_model = c(Y[,'y1_obs'], Y[,'y0_obs'])
  }
  df_train_model = model_train 
  
  assign('y_train',y_train, pos=1)
  assign('df_train',df_train, pos=1)
  assign('df_train_y', df_train_y, pos= 1)
  assign('y_train_model',y_train_model, pos=1)
  assign('df_train_model', df_train_model, pos= 1)
  
}


create_test_data <- function(data= data, df_train = data$df_train){
  # creates the dataframes returned to the environment to test models
  # 
  # **input**
  # data- includes the datasets from split data
  # df_train- df used to train, required to create insample test set
  # 
  # **output**
  # returns test sets 
  # - is_test- insample test set
  # - os_test- outsample test set
  # - df_test_model- test set for model specific checks
  
  
  # data for outsample evaluation
  os_test = data$df_test
  
  # data for insample evaluation
  is_test <- create_test_is(df_train)
  
  # test set
  df_test_model = os_test
  
  assign('os_test',os_test, pos=1)
  assign('is_test',is_test, pos=1)
  assign('df_test_model', df_test_model, pos= 1)
}