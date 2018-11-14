# functions
source('create_data.R')
source('helper.R')
source('evaluation_functions.R')
source('HTE_estimation.R')

data_path = "../Thesis/Code/Janine_Thesis/"

n = 100000
iter = 5
ws_name = paste0('uti_', iter, '_', n)

# values hidden confunders
hidden_status = FALSE # can be either true or false
prev_hidden_conf = 0.2 # can take on any value between 0 and 1


# prestimulation ------------------
# create tables to save results
create_results_tbl(iter = iter)


# simulation ---------------------------
for(i in 1:iter){
  set.seed(1 + i)
  
  #  sample features X
  X = sample_data("170831_dfmodel.csv", n, hidden_status, prev_hidden_conf)
  
  # create treatment assignment
  z = create_z(hidden_status)
  
  # create outcome
  # should return list: y0_true, y1_true, y_obs, tau_true
  
  
  # dataframe
  df = cbind.data.frame(X, t)
  
  
  # create outcomes----------------------------------
  y0_true <- plogis(.85*df[,1]+0.05*df[,1]^2+2)
  y1_true <- plogis((1/(1+exp(-((df[,1]*3)-1))))+(0.25*df[,2]))
  
  Y_obs <- rep(NA, n)
  prob_errror1 <- plogis(y1_true[t==1]+ rnorm(length(y1_true[t==1]), 0, 1))
  prob_error0 <- plogis(y1_true[t==0]+ rnorm(length(y1_true[t==0]), 0, 1))
  Y_obs[t==1] <- rbinom(n = length(which((t==1) == TRUE)), size= 1, prob = prob_errror1)
  Y_obs[t==0] <- rbinom(n = length(which((t==0) == TRUE)), 1, prob = prob_error0)
  
  # outcomes for two y_trues
  y0_obs = rbinom(n, size =1, y0_true)
  y1_obs = rbinom(n, size =1, y1_true)
  
  # #assemble data as a bivariate missing data problem
  y0 <- y1 <- rep(NA, n) # empty vectors
  y0[t == 0] <- Y_obs[t == 0] # observed
  y1[t == 1] <- Y_obs[t == 1] # observed
  
  Y <- data.frame(Y_obs, y0_obs, y1_obs, y0, y1) #is binary outcomes
  y_true <- data.frame(y0_true, y1_true) # is probabilities
  
  # save true estimands/ ITE and ate
  true_values <- save_true_estimands(y_true) # returns true_ite & true_ate
  
  # dataset ----------------------------------------
  # split train and test set (for outsample PEHE- each observation included twice)
  data <- split_data(t, X, Y[,'Y_obs'], hidden = FALSE,Xt, y_true) # return list of df_train, df_test, "yobs_train", "yobs_test", "ytrue_test", "ytrue_train"
  
  # create test sets
  create_test_data(data= data, df_train = data$df_train)
  n_is <- nrow(is_test)/2 #number of individuals insample
  
  # create datasets to train
  create_train_data(data = data, outcomes = Y, model_train = is_test)
  
  
  #add true ate------------------------
  result_ate[i,'true_ate'] = true_values$ate
  result_ite[i,'true_ite'] = mean(true_values$ite)
  
  # ------------------------------------------------------------------------
  #1. run regression -----------------------------
  # ------------------------------------------------------------------------
  reg <- glm(y ~., data= df_train_y, family = binomial(link='logit'))
  
  #extract coefficients from t
  est_ate = data.frame(summary(reg)$coef[])['t', 1] # if everything else remains the same average treatement effect will always be the slope parameter
  result_ate[i,'reg_te'] = est_ate
  rpehe_bias[i,'reg_adj.'] = rpehe_fct(est_ate, mean(data$ytrue_train[,'delta']))
  ate_bias[i,'reg_adj.'] = ate_bias_fct(est_ate, mean(data$ytrue_train[,'delta']))
  
  
  
  # ------------------------------------------------------------------------
  # 2. inverse propensity weighting-----------------------------------------
  # ------------------------------------------------------------------------
  # insample == outsample as regression coeffcient remains the same
  # generate propensity score model
  
  ps_equ <- as.formula(df_train_y[,c('t', colnames(X))]) 
  psmodel <- glm(ps_equ, data= df_train_y, family = binomial)# fit model
  pspredict <- psmodel$fitted # obtain predicted values of pr(t=1|X)
  ipweights = ifelse(df_train_y$t == 1, 1/ pspredict, 1/(1-pspredict)) 
  # if t== 1: weights = 1/ ps | if t== 0: weights = 1/(1-ps) from here: https://pareonline.net/pdf/v20n13.pdf
  ipw_summary <-summary(glm(y ~., data= df_train_y, weights = ipweights, family= binomial(link= 'logit')))
  t_coef = ipw_summary$coef['t', 1:2] #extract coefficients from t| if everything else remains the same average treatement effect will always be the slope parameter
  est_ate = t_coef[1]
  result_ate[i,'ipw_te'] = est_ate
  rpehe_bias[i,'ipw_te'] = rpehe_fct(est_ate, mean(data$ytrue_train[,'delta']))
  ate_bias[i,'ipw_te'] = ate_bias_fct(est_ate, mean(data$ytrue_train[,'delta']))
  
  
  # ------------------------------------------------------------------------
  # 3. random forest----------------------
  # ------------------------------------------------------------------------
  
  # y has to be factor
  rf_df = df_train_y
  rf_df$y = as.factor(df_train_y$y)
  rf <- randomForest(y~., data= rf_df, ntree = 1000, nsplit = 10, nodesize = 10)
  
  # insample PEHE 
  # with classification, it return probabilities for each class and with insample PEHE
  rf_is = data.frame(predict(rf, is_test, type = 'prob'))
  est_ite = rf_ite(rf_is)
  est_ate = mean(est_ite)
  
  result_ate[i,"rf_te_is"] = est_ate
  result_ite[i,"rf_te_is"] = mean(est_ite)
  rpehe_bias[i, "rf_is"] = rpehe_fct(est_ite, data$ytrue_train[,'delta'])
  pehe_bias[i, "rf_is"] = pehe_fct(est_ite, data$ytrue_train[,'delta'])
  ate_bias[i, "rf_is"] = ate_bias_fct(est_ate, mean(data$ytrue_train[,'delta']))
  
  
  # outsample PEHE
  rf_os = data.frame(predict(rf, os_test, type = 'prob'))
  est_ite = rf_ite(rf_os)
  est_ate = mean(est_ite)
  
  result_ate[i,"rf_te_os"] = est_ate
  result_ite[i,"rf_te_os"] = mean(est_ite)
  rpehe_bias[i, "rf_os"] = rpehe_fct(est_ite, data$ytrue_test[1:length(est_ite),'delta'])
  pehe_bias[i, "rf_os"] = pehe_fct(est_ite, data$ytrue_test[1:length(est_ite),'delta'])
  ate_bias[i, "rf_os"] = ate_bias_fct(est_ate, mean(data$ytrue_test[1:length(est_ite),'delta']))
  
  
  # ------------------------------------------------------------------------
  # 4. run bart probit
  # ------------------------------------------------------------------------
  
  bart_prob = mc.pbart(x.train= df_train, y.train= y_train, mc.cores = 4) # yhat.train/ prob.train/ 
  
  # insample PEHE 
  bart_pred_is = predict(bart_prob, is_test, mc.cores = 4)
  est_ite= bart_pred_is$prob.test.mean[1:n_is]-bart_pred_is$prob.test.mean[(n_is+1):length(bart_pred_is$prob.test.mean)]# mean of iteartion for individuals
  est_ate = mean(est_ite)
  
  result_ate[i,"bartp_te_is"] = est_ate
  result_ite[i,"bartp_te_is"] = mean(est_ite)
  rpehe_bias[i, "bartp_is"] = rpehe_fct(est_ite, data$ytrue_train[,'delta'])
  pehe_bias[i, "bartp_is"] = pehe_fct(est_ite, data$ytrue_train[,'delta'])
  ate_bias[i, "bartp_is"] = ate_bias_fct(est_ate)
  
  # outsample PEHE
  bart_pred_os = predict(bart_prob, os_test, mc.cores = 4)
  est_ite= bart_pred_os$prob.test.mean[1:data$n_test]-bart_pred_os$prob.test.mean[(data$n_test+1):length(bart_pred_os$prob.test.mean)]# mean of iteartion for individuals
  est_ate = mean(est_ite)
  
  result_ate[i,"bartp_te_os"] = est_ate
  result_ite[i,"bartp_te_os"] = mean(est_ite)
  rpehe_bias[i, "bartp_os"] = rpehe_fct(est_ite, data$ytrue_test[1:length(est_ite),'delta'])
  pehe_bias[i, "bartp_os"] = pehe_fct(est_ite, data$ytrue_test[1:length(est_ite),'delta'])
  ate_bias[i, "bartp_os"] = ate_bias_fct(est_ate)
  
  
  # ------------------------------------------------------------------------
  # 5. run bart logit-------------------------
  # ------------------------------------------------------------------------
  
  bart_logit = mc.lbart(x.train= df_train, y.train= y_train, mc.cores = 4) # yhat.train/ prob.train/ 
  
  # insample PEHE 
  bart_pred_is = predict(bart_logit, is_test, mc.cores = 4)
  est_ite= bart_pred_is$prob.test.mean[1:n_is]-bart_pred_is$prob.test.mean[(n_is+1):length(bart_pred_is$prob.test.mean)]# mean of iteartion for individuals
  est_ate = mean(est_ite)
  
  result_ate[i,"bartl_te_is"] = est_ate
  result_ite[i,"bartl_te_is"] = mean(est_ite)
  rpehe_bias[i, "bartl_is"] = rpehe_fct(est_ite, data$ytrue_train[,'delta'])
  pehe_bias[i, "bartl_is"] = pehe_fct(est_ite, data$ytrue_train[,'delta'])
  ate_bias[i, "bartl_is"] = ate_bias_fct(est_ate)
  
  # outsample PEHE
  bart_pred_os = predict(bart_logit, os_test, mc.cores = 4)
  est_ite= bart_pred_os$prob.test.mean[1:data$n_test]-bart_pred_os$prob.test.mean[(data$n_test+1):length(bart_pred_os$prob.test.mean)]# mean of iteartion for individuals
  est_ate = mean(est_ite)
  
  result_ate[i,"bartl_te_os"] = est_ate
  result_ite[i,"bartl_te_os"] = mean(est_ite)
  rpehe_bias[i, "bartl_os"] = rpehe_fct(est_ite, data$ytrue_test[1:length(est_ite),'delta'])
  pehe_bias[i, "bartl_os"] = pehe_fct(est_ite, data$ytrue_test[1:length(est_ite),'delta'])
  ate_bias[i, "bartl_os"] = ate_bias_fct(est_ate, mean(data$ytrue_test[1:length(est_ite),'delta']))
  
  
  
  
  # ------------------------------------------------------------------------
  # 6. synthetic R forest----------------
  # ------------------------------------------------------------------------
  srf <- rfsrcSyn(y~., data= df_train_y, ntree = 1000, mtrySeq = c(1, 10, 20), 
                  nodesizeSeq = c(1:10,20,30,50,100))
  # randomForestSRC package does not allow to apply predict() on rfsrcSyn function
  
  # # insample PEHE 
  srf_is= as.data.frame(rfsrcSyn(object= srf, newdata = data.frame(is_test))$rfSynPred$predicted)
  est_ite = rf_ite(srf_is, method = 'srf')
  est_ate = mean(est_ite)
  
  result_ate[i,"srf_te_is"] = est_ate
  result_ite[i,"srf_te_is"] = mean(est_ite)
  rpehe_bias[i, "srf_is"] = rpehe_fct(est_ite, data$ytrue_train[,'delta'])
  pehe_bias[i, "srf_is"] = pehe_fct(est_ite, data$ytrue_train[,'delta'])
  ate_bias[i, "srf_is"] = ate_bias_fct(est_ate)
  
  
  # # outsample PEHE 
  srf_os= as.data.frame(rfsrcSyn(object= srf, newdata = data.frame(os_test))$rfSynPred$predicted)
  est_ite = rf_ite(srf_os, method = 'srf') # succes - no success
  est_ate = mean(est_ite)
  
  result_ate[i,"srf_te_os"] = est_ate
  result_ite[i,"srf_te_os"] = mean(est_ite)
  rpehe_bias[i, "srf_os"] = rpehe_fct(est_ite, data$ytrue_test[1:length(est_ite),'delta'])
  pehe_bias[i, "srf_os"] = pehe_fct(est_ite, data$ytrue_test[1:length(est_ite),'delta'])
  ate_bias[i, "srf_os"] = ate_bias_fct(est_ate, mean(data$ytrue_test[1:length(est_ite),'delta']))
  
  
  final_results= list(pehe_model = pehe_model, rpehe_model = rpehe_model,
                      pehe_bias = pehe_bias, rpehe_bias = rpehe_bias, ate_bias = ate_bias,
                      result_ate = result_ate, result_ite = result_ite)
  
  
  save.image()
  save(final_results,file=ws_name)
}
