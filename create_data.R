# new file to create data

sample_data <- function(csv_file, n, hidden = FALSE, prev_hidden = 0.2){
  
  # Samples data from the original UTI. Before doing so, 
  # removes all irrelevant columns and only keeps columns of interest. If hidden status is true, 
  # hidden confounders will be created that will be correlated with data. 
      # prev_hidden = 0.1 --> 0.03 unmeasured
      # prev_hidden = 0.4 --> 0.1 unmeasured
      # prev_hidden = 0.6 --> 0.15 unmeasured
  
  # load data
  df_original = read.csv(csv_file, sep = ";")
  
  # remove values not required & without outcome/ treatment assignement
  # extract rows that either received J01MA02- Cipro or J01XE01- Nitro, 
  # extract subset
  med = c("J01MA02", "J01XE01")
  df_subset = df_original %>% filter(grepl(paste(med, collapse="|"), X0_atc))
  columns_to_remove = c(names(df_subset)[1:14],"X0_id_client_ozps", "X0_laatste")
  df_subset = df_subset %>% select(-one_of(columns_to_remove))
  
  if(hidden == FALSE){
    X = sample(df_subset, n, replace = TRUE) 
    
    
  }else{
    # create hidden confounders
    # measured koorts = 2_weefselinvasie_2
      # in data meausred total: 1691 /(95199 + 1691) = 0.0174
      # in nitro: 1083 /(80974+ 1083) = 0.0132 ~ 1%
      # in cipro: 608/(14225+608) = 0.041 ~4%
    # add unmeausred 
    # NHG: fever more likely for patients with diabetes, pregnancy, kidney stones, pyelonefritis, prostatitis, weefselinvasie
    
    X = sample(df_subset, n, replace = TRUE) 
    
    fever_var = X %>% 
      select(matches("zwanger|diabetes|afwijkingen_nier_urinewegen|nierstenen|prostatitis|pyelonefritis|weefselinvasie"))
    
    
    X['fever'] = apply(fever_var, 1, function(x) ifelse(sum(x) >1,
                                                   rbinom(1, 1, prob = prev_hidden),
                                                   rbinom(1, 1, prob = .0174)))
    
  }
  return(X)
}






create_z <- function(hidden = FALSE){
  # curently only interaction between wieghts are significant/ among the top 20 plus/ minus
  
  if(hidden == FALSE){
    # import weights
    
    # sort dataframe or manually enter
    prob_z = plogis(1.1131 + 2.364  * female - 0.022 * age)
  }else{
    # import weights
    
    # sort dataframe or manually enter
    prob_z = plogis(1.1131 + 2.364  * female - 0.022 * age)
  }
  z = rbinom(n, 1, prob_z) # treatment assignement similiar to actual data
  return(z)
}






create_y <- function(hidden = FALSE){
  
}