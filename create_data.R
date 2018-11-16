# new file to create data
source("helper.R")

sample_data <- function(csv_file, n, hidden = FALSE, prev_hidden = 0.2){
  
  # Samples data from the original UTI. Before doing so, 
  # removes all irrelevant columns and only keeps columns of interest. If hidden status is true, 
  # hidden confounders will be created that will be correlated with data. 
      # prev_hidden = 0.1 --> 0.03 unmeasured
      # prev_hidden = 0.4 --> 0.1 unmeasured
      # prev_hidden = 0.6 --> 0.15 unmeasured
  data_path = "../Thesis/Code/Janine_Thesis/"
  
  # load data
  df_original = read.csv(paste0(data_path, csv_file), sep = ";")
  
  # remove values not required & without outcome/ treatment assignement
  # extract rows that either received J01MA02- Cipro or J01XE01- Nitro, 
  # extract subset
  med = c("J01MA02", "J01XE01")
  df_subset = df_original %>% filter(grepl(paste(med, collapse="|"), X0_atc))
  df_subset = df_subset %>% select(-one_of(remove_from_df))
  
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






create_z <- function(data, hidden = FALSE, state = 'random'){
  # variables impacting assignement and interactions as indicated by doctors
  if(state = 'random'){
    prob_z = rep(0.5, n)
  }else{
    if(hidden == FALSE){
      prob_z = plogis(1.063 + 3.29  * data$`4_geslacht` -0.007 * data$`4_leeftijd` -0.185 * data$`2_epiteller`-
                      0.288 * data$`2_n-prescripties` + 0.003 * data$`2_urineweg_a` -
                      0.293 * data$`3_diabetes_b_a` -0.958 * data$`3_lage_weerstand_a` -
                      0.660 * data$`3_afwijkingen_nier_urinewegen_b_a` + 1.279 * data$`2_weefselinvasie_a` -
                      0.507 * data$`3_nierschade_b_a` + 1.459 * data$`2_gecompliceerde_uwi_b_a` -
                      0.013 * data$`age_x_gender` + 0.001 * data$`age_x_compUTI` - 0.531 * data$`gender_x_compUTI`)
      }else{
    # with hidden confounder, assign it same weight as complicated UTI
        prob_z = plogis(1.063 + 3.29  * data$`4_geslacht` -0.007 * data$`4_leeftijd` -0.185 * data$`2_epiteller`-
                      0.288 * data$`2_n-prescripties` + 0.003 * data$`2_urineweg_a` -
                      0.293 * data$`3_diabetes_b_a` -0.958 * data$`3_lage_weerstand_a` -
                      0.660 * data$`3_afwijkingen_nier_urinewegen_b_a` + 1.279 * data$`2_weefselinvasie_a` -
                      0.507 * data$`3_nierschade_b_a` + 1.459 * data$`2_gecompliceerde_uwi_b_a` -
                      0.013 * data$`age_x_gender` + 0.001 * data$`age_x_compUTI` - 0.531 * data$`gender_x_compUTI`+
                      1.459 * data$fever)
        }}
  
  z = rbinom(n, 1, prob_z) # treatment assignement similiar to actual data
  return(z)
}






create_y <- function(data, z, hidden = FALSE){
  if(hidden == FALSE){
    yn_true = plogis(
      2.586 +1.848 * data$`2_bloed_e` + +1.638 * data$`6_u71` +
        1.603 * data$`2_bloed_h` + 1.543 * data$`2_leukocyten` +
        1.457 * data$`1_presteller` + 
        1.817 * data$`1_presteller`  * data$`2_leukocyten` +
        2.202 * data$`1_presteller` * data$`6_u71` +
        1.558 * data$`1_presteller`  * data$`2_nitriet` +
        1.517 * data$`1_presteller` * data$`2_bloed_h` -
        2.057 * data$`2_n-prescripties` * data$`2_bloed_e` -
        1.985 * data$`2_n-prescripties+6_u71` -
        1.968 * data$`2_n-prescripties` * data$`2_leukocyten` -
        1.931 * data$`2_n-prescripties+2_bloed_h` -
        1.876 * data$`2_n-prescripties` * data$`2_nitriet` -
        1.784 * data$`2_n-prescripties` -
        1.289 * data$`4_geslacht` * data$`2_n-prescripties` -
        0.995 * data$`2_epiteller` * data$`2_n-prescripties` -
        0.065 * data$`2_n-prescripties` * data$`4_leeftijd` -
        0.014 * data$`4_leeftijd` * data$`2_bloed_b_a`
    )
    yc_true = plogis(
      
    )
    prob_errrorn <- plogis(yn_true[z==1]+ rnorm(length(yn_true[t==1]), 0, 1))
    prob_errorn <- plogis(yc_true[z==0]+ rnorm(length(yc_true[t==0]), 0, 1))
   
     # outcomes for two y_trues
    Y_obs[z==1] <- rbinom(n = length(which((z==1) == TRUE)), size= 1, prob = prob_errrorn)
    Y_obs[z==0] <- rbinom(n = length(which((z==0) == TRUE)), 1, prob = prob_errorc)
    
    # outcomes for two y_trues
    yn_obs = rbinom(n, size =1, yn_true)
    yc_obs = rbinom(n, size =1, yc_true)
  }else{
    # with hidden confounder, assign it same weight as complicated UTI
    prob_z = plogis(1.063 + 3.29  * data$`4_geslacht` -0.007 * data$`4_leeftijd` -0.185 * data$`2_epiteller`-
                      0.288 * data$`2_n-prescripties` + 0.003 * data$`2_urineweg_a` -
                      0.293 * data$`3_diabetes_b_a` -0.958 * data$`3_lage_weerstand_a` -
                      0.660 * data$`3_afwijkingen_nier_urinewegen_b_a` + 1.279 * data$`2_weefselinvasie_a` -
                      0.507 * data$`3_nierschade_b_a` + 1.459 * data$`2_gecompliceerde_uwi_b_a` -
                      0.013 * data$`age_x_gender` + 0.001 * data$`age_x_compUTI` - 0.531 * data$`gender_x_compUTI`+
                      1.459 * data$fever)
  }
  Y <- data.frame(Y_obs, yn_true, yc_true) #is binary outcomes, & probabilities of outcomes if either tretament was received
}