# new file to create data

sample_data <- function(csv_file, n, hidden = FALSE){
  
  # Samples data from the original UTI. Before doing so, 
  # removes all irrelevant columns and only keeps columns of interest. If hidden status is true, 
  # hidden confounders will be created that will be correlated with data. 
  
  # load data
  df_original = read.csv(csv_file, sep = ";")
  
  # remove values not required & without outcome/ treatment assignement
  # pretend that all samples will either get J01MA02- Cipro or J01XE01- Nitro, 
  # Q: should we only sample from these patients that either received C or N, or just all of them?
  outcome_column = df_original["X0_laatste"]
  columns_to_remove = c(names(df_original)[1:14],
                        "X0_id_client_ozps", 
                        "X0_laatste")
  
  df_orginal = df_orginal[,-columns_to_remove]
  
  if(hidden == FALSE){
    X = sample(df_orginal, n, replace = TRUE) 
  }else{
    # create hidden confounders
    
    # fever more likely to prescribe Cipro if fever present
    
    # patient adherence- case 6 out of 20 elderly patients
  }
  return(X)
}


create_z <- function(hidden = FALSE){
  
  if(hidden == FALSE){
    prob_z = plogis(1.1131 + 2.364  * female - 0.022 * age)
  }else{
    
    prob_z = plogis(1.1131 + 2.364  * female - 0.022 * age)
  }
  z = rbinom(n, 1, prob_z) # treatment assignement similiar to actual data
  return(z)
}


create_y <- function(hidden = FALSE){
  
}