data_path = "../Thesis/Code/Janine_Thesis/"

# load data
df_original = read.csv(paste0(data_path, "170831_dfmodel.csv"), sep = ";")

# extract rows that either received J01MA02- Cipro or J01XE01- Nitro, 
med = c("J01MA02", "J01XE01")
df_subset = df_original %>% filter(grepl(paste(med, collapse="|"), X0_atc))

# remove values not required & without outcome/ treatment assignement
columns_to_remove = c(names(df_subset)[1:14], "X0_id_client_ozps", "X0_laatste")
df_subset = df_subset %>% select(-one_of(columns_to_remove))

fever_var = df_subset %>%  select(matches("zwanger|diabetes|
                                          afwijkingen_nier_urinewegen|nierstenen|
                                          prostatitis|pyelonefritis|weefselinvasie"))


# create fever prevalence table
sample_prev = sum(df_subset$X2_weefselinvasie_2) / length(df_subset$X2_weefselinvasie_2)
fever_prev = c()

for(i in seq(0.00, 0.9,0.1)){
  
  fever = apply(fever_var, 1, function(x) ifelse(sum(x) >1,
                                                 rbinom(1, 1, prob = i),
                                                 rbinom(1, 1, prob = .0174)))
  
  measured = ifelse((sum(fever)/ length(fever)) > sample_prev, 
                    (sum(fever)/ length(fever)),
                    sample_prev) # ensures that minimum is the same as in observed data
  
  prevalence = i
  row = c(i, measured)
  fever_prev = rbind(fever_prev, row)
}

plot(fever_prev, col = "red", pch = 20, ylab = "prop_total_sample_unmeasured",
     xlab= "prevalence in subsample", main = "Prevalence of subsample fever relative to unmeasured in total sample")
abline(h = sample_prev, col = "blue")
