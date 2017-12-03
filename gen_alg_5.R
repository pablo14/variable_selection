source("lib_ga.R")
source("lib_var_selection.R")
source("models.R")


# max cant d genes= max 
data=training
d_cat=create_categ_df(data = data, n_bins = 5);
target="Class"

## Rank best vars based on IG
rank_var=var_rank_info(data = d_cat, target = "Class")


cross_plot(training, str_input=c("TwoFactor2","Linear01", "Noise001"), str_target = "Class")



## Generic Data Prep
target_var=d_cat[[target]]
d_cat_in=d_cat %>% select(-one_of(target))
param_nBits=ncol(d_cat_in)
col_names=colnames(d_cat_in)

## Calling the GA function
system.time(ga_GA_1 <- ga(type = "binary", 
                          fitness = fitness_gain_ratio, 
                          data=d_cat_in, 
                          target_var=target_var, 
                          p_sampling=0.91,
                          tot_vars = param_nBits,
                          elitism = 0.01,
                          popSize = 20, # the number of indivduals
                          pmutation = .1, 
                          nBits = param_nBits,
                          run=50,
                          maxiter = 8,
                          monitor=plot,
                          keepBest = TRUE,
                          parallel = T,
                          names=col_names
                          ) 
)


summary(ga_GA_1)

best_vars_ga=col_names[ga_GA_1@solution[1,]==1];best_vars_ga
#save(best_vars_ga, file="vars_ga.rds")
fit_model_1=create_model_rf_select_vars(data_tr_sample = d_cat_in, target = target_var,best_vars = best_vars_ga);fit_model_1

## no noisy vars
vars_no_noise=select(d_cat_in, -contains("Noise")) %>% colnames(.)
fit_model_2=create_model_rf_select_vars(data_tr_sample = d_cat_in, target = target_var,best_vars =vars_no_noise);fit_model_2

## only noisy vars
vars_noise=select(d_cat_in, contains("Noise")) %>% colnames(.)
fit_model_3=create_model_rf_select_vars(data_tr_sample = d_cat_in, target = target_var,best_vars =vars_noise);fit_model_3


## Cheuing all
resamp=resamples(list(ga = fit_model_1, no_noise = fit_model_2,noise=fit_model_3));bwplot(resamp, layout = c(3, 1))

