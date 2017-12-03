library(GA)
#vignette("GA")
source("lib_var_selection.R")


#### Example hear_disease
heart_disease_cat=create_categ_df(heart_disease)

# calculo columna target
target="has_heart_disease"
target_var=heart_disease_cat[[target]]

# le saco el target
heart_disease_cat_2=select(heart_disease_cat, -has_heart_disease)


fitness_gain_ratio_aux <- function(vars, data, target_var, tot_vars, p_sampling)
{
  # speed up things with sampling
  ix=get_sample(data, percentage_tr_rows = p_sampling)
  data_2=data[ix,]
  target_var_s=target_var[ix]
  
  names=colnames(data_2)
  names_2=names[vars==1]
  d_filtered=data_2[, names_2]
  
  all_x=concatenate_n_vars(data_2, names_2)
  
  gr=gain_ratio(all_x, target_var_s)
  
  q_vars=sum(vars)
  
  q_var_segment=selec_vars_segment(q_vars, tot_vars)
  
  round(gr/q_var_segment,4)
}


fitness_gain_ratio <- function(vars, ...)
{
  fitness_gain_ratio_aux(vars, ...) 
}



# gen: one-hot vector indicating variables
# fitness: calculates and returns Info Gain

# max cant d genes= max 
param_nBits=ncol(heart_disease_cat_2)

param_nBits=ncol(data_2)
col_names=colnames(data_2)

system.time(ga_GA_1 <- ga(type = "binary", 
            fitness = fitness_gain_ratio, 
            data=heart_disease_cat_2, 
            target_var=target_var, 
            p_sampling=0.01,
            tot_vars = param_nBits,
            elitism = 0.01,
            popSize = 10, # the number of indiivduals
            pmutation = .1, 
            nBits = param_nBits,
            run=10,
            monitor=plot,
            keepBest = TRUE,
            parallel = T,
            names=col_names
            ) # it adds the best solution (ga_GA_1@bestSol) at each iteration 
            
)


summary(ga_GA_1)

best_vars_ga=col_names[ga_GA@solution[1,]==1]
save(best_vars_ga, file="vars_ga.rds")

# tengo popSize individuos, de ellos selecciono los de mejor fitness, y hago crossover

# el grafico de plot(ga) es igual al de monitor=TRUE, solo q esta fuera de escala en el eje X
plot(ga_GA_1)
ga_GA_1@solution
ga_GA_1@fitnessValue

#####################################################################################
system.time(ga_GA_tournament <- ga(type = "binary", 
              fitness = fitness_gain_ratio, 
              data=heart_disease_cat_2, 
              target_var=target_var, 
              popSize = 10, # the number of indiivduals
              pmutation = .1, 
              nBits = param_nBits,
              run=10,
              monitor=plot,
              keepBest = TRUE,
              selection = gabin_tourSelection,
              parallel = T,
              names=names(heart_disease_cat_2)) # it adds the best solution (ga_GA_1@bestSol) at each iteration 
)

plot(ga_GA_tournament)
ga_GA_tournament@solution
ga_GA_tournament@fitnessValue



# El metodo torneo tarda la mitad que el default (ambos sin parallel computing) 53 vs 25.
# Parallel: 22 vs 31. Se invierte 




########################################################################
ga_GA <- ga(type = "binary", 
            fitness = fitness_gain_ratio, 
            data=heart_disease_cat_2, 
            target_var=target_var, 
            popSize = 10, 
            pmutation = .01, 
            nBits = param_nBits,
            maxiter = 10,
            run = 3, # q iter without improvement
            parallel = TRUE,
            elitism = 0.01,
            monitor = plot,
            keepBest = TRUE
)

plot(ga_GA)
ga_GA@solution


names=colnames(data)
vars=ga_GA@solution
names_2=names[vars==1];names_2


