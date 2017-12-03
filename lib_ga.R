library(GA)

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

selec_vars_segment <- function(selec_vars, tot_vars)
{
  per_v_used=selec_vars/tot_vars
  
  findInterval(per_v_used, c(0.2,0.4,0.6,0.8,1), all.inside = T)
  
}