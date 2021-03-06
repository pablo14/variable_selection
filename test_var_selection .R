source("lib_var_selection.R")

## Clean categorical and character vector
heart_disease_cat=convert_df_to_char(heart_disease);df_status(heart_disease_cat)

## Ranking all vars
rank=var_rank_info(data = heart_disease_cat, target = "has_heart_disease")

## Information with all vars
vars=colnames(heart_disease_cat)[colnames(heart_disease_cat)!="has_heart_disease"] 
heart_disease_cat$all_vars=concatenate_n_vars(heart_disease_cat, vars)
r1=infor_magic(heart_disease_cat$all_vars, heart_disease_cat$has_heart_disease)

## Information with top 3 (best)
heart_disease_cat$all_vars_top3=concatenate_n_vars(heart_disease_cat, c("heart_disease_severity","thal","chest_pain"))
r2=infor_magic(heart_disease_cat$all_vars_top3, heart_disease_cat$has_heart_disease)

## Information with bottom 3 (worst)
heart_disease_cat$all_vars_bott3=concatenate_n_vars(heart_disease_cat, c("fasting_blood_sugar","serum_cholestoral","resting_blood_pressure"))
r3=infor_magic(heart_disease_cat$all_vars_bott3, heart_disease_cat$has_heart_disease)

df_res=data.frame(exp=c("all","best3", "worst3"),t(data.frame(r1,r2,r3)))
colnames(df_res)=c("exp", "en", "mi", "ig", "gr")
df_res

## Time to plot the cost-benefit, top-down based on rank
cols=rank$var 
df_res_iter=data.frame(vars=NULL, en=NULL, mi=NULL, ig=NULL, gr=NULL)
for(i in 1:length(cols))
{
  vars=cols[1:i]
  heart_disease_cat$all_x=concatenate_n_vars(heart_disease_cat, vars)
  
  rx=infor_magic(heart_disease_cat$all_x, heart_disease_cat$has_heart_disease)
  
  #df_res_iter=rbind(df_res_iter, data.frame(paste(vars, collapse = ", "), rx[1], rx[2], rx[3]))
  df_res_iter=rbind(df_res_iter, data.frame(vars=paste(c(1,i), collapse=","), en=rx[1], mi=rx[2], ig=rx[3], gr=rx[4]))
}

df_res_iter
plot(df_res_iter$en)
plot(df_res_iter$gr)
profiling_num(df_res_iter$gr)
plot_num(df_res_iter)

df_res_iter$ratio_gr=round((lag(df_res_iter$gr)-df_res_iter$gr)/df_res_iter$gr,3)
plot(df_res_iter$ratio_gr)


## Time to plot the cost-benefit, based on mi between input vars
cols=rank$var 
df_res_iter=data.frame(vars=NULL, en=NULL, mi=NULL, ratio=NULL)
for(i in 1:length(cols))
{
  rank_x=var_rank_info(data = heart_disease_cat, target = i)
  vars=cols[1:i]
  heart_disease_cat$all_x=concatenate_n_vars(heart_disease_cat, vars)
  rx=infor_magic(heart_disease_cat$all_x, heart_disease_cat$has_heart_disease)
  df_res_iter=rbind(df_res_iter, data.frame(vars=paste(1:i, collapse=","), en=rx[1], mi=rx[2], ratio=rx[3]))
}

df_res_iter
plot(df_res_iter$en)

