library(funModeling)

#### Example from: http://www.saedsayad.com/decision_tree.htm
d_tennis=read.csv(file="tennis.csv", header = T, stringsAsFactors = F)
# creating the id variable
d_tennis$id=1:nrow(d_tennis)

en_tar=entropy(table(d_tennis$jugar_tenis), unit = "log2")

tbl_2v=t(table(d_tennis$jugar_tenis, d_tennis$clima))
H12 = entropy(tbl_2v, unit = "log2",method = );H12; # default= method="ML",
entropy_2(d_tennis$jugar_tenis, d_tennis$clima)
entropy_2( d_tennis$clima, d_tennis$jugar_tenis)


## Information Gain IUPII
information_gain(d_tennis$clima, d_tennis$jugar_tenis)
information_gain(d_tennis$temperatura, d_tennis$jugar_tenis)
information_gain(d_tennis$humedad, d_tennis$jugar_tenis)
information_gain(d_tennis$ventoso, d_tennis$jugar_tenis)


## Testing ID variable
# entropy between id and target is 0, perfect correspondence, "no chaos based on that var"
entropy_2(d_tennis$id, d_tennis$jugar_tenis)

# Info gain of an Id var, doesn't work on high card features, IG is almost max
information_gain(d_tennis$id, d_tennis$jugar_tenis) 

# Split: it's the info gain of the variable against itself
split_id=information_gain(d_tennis$id, d_tennis$id)
split_weather=information_gain(d_tennis$clima, d_tennis$clima)

# One solution to decrease this bias: gain raio
gain_ratio(d_tennis$clima, d_tennis$jugar_tenis)
gain_ratio(d_tennis$id, d_tennis$jugar_tenis)


rank_tenis=var_rank_info(data = d_tennis, "jugar_tenis")

pairs(select(rank_tenis, -var))
cor(select(rank_tenis, -var))





