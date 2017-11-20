d_tennis=read.csv(file="tennis.csv", header = T, stringsAsFactors = F)

en_tar=entropy(table(d_tennis$jugar_tenis), unit = "log2")

tbl_2v=t(table(d_tennis$jugar_tenis, d_tennis$clima))
H12 = entropy(tbl_2v, unit = "log2");H12; # default= method="ML",


## Information Gain IUPII
en_tar-entropy_bis(d_tennis$clima, d_tennis$jugar_tenis)
en_tar-entropy_bis(d_tennis$clima, d_tennis$jugar_tenis)
en_tar-entropy_bis(d_tennis$clima, d_tennis$jugar_tenis)
