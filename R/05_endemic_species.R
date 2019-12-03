### Effect on endemic species
library(flora)

## lista de especies afetadas

sp.afetadas <- read.csv("results/registros_especies_fogo.csv")

head(sp.afetadas)

lista.sp <- as.character(sort(unique(sp.afetadas$nome_cient)))

head(lista.sp)

sp.flora <- get.taxa(lista.sp, habitat=TRUE, life.form=TRUE)

head(sp.flora)

table(sp.flora$life.form)

unique(sp.flora$family)
