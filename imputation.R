# mice

library(mice)
library(magrittr)
library(dplyr)
library(missForest)


Erhebung_numeric <- Erhebung %>% select(A, S, L, G, FL, SP, SC, FV, FN, SV, SN, Fehler, Erstes, Dauer, Fortschritt, Seiten)



md.pattern(Erhebung_numeric)

mice_imputed <- data.frame(
  original = Erhebung,
  imputed_pmm = complete(mice(Erhebung_numeric, method = "pmm")),
  imputed_cart = complete(mice(Erhebung_numeric, method = "cart")),
  imputed_lasso = complete(mice(Erhebung_numeric, method = "lasso.norm"))
)

mice_imputed
md.pattern(mice_imputed)

neu <- as.data.frame(mice_imputed)
writexl::write_xlsx(neu, "imputed.xlsx")


Daten <- sapply(Erhebung_numeric, as.numeric)
Daten.missForest <- missForest(Daten)
Daten.Imputiert <- Daten.missForest$ximp
md.pattern(Daten.Imputiert)

neu <- as.data.frame(Daten.Imputiert)
writexl::write_xlsx(neu, "asdfg.xlsx")




