# mice

install.packages("mice")
install.packages("missForest")

library(mice)
library(magrittr)
library(dplyr)
library(missForest)


Erhebung_numeric <- Erhebung %>% select(C, G, FL, SP, SC, SV, SN, FV, FN
                                        )



md.pattern(Erhebung_numeric)

mice_imputed <- data.frame(
  original = Erhebung$SN,
  #imputed_pmm = complete(mice(Erhebung_numeric, method = "pmm")),
  imputed_cart = complete(mice(Erhebung_numeric, method = "cart")),
  imputed_lasso = complete(mice(Erhebung_numeric, method = "lasso.norm"))
)
mice_imputed
md.pattern(mice_imputed)

neu2 <- as.data.frame(mice_imputed)
writexl::write_xlsx(neu2, "asd.xlsx")


Daten <- sapply(Erhebung_numeric, as.numeric)
Daten.missForest <- missForest(Daten)
Daten.Imputiert <- Daten.missForest$ximp
md.pattern(Daten.Imputiert)

neu <- as.data.frame(Daten.Imputiert)
writexl::write_xlsx(neu, "asdfg.xlsx")




