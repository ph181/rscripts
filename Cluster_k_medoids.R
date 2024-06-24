library(cluster)
library(fpc)
library(factoextra)

mydata <- pmm

mydata <- as.data.frame(subset(mydata, select=c(G, FL, SP, SC, FV, FN, SV, SN, Fehler, Erstes, Dauer, Seiten))) #Fortschritt NAs

df <- scale(mydata)

set.seed(1234)


######################Anzahl Cluster
fviz_nbclust(df, FUNcluster = pam)
fviz_nbclust(df, pam, method = "wss")

#plot number of clusters vs. gap statistic

gap_stat <- clusGap(df,
                    FUN = pam,
                    K.max = 10, #max clusters to consider
                    B = 50) #total bootstrapped iterations
fviz_gap_stat(gap_stat)

#perform k-medoids clustering
kmed <- pam(df, 3, metric = "euclidean", stand = FALSE)
kmed
kmed$medoids
head(kmed)

head(kmed$clustering)
fviz_cluster(kmed, data = df)

final_data <- cbind(PD.klasse, cluster = kmed$cluster)

#final_data <- cbind(PD.num, cluster = kmed$cluster)
aggar.kmean <- aggregate(final_data[,1:8], by=list(final_data$cluster), mean) %>% as.data.frame()
aggar.kmean%>%kable()

