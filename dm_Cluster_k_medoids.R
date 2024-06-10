install.packages("cluster")
install.packages("fpc")


library(cluster)
library(fpc)
library(factoextra)

swe2 <- as.data.frame  (subset(swe, select=c(G, N, L, FL, SP, SC, Fehler, Dauer, Fortschritt, Erstes, Seiten, DS)))

swe2 <- swe2[complete.cases(swe2),]


#PD.klasse <- as.data.frame  (subset(swe2, select=c(G, Fortschritt, N, L, Fehler, Dauer, Seiten, FL, SP, SC, DS))) #bester HOpkins
#PD.klasse <- as.data.frame  (subset(swe2, select=c(G, Erstes, Fehler, Dauer, Seiten, FL, SP, SC, DS))) #perfekte 3 CLuster

#PD.klasse <- as.data.frame  (subset(swe2, select=c(G, Fehler, Dauer, Seiten, FL, SP, SC, DS)))
#PD.klasse <- as.data.frame  (subset(swe2, select=c(G,Fehler, Dauer, Seiten)))
#PD.klasse <- as.data.frame  (subset(swe2, select=c(G,FL, SP, SC, DS)))

PD.num<-PD.klasse

PD.num<-round(PD.num,2)
df<- nor 


df <- scale(PD.num)

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

