library(cluster)
library(fpc)
library(factoextra)

mydata <- pmm
mydata <- as.data.frame(subset(mydata, select=c(G, FL, SP, SC, FV, FN, SV, SN, Fehler, Erstes, Dauer, Seiten))) #Fortschritt NAs


shuffle_index <- sample(1:nrow(mydata))
mydata <- mydata[shuffle_index, ]
PD.num <- mydata[complete.cases(mydata),]
PD.num<-round(PD.num,2)


#Normalisierung
z <- PD.num
means <- apply(z,2,mean)
sds <- apply(z,2,sd)
nor <- scale(z,center=means,scale = sds)
#Distanzmatrix
distance = dist(nor)

#Skalierungsvarianten
PD.scale <- as.data.frame(scale(PD.num))


fviz_nbclust(nor, FUNcluster = hcut)
# Elbow method
fviz_nbclust(nor, hcut, method = "wss") + geom_vline(xintercept = 3, linetype = 2)+labs(subtitle = "Elbow method")



#average linkage
hier <- hclust(distance, method = "average")
plot(hier, hang = -1)
hier <- hclust(PD.dist, method = "ward.D2")
plot(hier,  cex = 0.5, hang = -1)
rect.hclust(hier, k = 4, border = "red")
#hierclus1 <- cutree(hier, k = 3)
#plot(hierclus1)
PD.hclust <- hier

#cluster member
member = cutree(PD.hclust,5)
table(member)


#characterizing clusters
aggregate(nor,list(member), mean)
plot(silhouette(cutree(PD.num, 3), distance))
hclust


# doing the hierarchical clustering for diffrent linkage
hc.comp<-hclust(distance,method = "complete")
hc.ward<-hclust(distance,method = "ward.D2")
hc.avg<-hclust(distance,method = "average")

# Complete linkage (k=3)
suppressWarnings(
  # Cut in 3 groups and color by groups
  fviz_dend(hc.comp, k = 3, # Cut in three groups
            cex = 0.5, # label size
            
            color_labels_by_k = TRUE, # color labels by groups
            rect = TRUE, # Add rectangle around groups
            
            rect_fill=F,main = "Dendrogram - Complete (k=3)",
            xlab = "Objects", ylab = "Distance", sub = "",
            ggtheme = theme_minimal()
  )
)
# Ward.D2 linkage (k=3)
suppressWarnings(
  # Cut in 3 groups and color by groups
  fviz_dend(hc.ward, k = 3, # Cut in three groups
            cex = 0.5, # label size
            color_labels_by_k = TRUE, # color labels by groups
            rect = TRUE, # Add rectangle around groups
            main = "Dendrogram - ward.D2",xlab = "Objects", ylab = "Distance", sub = "",ggtheme = theme_minimal()
  )
)
# Average linkage (k=2)
suppressWarnings(
  # Cut in 2 groups and color by groups
  fviz_dend(hc.avg, k = 3, # Cut in groups
            cex = 0.5, # label size
            color_labels_by_k = TRUE, # color labels by groups
            rect = TRUE, # Add rectangle around groups
            rect_fill = F,main = "Dendrogram - Average(k=2)",
            xlab = "Objects", ylab = "Distance", sub = "",
            ggtheme = theme_minimal()
  )
)
# cutting the dendrogram (complete linkage) at k=3
grp.comp <- cutree(hc.comp,3)
# making cluster as factor
grp.comp <- as.factor(grp.comp)
# assining cluster to the original wine data set
data.clust.hier <- cbind(nor, cluster = grp.comp)
data.clust.hier
data.clust.hier <- as.data.frame(data.clust.hier)
suppressMessages( ggpairs(data.clust.hier,aes(color="cluster", alpha=0.5),lower = list(combo = wrap("facethist", binwidth = 0.1))))


ot <- nor
datadistshortset <- dist(ot,method = "euclidean")
hc1 <- hclust(datadistshortset, method = "complete")
pamvshortset <- pam(datadistshortset,3,diss = FALSE)




prozessdaten <- as.data.frame  (subset(swe, select=c(G,   L, DS, Fehler, Dauer, Fortschritt, Seiten, Erstes)))
shuffle_index <- sample(1:nrow(PD.sub))
prozessdaten <- prozessdaten[shuffle_index, ]
PD.num <- prozessdaten[complete.cases(prozessdaten),]
PD.num<-round(PD.num,2)


fragebogen <- as.data.frame(subset(swe, select=c(G, N, FL, SP, SC, DS)))
shuffle_index <- sample(1:nrow(fragebogen))
fragebogen <- fragebogen[shuffle_index, ]
PD.num <- fragebogen[complete.cases(fragebogen),]
PD.num<-round(PD.num,2)


#Normalisierung
z <- PD.num
means <- apply(z,2,mean)
sds <- apply(z,2,sd)
nor <- scale(z,center=means,scale = sds)
#Distanzmatrix
distance = dist(nor)

#Skalierungsvarianten
PD.scale <- as.data.frame(scale(PD.num))


fviz_nbclust(nor, FUNcluster = hcut)
# Elbow method
fviz_nbclust(nor, hcut, method = "wss") + geom_vline(xintercept = 3, linetype = 2)+labs(subtitle = "Elbow method")



#average linkage
hier <- hclust(distance, method = "average")
plot(hier, hang = -1)
hier <- hclust(PD.dist, method = "ward.D2")
plot(hier,  cex = 0.5, hang = -1)
rect.hclust(hier, k = 4, border = "red")
#hierclus1 <- cutree(hier, k = 3)
#plot(hierclus1)


#cluster member
member = cutree(PD.hclust,5)
table(member)


#characterizing clusters
aggregate(nor,list(member), mean)
plot(silhouette(cutree(PD.num, 3), distance))
hclust


# doing the hierarchical clustering for diffrent linkage
hc.comp<-hclust(distance,method = "complete")
hc.ward<-hclust(distance,method = "ward.D2")
hc.avg<-hclust(distance,method = "average")

# Complete linkage (k=3)
suppressWarnings(
  # Cut in 3 groups and color by groups
  fviz_dend(hc.comp, k = 3, # Cut in three groups
            cex = 0.5, # label size
            
            color_labels_by_k = TRUE, # color labels by groups
            rect = TRUE, # Add rectangle around groups
            
            rect_fill=F,main = "Dendrogram - Complete (k=3)",
            xlab = "Objects", ylab = "Distance", sub = "",
            ggtheme = theme_minimal()
  )
)
# Ward.D2 linkage (k=3)
suppressWarnings(
  # Cut in 3 groups and color by groups
  fviz_dend(hc.ward, k = 3, # Cut in three groups
            cex = 0.5, # label size
            color_labels_by_k = TRUE, # color labels by groups
            rect = TRUE, # Add rectangle around groups
            main = "Dendrogram - ward.D2",xlab = "Objects", ylab = "Distance", sub = "",ggtheme = theme_minimal()
  )
)
# Average linkage (k=2)
suppressWarnings(
  # Cut in 2 groups and color by groups
  fviz_dend(hc.avg, k = 3, # Cut in groups
            cex = 0.5, # label size
            color_labels_by_k = TRUE, # color labels by groups
            rect = TRUE, # Add rectangle around groups
            rect_fill = F,main = "Dendrogram - Average(k=2)",
            xlab = "Objects", ylab = "Distance", sub = "",
            ggtheme = theme_minimal()
  )
)
# cutting the dendrogram (complete linkage) at k=3
grp.comp <- cutree(hc.comp,3)
# making cluster as factor
grp.comp <- as.factor(grp.comp)
# assining cluster to the original wine data set
data.clust.hier <- cbind(nor, cluster = grp.comp)
data.clust.hier
data.clust.hier <- as.data.frame(data.clust.hier)
suppressMessages( ggpairs(data.clust.hier,aes(color="cluster", alpha=0.5),lower = list(combo = wrap("facethist", binwidth = 0.1))))


ot <- nor
datadistshortset <- dist(ot,method = "euclidean")
hc1 <- hclust(datadistshortset, method = "complete")
pamvshortset <- pam(datadistshortset,3,diss = FALSE)
=======




prozessdaten <- as.data.frame  (subset(swe, select=c(G, L, DS, Fehler, Dauer, Fortschritt, Seiten)))
shuffle_index <- sample(1:nrow(PD.sub))
prozessdaten <- prozessdaten[shuffle_index, ]
PD.num <- prozessdaten[complete.cases(prozessdaten),]
PD.num<-round(PD.num,2)

fragebogen <- as.data.frame  (subset(swe, select=c(G, L, Fehlerc, Dauerc, Fortschrittc, Seitenc, FLc, SPc, SCc, DS)))
fragebogen <- as.data.frame(subset(swe, select=c(G, L, FL, SP, SC, DS)))
shuffle_index <- sample(1:nrow(fragebogen))
fragebogen <- fragebogen[shuffle_index, ]
PD.num <- fragebogen[complete.cases(fragebogen),]
PD.num<-round(PD.num,2)


#Normalisierung
z <- PD.num
means <- apply(z,2,mean)
sds <- apply(z,2,sd)
nor <- scale(z,center=means,scale = sds)
#Distanzmatrix
distance = dist(nor)

#Skalierungsvarianten
PD.scale <- as.data.frame(scale(PD.num))


fviz_nbclust(nor, FUNcluster = hcut)
# Elbow method
fviz_nbclust(nor, hcut, method = "wss") + geom_vline(xintercept = 3, linetype = 2)+labs(subtitle = "Elbow method")



#average linkage
hier <- hclust(distance, method = "average")
plot(hier, hang = -1)
hier <- hclust(PD.dist, method = "ward.D2")
plot(hier,  cex = 0.5, hang = -1)
rect.hclust(hier, k = 4, border = "red")
#hierclus1 <- cutree(hier, k = 3)
#plot(hierclus1)


#cluster member
member = cutree(PD.hclust,5)
table(member)


#characterizing clusters
aggregate(nor,list(member), mean)
plot(silhouette(cutree(PD.num, 3), distance))
hclust


# doing the hierarchical clustering for diffrent linkage
hc.comp<-hclust(distance,method = "complete")
hc.ward<-hclust(distance,method = "ward.D2")
hc.avg<-hclust(distance,method = "average")

# Complete linkage (k=3)
suppressWarnings(
  # Cut in 3 groups and color by groups
  fviz_dend(hc.comp, k = 3, # Cut in three groups
            cex = 0.5, # label size
            
            color_labels_by_k = TRUE, # color labels by groups
            rect = TRUE, # Add rectangle around groups
            
            rect_fill=F,main = "Dendrogram - Complete (k=3)",
            xlab = "Objects", ylab = "Distance", sub = "",
            ggtheme = theme_minimal()
  )
)
# Ward.D2 linkage (k=3)
suppressWarnings(
  # Cut in 3 groups and color by groups
  fviz_dend(hc.ward, k = 3, # Cut in three groups
            cex = 0.5, # label size
            color_labels_by_k = TRUE, # color labels by groups
            rect = TRUE, # Add rectangle around groups
            main = "Dendrogram - ward.D2",xlab = "Objects", ylab = "Distance", sub = "",ggtheme = theme_minimal()
  )
)
# Average linkage (k=2)
suppressWarnings(
  # Cut in 2 groups and color by groups
  fviz_dend(hc.avg, k = 3, # Cut in groups
            cex = 0.5, # label size
            color_labels_by_k = TRUE, # color labels by groups
            rect = TRUE, # Add rectangle around groups
            rect_fill = F,main = "Dendrogram - Average(k=2)",
            xlab = "Objects", ylab = "Distance", sub = "",
            ggtheme = theme_minimal()
  )
)
# cutting the dendrogram (complete linkage) at k=3
grp.comp <- cutree(hc.comp,3)
# making cluster as factor
grp.comp <- as.factor(grp.comp)
# assining cluster to the original wine data set
data.clust.hier <- cbind(nor, cluster = grp.comp)
data.clust.hier
data.clust.hier <- as.data.frame(data.clust.hier)
suppressMessages( ggpairs(data.clust.hier,aes(color="cluster", alpha=0.5),lower = list(combo = wrap("facethist", binwidth = 0.1))))


ot <- nor
datadistshortset <- dist(ot,method = "euclidean")
hc1 <- hclust(datadistshortset, method = "complete")
pamvshortset <- pam(datadistshortset,3,diss = FALSE)
>>>>>>> .r60
clusplot(pamvshortset, shade = FALSE, labels = 2, col.clus ="blue", col.p ="red", span = FALSE, main = "Cluster Map", cex = 1.2)

