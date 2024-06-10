library(factoextra)


AN <-  subset(Erhebung, select=c(SP1, SP2, SP3, SP4, SP5, SP6, SP7, SV1, SV2, SV3, SV4, SV5))


#principle component
pca <- prcomp(AN,retx=TRUE,center=TRUE,scale.=TRUE,tol=NULL)
pca <- princomp(AN, cor = TRUE, scores = TRUE, covmat = NULL)
pca <- psych::principal(AN, rotate="varimax", nfactors=5, scores=TRUE)


tet.mat <- tetrachoric2( AN, method="Bo" )$rho
tet.mat <- tetrachoric2( BN, method="Bo" )$rho

#principle component
pca <- prcomp(tet.mat,retx=TRUE,center=TRUE,scale.=TRUE,tol=NULL)
pca <- princomp(tet.mat, cor = TRUE, scores = TRUE, covmat = NULL)
pca <- psych::principal(tet.mat, rotate="varimax", nfactors=5, scores=TRUE)

#grafisch
screeplot(pca, type = "l", npcs = 15, main = "Screeplot of the first 10 PCs")
abline(h = 1, col="red", lty=5)
legend("topright", legend=c("Eigenvalue = 1"), col=c("red"), lty=5, cex=0.6)


print(pca)
summary(pca)
pca$STATISTIC
pca$dof 
pca$PVAL
biplot(pca, choices=c(1,2))
fviz_pca_biplot(pca)
ggbiplot(pca,ellipse=TRUE, groups=AN$Score)
pca$loadings
b <- as.data.frame(pca$rotation[, 1:3])
write_xlsx(b, "b_ladung.xls")



str(pca)
dim(pca$x)
pca$x
pca$sdev
var_coord_func <- function(loadings, comp.sdev){ loadings*comp.sdev}

  
loadings <- pca$rotation[, 1:3]
sdev <- pca$sdev
var.coord <- t(apply(loadings, 1, var_coord_func, sdev)) 
head(var.coord[, 1:3])

var.cos2 <- var.coord^2
summary(var.cos2[, 1:4])


comp.cos2 <- apply(var.cos2, 2, sum)
contrib <- function(var.cos2, comp.cos2){var.cos2*100/comp.cos2}
var.contrib <- t(apply(var.cos2,1, contrib, comp.cos2))
head(var.contrib[, 1:4])

ind.coord <- pca$x
head(ind.coord[, 1:4])






#PVE proportion of variance explained by each principle components
pr.var <- pca$sdev^2
pve <- pr.var/sum(pr.var)
# plot of PVE explained by each principle component
plot(cumsum(pve), xlab="Principle Component", ylab = "Proportion of variance explained", ylim=c(0,1), type="b")
abline(v = 3, col="blue", lty=5)
abline(h = 0.7, col="blue", lty=5)

# Find Top n principal component
# which will atleast cover 90 % variance of dimension
which(cumsum(pve) >= 0.7)[1]

pre <- predict(pca)
summary(pre)

heatmap.2(as.matrix(scale(PD.sub)),scale = "none")
install.packages("vegan")
library(vegan)
install.packages("pracma")

library(pracma)
pca.env = rda(tet.mat, scale=T)
loading = scores(pca.env, choices=c(1,2))$species    #choices determines which pc to be taken
rloading = varimax(loading)$loadings
iloading = t(pinv(rloading))
scores = scale(mydata) %*% iloading

