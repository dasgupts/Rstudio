y.1=read.csv("C:\\Users\\sarbani.dasgupta\\Desktop\\New folder (2)\\Handover_Sarbani\\Handover\\data_files_intermediate\\y.1.csv")
head(y.1)
upper=function(x)
{for( i in colnames(x)){
  colnames(x)[which(colnames(x)==i)] = toupper(i)
}
  return(x)
}
library(dplyr)
# remove CURRENTTENEURE
df.All=upper(y.1)
df.4=select(df.All,c(R46_MAL_VS_AVG_MAL,R26_MONTHS_AT_LEVEL,AVG_TRAVEL_12,	
                     TOT_DAYS_TRAVEL_12,R25_MONTHS_LAST_PROMOTION_DATE,
                     R12M10_PCT_HIKE,		
                     R1_24M10_PCT_HIKE,	
                     R13_24M10_PCT_HIKE,	AVG_HIKE_LAST_2_YRS, 
                     PCTLE_BONUS_NEW,	BONUS_12_1,
                     DIFF_CN_6,DIFF_CN_12,
                     R1_TENURE,ATTRITION_RATE_PEER,ATTRITION_RATE_SKILL,
                     KEY_TALENT_FLAG,KEY_TALENT_DESCRIPTION,
                     DTE,CAREER_TRACK,R6M6_CHANGE_CC_IND,	R12M6_CHANGE_CC_IND
                     
))



df.4$R6M6_CHANGE_CC_IND=as.factor(df.4$R6M6_CHANGE_CC_IND)
df.4$R12M6_CHANGE_CC_IND=as.factor(df.4$R12M6_CHANGE_CC_IND)
#install.packages('PCAmixdata')
library(PCAmixdata)
X.quanti <- splitmix(df.4)$X.quanti
X.quali <- splitmix(df.4)$X.quali

dim(X.quanti)
#pca<-PCAmix(X.quanti,X.quali,ndim=9, rename.level=TRUE,graph=FALSE)

##9 increase t 10 cl
pca<-PCAmix(X.quanti[,1:16],X.quali,ndim=9, rename.level=TRUE,graph=FALSE)
res.pcarot<-PCArot(pca,dim=9,itermax = 10000,graph=FALSE)
options(scipen = 999,digits=3) 
res.pcarot$sqload
pca_out=res.pcarot$sqload
pca_out
#write.csv(pca_out,"/mrsprd_data/Users/Sarbani.Dasgupta/pca_out_KT_KTDES_NVAR.csv")

result.pca=res.pcarot$scores
names(df.All)
Des=df.All[,17]
result.pca.1=cbind(Des,result.pca)
head(result.pca.1)
dim(result.pca.1)
wt=c(.7,1.1,1.1,1,1,1,1,1,1,1.8)
length(wt)
diss_mat <- daisy(result.pca.1, metric = "gower",weight=wt)
gower_mat <- as.matrix(diss_mat)
#gow.mat <- vegdist(clustdata, method="gower")
# Computes agglomerative hierarchical clustering of the dataset.
#https://stackoverflow.com/questions/44568420/clustering-a-mixed-data-set-in-r
plot.new()
layout(c(1,2),widths=c(5,1),heights=c(5,1),T)
par(mar=c(1,1,1,1))


# Plot sihouette width (higher is better)
layout(c(1,2),widths=c(5,1),heights=c(5,1),T)
par(mar=c(1,1,1,1))
sil_width<-c(NA)

for (i in 2:10) {
  pam_fit<-pam(diss_mat,diss = TRUE, k = i) 
  sil_width[i]<-pam_fit$silinfo$avg.width
}
## Plotting Silhouette width (higher is better)
plot(1:10,sil_width)
plot.new()
layout(c(1,2),widths=c(5,1),heights=c(5,1),T)
par(mar=c(1,1,1,1))

sil_width <- c(NA)

for(i in 2:10){
  
  pam_fit <- pam(diss_mat,
                 diss = TRUE,
                 k = i)
  
  sil_width[i] <- pam_fit$silinfo$avg.width
  
}

# Plot sihouette width (higher is better)

plot(1:10, sil_width,
     xlab = "Number of clusters",
     ylab = "Silhouette Width")
lines(1:10, sil_width)
set.seed(0)

k <-4
pam_clust <- pam(gower_mat, diss = TRUE,k,do.swap=TRUE)

pam_clust$clusinfo
pam_clust$silinfo$clus.avg.widths
pam_clust$medoids
result.clust=data.frame(df.All,pam_clust$clustering)
table(result.clust$KEY_TALENT_FLAG,result.clust$pam_clust.clustering)
table(result.clust$R30_DESIGNATION_LEVEL,result.clust$pam_clust.clustering)
#agg_data$AVG_HIKE_LAST_2_YRS
library(cluster)
library(fpc)
library(Rtsne)
#install.packages('ggplot2')
library(ggplot2)
dev.off()
par(mar=c(5.1, 4.1, 4.1, 2.1), mgp=c(3, 1, 0), las=0)

clusplot(gower_mat, pam_clust$cluster, color=TRUE, shade=TRUE, labels=2, lines=0)
df.4$cluster=pam_clust$clustering
tsne_obj = Rtsne(diss_mat,is_distance = TRUE)
data_plot = data.frame(tsne_obj$Y)
names(data_plot) = c("X","Y")
data_plot$cluster=as.factor(df.4$cluster)
ggplot(aes(x=X, y=Y), data=data_plot) + geom_point(aes(color=cluster))

nums <- unlist(lapply(result.clust, is.numeric))  
res.num=result.clust[ , nums]
str(res.num)
head(num)
dim(num)
agg_data=aggregate(res.num, by=list(cluster=pam_clust$cluster), median)
head(agg_data)
agg_data$R1_TENURE
#write.csv(agg_data,"/mrsprd_data/Users/Sarbani.Dasgupta/PCA_DA_T_agg_2_median.csv")


#write.csv(result.clust,"/mrsprd_data/Users/Sarbani.Dasgupta/PCA_PAM_withKT_DA_newvars_v1.csv")
