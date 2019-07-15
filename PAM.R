
y.1=read.csv("C:\\Users\\sarbani.dasgupta\\Desktop\\New folder (2)\\Handover_Sarbani\\Handover\\data_files_intermediate\\y.1.csv")
head(y.1)
upper=function(x)
{for( i in colnames(x)){
  colnames(x)[which(colnames(x)==i)] = toupper(i)
}
  return(x)
}

df.All=upper(y.1)
names(df.All)
library(dplyr)
df=select(df.All,c(R30_DESIGNATION_LEVEL,KEY_TALENT_FLAG,R1_TENURE,
                   AVG_HIKE_LAST_2_YRS,R26_MONTHS_AT_LEVEL,TOT_DAYS_TRAVEL_12,
                   CHG_PCT_12
                   
))

dim(df)
names(df)
str(df)


#install.packages('cluster')
library(cluster)

dim(df)
diss_mat <- daisy(df, metric = "gower")
gower_mat <- as.matrix(diss_mat)

plot.new()
layout(c(1,2),widths=c(5,1),heights=c(5,1),T)
par(mar=c(1,1,1,1))

#names(clustdata)
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
pam_clust <- pam(diss_mat, diss = TRUE,k,do.swap=TRUE)
pam_clust$medoids
pam_clust$clusinfo
pam_clust$silinfo$avg.width
pam_clust$silinfo$clus.avg.widths

result.clust=data.frame(df,pam_clust$clustering)
dim(result.clust)
aggregate(result.clust[,c(3:8)], by=list(cluster=pam_clust$cluster), median)
table(result.clust$R30_DESIGNATION_LEVEL,result.clust$pam_clust.clustering)
table(result.clust$KEY_TALENT_FLAG,result.clust$pam_clust.clustering)
plot.new()
library(cluster)
#install.packages('fpc')
library(fpc)
dev.off()
par(mar=c(5.1, 4.1, 4.1, 2.1), mgp=c(3, 1, 0), las=0)
c=pam_clust$cluster
clusplot(gower_mat, c, color=TRUE, shade=TRUE, labels=2, lines=0)
#install.packages('Rtsne')
library(Rtsne)
#install.packages('ggplot2')
library(ggplot2)

df$cluster=pam_clust$clustering
tsne_obj = Rtsne(diss_mat,is_distance = TRUE)
data_plot = data.frame(tsne_obj$Y)
names(data_plot) = c("X","Y")
data_plot$cluster=as.factor(df$cluster)
ggplot(aes(x=X, y=Y), data=data_plot) + geom_point(aes(color=cluster))



nums <- unlist(lapply(result.clust, is.numeric))  
res.num=result.clust[ , nums]

pam_results <- res.num %>%
  dplyr::mutate(cluster = pam_clust$clustering) %>%
  group_by(cluster) %>%
  summarise_all(funs(median,mean))%>%
  ungroup %>%
  as.data.frame()
head(pam_results)

Merged_data=data.frame(df.All,pam_clust$clustering)

write.csv(Merged_data,"/mrsprd_data/Users/Sarbani.Dasgupta/Merged_data_WO_DTE_OP2V2.csv")
write.csv(pam_results.All,"/mrsprd_data/Users/Sarbani.Dasgupta/option2_WODTE_Agg_All.csv")





