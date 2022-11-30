rm(list=ls())
#Path to local drive
root <- "~/GitHub/public_draft_alternatives_BM"
setwd(root)

data_root<-file.path(root,"data-raw")
code_root <- file.path(root,"R")
output_root <- file.path(root,"output")




library(tidyverse)
library(lubridate)
library(vegan)
library(cluster)
library(dendextend)
library(PTAk)


#################### Read dayflow data
data_dayflow_1956_1969<-read.csv(file.path(data_root, "Dayflow", "dayflow-results-1956-1969.csv"))
data_dayflow_1970_1983<-read.csv(file.path(data_root, "Dayflow", "dayflow-results-1970-1983.csv"))
data_dayflow_1984_1996<-read.csv(file.path(data_root, "Dayflow", "dayflow-results-1984-1996.csv"))
data_dayflow_1997_2020<-read.csv(file.path(data_root, "Dayflow", "dayflow-results-1997-2020.csv")) %>% mutate(EXPORT=EXPORTS)
data_dayflow_2021<-read.csv(file.path(data_root, "Dayflow", "dayflowcalculations2021.csv"))%>% mutate(EXPORT=EXPORTS)



data_dayflow<-bind_rows(#data_dayflow_1929_1939,data_dayflow_1940_1949,data_dayflow_1950_1955,
  data_dayflow_1956_1969,data_dayflow_1970_1983,data_dayflow_1984_1996,data_dayflow_1997_2020,data_dayflow_2021)
data_dayflow$Date <- as.Date(data_dayflow$Date,"%m/%d/%Y")

#Add water year to dayflow
data_dayflow$WY<-as.numeric(ifelse(month(data_dayflow$Date)>9,data_dayflow$Year+1,data_dayflow$Year))

data_dayflow$Month<-month(data_dayflow$Date)

#Add julian day
data_dayflow$julianday<-yday(data_dayflow$Date)



#################### Add Water Year information

#Data from https://cdec.water.ca.gov/reportapp/javareports?name=WSIHIST
#Copy and pasted into excel
data_wateryear<-read.csv(file.path(data_root, "Dayflow", "DWR_WSIHIST.csv"))

#Add WY type information
data_dayflow_added<-left_join(data_dayflow,data_wateryear[,c("WY","Sac_WY")])

##########
data_dayflow_edit<-data_dayflow_added %>% group_by(WY,Month) %>% summarise(SAC=mean(SAC),SJR=mean(SJR),YOLO=mean(YOLO),SWP=mean(SWP),CVP=mean(CVP)) %>%
  pivot_wider(names_from = Month, values_from = c(SAC,SJR,YOLO,SWP,CVP), names_sep="_")

names(data_dayflow_edit)

# WY_list<-data_dayflow_edit$WY
# data_dayflow_edit$WY<-NULL
# 
# wy_dist<-as.matrix(vegdist(data_dayflow_edit,method="euclidean"))
# 
# wy_dist

summary(data_dayflow_edit)


names(data_dayflow_edit)
length(unique(data_dayflow_edit$WY))

dat_array=array(
  data = NA, dim = c(66, 5, 12), 
      dimnames=list(WY=paste0("wy_", data_dayflow_edit$WY),
                    location=c("SAC", "SJR", "YOLO", "SWP", "CVP"),
                    month=paste("wym_", 1:12)))




sub_scale=apply(data_dayflow_edit[,-1], MARGIN = 2,
                FUN = function(x){
                  (x-mean(x))/sd(x)
                }
)
colnames(sub_scale) = names(data_dayflow_edit[,-1])



site_names = c("SAC", "SJR", "YOLO", "SWP", "CVP")
for(i in 1:66){
  for(j in 1:length(site_names)){
    dat_array[i,j,] = sub_scale[i,grep(pattern = site_names[j], x=colnames(sub_scale))]
  }
}

###~~~~~~~~~~~~~~~~~~~~~~
# remove 1983, 1974, and 1984 due to uniqueness

dimnames(dat_array)
remove_year = c(19,28:29, 43)

dat_array = dat_array[-remove_year,,]

####################################################################
#~  Principal tensor analysis 'a la' Frelat etal 2017 PlosOne
##################################################################
rm(list=c("pta"))
pta<-PTA3(dat_array, nbPT = 3, nbPT2 = 3, minpct = 0.1)
summary.PTAk(pta,testvar = 0)
par(mfrow=c(1,1), oma=c(0,0,0,0), mar=c(4.5,4.5,1.5,4.5)+0.1)
plot(pta, scree=TRUE, las=1)
keep = c(1,9,11)#,4)

pta[[1]]$n
pta[[2]]$n
pta[[3]]$n



########################################################################
###~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#####   Clustering:
###~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#########################################################################

coo<-t(pta[[1]]$v[c(keep),])
row.names(coo) = pta[[1]]$n

# Numerical Ecology in R p.63: cophenetic dist is dist where 2 objects become members of same group
# correlation is between original dist matrix and cophenetic distance matrix
# higher value is good - implies the cophenetic dist matrix reproduces original dist matrix
# above .7 is good
#1. Compute the distance between species
dist1=dist(coo, method = "euclidean")

#2. Build a trees with various linkage


##  Ward_2 clustering
hclust.ward2<- hclust(dist1, method = "ward.D2")
ward2.coph <- cophenetic(hclust.ward2)
cophcorr.ward2 <- cor(dist1, ward2.coph)

##  Single linkage
hclust.single <- hclust(dist1, method="single")
single.coph <- cophenetic(hclust.single)
cophcorr.single <- cor(dist1, single.coph)

##  Complete linkage
hclust.complete <- hclust(dist1, method="complete")
complete.coph <- cophenetic(hclust.complete)
cophcorr.complete <- cor(dist1, complete.coph)

##  Average clustering
hclust.avg <- hclust(dist1, method="average")
avg.coph <- cophenetic(hclust.avg)
cophcorr.avg <- cor(dist1, avg.coph)

#####

data.frame(cophcorr.ward2, cophcorr.single,
           cophcorr.complete, cophcorr.avg)


###~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#####   Gower (1983) distances: to compare clustering methods   ####
###~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# p65 Numerical Ecology in R: computed as the sum of squared differences between original and cophenetic distances
# p413 Numerical Ecology: measure only has relative value for clustering results from same distance matrix

## The clustering method that produces the smallest distance is best

gow.dist.single <- sum((dist1 - single.coph)^2)
gow.dist.complete <- sum((dist1 - complete.coph)^2)
gow.dist.average <- sum((dist1 - avg.coph)^2)
gow.dist.ward2 <- sum((dist1 - ward2.coph)^2)

###
#####  Combine coph corr and Gower dist in a dataframe   #####
###

fit.metrics = data.frame(clustmethod = c("single", "complete", "avg", "ward2"),
                         cophcorr = c(cophcorr.single, cophcorr.complete,
                                      cophcorr.avg, cophcorr.ward2),
                         gowdist = c(gow.dist.single, gow.dist.complete,
                                     gow.dist.average, gow.dist.ward2))
fit.metrics




clustObj = list("hclust.single" = hclust.single, 
                "hclust.complete" = hclust.complete,
                "hclust.avg" = hclust.avg,
                "hclust.ward2" = hclust.ward2,
                "hclust.fit" = fit.metrics)
clustObj

#############################################################################
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#   Optimal number of clusters (k) analysis 
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#############################################################################

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#   Silhouette Widths (pg 70 in Numerical Ecology)
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

hclust.avg$labels = dimnames(dat_array)$WY

# create empty vector to write silhouette values
asw = numeric(nrow(sub_scale))

# write values
for(k in 2:(length(dimnames(dat_array)$WY)-1)) {
  sil = silhouette(cutree(hclust.avg, k=k), dist1)
  asw[k] = summary(sil)$avg.width
}

#best (larggest Silhouette width)
k.best.silhouette = which.max(asw)


par(mfrow=c(1,1), mar=c(4.5,4.5,1.5,1.5)+0.1)
plot(1:nrow(sub_scale), asw, 
     main="", xlab="Number of Clusters",
     ylab="Average Silhouette Width", type='h', lend=1,
     las=1, lwd=2, ylim=c(0, max(asw)*1.05), yaxs='i')
lines(y = c(0,max(asw)), x=c(k.best.silhouette, k.best.silhouette),
      col="red3", lwd=4, lend=1)
axis(1, k.best.silhouette,  col.axis="red3", col.ticks = "red3",
     font=2, labels=FALSE)
axis(1, k.best.silhouette,  col.axis="red3", col.ticks = "red3", font=2,
     line = -0.7)
axis(1, k.best.silhouette+1.5, "(optimum k)", col="red3",
     font=2, col.axis="red3", line = 0.75, tick = FALSE)



# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#   Silhouette plot 
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


red_dend_ave <- as.dendrogram(hclust.avg, hang=-1)
red_dend_ave <- rotate(red_dend_ave, 1:length(hclust.avg$labels))

par(mfrow=c(1,1), mar=c(10,4.5,1.5,1.5)+0.1)
plot(red_dend_ave)



optim_k = k.best.silhouette #asw_maxima
cutg = cutree(hclust.avg, k = optim_k)
sil = silhouette(cutg, dist1)
plot(sil)

#############################################################################
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#   Plot final dendrogram
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#############################################################################

taxa_dendro = as.dendrogram(hclust.avg)
taxaclusters <- cutree(taxa_dendro, k=optim_k)[order.dendrogram(taxa_dendro)]
taxacol_id = c(hcl.colors(n=9, palette = "Viridis")[c(1,3,7)],
               hcl.colors(n=9, palette = "Heat")[c(5)],
               hcl.colors(n=10, palette = "RdBu")[c(10,2)],
               hcl.colors(n=7, palette = "Dark 2")[c(3,5)],
               hcl.colors(n=10, palette = "BrBG")[c(2,10)],
               hcl.colors(n=10, palette = "PiYG")[c(2,10)])


taxa_dendro = taxa_dendro %>%
  branches_attr_by_clusters(taxaclusters, values = taxacol_id) %>%
  set("branches_lwd", 2) %>%
  set("labels_cex", 0.8)

nclust = optim_k
par(mar=c(1,3,1,1))
par(mfrow=c(1,1), mar=c(10,1.5,1.5,1.5)+0.1)
plot(taxa_dendro, xaxt='n', axes=FALSE)
rect.hclust(hclust.avg, k=nclust, border=gray(0.2,0.2))

clust_3D <- as.factor(cutree(taxa_dendro, k=nclust))
dendro_df = data.frame(taxa = unlist(partition_leaves(taxa_dendro)[1]),
                       dendr_order=1:(length(unlist(partition_leaves(taxa_dendro)[1]))))
taxa_cluster_df=data.frame( taxa = row.names(coo), cluster = clust_3D)
taxa_cluster_df=merge(taxa_cluster_df ,dendro_df)
taxa_cluster_df=taxa_cluster_df[order(taxa_cluster_df$dendr_order),]
clust_col_order=taxa_cluster_df$cluster[!duplicated(taxa_cluster_df$cluster)]



#WY type data from Jereme
drought = read.csv(file.path(data_root, "Dayflow", "drought_metrics2.csv"))
dendro_wy = data.frame(WY=as.integer(unlist(strsplit(x = names(taxaclusters), split="wy_"))[c(F,T)]))
drought_df = left_join(x=dendro_wy, y = drought, by = "WY")

drought_df$color = "#a50026"
drought_df$color[which(drought_df$Yr_type %in% c("W"))] = "#313695"
drought_df$color[which(drought_df$Yr_type %in% c("AN"))] = "#74add1"
drought_df$color[which(drought_df$Yr_type %in% c("BN"))] = "#fdae61"
drought_df$color[which(drought_df$Yr_type %in% c("D"))] = "#f46d43"


taxa_cluster_df = taxa_cluster_df[order(taxa_cluster_df$cluster),]
taxa_dendro_black=as.dendrogram(hclust.avg)
taxa_dendro_black = taxa_dendro_black %>%
  set("branches_lwd", 1.5) %>%
  set("labels_cex", 0.9) %>%
  set("labels_colors", drought_df$color) 


# pdf(file = "PTA_rect_dendo.pdf", width = 12, height=7, paper = "special")
# windows(width = 15, height=7)
# quartz(width = 12, height=7)

par(mfrow=c(1,1), mar=c(5,0.5,0.5,0.5)+0.1)
plot(taxa_dendro_black, xaxt='n', axes=FALSE, lwd=5)
rect.dendrogram(taxa_dendro_black, k=nclust,
                border=hcl.colors(n=nclust+1,
                                  palette = "Viridis")[clust_col_order],
                lwd=2)
# dev.off()
legend("top", legend = c("Wet", "Above Norm", "Below Norm", "Dry", "Critical"),
       fill = c("#313695", "#74add1", "#fdae61", "#f46d43", "#a50026"), 
       inset=c(0.075))

