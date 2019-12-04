# working directory
setwd

# read data
library(readxl)
year= "2016" 
Data <- read_excel("Data_1.xlsx",sheet = year)

 
### select columns
#Cols=4:28
#Cols=c(5,8,14,20:24)

x=as.data.frame(Data[,c(5,8,15,20:22,25)]) # choose columns to analyze
#sd_key=apply(x, 2, function(x)  sd(x))
#x=x[,sd_key>0]
#Cols=Cols[sd_key>0]
# zscore trafo
#x=x[x$Area>0,]
xz=apply(x, 2, function(x) (x - mean(x)) / sd(x))



wss = sum(kmeans(xz, centers=1,iter.max = 1e+9)$withinss,nstart=100)
for(i in 2:13){
  wss[i]=sum(kmeans(xz, centers=i,iter.max = 1e+9)$withinss)
}


### scree plot
# plot(wss,t='l')
# points(wss)



# save scree plot as PDF
pdf(paste("screeplot_",year,"_.pdf",sep=""),width=6,height=4,paper='special') 
plot(wss,t='l',main=paste("scree plot year = ",year),ylab = "Distances within clusters",xlab = "Number of clustrers")
points(wss)
points(wss)
dev.off()



############ data analysis

# correlation matrix
Corr=cor(x)
Corr_p=Corr


for(i in 1:nrow(Corr)){
  for(j in 1:nrow(Corr)){
    c=cor.test(as.numeric(xz[,i]),as.numeric(xz[,j]))
    
    Corr_p[i,j]=c$p.value
    Corr[i,j]=c$estimate
    
  }}

# only singnifiicant values
#Corr[Corr_p<.001]=NaN


write.table(round(Corr,3),paste("Corr",year,".csv"),dec = ".",row.names = T,col.names = T,sep = ",")
write.table(round(Corr_p,3),paste("Corr_p",year,".csv"),dec = ".",row.names = T,col.names = T,sep = ",")


# clustering 
k=2 # number of clusters
clust=kmeans(xz,centers=k,iter.max = 1e+9,nstart = k,algorithm = "Lloyd")

### results
# clsuters counties
write.table(cbind(clust$cluster,x$County),paste("County_clusters",year,".csv"),dec = ".",row.names = F,col.names = F,sep = ",")


# help arrays
Descr_Means=clust$centers
rownames(Descr_Means)=rep("Mean",k)
Descr_SD=clust$centers
rownames(Descr_SD)=rep("Standard Dev",k)


for(i in 1:k){

  Descr_Means[i,]=colMeans(x[clust$cluster==i,])
  Descr_SD[i,]= apply(x[clust$cluster==i,], 2, function(x)  sd(x))
}


Descr=rbind(Descr_Means,Descr_SD)


write.table(Descr,paste("Descriptives",year,".csv",sep=''),dec = ".",row.names = T,col.names = T,sep = ",")



# t Tests clustewr
Means_t=matrix(0,k*(k-1)/2,ncol(x)+2)


for(Col in 1:ncol(x)){
  K=1
for(c1 in 1:k){
  for(c2 in 1:k){
    #print(K)
    if(sum(clust$cluster==c2)>2 & sum(clust$cluster==c1)>2 & c1<c2 & sd(x[clust$cluster==c1,Col])*sd(x[clust$cluster==c2,Col])>0  ){ 
       t=t.test(x[clust$cluster==c1,Col], x[clust$cluster==c2,Col], alternative = "two.sided", var.equal = FALSE)
      Means_t[K,Col]=round(t$p.value,3)
      Means_t[K,ncol(x)+1]=c1
      Means_t[K,ncol(x)+2]=c2
      K=K+1}
    
    }
  }
}



colnames(Means_t)=c(colnames(x),"c1","c2")
write.table(Means_t,paste("Pairwise_t_test",year,".csv"),dec = ".",row.names = F,col.names = T,sep = ",")

  



