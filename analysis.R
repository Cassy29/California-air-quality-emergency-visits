rm(list=ls())
Years=as.character(2012:2016)
# cluster number
k=2
year="2016"
source('clustering.R')


Sizes=1:k
Clusters=1:nrow(x)

for(i in 1:5){
  year=Years[i]
  source('clustering.R')
Sizes=rbind(Sizes,sort(clust$size))
Clusters=cbind(Clusters,clust$cluster)


# REGRESSION
Reg=lm(x$Visits_all~.,data=x)

s=summary(Reg)
Coefs=round(s$coefficients,2)
write.table(Coefs,paste("Regression",year,".csv",sep=''),dec = ".",row.names = T,col.names = T,sep = ",")

Rsq=round(s$adj.r.squared,2)
write.table(Rsq,paste("R2_",year,".csv",sep=''),dec = ".",row.names = T,col.names = T,sep = ",")

}


Sizes=Sizes[2:6,]
rownames(Sizes)=Years
Clusters=Clusters[,2:6]
colnames(Clusters)=Years
print(Sizes)

# cluste renaming 
Length=1:k
for(i in 1:5){
for(j in 1:k){
Length[j]=sum(Clusters[,i]==j)
}}




