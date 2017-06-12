# R-Code-Cluster-Analysis
x=Melanoma
#Hierarchichal - Manhattan-Average Linkage
d = dist(x, method = "manhattan") 
hc = hclust(d, method="average")
plot(hc)

#Hierarchichal - Manhattan-Ward
d = dist(x, method = "manhattan") 
hc = hclust(d, method="ward.D")
plot(hc)

#Hierarchichal - Hamming-Average Linkage
d = dist(x, method = "binary") 
hc = hclust(d, method="average")
plot(hc)

#Hierarchichal - Hamming-Ward
d = dist(x, method = "binary") 
hc = hclust(d, method="ward.D")
plot(hc)

clusters=cutree(hc, k=4) # cut tree into k clusters
rect.hclust(hc, k=4, border="red")

centers=aggregate(x, list(clusters), mean) 
centers=centers[,-1]; 
k=nrow(centers)
plot(x,col=clusters , pch=19, cex=0.75) 
points(centers, col = 1:k, pch = 8, cex=2) 
points(centers, col = 1:k, pch = 19, cex=1) 
clusters; 
table(clusters)
centers

# Computes R2 
R2=function(x,clusters,k){
n=nrow(x); tss=var(x); tss=(n-1)*sum(diag(tss)); wss=0
for(j in 1:k){
  cj=x[clusters==j,]; 
  nj=nrow(cj); 
  vj=var(cj); 
  wssj=(nj-1)*sum(diag(vj)); 
  if(!is.matrix(cj)) 
    wssj=0 
  wss=wss+wssj }
r2=1-wss/tss; 
cat("R2 = ",r2,"\n")
return(r2)
} 
r2=R2(x,clusters,k)
#Partitioning Algorithm; k-means
k=4; 
kmc = kmeans(x, k); 
clusters=kmc$cluster
plot(x, col = kmc$cluster)
points(kmc$centers, col = 1:k, pch = 8, cex=2) 
points(kmc$centers, col = 1:k, pch = 19, cex=1) 
clusters; 
table(clusters)
centers

x=scale(x,center=T,scale=T); # scale x first 
wss = (nrow(x)-1)*sum(apply(x,2,var))
for (i in 2:10) {
  wss[i] = sum(kmeans(x,centers=i)$withinss)}   
plot(1:10, wss, type="b", xlab="k", ylab="WSS")

k=4; 
kmc = kmeans(x, k); 
clusters=kmc$cluster
plot(x, col = kmc$cluster)
points(kmc$centers, col = 1:k, pch = 8, cex=2) 
points(kmc$centers, col = 1:k, pch = 19, cex=1) 
clusters; 
table(clusters)
centers
