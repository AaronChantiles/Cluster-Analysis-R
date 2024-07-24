#Task 1: K-means Clustering and Scaling

mydata = read.csv("ShoppingVisits.csv")

plot(mydata, pch=20)

km.res.ns = kmeans(mydata, 3, nstart=20)
km.res.ns
km.res.ns$cluster
plot(mydata, col=km.res.ns$cluster, pch=20)

mydata.sc = scale(mydata)
mydata.sc

km.res.2 = kmeans(mydata.sc, 3, nstart=20)
km.res.2
plot(mydata, col=km.res.2$cluster, pch=20)


par(mfrow=c(1,2))
plot(mydata, col=km.res.ns$cluster, pch=20, main="No scaling")
plot(mydata, col=km.res.2$cluster, pch=20, main="scaled")


# Task 2: K-means with Different Number of Clusters

mydata.sc = scale(mydata)
mydata.sc

km.res.two = kmeans(mydata.sc, 2, nstart=20)
km.res.two
plot(mydata, col=km.res.two$cluster, pch=20, main="2 Clusters")

km.res.four = kmeans(mydata.sc, 4, nstart=20)
km.res.four
plot(mydata, col=km.res.four$cluster, pch=20, main="4 Clusters")

km.res.five = kmeans(mydata.sc, 5, nstart=20)
km.res.five
plot(mydata, col=km.res.five$cluster, pch=20, main="5 Clusters")


par(mfrow=c(2,2))
plot(mydata, col=km.res.two$cluster, pch=20, main="2 Clusters")
plot(mydata, col=km.res.2$cluster, pch=20, main="3 Clusters")
plot(mydata, col=km.res.four$cluster, pch=20, main="4 Clusters")
plot(mydata, col=km.res.five$cluster, pch=20, main="5 Clusters")


# Task 3: Choosing k

ss1 = km.res.two$totss
ss2 = km.res.two$tot.withinss
ss3 = km.res.2$tot.withinss
ss4 = km.res.four$tot.withinss
ss5 = km.res.five$tot.withinss
ss.vec = c(ss1,ss2,ss3,ss4,ss5)
par(mfrow=c(1,1))
plot(ss.vec, type="b", xlab="Number of Clusters", ylab="Total Within-Cluster SS")


plot(mydata, col=km.res.2$cluster, pch=20, main="3 Clusters")


# Task 4: Hierarchical Clustering and Dendrogram

hc.res.avg = hclust(dist(mydata.sc), method="average")
plot(hc.res.avg, cex=0.5, main="Dendrogram (Average Linkage)")
abline(h=1.8, col="red",lty=2)

cutree(hc.res.avg, k=3)

# Task 5: K-means vs. Hierarchical

par(mfrow=c(1,2))
plot(mydata, col=km.res.2$cluster, pch=20, main="K-means 3 Clusters")
plot(mydata, col=cutree(hc.res.avg, k=3), pch=20, main="Hierarchical 3 Cluster Avg")


# Task 6: Hierarchical Clustering with Different Linkages
par(mfrow=c(1,1))

hc.res.complete = hclust(dist(mydata.sc), method="complete")
plot(hc.res.complete, cex=0.5, main="Dendrogram (Complete Linkage)")

hc.res.single = hclust(dist(mydata.sc), method="single")
plot(hc.res.single, cex=0.5, main="Dendrogram (Single Linkage)")

hc.res.centroid = hclust(dist(mydata.sc), method="centroid")
plot(hc.res.centroid, cex=0.5, main="Dendrogram (Centroid Linkage)")


par(mfrow=c(2,2))
plot(mydata, col=cutree(hc.res.avg, k=3), pch=20, main="Hierarchical 3 Cluster Avg")
plot(mydata, col=cutree(hc.res.complete, k=3), pch=20, main="Hierarchical 3 Cluster Complete")
plot(mydata, col=cutree(hc.res.single, k=3), pch=20, main="Hierarchical 3 Cluster Single")
plot(mydata, col=cutree(hc.res.centroid, k=3), pch=20, main="Hierarchical 3 Cluster Centroid")


