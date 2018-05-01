#k means
library(tm)
library(flexclust)

avisos<- scan("F:/IRS/search/vldb_icse_titles.txt", what="character", sep="\n")
avisos1 <- tolower(avisos)

# Words that appear in at least two of the titles
D <- avisos1

corpus <- Corpus(VectorSource(D))

# Remove Punctuation
corpus <- tm_map(corpus, removePunctuation)

# tolower
corpus <- tm_map(corpus, content_transformer(tolower))

# Stopword Removal
corpus <- tm_map(corpus, function(x) removeWords(x, stopwords("english")))

# term document Matrix
tdm <- TermDocumentMatrix(corpus)

#find the euclidean distance
freq <- colSums(as.matrix(tdm))
d <- dist(t(tdm), method="euclidian") 

#apply k means algorithm and set clusters as 2
kfit <- kmeans(d, 2)
clusplot(as.matrix(d), kfit$cluster, color=T, shade=T, labels=2, lines=0)

clusterfile<-paste(colnames(tdm),kfit$cluster, sep="\\t")
cat("Document Cluster", clusterfile, file="F:/IRS/search/clust.txt", sep="\n")

#No of documents
n = length(avisos1)
x <- rep(1, n/2)
y <- rep(2, n/2)
classes<-c(x,y)
#function to calculate purity
ClusterPurity <- function(clusters, classes) {
  sum(apply(table(classes, clusters), 2, max)) / length(clusters)
}
ClusterPurity(kfit$cluster, classes)

tf<-as.matrix(tdm)
write.table(tf, file="F:/IRS/search/tfidf.txt", row.names=TRUE, col.names=TRUE)
vocab<-length(rownames(tdm))
doc<-seq(1,n)
df_total = data.frame()

for(i in 1:vocab)
{
val<-cbind(strwrap(tdm[i,]),doc)
require(reshape2)
df <- melt(data.frame(val))
df1<-df[!apply(df[,1:2] == 0, 1, FUN = any, na.rm = TRUE),]
word<-rep(i)
tfidf<-cbind(word,df1)
df_total <- rbind(df_total,tfidf)
}

tf<-as.matrix(df_total)
write.table(tf, file="F:/IRS/search/tfidf0and1.txt", row.names=TRUE, col.names=TRUE)


