library(lsa) 
library(tm)
library(flexclust)
library(wordcount)
library(ggplot2)

avisos<- scan("G:/sigmod data/sigmod_title.txt", what="character", sep="\n")
avisos1 <- tolower(avisos)
avisos2 <-  str_replace_all(avisos1,"[\r\t]"," ")

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
myMatrix <- TermDocumentMatrix(corpus)

m <- as.matrix(myMatrix)
# calculate the frequency of words and sort it by frequency
word.freq <- sort(rowSums(m), decreasing = T)
wordcloud(words = names(word.freq), freq = word.freq, min.freq = 3,
          random.order = F)

term.freq <- rowSums(m)
term.freq <- subset(term.freq, term.freq >=300)
df <- data.frame(term = names(term.freq), freq = term.freq)
ggplot(df, aes(x=term, y=freq)) + geom_bar(stat = "identity") + xlab("Terms") + ylab("Frequency Count") +coord_flip()

# Delete terms that only appear in a document
rowTotals <- apply(myMatrix, 1, sum)
myMatrix.new <- myMatrix[rowTotals > 1, ]
inspect(myMatrix.new)
# Correlation Matrix of terms
cor(t(as.matrix(myMatrix.new)))

# lsaSpace <- lsa(myMatrix.new)
# myMatrix.reduced <- lsaSpace$tk %*% diag(lsaSpace$sk) %*% t(lsaSpace$dk)

mySVD <- svd(myMatrix.new)
Mp <- mySVD$u[, c(1,2)] %*% diag(mySVD$d)[c(1, 2), c(1, 2)] %*% t(mySVD$v[, c(1, 2)])

rownames(Mp) <- rownames(myMatrix.new)
cor(t(Mp))
#to find optimal number of clusters for k means
wss <- (nrow(head(Mp,20))-1)*sum(apply(head(Mp,20),2,var))
for (i in 2:5) wss[i] <- sum(kmeans(head(Mp,20),centers=i)$withinss) 

plot( wss, type="b", xlab="Number of Clusters",ylab="Within groups sum of squares")
plot(head(Mp,400), col=kmean$cluster)
points(kmean$centers, pch=3, cex=2) # this adds the centroids
text(kmean$centers, labels=1:2, pos=2)

n = 400
classes = sample(2, n, replace=T)

#function to calculate purity
ClusterPurity <- function(clusters, classes) {
  sum(apply(table(classes, clusters), 2, max)) / length(clusters)
}
ClusterPurity(kmean$cluster, classes)

avisos3 <- unlist(avisos2)
avisos4<-removeWords(avisos3, stopwords("english"))
freq<-table(avisos4)
freq1<-sort(freq, decreasing=TRUE)

n = 11107
cluster= sample(2, n, replace=T)
clusterfile<-paste(names(freq1),cluster, sep="\\t")
cat("Word Cluster", clusterfile, file="F:/IRS/search/clust.txt", sep="\n")

