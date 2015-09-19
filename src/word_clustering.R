library(tm)

ny <- getPage("nytimes", token, n = 100)
ny_web <- html(ny[1,6])
ny_title <- ny_web %>% html_nodes("#story-heading") %>% html_text()
ny_article <- ny_web %>% html_nodes(".story-content") %>% html_text()
ny_article <- gsub("\n", "", ny_article)

text <- ny_article
tmc <- Corpus(VectorSource(text))
tmc <- tm_map(tmc, removePunctuation)
tmc <- tm_map(tmc, removeNumbers)
tmc <- tm_map(tmc, removeWords, stopwords("SMART"))
tmc <- tm_map(tmc, content_transformer(tolower))
tmc <- tm_map(tmc, stemDocument)

tdm <- TermDocumentMatrix(tmc, control = list(wordLengths = c(2, Inf)))
inspect(tmc[5])
findFreqTerms(tdm, lowfreq = 10)

tdm2 <- removeSparseTerms(tdm, sparse = 0.95)
tdm2_mat <- as.matrix(tdm2)
dist_mat <- dist(scale(tdm2_mat))

fit <- hclust(dist_mat, method = "ward.D2")
plot(fit, cex = 0.9, hang = -1, main = "Hierarchical Word Cluster Dendrogram")
rect.hclust(fit, k = 5)

